#[cfg(test)]
mod tests {
    use crate::{
        self as eggplant,
        artifact::{
            ArtifactDslFieldKind, BinaryArtifactCodecError, BinaryArtifactCompatibilityPolicy,
            BinaryArtifactIoError, BinaryArtifactPayloadCodec, BinaryArtifactPayloadKind,
            build_persisted_snapshot_v2_eqclass, compare_persisted_snapshot_to_current,
            current_dsl_schema_manifest, dsl_metadata_fingerprint, dsl_runtime_fingerprint,
            engine_schema_fingerprint, persisted_snapshot_capability_summary,
            read_binary_artifact_header, read_binary_artifact_header_from_file,
        },
        tx_rx_vt_pr,
    };
    use eggplant::prelude::*;
    use eggplant::wrap::{
        ActionSampleEvent, ActionSampleRecorder, EgglogEnumVariantTy, SchemaFieldKind,
    };
    use std::collections::BTreeMap;
    use std::sync::{
        Arc, Mutex,
        atomic::{AtomicBool, Ordering},
    };

    #[eggplant::dsl]
    enum Expr {
        #[cost(3)]
        Const { num: i64 },
    }
    #[eggplant::dsl]
    enum GraphRoot {
        Root { node: Expr },
    }
    #[eggplant::dsl]
    enum DisplayMath {
        #[display("{x} + {f}")]
        #[typst("diff({x}, {f})")]
        #[precedence(5)]
        MDiff {
            x: DisplayMath,
            f: DisplayMath,
        },
        #[display("integ {f} {x}")]
        #[typst("integral({f}, {x})")]
        MIntegral {
            f: DisplayMath,
            x: DisplayMath,
        },
        MLeaf {
            n: i64,
        },
    }
    #[eggplant::dsl]
    enum PrecedenceExpr {
        #[typst("{name}")]
        Var { name: String },
        #[typst("{lhs} + {rhs}")]
        #[precedence(10)]
        Add {
            lhs: PrecedenceExpr,
            rhs: PrecedenceExpr,
        },
        #[typst("{lhs} * {rhs}")]
        #[precedence(20)]
        Mul {
            lhs: PrecedenceExpr,
            rhs: PrecedenceExpr,
        },
    }
    tx_rx_vt_pr!(MyTx, MyPatRec);
    // bind pattern recorder for MyTx

    #[eggplant::pat_vars]
    struct MyPatternVars<PR: PatRecSgl> {
        expr: Expr<PR>,
    }
    fn my_pat<PR: PatRecSgl>() -> MyPatternVars<PR> {
        let expr_var = Expr::query_leaf();
        let _root = GraphRoot::query(&expr_var);
        MyPatternVars::new(expr_var)
    }

    #[eggplant::relation(typst = "edge({src}, {dst})", precedence = 40)]
    struct RelEdge {
        src: i64,
        dst: i64,
    }

    #[eggplant::relation]
    struct RelPath {
        src: i64,
        dst: i64,
    }

    #[eggplant::dsl]
    enum RelPerson {
        Human { id: i64 },
    }

    #[derive(Debug, Clone, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
    #[eggplant::base_ty]
    struct PersistedUserBase {
        n: i64,
    }

    #[derive(Debug, Clone, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
    #[eggplant::base_ty]
    struct HookedPersistedUserBase {
        n: i64,
    }

    static HOOKED_PERSISTED_USER_BASE_RESTORE_HOOK: eggplant::wrap::SerdeJsonUserBaseSortHook<
        HookedPersistedUserBase,
    > = eggplant::wrap::SerdeJsonUserBaseSortHook::new("test-hooked-json-object");

    inventory::submit! {
        eggplant::wrap::PersistedSnapshotUserBaseSortHookRegistration::new(
            "HookedPersistedUserBase",
            &HOOKED_PERSISTED_USER_BASE_RESTORE_HOOK,
        )
    }

    impl<T: eggplant::wrap::NodeDropperSgl, V: EgglogEnumVariantTy> std::fmt::Debug
        for RelPerson<T, V>
    {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}", self.cur_sym())
        }
    }

    #[eggplant::relation]
    struct RelOwns {
        owner: RelPerson,
        item_id: i64,
    }

    #[eggplant::relation]
    struct RelFriend {
        left: RelPerson,
        right: RelPerson,
    }

    #[allow(non_camel_case_types)]
    #[eggplant::func(output = bool, no_merge)]
    struct rel_path_mark {
        src: i64,
        dst: i64,
    }

    #[allow(non_camel_case_types)]
    #[eggplant::func(output = bool, no_merge)]
    struct rel_owns_mark {
        owner_id: i64,
        item_id: i64,
    }

    #[allow(non_camel_case_types)]
    #[eggplant::func(output = bool, no_merge)]
    struct rel_friend_mark {
        left_id: i64,
        right_id: i64,
    }

    #[test]
    fn pattern_test() {
        env_logger::init();
        let root = Root::<MyTx>::new(&Const::new(3));
        root.commit();

        let ruleset = MyTx::new_ruleset("my_rule_set");
        let executed = Arc::new(Mutex::new(false));
        let cloned_flag = executed.clone();
        MyTx::add_rule("my_rule", ruleset, my_pat, move |_ctx, my_pattern_vars| {
            println!("{:?}", my_pattern_vars.expr);
            let mut locked = cloned_flag.lock().unwrap();
            *locked = true;
        });
        MyTx::run_ruleset(ruleset, RunConfig::Once);
        assert_eq!(*executed.lock().unwrap(), true);
    }

    #[test]
    fn typed_relation_api_supports_seed_query_and_action_insert() {
        let _ = env_logger::builder().is_test(true).try_init();

        tx_rx_vt_pr!(RelTx, RelPatRec);

        RelEdge::<RelTx>::insert(1, 2);
        RelEdge::<RelTx>::insert(2, 3);

        let ruleset = RelTx::new_ruleset("typed_relation_path");
        RelTx::add_rule(
            "seed_path",
            ruleset,
            || {
                let edge = RelEdge::query();
                #[eggplant::pat_vars_catch]
                struct Pat {
                    edge: RelEdge,
                }
            },
            |ctx, pat| {
                let src = ctx.devalue(pat.edge.src);
                let dst = ctx.devalue(pat.edge.dst);
                ctx.insert_rel_path(src, dst);
                ctx.set_rel_path_mark(src, dst, true);
            },
        );
        RelTx::add_rule(
            "extend_path",
            ruleset,
            || {
                let path = RelPath::query();
                let edge = RelEdge::query();
                let join = path.handle_dst().eq(&edge.handle_src());
                #[eggplant::pat_vars]
                struct Pat {
                    path: RelPath,
                    edge: RelEdge,
                }
                Pat::new(path, edge).assert(join)
            },
            |ctx, pat| {
                let src = ctx.devalue(pat.path.src);
                let dst = ctx.devalue(pat.edge.dst);
                ctx.insert_rel_path(src, dst);
                ctx.set_rel_path_mark(src, dst, true);
            },
        );

        let report = RelTx::run_ruleset(ruleset, RunConfig::Sat);

        assert!(
            report
                .num_matches_per_rule
                .get("@seed_path")
                .copied()
                .unwrap_or(0)
                > 0
        );
        assert!(
            report
                .num_matches_per_rule
                .get("@extend_path")
                .copied()
                .unwrap_or(0)
                > 0
        );
        assert_eq!(rel_path_mark::<RelTx>::get((&1, &2)), true);
        assert_eq!(rel_path_mark::<RelTx>::get((&1, &3)), true);
    }

    #[allow(non_camel_case_types)]
    #[eggplant::func(output = bool, no_merge)]
    struct rel_handle_sugar_mark {
        src: i64,
        dst: i64,
    }

    #[eggplant::relation]
    struct RelTransferOwns {
        owner: RelPerson,
        item_id: i64,
    }

    #[eggplant::relation]
    struct RelTransferRequest {
        new_owner: RelPerson,
        item_id: i64,
    }

    #[test]
    fn typed_relation_query_exposes_handle_field_sugar() {
        let _ = env_logger::builder().is_test(true).try_init();

        tx_rx_vt_pr!(RelHandleTx, RelHandlePatRec);

        RelEdge::<RelHandleTx>::insert(11, 13);
        RelEdge::<RelHandleTx>::insert(17, 5);

        let ruleset = RelHandleTx::new_ruleset("typed_relation_handle_sugar");
        RelHandleTx::add_rule(
            "mark_increasing_edge",
            ruleset,
            || {
                let edge = RelEdge::query();
                let increasing = edge.handle_src().lt(&edge.handle_dst());
                #[eggplant::pat_vars]
                struct Pat {
                    edge: RelEdge,
                }
                Pat::new(edge).assert(increasing)
            },
            |ctx, pat| {
                let src = ctx.devalue(pat.edge.src);
                let dst = ctx.devalue(pat.edge.dst);
                ctx.set_rel_handle_sugar_mark(src, dst, true);
            },
        );

        let report = RelHandleTx::run_ruleset(ruleset, RunConfig::Sat);
        assert_eq!(
            report
                .num_matches_per_rule
                .get("@mark_increasing_edge")
                .copied()
                .unwrap_or(0),
            1
        );
        assert_eq!(rel_handle_sugar_mark::<RelHandleTx>::get((&11, &13)), true);
    }

    #[test]
    fn typed_relation_query_accepts_explicit_complex_args_for_mixed_relations() {
        let _ = env_logger::builder().is_test(true).try_init();

        tx_rx_vt_pr!(RelMixedTx, RelMixedPatRec);

        let ruleset = RelMixedTx::new_ruleset("typed_relation_mixed_query");
        RelMixedTx::add_rule(
            "seed_owns",
            ruleset,
            || {
                #[eggplant::pat_vars_catch]
                struct Pat {}
            },
            |ctx, _| {
                let alice = ctx.insert_human(1);
                ctx.insert_rel_owns(alice, 7);
            },
        );
        RelMixedTx::add_rule(
            "mark_owns",
            ruleset,
            || {
                let owner = Human::query();
                let owns = RelOwns::query(&owner);
                #[eggplant::pat_vars]
                struct Pat {
                    owns: RelOwns,
                }
                Pat::new(owns)
            },
            |ctx, _| {
                ctx.set_rel_owns_mark(1, 7, true);
            },
        );

        let report = RelMixedTx::run_ruleset(ruleset, RunConfig::Sat);
        assert_eq!(
            report
                .num_matches_per_rule
                .get("@mark_owns")
                .copied()
                .unwrap_or(0),
            1
        );
        assert_eq!(rel_owns_mark::<RelMixedTx>::get((&1, &7)), true);
    }

    #[test]
    fn typed_relation_query_accepts_explicit_complex_args_for_all_complex_relations() {
        let _ = env_logger::builder().is_test(true).try_init();

        tx_rx_vt_pr!(RelComplexTx, RelComplexPatRec);

        let ruleset = RelComplexTx::new_ruleset("typed_relation_all_complex_query");
        RelComplexTx::add_rule(
            "seed_friend",
            ruleset,
            || {
                #[eggplant::pat_vars_catch]
                struct Pat {}
            },
            |ctx, _| {
                let alice = ctx.insert_human(1);
                let bob = ctx.insert_human(2);
                ctx.insert_rel_friend(alice, bob);
            },
        );
        RelComplexTx::add_rule(
            "mark_friend",
            ruleset,
            || {
                let left = Human::query();
                let right = Human::query();
                let friend = RelFriend::query(&left, &right);
                let friend_left_matches = friend.left.handle().eq(&left.handle());
                let friend_right_matches = friend.right.handle().eq(&right.handle());
                #[eggplant::pat_vars]
                struct Pat {
                    left: Human,
                    right: Human,
                    friend: RelFriend,
                }
                Pat::new(left, right, friend)
                    .assert(friend_left_matches)
                    .assert(friend_right_matches)
            },
            |ctx, pat| {
                let left_id = ctx.devalue(pat.left.id);
                let right_id = ctx.devalue(pat.right.id);
                ctx.set_rel_friend_mark(left_id, right_id, true);
            },
        );

        let report = RelComplexTx::run_ruleset(ruleset, RunConfig::Sat);
        assert_eq!(
            report
                .num_matches_per_rule
                .get("@mark_friend")
                .copied()
                .unwrap_or(0),
            1
        );
        assert_eq!(rel_friend_mark::<RelComplexTx>::get((&1, &2)), true);
    }

    #[test]
    fn typed_relation_supports_complex_and_base_fields_during_transfer() {
        let _ = env_logger::builder().is_test(true).try_init();

        const ALICE_ID: i64 = 1;
        const BOB_ID: i64 = 2;
        const CAROL_ID: i64 = 3;
        const RING_ID: i64 = 1001;
        const BOOK_ID: i64 = 2002;

        tx_rx_vt_pr!(RelTransferTx, RelTransferPatRec);

        let ruleset = RelTransferTx::new_ruleset("typed_relation_transfer");
        RelTransferTx::add_rule(
            "seed_story",
            ruleset,
            || {
                #[eggplant::pat_vars_catch]
                struct Unit {}
            },
            |ctx, _| {
                let alice = ctx.insert_human(ALICE_ID);
                let bob = ctx.insert_human(BOB_ID);
                let carol = ctx.insert_human(CAROL_ID);
                ctx.insert_rel_transfer_owns(alice, RING_ID);
                ctx.insert_rel_transfer_owns(carol, BOOK_ID);
                ctx.insert_rel_transfer_request(bob, RING_ID);
            },
        );
        RelTransferTx::add_rule(
            "mark_ownership",
            ruleset,
            || {
                let owner = Human::query();
                let owns = RelTransferOwns::query(&owner);
                let same_owner = owns.owner.handle().eq(&owner.handle());
                #[eggplant::pat_vars]
                struct Pat {
                    owns: RelTransferOwns,
                    owner: Human,
                }
                Pat::new(owns, owner).assert(same_owner)
            },
            |_ctx, _pat| {},
        );
        RelTransferTx::add_rule(
            "apply_transfer",
            ruleset,
            || {
                let current_owner = Human::query();
                let owns = RelTransferOwns::query(&current_owner);
                let new_owner = Human::query();
                let request = RelTransferRequest::query(&new_owner);
                let same_current_owner = owns.owner.handle().eq(&current_owner.handle());
                let same_new_owner = request.new_owner.handle().eq(&new_owner.handle());
                let same_item = owns.handle_item_id().eq(&request.handle_item_id());
                let owner_changes = current_owner.handle().ne(&new_owner.handle());
                #[eggplant::pat_vars]
                struct Pat {
                    owns: RelTransferOwns,
                    current_owner: Human,
                    request: RelTransferRequest,
                    new_owner: Human,
                }
                Pat::new(owns, current_owner, request, new_owner)
                    .assert(same_current_owner)
                    .assert(same_new_owner)
                    .assert(same_item)
                    .assert(owner_changes)
            },
            |ctx, pat| {
                let item_id = ctx.devalue(pat.owns.item_id);
                ctx.insert_rel_transfer_owns(pat.new_owner, item_id);
            },
        );

        let report = RelTransferTx::run_ruleset(ruleset, RunConfig::Sat);
        assert!(
            report
                .num_matches_per_rule
                .get("@mark_ownership")
                .copied()
                .unwrap_or(0)
                >= 3
        );
        assert_eq!(
            report
                .num_matches_per_rule
                .get("@apply_transfer")
                .copied()
                .unwrap_or(0),
            1
        );
    }

    #[test]
    fn dsl_display_template_metadata_smoke() {
        assert_eq!(
            <MDiffTy as EgglogEnumVariantTy>::DISPLAY_TEMPLATE,
            Some("{x} + {f}")
        );
        assert_eq!(
            <MDiffTy as EgglogEnumVariantTy>::TYPST_TEMPLATE,
            Some("diff({x}, {f})")
        );
        assert_eq!(<MDiffTy as EgglogEnumVariantTy>::PRECEDENCE, 5);
        assert_eq!(
            <MIntegralTy as EgglogEnumVariantTy>::DISPLAY_TEMPLATE,
            Some("integ {f} {x}")
        );
        assert_eq!(
            <MIntegralTy as EgglogEnumVariantTy>::TYPST_TEMPLATE,
            Some("integral({f}, {x})")
        );
        assert_eq!(<MLeafTy as EgglogEnumVariantTy>::DISPLAY_TEMPLATE, None);
        assert_eq!(<MLeafTy as EgglogEnumVariantTy>::TYPST_TEMPLATE, None);
        assert_eq!(<MLeafTy as EgglogEnumVariantTy>::PRECEDENCE, u16::MAX);
    }

    #[test]
    fn dsl_typst_precedence_render_smoke() {
        let x = RenderedTemplateField::atom("x");
        let y = RenderedTemplateField::atom("y");
        let z = RenderedTemplateField::atom("z");

        let add_xy =
            render_variant_typst::<AddTy>(&[("lhs", x.clone()), ("rhs", y.clone())]).unwrap();
        let mul_yz =
            render_variant_typst::<MulTy>(&[("lhs", y.clone()), ("rhs", z.clone())]).unwrap();
        let add_x_mul_yz = render_variant_typst::<AddTy>(&[
            (
                "lhs",
                RenderedTemplateField::new("x", <VarTy as EgglogEnumVariantTy>::PRECEDENCE),
            ),
            (
                "rhs",
                RenderedTemplateField::new(mul_yz, <MulTy as EgglogEnumVariantTy>::PRECEDENCE),
            ),
        ])
        .unwrap();
        let mul_add_xy_z = render_variant_typst::<MulTy>(&[
            (
                "lhs",
                RenderedTemplateField::new(add_xy, <AddTy as EgglogEnumVariantTy>::PRECEDENCE),
            ),
            (
                "rhs",
                RenderedTemplateField::new("z", <VarTy as EgglogEnumVariantTy>::PRECEDENCE),
            ),
        ])
        .unwrap();

        assert_eq!(<AddTy as EgglogEnumVariantTy>::PRECEDENCE, 10);
        assert_eq!(<MulTy as EgglogEnumVariantTy>::PRECEDENCE, 20);
        assert_eq!(add_x_mul_yz, "x + y * z");
        assert_eq!(mul_add_xy_z, "(x + y) * z");
    }

    #[test]
    fn serialized_artifact_matches_current_schema() {
        let artifact = {
            let egraph_handle = MyTx::egraph();
            let egraph = egraph_handle.lock().unwrap();
            build_serialized_eggplant_artifact(&egraph, egglog::SerializeConfig::default()).unwrap()
        };
        let report = {
            let egraph_handle = MyTx::egraph();
            let egraph = egraph_handle.lock().unwrap();
            compare_artifact_to_current(&artifact, &egraph)
        };

        assert!(report.typed_continuation_allowed);
        assert!(report.issues.is_empty());
    }

    #[test]
    fn changing_dsl_metadata_is_metadata_only_change() {
        let mut artifact = {
            let egraph_handle = MyTx::egraph();
            let egraph = egraph_handle.lock().unwrap();
            build_serialized_eggplant_artifact(&egraph, egglog::SerializeConfig::default()).unwrap()
        };

        let runtime_before = dsl_runtime_fingerprint(&artifact.dsl_schema).unwrap();
        let metadata_before = dsl_metadata_fingerprint(&artifact.dsl_schema).unwrap();
        let variant = artifact
            .dsl_schema
            .variants
            .iter_mut()
            .find(|variant| variant.owner_ty == "DisplayMath" && variant.variant_name == "MDiff")
            .unwrap();
        variant.fields[0].name = "renamed_x".to_string();
        variant.display_template = Some("{renamed_x} :: {f}".to_string());
        variant.typst_template = Some("pair({renamed_x}, {f})".to_string());
        variant.precedence = 99;

        assert_eq!(
            runtime_before,
            dsl_runtime_fingerprint(&artifact.dsl_schema).unwrap()
        );
        assert_ne!(
            metadata_before,
            dsl_metadata_fingerprint(&artifact.dsl_schema).unwrap()
        );

        let report = {
            let egraph_handle = MyTx::egraph();
            let egraph = egraph_handle.lock().unwrap();
            compare_artifact_to_current(&artifact, &egraph)
        };
        assert!(report.typed_continuation_allowed);
        assert!(
            report
                .issues
                .iter()
                .any(|issue| issue.layer == ArtifactSchemaLayer::DslMetadata && !issue.blocking)
        );
        assert!(!report.issues.iter().any(|issue| issue.blocking));
    }

    #[test]
    fn changing_dsl_field_kind_blocks_typed_continuation() {
        let mut artifact = {
            let egraph_handle = MyTx::egraph();
            let egraph = egraph_handle.lock().unwrap();
            build_serialized_eggplant_artifact(&egraph, egglog::SerializeConfig::default()).unwrap()
        };

        let variant = artifact
            .dsl_schema
            .variants
            .iter_mut()
            .find(|variant| variant.owner_ty == "DisplayMath" && variant.variant_name == "MLeaf")
            .unwrap();
        variant.fields[0].kind = ArtifactDslFieldKind::Container;

        let report = {
            let egraph_handle = MyTx::egraph();
            let egraph = egraph_handle.lock().unwrap();
            compare_artifact_to_current(&artifact, &egraph)
        };
        assert!(!report.typed_continuation_allowed);
        assert!(
            report
                .issues
                .iter()
                .any(|issue| issue.layer == ArtifactSchemaLayer::DslRuntime && issue.blocking)
        );
    }

    #[test]
    fn schema_header_captures_variant_metadata() {
        let header = ArtifactSchemaHeader::current();
        let mdiff = header
            .dsl
            .variants
            .iter()
            .find(|variant| variant.name == "MDiff" && variant.output_sort == "DisplayMath")
            .expect("MDiff variant should be present in DSL schema manifest");

        assert_eq!(mdiff.field_names, vec!["x".to_string(), "f".to_string()]);
        assert_eq!(
            mdiff.field_kinds,
            vec![SchemaFieldKind::Complex, SchemaFieldKind::Complex]
        );
        assert_eq!(mdiff.display_template.as_deref(), Some("{x} + {f}"));
        assert_eq!(mdiff.typst_template.as_deref(), Some("diff({x}, {f})"));
        assert_eq!(mdiff.precedence, 5);
    }

    #[test]
    fn serialized_artifact_exposes_variant_precedence_and_typst_templates_without_expansion() {
        let artifact = {
            let egraph_handle = MyTx::egraph();
            let egraph = egraph_handle.lock().unwrap();
            build_serialized_eggplant_artifact(&egraph, egglog::SerializeConfig::default()).unwrap()
        };
        let add = artifact
            .dsl_schema
            .variants
            .iter()
            .find(|variant| variant.owner_ty == "PrecedenceExpr" && variant.variant_name == "Add")
            .expect("Add variant should be present in serialized DSL schema");

        assert_eq!(add.precedence, 10);
        assert_eq!(add.typst_template.as_deref(), Some("{lhs} + {rhs}"));
        assert!(
            add.typst_template
                .as_deref()
                .is_some_and(|template| template.contains("{lhs}") && template.contains("{rhs}")),
            "serialized typst metadata should preserve placeholders rather than pre-rendering per-node strings"
        );
    }

    #[test]
    fn serialized_artifact_json_keeps_variant_precedence_and_typst_template_fields() {
        let artifact = {
            let egraph_handle = MyTx::egraph();
            let egraph = egraph_handle.lock().unwrap();
            build_serialized_eggplant_artifact(&egraph, egglog::SerializeConfig::default()).unwrap()
        };
        let json = serde_json::to_value(&artifact).unwrap();
        let add = json["dsl_schema"]["variants"]
            .as_array()
            .unwrap()
            .iter()
            .find(|variant| {
                variant["owner_ty"].as_str() == Some("PrecedenceExpr")
                    && variant["variant_name"].as_str() == Some("Add")
            })
            .expect("Add variant should be present in artifact json");

        assert_eq!(add["precedence"].as_u64(), Some(10));
        assert_eq!(add["typst_template"].as_str(), Some("{lhs} + {rhs}"));
    }

    #[test]
    fn serialized_artifact_envelope_checked_json_load_allows_metadata_only_change() {
        let mut envelope = SerializedArtifactEnvelope::new("payload".to_string());
        let mdiff = envelope
            .schema
            .dsl
            .variants
            .iter_mut()
            .find(|variant| variant.name == "MDiff" && variant.output_sort == "DisplayMath")
            .expect("MDiff variant should be present in DSL schema manifest");
        mdiff.field_names = vec!["lhs".to_string(), "rhs".to_string()];
        mdiff.display_template = Some("{lhs} :: {rhs}".to_string());
        mdiff.typst_template = Some("pair({lhs}, {rhs})".to_string());
        mdiff.precedence = 99;
        envelope.schema.refresh_fingerprints();

        let json = envelope.to_json_string().unwrap();
        let loaded = SerializedArtifactEnvelope::<String>::from_json_str_checked(&json).unwrap();
        assert_eq!(loaded.payload, "payload");
        assert!(loaded.compatibility_with_current().continuation_allowed);
        assert!(!loaded.compatibility_with_current().dsl.exact_match);
    }

    #[test]
    fn serialized_artifact_envelope_checked_json_load_rejects_runtime_mismatch() {
        let mut envelope = SerializedArtifactEnvelope::new("payload".to_string());
        let mdiff = envelope
            .schema
            .dsl
            .variants
            .iter_mut()
            .find(|variant| variant.name == "MDiff" && variant.output_sort == "DisplayMath")
            .expect("MDiff variant should be present in DSL schema manifest");
        mdiff.field_kinds[0] = SchemaFieldKind::Base;

        let json = envelope.to_json_string().unwrap();
        let err = SerializedArtifactEnvelope::<String>::from_json_str_checked(&json).unwrap_err();
        assert!(format!("{err}").contains("dsl schema diff"));
    }

    #[test]
    fn serialized_artifact_envelope_checked_json_load_rejects_format_version_mismatch() {
        let mut envelope = SerializedArtifactEnvelope::new("payload".to_string());
        envelope.schema.format_version += 1;

        let json = envelope.to_json_string().unwrap();
        let err = SerializedArtifactEnvelope::<String>::from_json_str_checked(&json).unwrap_err();
        assert!(format!("{err}").contains("format mismatch"));
    }

    fn temp_binary_path(stem: &str) -> std::path::PathBuf {
        let nanos = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos();
        std::env::temp_dir().join(format!("{stem}_{nanos}.egbin"))
    }

    #[test]
    fn serialized_artifact_binary_round_trip_preserves_payload() {
        let artifact = {
            let egraph_handle = MyTx::egraph();
            let egraph = egraph_handle.lock().unwrap();
            build_serialized_eggplant_artifact(&egraph, egglog::SerializeConfig::default()).unwrap()
        };

        let bytes = artifact.to_binary_vec().unwrap();
        let loaded = SerializedEggplantArtifact::from_binary_slice(&bytes).unwrap();

        assert_eq!(loaded, artifact);
    }

    #[test]
    fn serialized_artifact_binary_header_exposes_contract_fields() {
        let artifact = {
            let egraph_handle = MyTx::egraph();
            let egraph = egraph_handle.lock().unwrap();
            build_serialized_eggplant_artifact(&egraph, egglog::SerializeConfig::default()).unwrap()
        };

        let bytes = artifact.to_binary_vec().unwrap();
        let header = read_binary_artifact_header(&bytes).unwrap();

        assert_eq!(
            header.payload_kind,
            BinaryArtifactPayloadKind::SerializedEggplantArtifact
        );
        assert_eq!(
            header.payload_codec,
            BinaryArtifactPayloadCodec::MessagePack
        );
        assert_eq!(
            header.payload_format,
            EGGPLANT_SERIALIZED_ARTIFACT_FORMAT.to_string()
        );
        assert_eq!(header.payload_version, artifact.format_version);
        assert_eq!(header.payload_profile, None);
        assert_eq!(
            header.compatibility_policy,
            BinaryArtifactCompatibilityPolicy::ArtifactTypedContinuationOrViewerFallback
        );
    }

    #[test]
    fn serialized_artifact_binary_file_io_round_trip_preserves_payload() {
        let artifact = {
            let egraph_handle = MyTx::egraph();
            let egraph = egraph_handle.lock().unwrap();
            build_serialized_eggplant_artifact(&egraph, egglog::SerializeConfig::default()).unwrap()
        };
        let path = temp_binary_path("serialized_artifact_round_trip");

        artifact.write_binary_file(&path).unwrap();
        let header = read_binary_artifact_header_from_file(&path).unwrap();
        let loaded = SerializedEggplantArtifact::read_binary_file(&path).unwrap();
        let _ = std::fs::remove_file(&path);

        assert_eq!(
            header.payload_kind,
            BinaryArtifactPayloadKind::SerializedEggplantArtifact
        );
        assert_eq!(loaded, artifact);
    }

    #[test]
    fn persisted_snapshot_binary_round_trip_preserves_payload() {
        MyTx::reset_for_bench();
        let root = Root::<MyTx>::new(&Const::new(17));
        root.commit();
        RelEdge::<MyTx>::insert(4, 5);

        let snapshot = {
            let egraph_handle = MyTx::egraph();
            let egraph = egraph_handle.lock().unwrap();
            build_persisted_snapshot_v2_eqclass(&egraph, egglog::SerializeConfig::default())
        };

        let bytes = snapshot.to_binary_vec().unwrap();
        let loaded = PersistedSnapshot::from_binary_slice(&bytes).unwrap();

        assert_eq!(loaded, snapshot);
    }

    #[test]
    fn persisted_snapshot_binary_header_exposes_contract_fields() {
        MyTx::reset_for_bench();
        let root = Root::<MyTx>::new(&Const::new(19));
        root.commit();

        let snapshot = {
            let egraph_handle = MyTx::egraph();
            let egraph = egraph_handle.lock().unwrap();
            build_persisted_snapshot_v2_eqclass(&egraph, egglog::SerializeConfig::default())
        };

        let bytes = snapshot.to_binary_vec().unwrap();
        let header = read_binary_artifact_header(&bytes).unwrap();

        assert_eq!(
            header.payload_kind,
            BinaryArtifactPayloadKind::PersistedSnapshot
        );
        assert_eq!(
            header.payload_codec,
            BinaryArtifactPayloadCodec::MessagePack
        );
        assert_eq!(header.payload_format, snapshot.format);
        assert_eq!(header.payload_version, snapshot.snapshot_version);
        assert_eq!(header.payload_profile, Some(snapshot.profile));
        assert_eq!(
            header.compatibility_policy,
            BinaryArtifactCompatibilityPolicy::PersistedSnapshotSourceSchemaAwareRestore
        );
    }

    #[test]
    fn persisted_snapshot_binary_file_io_round_trip_preserves_payload() {
        MyTx::reset_for_bench();
        let root = Root::<MyTx>::new(&Const::new(29));
        root.commit();

        let snapshot = {
            let egraph_handle = MyTx::egraph();
            let egraph = egraph_handle.lock().unwrap();
            build_persisted_snapshot_v1(&egraph, egglog::SerializeConfig::default())
        };
        let path = temp_binary_path("persisted_snapshot_round_trip");

        snapshot.write_binary_file(&path).unwrap();
        let header = read_binary_artifact_header_from_file(&path).unwrap();
        let loaded = PersistedSnapshot::read_binary_file(&path).unwrap();
        let _ = std::fs::remove_file(&path);

        assert_eq!(
            header.payload_kind,
            BinaryArtifactPayloadKind::PersistedSnapshot
        );
        assert_eq!(loaded, snapshot);
    }

    #[test]
    fn binary_codec_rejects_payload_kind_mismatch() {
        MyTx::reset_for_bench();
        let root = Root::<MyTx>::new(&Const::new(23));
        root.commit();

        let snapshot = {
            let egraph_handle = MyTx::egraph();
            let egraph = egraph_handle.lock().unwrap();
            build_persisted_snapshot_v1(&egraph, egglog::SerializeConfig::default())
        };
        let bytes = snapshot.to_binary_vec().unwrap();

        let err = SerializedEggplantArtifact::from_binary_slice(&bytes).unwrap_err();
        assert!(matches!(
            err,
            BinaryArtifactCodecError::PayloadKindMismatch {
                expected: BinaryArtifactPayloadKind::SerializedEggplantArtifact,
                actual: BinaryArtifactPayloadKind::PersistedSnapshot,
            }
        ));
    }

    #[test]
    fn binary_file_io_rejects_payload_kind_mismatch() {
        MyTx::reset_for_bench();
        let root = Root::<MyTx>::new(&Const::new(31));
        root.commit();

        let snapshot = {
            let egraph_handle = MyTx::egraph();
            let egraph = egraph_handle.lock().unwrap();
            build_persisted_snapshot_v1(&egraph, egglog::SerializeConfig::default())
        };
        let path = temp_binary_path("binary_kind_mismatch");
        snapshot.write_binary_file(&path).unwrap();

        let err = SerializedEggplantArtifact::read_binary_file(&path).unwrap_err();
        let _ = std::fs::remove_file(&path);

        assert!(matches!(
            err,
            BinaryArtifactIoError::Codec(BinaryArtifactCodecError::PayloadKindMismatch {
                expected: BinaryArtifactPayloadKind::SerializedEggplantArtifact,
                actual: BinaryArtifactPayloadKind::PersistedSnapshot,
            })
        ));
    }

    #[test]
    fn artifact_format_version_mismatch_blocks_typed_continuation() {
        let mut artifact = {
            let egraph_handle = MyTx::egraph();
            let egraph = egraph_handle.lock().unwrap();
            build_serialized_eggplant_artifact(&egraph, egglog::SerializeConfig::default()).unwrap()
        };
        artifact.format_version += 1;

        let report = {
            let egraph_handle = MyTx::egraph();
            let egraph = egraph_handle.lock().unwrap();
            compare_artifact_to_current(&artifact, &egraph)
        };

        assert!(!report.typed_continuation_allowed);
        assert!(
            report
                .issues
                .iter()
                .any(|issue| issue.layer == ArtifactSchemaLayer::Artifact && issue.blocking)
        );
    }

    #[test]
    fn persisted_snapshot_v1_exports_common_path_rows() {
        let root_a = Root::<MyTx>::new(&Const::new(7));
        let root_b = Root::<MyTx>::new(&Const::new(9));
        root_a.commit();
        root_b.commit();

        let snapshot = {
            let egraph_handle = MyTx::egraph();
            let egraph = egraph_handle.lock().unwrap();
            build_persisted_snapshot_v1(&egraph, egglog::SerializeConfig::default())
        };

        assert_eq!(
            snapshot.format,
            EGGPLANT_PERSISTED_SNAPSHOT_FORMAT.to_string()
        );
        assert_eq!(
            snapshot.snapshot_version,
            EGGPLANT_PERSISTED_SNAPSHOT_VERSION
        );
        assert_eq!(
            snapshot.profile,
            EGGPLANT_PERSISTED_SNAPSHOT_PROFILE.to_string()
        );
        assert!(!snapshot.schema.sort_decls.is_empty());
        assert!(!snapshot.schema.constructor_decls.is_empty());
        assert!(!snapshot.state.function_rows.is_empty());
        assert!(!snapshot.restore_mapping.value_ids.is_empty());
        assert!(
            snapshot
                .diagnostics
                .iter()
                .any(|diag| diag.path.as_deref() == Some("state.unions"))
        );
        assert!(
            snapshot
                .diagnostics
                .iter()
                .any(|diag| diag.path.as_deref() == Some("state.runs"))
        );
    }

    #[test]
    fn persisted_snapshot_v1_keeps_base_literals_and_logical_refs_separate() {
        let root = Root::<MyTx>::new(&Const::new(11));
        root.commit();

        let snapshot = {
            let egraph_handle = MyTx::egraph();
            let egraph = egraph_handle.lock().unwrap();
            build_persisted_snapshot_v1(&egraph, egglog::SerializeConfig::default())
        };

        let const_decl = snapshot
            .schema
            .constructor_decls
            .iter()
            .find(|decl| decl.name == "Const")
            .unwrap();
        let const_row = snapshot
            .state
            .function_rows
            .iter()
            .find(|row| row.op_id == const_decl.op_id)
            .unwrap();

        assert!(matches!(
            const_row.inputs.first().unwrap(),
            PersistedSnapshotValue::Lit { .. }
        ));
        assert!(matches!(
            const_row.output,
            PersistedSnapshotValue::Ref { .. }
        ));
    }

    #[test]
    fn persisted_snapshot_v1_restore_replays_constructor_rows() {
        MyTx::reset_for_bench();

        let root_a = Root::<MyTx>::new(&Const::new(7));
        let root_b = Root::<MyTx>::new(&Const::new(9));
        root_a.commit();
        root_b.commit();

        let before = {
            let egraph_handle = MyTx::egraph();
            let egraph = egraph_handle.lock().unwrap();
            build_persisted_snapshot_v1(&egraph, egglog::SerializeConfig::default())
        };

        MyTx::reset_for_bench();
        let report = {
            let egraph_handle = MyTx::egraph();
            let mut egraph = egraph_handle.lock().unwrap();
            restore_persisted_snapshot_v1(&mut egraph, &before).unwrap()
        };
        let after = {
            let egraph_handle = MyTx::egraph();
            let egraph = egraph_handle.lock().unwrap();
            build_persisted_snapshot_v1(&egraph, egglog::SerializeConfig::default())
        };

        let row_count_by_op = |snapshot: &PersistedSnapshot| {
            let mut counts = BTreeMap::<usize, usize>::new();
            for row in &snapshot.state.function_rows {
                *counts.entry(row.op_id).or_default() += 1;
            }
            counts
        };

        assert_eq!(report.restored_facts, 0);
        assert_eq!(
            report.restored_function_rows,
            before.state.function_rows.len()
        );
        assert_eq!(
            row_count_by_op(&after),
            row_count_by_op(&before),
            "restored constructor rows should match exported counts by op"
        );
    }

    #[test]
    fn persisted_snapshot_v1_restore_replays_relation_facts() {
        tx_rx_vt_pr!(RelRestoreTx, RelRestorePatRec);
        RelRestoreTx::reset_for_bench();

        RelEdge::<RelRestoreTx>::insert(1, 2);
        RelEdge::<RelRestoreTx>::insert(2, 3);

        let before = {
            let egraph_handle = RelRestoreTx::egraph();
            let egraph = egraph_handle.lock().unwrap();
            build_persisted_snapshot_v1(&egraph, egglog::SerializeConfig::default())
        };

        RelRestoreTx::reset_for_bench();
        let report = {
            let egraph_handle = RelRestoreTx::egraph();
            let mut egraph = egraph_handle.lock().unwrap();
            restore_persisted_snapshot_v1(&mut egraph, &before).unwrap()
        };
        let after = {
            let egraph_handle = RelRestoreTx::egraph();
            let egraph = egraph_handle.lock().unwrap();
            build_persisted_snapshot_v1(&egraph, egglog::SerializeConfig::default())
        };

        let fact_payloads = |snapshot: &PersistedSnapshot| {
            let mut rows = snapshot
                .state
                .facts
                .iter()
                .map(|fact| {
                    let payload = fact
                        .inputs
                        .iter()
                        .map(|value| match value {
                            PersistedSnapshotValue::Lit { value, .. } => value.value.clone(),
                            PersistedSnapshotValue::Ref { logical_id, .. } => logical_id.clone(),
                        })
                        .collect::<Vec<_>>();
                    (fact.op_id, payload)
                })
                .collect::<Vec<_>>();
            rows.sort();
            rows
        };

        assert_eq!(
            report.restored_function_rows,
            before.state.function_rows.len()
        );
        assert_eq!(report.restored_facts, before.state.facts.len());
        assert_eq!(
            fact_payloads(&after),
            fact_payloads(&before),
            "restored relation facts should match exported payloads"
        );
    }

    #[test]
    fn persisted_snapshot_v1_classifies_eggplant_native_relations_as_facts() {
        tx_rx_vt_pr!(RelPolicyTx, RelPolicyPatRec);
        RelPolicyTx::reset_for_bench();

        RelEdge::<RelPolicyTx>::insert(7, 8);

        let snapshot = {
            let egraph_handle = RelPolicyTx::egraph();
            let egraph = egraph_handle.lock().unwrap();
            build_persisted_snapshot_v1(&egraph, egglog::SerializeConfig::default())
        };

        assert!(
            !snapshot.state.facts.is_empty(),
            "eggplant-native relations should serialize as facts"
        );
        assert!(
            snapshot.state.function_rows.is_empty(),
            "with only relation facts inserted, the supported eggplant-native path should not need function_rows"
        );
    }

    #[test]
    fn persisted_snapshot_v1_does_not_claim_plain_egglog_relations_as_supported_facts() {
        let mut egraph = eggplant::egglog::EGraph::default();
        egraph
            .parse_and_run_program(
                None,
                r#"
(relation edge (i64 i64))
(edge 1 2)
"#,
            )
            .unwrap();

        let snapshot = build_persisted_snapshot_v1(&egraph, egglog::SerializeConfig::default());

        assert!(
            snapshot.state.facts.is_empty(),
            "plain/non-eggplant relations should not be silently promoted into supported facts"
        );
    }

    #[test]
    fn persisted_snapshot_v1_reports_plain_source_relation_like_diagnostic() {
        let mut egraph = eggplant::egglog::EGraph::default();
        egraph
            .parse_and_run_program(
                None,
                r#"
(relation edge (i64 i64))
(edge 1 2)
"#,
            )
            .unwrap();

        let snapshot = build_persisted_snapshot_v1(&egraph, egglog::SerializeConfig::default());

        assert!(snapshot.diagnostics.iter().any(|diag| {
            diag.code == "plain-source-non-goal"
                && diag.path.as_deref() == Some("state.plain_source_relation_like.edge")
        }));
    }

    #[test]
    fn persisted_snapshot_v1_plain_source_diagnostic_does_not_fire_for_supported_rows() {
        let mut egraph = eggplant::egglog::EGraph::default();
        egraph
            .parse_and_run_program(
                None,
                r#"
(datatype Expr (Const i64))
(Const 3)
(function touch (i64) Unit :no-merge)
(set (touch 1) ())
"#,
            )
            .unwrap();

        let snapshot = build_persisted_snapshot_v1(&egraph, egglog::SerializeConfig::default());

        assert!(
            snapshot
                .diagnostics
                .iter()
                .all(|diag| diag.code != "plain-source-non-goal"),
            "supported constructor/unit-function rows should not trigger the plain-source relation diagnostic"
        );
    }

    #[test]
    fn persisted_snapshot_v1_keeps_constructor_rows_out_of_facts() {
        let mut egraph = eggplant::egglog::EGraph::default();
        egraph
            .parse_and_run_program(
                None,
                r#"
(datatype Expr (Const i64))
(Const 3)
"#,
            )
            .unwrap();

        let snapshot = build_persisted_snapshot_v1(&egraph, egglog::SerializeConfig::default());
        let const_decl = snapshot
            .schema
            .constructor_decls
            .iter()
            .find(|decl| decl.name == "Const")
            .expect("constructor decl should be exported");

        assert!(
            snapshot
                .state
                .function_rows
                .iter()
                .any(|row| row.op_id == const_decl.op_id),
            "constructors should serialize as function_rows/common-path rows"
        );
        assert!(
            snapshot
                .state
                .facts
                .iter()
                .all(|fact| fact.op_id != const_decl.op_id),
            "constructors must not be classified as relation facts"
        );
    }

    #[test]
    fn persisted_snapshot_v1_keeps_unit_returning_function_rows_out_of_facts() {
        let mut egraph = eggplant::egglog::EGraph::default();
        egraph
            .parse_and_run_program(
                None,
                r#"
(function touch (i64) Unit :no-merge)
(set (touch 1) ())
"#,
            )
            .unwrap();

        let snapshot =
            build_persisted_snapshot_v1(&egraph, eggplant::egglog::SerializeConfig::default());
        let touch_decl = snapshot
            .schema
            .function_decls
            .iter()
            .find(|decl| decl.name == "touch")
            .expect("touch function decl should be exported");

        assert!(
            snapshot
                .state
                .function_rows
                .iter()
                .any(|row| row.op_id == touch_decl.op_id),
            "unit-returning functions should stay in function_rows"
        );
        assert!(
            snapshot
                .state
                .facts
                .iter()
                .all(|fact| fact.op_id != touch_decl.op_id),
            "unit-returning functions should not be downgraded into relation facts"
        );
    }

    inventory::submit! {
        eggplant::wrap::Decl::EgglogRelationTy {
            name: "PersistedMetaEdgePlain",
            input: &["i64", "i64"],
            typst_template: Some("edge({src}, {dst})"),
            precedence: 40,
        }
    }

    #[eggplant::func(output = bool, no_merge, typst = "touch({x})", precedence = 70)]
    struct persisted_meta_touch {
        x: i64,
    }

    #[test]
    fn persisted_snapshot_v1_keeps_decl_typst_and_precedence_metadata() {
        let mut constructor_egraph = eggplant::egglog::EGraph::default();
        constructor_egraph
            .parse_and_run_program(
                None,
                r#"
(datatype PrecedenceExpr
  (Var String)
  (Add PrecedenceExpr PrecedenceExpr)
  (Mul PrecedenceExpr PrecedenceExpr))
(let x (Var "x"))
(let y (Var "y"))
(let z (Add x y))
"#,
            )
            .unwrap();
        let constructor_snapshot =
            build_persisted_snapshot_v1(&constructor_egraph, egglog::SerializeConfig::default());
        let add_decl = constructor_snapshot
            .schema
            .constructor_decls
            .iter()
            .find(|decl| decl.name == "Add")
            .expect("Add constructor should be exported");
        let add_meta = add_decl
            .metadata
            .as_ref()
            .expect("constructor metadata should be exported");
        assert_eq!(
            add_meta.get("precedence").and_then(|value| value.as_u64()),
            Some(10)
        );
        assert_eq!(
            add_meta
                .get("typst_template")
                .and_then(|value| value.as_str()),
            Some("{lhs} + {rhs}")
        );

        tx_rx_vt_pr!(PersistedMetaTx, PersistedMetaPatRec);
        PersistedMetaTx::reset_for_bench();
        RelEdge::<PersistedMetaTx>::insert(1, 2);
        let relation_snapshot = {
            let egraph_handle = PersistedMetaTx::egraph();
            let egraph = egraph_handle.lock().unwrap();
            build_persisted_snapshot_v1(&egraph, egglog::SerializeConfig::default())
        };
        let relation_decl = relation_snapshot
            .schema
            .function_decls
            .iter()
            .find(|decl| decl.name == "RelEdge")
            .expect("relation decl should be exported");
        let relation_meta = relation_decl
            .metadata
            .as_ref()
            .expect("relation metadata should be exported");
        assert_eq!(
            relation_meta
                .get("precedence")
                .and_then(|value| value.as_u64()),
            Some(40)
        );
        assert_eq!(
            relation_meta
                .get("typst_template")
                .and_then(|value| value.as_str()),
            Some("edge({src}, {dst})")
        );

        let mut egraph = eggplant::egglog::EGraph::default();
        egraph
            .parse_and_run_program(
                None,
                r#"
(function persisted_meta_touch (i64) bool :no-merge)
(set (persisted_meta_touch 1) false)
"#,
            )
            .unwrap();
        let snapshot = build_persisted_snapshot_v1(&egraph, egglog::SerializeConfig::default());

        let func_decl = snapshot
            .schema
            .function_decls
            .iter()
            .find(|decl| {
                decl.metadata
                    .as_ref()
                    .and_then(|meta| meta.get("typst_template"))
                    .and_then(|value| value.as_str())
                    == Some("touch({x})")
            })
            .expect("func decl should be exported");
        let func_meta = func_decl
            .metadata
            .as_ref()
            .expect("func metadata should be exported");
        assert_eq!(
            func_meta.get("precedence").and_then(|value| value.as_u64()),
            Some(70)
        );
        assert_eq!(
            func_meta
                .get("typst_template")
                .and_then(|value| value.as_str()),
            Some("touch({x})")
        );
    }

    #[test]
    fn relation_decl_attrs_capture_typst_and_precedence_metadata() {
        let relation_decl = inventory::iter::<eggplant::wrap::Decl>
            .into_iter()
            .find_map(|decl| match decl {
                eggplant::wrap::Decl::EgglogRelationTy {
                    name,
                    typst_template,
                    precedence,
                    ..
                } if *name == "PersistedMetaEdgePlain" => Some((typst_template, precedence)),
                _ => None,
            })
            .expect("relation decl should be registered in inventory");

        assert_eq!(*relation_decl.0, Some("edge({src}, {dst})"));
        assert_eq!(*relation_decl.1, 40);
    }

    #[test]
    fn persisted_snapshot_json_keeps_decl_typst_and_precedence_metadata_fields() {
        let mut constructor_egraph = eggplant::egglog::EGraph::default();
        constructor_egraph
            .parse_and_run_program(
                None,
                r#"
(datatype PrecedenceExpr
  (Var String)
  (Add PrecedenceExpr PrecedenceExpr)
  (Mul PrecedenceExpr PrecedenceExpr))
(let x (Var "x"))
(let y (Var "y"))
(let z (Add x y))
"#,
            )
            .unwrap();
        let constructor_snapshot =
            build_persisted_snapshot_v1(&constructor_egraph, egglog::SerializeConfig::default());
        let constructor_json = serde_json::to_value(&constructor_snapshot).unwrap();
        let add_decl = constructor_json["schema"]["constructor_decls"]
            .as_array()
            .unwrap()
            .iter()
            .find(|decl| decl["name"].as_str() == Some("Add"))
            .expect("Add constructor should be present in persisted snapshot json");
        assert_eq!(add_decl["metadata"]["precedence"].as_u64(), Some(10));
        assert_eq!(
            add_decl["metadata"]["typst_template"].as_str(),
            Some("{lhs} + {rhs}")
        );

        tx_rx_vt_pr!(PersistedMetaJsonTx, PersistedMetaJsonPatRec);
        PersistedMetaJsonTx::reset_for_bench();
        RelEdge::<PersistedMetaJsonTx>::insert(1, 2);
        let relation_snapshot = {
            let egraph_handle = PersistedMetaJsonTx::egraph();
            let egraph = egraph_handle.lock().unwrap();
            build_persisted_snapshot_v1(&egraph, egglog::SerializeConfig::default())
        };
        let relation_json = serde_json::to_value(&relation_snapshot).unwrap();
        let relation_decl = relation_json["schema"]["function_decls"]
            .as_array()
            .unwrap()
            .iter()
            .find(|decl| decl["name"].as_str() == Some("RelEdge"))
            .expect("relation decl should be present in persisted snapshot json");
        assert_eq!(relation_decl["metadata"]["precedence"].as_u64(), Some(40));
        assert_eq!(
            relation_decl["metadata"]["typst_template"].as_str(),
            Some("edge({src}, {dst})")
        );

        let mut func_egraph = eggplant::egglog::EGraph::default();
        func_egraph
            .parse_and_run_program(
                None,
                r#"
(function persisted_meta_touch (i64) bool :no-merge)
(set (persisted_meta_touch 1) false)
"#,
            )
            .unwrap();
        let func_snapshot =
            build_persisted_snapshot_v1(&func_egraph, egglog::SerializeConfig::default());
        let func_json = serde_json::to_value(&func_snapshot).unwrap();
        let func_decl = func_json["schema"]["function_decls"]
            .as_array()
            .unwrap()
            .iter()
            .find(|decl| decl["name"].as_str() == Some("persisted_meta_touch"))
            .expect("function decl should be present in persisted snapshot json");
        assert_eq!(func_decl["metadata"]["precedence"].as_u64(), Some(70));
        assert_eq!(
            func_decl["metadata"]["typst_template"].as_str(),
            Some("touch({x})")
        );
    }

    #[test]
    fn persisted_snapshot_v1_restore_supports_bigrat_literals() {
        let mut seeded = eggplant::egglog::EGraph::default();
        seeded
            .parse_and_run_program(
                None,
                r#"
(datatype RatWrap (RatBox BigRat))
(RatBox (bigrat (bigint 1) (bigint 2)))
"#,
            )
            .unwrap();

        let snapshot =
            build_persisted_snapshot_v1(&seeded, eggplant::egglog::SerializeConfig::default());
        let rat_decl = snapshot
            .schema
            .constructor_decls
            .iter()
            .find(|decl| decl.name == "RatBox")
            .expect("RatBox constructor decl should be exported");
        let rat_row = snapshot
            .state
            .function_rows
            .iter()
            .find(|row| row.op_id == rat_decl.op_id)
            .expect("RatBox row should be exported");

        assert!(matches!(
            rat_row.inputs.first().unwrap(),
            PersistedSnapshotValue::Lit { value, .. }
                if value.machine_value.is_some()
        ));

        let mut restored = eggplant::egglog::EGraph::default();
        restored
            .parse_and_run_program(
                None,
                r#"
(datatype RatWrap (RatBox BigRat))
"#,
            )
            .unwrap();

        restore_persisted_snapshot_v1(&mut restored, &snapshot).unwrap();
        let restored_snapshot =
            build_persisted_snapshot_v1(&restored, eggplant::egglog::SerializeConfig::default());

        let row_count_by_op = |snapshot: &PersistedSnapshot| {
            let mut counts = BTreeMap::<usize, usize>::new();
            for row in &snapshot.state.function_rows {
                *counts.entry(row.op_id).or_default() += 1;
            }
            counts
        };

        assert_eq!(
            row_count_by_op(&restored_snapshot),
            row_count_by_op(&snapshot),
            "BigRat constructor rows should round-trip through persisted snapshot restore"
        );
    }

    #[test]
    fn persisted_snapshot_v1_restore_rejects_non_fresh_target() {
        MyTx::reset_for_bench();
        let root = Root::<MyTx>::new(&Const::new(7));
        root.commit();

        let snapshot = {
            let egraph_handle = MyTx::egraph();
            let egraph = egraph_handle.lock().unwrap();
            build_persisted_snapshot_v1(&egraph, egglog::SerializeConfig::default())
        };

        let mut target = eggplant::egglog::EGraph::default();
        target
            .parse_and_run_program(
                None,
                r#"
(datatype Expr (Const i64))
(let x (Const 1))
"#,
            )
            .unwrap();

        let err = restore_persisted_snapshot_v1(&mut target, &snapshot).unwrap_err();
        assert!(
            matches!(err, PersistedSnapshotRestoreError::TargetNotFresh(_)),
            "restore should reject non-fresh target egraphs"
        );
    }

    #[test]
    fn persisted_snapshot_v1_persists_ruleset_name_provenance_only() {
        MyTx::reset_for_bench();
        let ruleset = MyTx::new_ruleset("persisted_snapshot_ruleset_name");
        MyTx::add_rule(
            "persisted_snapshot_ruleset_name_rule",
            ruleset,
            || {
                let expr = Expr::query_leaf();
                let root = Root::query(&expr);
                #[eggplant::pat_vars_catch]
                struct Pat {
                    root: Root,
                }
            },
            |_ctx, _pat| {},
        );

        let snapshot = {
            let egraph_handle = MyTx::egraph();
            let egraph = egraph_handle.lock().unwrap();
            build_persisted_snapshot_v1(&egraph, egglog::SerializeConfig::default())
        };

        assert!(
            snapshot
                .dictionary
                .rulesets
                .iter()
                .any(|name| name == "persisted_snapshot_ruleset_name")
        );
        assert!(
            snapshot
                .schema
                .ruleset_decls
                .iter()
                .any(|decl| decl.name == "persisted_snapshot_ruleset_name")
        );
        assert!(
            snapshot.diagnostics.iter().any(|diag| {
                diag.path.as_deref() == Some("schema.ruleset_decls")
                    && diag.message.contains("provenance-only")
            }),
            "ruleset declarations should be explicitly documented as provenance-only metadata"
        );
    }

    #[test]
    fn persisted_snapshot_v1_restore_ignores_ruleset_name_provenance() {
        MyTx::reset_for_bench();
        let ruleset = MyTx::new_ruleset("persisted_snapshot_restore_ignores_ruleset_name");
        MyTx::add_rule(
            "persisted_snapshot_restore_ignores_ruleset_name_rule",
            ruleset,
            || {
                let expr = Expr::query_leaf();
                let root = Root::query(&expr);
                #[eggplant::pat_vars_catch]
                struct Pat {
                    root: Root,
                }
            },
            |_ctx, _pat| {},
        );
        let root = Root::<MyTx>::new(&Const::new(5));
        root.commit();

        let snapshot = {
            let egraph_handle = MyTx::egraph();
            let egraph = egraph_handle.lock().unwrap();
            build_persisted_snapshot_v1(&egraph, egglog::SerializeConfig::default())
        };

        MyTx::reset_for_bench();
        let report = {
            let egraph_handle = MyTx::egraph();
            let mut egraph = egraph_handle.lock().unwrap();
            restore_persisted_snapshot_v1(&mut egraph, &snapshot).unwrap()
        };

        assert_eq!(report.restored_facts, snapshot.state.facts.len());
        assert_eq!(
            report.restored_function_rows,
            snapshot.state.function_rows.len()
        );
    }

    #[test]
    fn persisted_snapshot_v1_captures_source_schema_alignment_header() {
        MyTx::reset_for_bench();
        let root = Root::<MyTx>::new(&Const::new(13));
        root.commit();

        let (snapshot, current_engine_fingerprint) = {
            let egraph_handle = MyTx::egraph();
            let egraph = egraph_handle.lock().unwrap();
            (
                build_persisted_snapshot_v1(&egraph, egglog::SerializeConfig::default()),
                engine_schema_fingerprint(&crate::artifact::current_engine_schema_manifest(
                    &egraph,
                ))
                .unwrap(),
            )
        };
        let source_schema = snapshot
            .source_schema
            .as_ref()
            .expect("v1 snapshots should carry source schema alignment header");
        let current_dsl = current_dsl_schema_manifest();

        assert_eq!(
            source_schema.engine_fingerprint, current_engine_fingerprint,
            "snapshot source schema should carry the producer engine fingerprint"
        );
        assert_eq!(
            source_schema.dsl_runtime_fingerprint,
            dsl_runtime_fingerprint(&current_dsl).unwrap()
        );
        assert_eq!(
            source_schema.dsl_metadata_fingerprint,
            dsl_metadata_fingerprint(&current_dsl).unwrap()
        );
        assert_eq!(source_schema.macro_rev, current_dsl.macro_rev);
    }

    #[test]
    fn persisted_snapshot_v1_alignment_report_allows_metadata_only_drift() {
        MyTx::reset_for_bench();
        let root = Root::<MyTx>::new(&Const::new(21));
        root.commit();

        let mut snapshot = {
            let egraph_handle = MyTx::egraph();
            let egraph = egraph_handle.lock().unwrap();
            build_persisted_snapshot_v1(&egraph, egglog::SerializeConfig::default())
        };
        snapshot
            .source_schema
            .as_mut()
            .unwrap()
            .dsl_metadata_fingerprint = "metadata-mismatch".to_string();

        let report = {
            let egraph_handle = MyTx::egraph();
            let egraph = egraph_handle.lock().unwrap();
            compare_persisted_snapshot_to_current(&snapshot, &egraph)
        };

        assert!(report.restore_schema_compatible);
        assert!(!report.exact_source_match);
        assert!(!report.dsl_metadata_fingerprint_matches);
        assert!(report.issues.iter().any(|issue| {
            issue.layer == crate::artifact::PersistedSnapshotAlignmentLayer::DslMetadata
                && !issue.blocking
        }));
    }

    #[test]
    fn persisted_snapshot_v1_restore_rejects_runtime_alignment_mismatch() {
        MyTx::reset_for_bench();
        let root = Root::<MyTx>::new(&Const::new(34));
        root.commit();

        let mut snapshot = {
            let egraph_handle = MyTx::egraph();
            let egraph = egraph_handle.lock().unwrap();
            build_persisted_snapshot_v1(&egraph, egglog::SerializeConfig::default())
        };
        snapshot
            .source_schema
            .as_mut()
            .unwrap()
            .dsl_runtime_fingerprint = "runtime-mismatch".to_string();

        MyTx::reset_for_bench();
        let err = {
            let egraph_handle = MyTx::egraph();
            let mut egraph = egraph_handle.lock().unwrap();
            restore_persisted_snapshot_v1(&mut egraph, &snapshot).unwrap_err()
        };

        assert!(matches!(
            err,
            PersistedSnapshotRestoreError::SchemaMismatch(_)
        ));
        assert!(format!("{err}").contains("dsl runtime fingerprint mismatch"));
    }

    #[test]
    fn persisted_snapshot_user_base_sort_defaults_to_registered_without_restore_hook() {
        assert_eq!(
            eggplant::wrap::user_base_sort_restore_support("PersistedUserBase"),
            Some(eggplant::wrap::PersistedSnapshotUserBaseSortSupport::RegisteredWithoutHook)
        );
        assert!(eggplant::wrap::user_base_sort_restore_hook("PersistedUserBase").is_none());
    }

    #[test]
    fn persisted_snapshot_v1_user_base_sort_metadata_and_diagnostic_report_missing_hook() {
        let mut egraph = eggplant::egglog::EGraph::default();
        for sort in inventory::iter::<eggplant::wrap::UserBaseSort> {
            if sort.name == "PersistedUserBase" {
                (sort.sort_insert_fn)(&mut egraph);
            }
        }
        egraph
            .parse_and_run_program(
                None,
                r#"
(datatype PersistedUserBaseExpr (PersistedUserLeaf PersistedUserBase))
"#,
            )
            .unwrap();
        eggplant::egglog::prelude::run_ephemeral_rust_rule(
            &mut egraph,
            "seed_persisted_user_base_snapshot",
            &[],
            eggplant::egglog::ast::Facts(Vec::new()),
            |ctx, _| {
                let base =
                    ctx.base_to_value(eggplant::egglog::sort::Boxed::new(PersistedUserBase {
                        n: 9,
                    }));
                let _ = ctx.lookup("PersistedUserLeaf", &[base]);
                Some(())
            },
        )
        .unwrap();

        let snapshot = build_persisted_snapshot_v1(&egraph, egglog::SerializeConfig::default());
        let sort_decl = snapshot
            .schema
            .sort_decls
            .iter()
            .find(|decl| decl.name == "PersistedUserBase")
            .expect("user base sort should be exported in schema");
        let restore_meta = sort_decl
            .metadata
            .as_ref()
            .and_then(|value| value.get("persisted_snapshot_restore"))
            .expect("user base sort should carry persisted snapshot restore metadata");

        assert_eq!(
            restore_meta.get("support").and_then(|value| value.as_str()),
            Some("registered_without_hook")
        );
        assert!(
            snapshot.diagnostics.iter().any(|diag| {
                diag.code == "user-base-restore-missing-hook"
                    && diag.path.as_deref() == Some("state.literal_sorts.PersistedUserBase")
            }),
            "exporting user-base literals without a hook should produce an explicit capability diagnostic"
        );
    }

    #[test]
    fn persisted_snapshot_v1_capability_summary_reports_plain_source_non_goal() {
        let mut egraph = eggplant::egglog::EGraph::default();
        egraph
            .parse_and_run_program(
                None,
                r#"
(relation edge (i64 i64))
(edge 1 2)
"#,
            )
            .unwrap();

        let snapshot =
            build_persisted_snapshot_v1(&egraph, eggplant::egglog::SerializeConfig::default());
        let summary = persisted_snapshot_capability_summary(&snapshot);

        assert!(summary.non_goals.iter().any(|entry| {
            entry.key == "state.plain_source_relation_like.edge"
                && entry.detail.contains("plain/non-eggplant")
        }));
        assert!(
            summary
                .guaranteed_restorable
                .iter()
                .any(|entry| { entry.key == "state.function_rows" })
        );
    }

    #[test]
    fn persisted_snapshot_v1_capability_summary_reports_missing_hook_and_requirements() {
        let mut egraph = eggplant::egglog::EGraph::default();
        for sort in inventory::iter::<eggplant::wrap::UserBaseSort> {
            if sort.name == "PersistedUserBase" {
                (sort.sort_insert_fn)(&mut egraph);
            }
        }
        egraph
            .parse_and_run_program(
                None,
                r#"
(datatype PersistedUserBaseExpr (PersistedUserLeaf PersistedUserBase))
"#,
            )
            .unwrap();
        eggplant::egglog::prelude::run_ephemeral_rust_rule(
            &mut egraph,
            "seed_missing_hook_summary",
            &[],
            eggplant::egglog::ast::Facts(Vec::new()),
            |ctx, _| {
                let base =
                    ctx.base_to_value(eggplant::egglog::sort::Boxed::new(PersistedUserBase {
                        n: 5,
                    }));
                let _ = ctx.lookup("PersistedUserLeaf", &[base]);
                Some(())
            },
        )
        .unwrap();

        let snapshot =
            build_persisted_snapshot_v1(&egraph, eggplant::egglog::SerializeConfig::default());
        let summary = persisted_snapshot_capability_summary(&snapshot);

        assert!(
            summary
                .missing_hooks
                .iter()
                .any(|entry| { entry.key == "state.literal_sorts.PersistedUserBase" })
        );
        assert!(
            summary
                .required_preconditions
                .iter()
                .any(|line| { line.contains("source_schema alignment") })
        );
    }

    #[test]
    fn persisted_snapshot_v1_capability_summary_reports_hooked_user_base_as_restorable() {
        let mut egraph = eggplant::egglog::EGraph::default();
        for sort in inventory::iter::<eggplant::wrap::UserBaseSort> {
            if sort.name == "HookedPersistedUserBase" {
                (sort.sort_insert_fn)(&mut egraph);
            }
        }
        egraph
            .parse_and_run_program(
                None,
                r#"
(datatype HookedPersistedUserBaseExpr (HookedPersistedUserLeaf HookedPersistedUserBase))
"#,
            )
            .unwrap();
        eggplant::egglog::prelude::run_ephemeral_rust_rule(
            &mut egraph,
            "seed_hooked_summary",
            &[],
            eggplant::egglog::ast::Facts(Vec::new()),
            |ctx, _| {
                let base = ctx.base_to_value(eggplant::egglog::sort::Boxed::new(
                    HookedPersistedUserBase { n: 8 },
                ));
                let _ = ctx.lookup("HookedPersistedUserLeaf", &[base]);
                Some(())
            },
        )
        .unwrap();

        let snapshot =
            build_persisted_snapshot_v1(&egraph, eggplant::egglog::SerializeConfig::default());
        let summary = persisted_snapshot_capability_summary(&snapshot);

        assert!(summary.guaranteed_restorable.iter().any(|entry| {
            entry.key == "state.literal_sorts.HookedPersistedUserBase"
                && entry.detail.contains("test-hooked-json-object")
        }));
        assert!(summary.missing_hooks.is_empty());
    }

    #[test]
    fn persisted_snapshot_v1_user_base_sort_hook_round_trips() {
        let mut seeded = eggplant::egglog::EGraph::default();
        for sort in inventory::iter::<eggplant::wrap::UserBaseSort> {
            if sort.name == "HookedPersistedUserBase" {
                (sort.sort_insert_fn)(&mut seeded);
            }
        }
        seeded
            .parse_and_run_program(
                None,
                r#"
(datatype HookedPersistedUserBaseExpr (HookedPersistedUserLeaf HookedPersistedUserBase))
"#,
            )
            .unwrap();
        eggplant::egglog::prelude::run_ephemeral_rust_rule(
            &mut seeded,
            "seed_hooked_user_base_snapshot",
            &[],
            eggplant::egglog::ast::Facts(Vec::new()),
            |ctx, _| {
                let base = ctx.base_to_value(eggplant::egglog::sort::Boxed::new(
                    HookedPersistedUserBase { n: 17 },
                ));
                let _ = ctx.lookup("HookedPersistedUserLeaf", &[base]);
                Some(())
            },
        )
        .unwrap();

        let snapshot =
            build_persisted_snapshot_v1(&seeded, eggplant::egglog::SerializeConfig::default());
        let sort_decl = snapshot
            .schema
            .sort_decls
            .iter()
            .find(|decl| decl.name == "HookedPersistedUserBase")
            .expect("hooked user base sort should be exported in schema");
        let restore_meta = sort_decl
            .metadata
            .as_ref()
            .and_then(|value| value.get("persisted_snapshot_restore"))
            .expect("hooked user base sort should carry restore metadata");

        assert_eq!(
            restore_meta.get("support").and_then(|value| value.as_str()),
            Some("registered_with_hook")
        );
        assert_eq!(
            restore_meta
                .get("capability_label")
                .and_then(|value| value.as_str()),
            Some("test-hooked-json-object")
        );
        assert!(
            snapshot
                .diagnostics
                .iter()
                .all(|diag| diag.code != "user-base-restore-missing-hook"),
            "hooked user base sort should not report missing-hook diagnostics"
        );

        let mut restored = eggplant::egglog::EGraph::default();
        for sort in inventory::iter::<eggplant::wrap::UserBaseSort> {
            if sort.name == "HookedPersistedUserBase" {
                (sort.sort_insert_fn)(&mut restored);
            }
        }
        restored
            .parse_and_run_program(
                None,
                r#"
(datatype HookedPersistedUserBaseExpr (HookedPersistedUserLeaf HookedPersistedUserBase))
"#,
            )
            .unwrap();

        restore_persisted_snapshot_v1(&mut restored, &snapshot).unwrap();
        let restored_snapshot =
            build_persisted_snapshot_v1(&restored, egglog::SerializeConfig::default());

        let row_count_by_op = |snapshot: &PersistedSnapshot| {
            let mut counts = BTreeMap::<usize, usize>::new();
            for row in &snapshot.state.function_rows {
                *counts.entry(row.op_id).or_default() += 1;
            }
            counts
        };

        assert_eq!(
            row_count_by_op(&restored_snapshot),
            row_count_by_op(&snapshot),
            "hook-backed user base rows should round-trip through persisted snapshot restore"
        );
    }

    #[test]
    fn persisted_snapshot_v1_user_base_sort_hook_rejects_invalid_machine_payload() {
        let mut seeded = eggplant::egglog::EGraph::default();
        for sort in inventory::iter::<eggplant::wrap::UserBaseSort> {
            if sort.name == "HookedPersistedUserBase" {
                (sort.sort_insert_fn)(&mut seeded);
            }
        }
        seeded
            .parse_and_run_program(
                None,
                r#"
(datatype HookedPersistedUserBaseExpr (HookedPersistedUserLeaf HookedPersistedUserBase))
"#,
            )
            .unwrap();
        eggplant::egglog::prelude::run_ephemeral_rust_rule(
            &mut seeded,
            "seed_hooked_user_base_invalid_payload",
            &[],
            eggplant::egglog::ast::Facts(Vec::new()),
            |ctx, _| {
                let base = ctx.base_to_value(eggplant::egglog::sort::Boxed::new(
                    HookedPersistedUserBase { n: 23 },
                ));
                let _ = ctx.lookup("HookedPersistedUserLeaf", &[base]);
                Some(())
            },
        )
        .unwrap();
        let mut snapshot =
            build_persisted_snapshot_v1(&seeded, eggplant::egglog::SerializeConfig::default());
        let row = snapshot
            .state
            .function_rows
            .iter_mut()
            .find(|row| !row.inputs.is_empty())
            .expect("hooked snapshot should have a constructor row");
        let PersistedSnapshotValue::Lit { value, .. } = &mut row.inputs[0] else {
            panic!("hooked user base input should export as a literal payload");
        };
        value.machine_value = Some(serde_json::json!({ "bad": true }));

        let mut restored = eggplant::egglog::EGraph::default();
        for sort in inventory::iter::<eggplant::wrap::UserBaseSort> {
            if sort.name == "HookedPersistedUserBase" {
                (sort.sort_insert_fn)(&mut restored);
            }
        }
        restored
            .parse_and_run_program(
                None,
                r#"
(datatype HookedPersistedUserBaseExpr (HookedPersistedUserLeaf HookedPersistedUserBase))
"#,
            )
            .unwrap();

        let err = restore_persisted_snapshot_v1(&mut restored, &snapshot).unwrap_err();
        assert!(matches!(
            err,
            PersistedSnapshotRestoreError::UnsupportedLiteral { .. }
        ));
    }

    #[test]
    fn persisted_snapshot_v1_restore_rejects_missing_source_schema_header() {
        MyTx::reset_for_bench();
        let root = Root::<MyTx>::new(&Const::new(55));
        root.commit();

        let mut snapshot = {
            let egraph_handle = MyTx::egraph();
            let egraph = egraph_handle.lock().unwrap();
            build_persisted_snapshot_v1(&egraph, egglog::SerializeConfig::default())
        };
        snapshot.source_schema = None;

        MyTx::reset_for_bench();
        let err = {
            let egraph_handle = MyTx::egraph();
            let mut egraph = egraph_handle.lock().unwrap();
            restore_persisted_snapshot_v1(&mut egraph, &snapshot).unwrap_err()
        };

        assert!(matches!(
            err,
            PersistedSnapshotRestoreError::SchemaMismatch(_)
        ));
        assert!(format!("{err}").contains("requires producer/source alignment proof"));
    }

    #[test]
    fn persisted_snapshot_v2_eqclass_uses_distinct_profile_and_version() {
        MyTx::reset_for_bench();
        let root = Root::<MyTx>::new(&Const::new(3));
        root.commit();

        let snapshot = {
            let egraph_handle = MyTx::egraph();
            let egraph = egraph_handle.lock().unwrap();
            build_persisted_snapshot_v2_eqclass(&egraph, egglog::SerializeConfig::default())
        };

        assert_eq!(
            snapshot.profile,
            crate::artifact::EGGPLANT_PERSISTED_SNAPSHOT_V2_EQCLASS_PROFILE
        );
        assert_eq!(
            snapshot.snapshot_version,
            crate::artifact::EGGPLANT_PERSISTED_SNAPSHOT_V2_EQCLASS_VERSION
        );
        assert!(snapshot.eq_class_payload.is_some());
    }

    #[test]
    fn persisted_snapshot_v2_eqclass_payload_is_inspect_only_and_groups_members() {
        let mut egraph = eggplant::egglog::EGraph::default();
        egraph
            .parse_and_run_program(
                None,
                r#"
(datatype Expr (Const i64) (Alias Expr))
(union (Const 7) (Alias (Const 7)))
"#,
            )
            .unwrap();

        let snapshot = build_persisted_snapshot_v2_eqclass(
            &egraph,
            eggplant::egglog::SerializeConfig::default(),
        );
        let payload = snapshot
            .eq_class_payload
            .as_ref()
            .expect("v2 eq-class snapshot should carry eq-class payload");

        assert_eq!(
            payload.semantics,
            crate::artifact::PersistedSnapshotEqClassSemantics::InspectOnly
        );
        assert!(payload.classes.iter().any(|class| class.members.len() >= 2));
    }

    #[test]
    fn persisted_snapshot_v2_restore_ignores_eqclass_payload_for_semantic_replay() {
        MyTx::reset_for_bench();
        let root = Root::<MyTx>::new(&Const::new(11));
        root.commit();
        RelEdge::<MyTx>::insert(1, 2);

        let snapshot = {
            let egraph_handle = MyTx::egraph();
            let egraph = egraph_handle.lock().unwrap();
            build_persisted_snapshot_v2_eqclass(&egraph, egglog::SerializeConfig::default())
        };

        MyTx::reset_for_bench();
        let report = {
            let egraph_handle = MyTx::egraph();
            let mut egraph = egraph_handle.lock().unwrap();
            restore_persisted_snapshot_v1(&mut egraph, &snapshot).unwrap()
        };

        assert_eq!(report.restored_facts, snapshot.state.facts.len());
        assert_eq!(
            report.restored_function_rows,
            snapshot.state.function_rows.len()
        );
    }

    #[test]
    fn persisted_snapshot_v2_restore_rejects_missing_eqclass_payload() {
        MyTx::reset_for_bench();
        let root = Root::<MyTx>::new(&Const::new(12));
        root.commit();

        let mut snapshot = {
            let egraph_handle = MyTx::egraph();
            let egraph = egraph_handle.lock().unwrap();
            build_persisted_snapshot_v2_eqclass(&egraph, egglog::SerializeConfig::default())
        };
        snapshot.eq_class_payload = None;

        MyTx::reset_for_bench();
        let err = {
            let egraph_handle = MyTx::egraph();
            let mut egraph = egraph_handle.lock().unwrap();
            restore_persisted_snapshot_v1(&mut egraph, &snapshot).unwrap_err()
        };

        assert!(matches!(
            err,
            PersistedSnapshotRestoreError::UnsupportedSnapshotFeature(_)
        ));
        assert!(format!("{err}").contains("require eq_class_payload"));
    }

    #[eggplant::dsl]
    enum FuncS {
        SConst { n: i64 },
    }
    #[eggplant::dsl]
    enum FuncE {
        EConst { n: i64 },
    }
    #[eggplant::dsl]
    enum SampleExpr {
        TraceConst { n: i64 },
        TraceAdd { lhs: SampleExpr, rhs: SampleExpr },
    }
    #[eggplant::dsl]
    enum SampleRoot {
        TraceRoot { node: SampleExpr },
    }
    inventory::submit! {
        eggplant::wrap::Decl::EgglogFuncTy {
            name: "manual-hidden-let-binding-func",
            input: &["FuncS"],
            output: "FuncE",
            merge: None,
            hidden: true,
            let_binding: true,
            typst_template: None,
            precedence: u16::MAX,
        }
    }
    #[eggplant::func(output=FuncE)]
    struct MAccumQ {
        s: FuncS,
    }
    tx_rx_vt_pr!(SampleTx, SamplePatRec);

    #[eggplant::pat_vars]
    struct SamplePatternVars<PR: PatRecSgl> {
        expr: SampleExpr<PR>,
    }
    fn sample_pat<PR: PatRecSgl>() -> SamplePatternVars<PR> {
        let expr = SampleExpr::query_leaf();
        let _root = TraceRoot::query(&expr);
        SamplePatternVars::new(expr)
    }

    #[test]
    fn serialized_artifact_captures_function_flags() {
        let artifact = {
            let egraph_handle = MyTx::egraph();
            let egraph = egraph_handle.lock().unwrap();
            build_serialized_eggplant_artifact(&egraph, egglog::SerializeConfig::default()).unwrap()
        };
        let func = artifact
            .engine_schema
            .functions
            .iter()
            .find(|func| func.name == "manual-hidden-let-binding-func")
            .expect("manual-hidden-let-binding-func should be present in engine schema manifest");

        assert!(func.hidden);
        assert!(func.let_binding);
    }

    #[test]
    fn func_query_pattern_smoke() {
        let _ = env_logger::builder().is_test(true).try_init();

        tx_rx_vt_pr!(MyTxFunc, MyPatRecFunc);

        let init = MyTxFunc::new_ruleset("func_query_init");
        MyTxFunc::add_rule(
            "seed_func",
            init,
            || {
                #[eggplant::pat_vars_catch]
                struct Unit {}
            },
            |ctx, _pat| {
                let s = SConst::<MyTxFunc>::new(1);
                s.commit();
                let e = EConst::<MyTxFunc>::new(2);
                e.commit();
                let sv = MyTxFunc::value(&s);
                let ev = MyTxFunc::value(&e);
                ctx.set_m_accum_q(sv, ev);
            },
        );
        MyTxFunc::run_ruleset(init, RunConfig::Once);

        let ruleset = MyTxFunc::new_ruleset("func_query_pattern");
        let hit = Arc::new(AtomicBool::new(false));
        let hit2 = Arc::clone(&hit);
        MyTxFunc::add_rule(
            "match_func_output",
            ruleset,
            || {
                let s = FuncS::query_leaf();
                let e = MAccumQ::query(&s);
                #[eggplant::pat_vars_catch]
                struct Pat {
                    s: FuncS,
                    e: FuncE,
                }
            },
            move |_ctx, _pat| {
                hit2.store(true, Ordering::SeqCst);
            },
        );
        MyTxFunc::run_ruleset(ruleset, RunConfig::Once);
        assert!(hit.load(Ordering::SeqCst));
    }

    #[test]
    fn func_ctx_query_smoke() {
        tx_rx_vt_pr!(MyTxRead, MyPatRecRead);

        #[eggplant::func(output=i64)]
        struct FibRead {
            x: i64,
        }

        let init = MyTxRead::new_ruleset("init");
        MyTxRead::add_rule(
            "init",
            init,
            || {
                #[eggplant::pat_vars_catch]
                struct Unit {}
            },
            |ctx, _pat| {
                ctx.set_fib_read(1, 1);
                ctx.set_fib_read(2, 2);
            },
        );
        MyTxRead::run_ruleset(init, RunConfig::Once);

        let use_read = MyTxRead::new_ruleset("use_read");
        MyTxRead::add_rule(
            "use_query",
            use_read,
            || {
                let x1 = FibRead::x().named("x1");
                let x2 = FibRead::x().named("x2");
                let v1 = FibRead::query(&x1);
                let v2 = FibRead::query(&x2);
                #[eggplant::pat_vars]
                struct Pat {
                    v1: i64,
                    v2: i64,
                }
                Pat::new(v1, v2)
                    .assert(x1.handle().eq(&1_i64))
                    .assert(x2.handle().eq(&2_i64))
            },
            |ctx, pat| {
                let v1 = ctx.devalue(pat.v1);
                let v2 = ctx.devalue(pat.v2);
                ctx.set_fib_read(3, v1 + v2);
            },
        );
        MyTxRead::run_ruleset(use_read, RunConfig::Once);

        assert_eq!(FibRead::<MyTxRead>::get(&3), 3);
    }

    // -------------------------------------------------------------------------
    // Upstream egglog test rewrites (positive-only)
    // Baseline: egraphs-good/egglog (see docs/egglog-upstream-suite-inventory.md)
    // -------------------------------------------------------------------------

    mod upstream_egglog_ported_smoke {
        use super::*;

        // For porting egglog relation snippets, we currently represent relations as a DSL “fact”
        // datatype (each tuple becomes a node). This keeps ported tests runnable without a native
        // relation API.
        #[eggplant::dsl]
        enum UpBoolTagFact {
            R { i: i64 },
        }

        #[allow(non_camel_case_types)]
        #[eggplant::func(output = bool, no_merge)]
        struct UpBoolTagF {
            i: i64,
        }

        #[test]
        fn upstream_primitives_egg() {
            let _ = env_logger::builder().is_test(true).try_init();

            tx_rx_vt_pr!(MyTxPrim, MyPatRecPrim);

            let ruleset = MyTxPrim::new_ruleset("upstream_primitives_egg");
            MyTxPrim::add_rule(
                "upstream_primitives_egg",
                ruleset,
                || {
                    #[eggplant::pat_vars]
                    struct Unit<PR: PatRecSgl> {}

                    let add_ok = ((&2_i64).as_handle() + (&2_i64).as_handle()).eq(&4_i64);
                    let sub_ok1 = ((&2_i64).as_handle() - (&1_i64).as_handle()).eq(&1_i64);
                    let sub_ok2 = ((&1_i64).as_handle() - (&2_i64).as_handle()).eq(&-1_i64);
                    let lt_ok = (&1_i64).as_handle().lt(&2_i64);
                    let gt_ok = (&1_i64).as_handle().gt(&-2_i64);

                    Unit::new()
                        .assert(add_ok)
                        .assert(sub_ok1)
                        .assert(sub_ok2)
                        .assert(lt_ok)
                        .assert(gt_ok)
                },
                |_ctx, _pat| {},
            );

            let report = MyTxPrim::run_ruleset(ruleset, RunConfig::Once);
            assert!(
                report
                    .num_matches_per_rule
                    .get("@upstream_primitives_egg")
                    .copied()
                    .unwrap_or(0)
                    > 0
            );
        }

        #[test]
        fn upstream_i64_to_string_egg() {
            let _ = env_logger::builder().is_test(true).try_init();

            tx_rx_vt_pr!(MyTxI64, MyPatRecI64);

            let ruleset = MyTxI64::new_ruleset("upstream_i64_to_string_egg");
            MyTxI64::add_rule(
                "upstream_i64_to_string_egg",
                ruleset,
                || {
                    #[eggplant::pat_vars]
                    struct Unit<PR: PatRecSgl> {}

                    let s = prim_call::<String>("to-string", vec![(&20_i64).into_handle_ty()]);
                    Unit::new().assert(s.eq(&"20".to_string()))
                },
                |_ctx, _pat| {},
            );

            let report = MyTxI64::run_ruleset(ruleset, RunConfig::Once);
            assert!(
                report
                    .num_matches_per_rule
                    .get("@upstream_i64_to_string_egg")
                    .copied()
                    .unwrap_or(0)
                    > 0
            );
        }

        #[test]
        fn upstream_bool_egg_primitives() {
            let _ = env_logger::builder().is_test(true).try_init();

            tx_rx_vt_pr!(MyTxBool, MyPatRecBool);

            let ruleset = MyTxBool::new_ruleset("upstream_bool_egg_primitives");
            MyTxBool::add_rule(
                "upstream_bool_egg_primitives",
                ruleset,
                || {
                    #[eggplant::pat_vars]
                    struct Unit<PR: PatRecSgl> {}

                    let and_tt = prim_call::<bool>(
                        "and",
                        vec![(&true).into_handle_ty(), (&true).into_handle_ty()],
                    )
                    .eq(&true);
                    let and_tf = prim_call::<bool>(
                        "and",
                        vec![(&true).into_handle_ty(), (&false).into_handle_ty()],
                    )
                    .eq(&false);
                    let or_tf = prim_call::<bool>(
                        "or",
                        vec![(&true).into_handle_ty(), (&false).into_handle_ty()],
                    )
                    .eq(&true);
                    let or_tf_ne_false = prim_call::<bool>(
                        "or",
                        vec![(&true).into_handle_ty(), (&false).into_handle_ty()],
                    )
                    .ne(&false);

                    let eq_11 = prim_call::<bool>(
                        "bool-=",
                        vec![(&1_i64).into_handle_ty(), (&1_i64).into_handle_ty()],
                    )
                    .eq(&true);
                    let eq_mm = prim_call::<bool>(
                        "bool-=",
                        vec![(&-5_i64).into_handle_ty(), (&-5_i64).into_handle_ty()],
                    )
                    .eq(&true);
                    let eq_13 = prim_call::<bool>(
                        "bool-=",
                        vec![(&1_i64).into_handle_ty(), (&3_i64).into_handle_ty()],
                    )
                    .eq(&false);
                    let eq_31 = prim_call::<bool>(
                        "bool-=",
                        vec![(&3_i64).into_handle_ty(), (&1_i64).into_handle_ty()],
                    )
                    .eq(&false);

                    let lt_12 = prim_call::<bool>(
                        "bool-<",
                        vec![(&1_i64).into_handle_ty(), (&2_i64).into_handle_ty()],
                    )
                    .eq(&true);
                    let lt_21 = prim_call::<bool>(
                        "bool-<",
                        vec![(&2_i64).into_handle_ty(), (&1_i64).into_handle_ty()],
                    )
                    .eq(&false);
                    let lt_11 = prim_call::<bool>(
                        "bool-<",
                        vec![(&1_i64).into_handle_ty(), (&1_i64).into_handle_ty()],
                    )
                    .eq(&false);

                    let le_12 = prim_call::<bool>(
                        "bool-<=",
                        vec![(&1_i64).into_handle_ty(), (&2_i64).into_handle_ty()],
                    )
                    .eq(&true);
                    let le_21 = prim_call::<bool>(
                        "bool-<=",
                        vec![(&2_i64).into_handle_ty(), (&1_i64).into_handle_ty()],
                    )
                    .eq(&false);
                    let le_11 = prim_call::<bool>(
                        "bool-<=",
                        vec![(&1_i64).into_handle_ty(), (&1_i64).into_handle_ty()],
                    )
                    .eq(&true);

                    let gt_12 = prim_call::<bool>(
                        "bool->",
                        vec![(&1_i64).into_handle_ty(), (&2_i64).into_handle_ty()],
                    )
                    .eq(&false);
                    let gt_21 = prim_call::<bool>(
                        "bool->",
                        vec![(&2_i64).into_handle_ty(), (&1_i64).into_handle_ty()],
                    )
                    .eq(&true);
                    let gt_11 = prim_call::<bool>(
                        "bool->",
                        vec![(&1_i64).into_handle_ty(), (&1_i64).into_handle_ty()],
                    )
                    .eq(&false);

                    let ge_12 = prim_call::<bool>(
                        "bool->=",
                        vec![(&1_i64).into_handle_ty(), (&2_i64).into_handle_ty()],
                    )
                    .eq(&false);
                    let ge_21 = prim_call::<bool>(
                        "bool->=",
                        vec![(&2_i64).into_handle_ty(), (&1_i64).into_handle_ty()],
                    )
                    .eq(&true);
                    let ge_11 = prim_call::<bool>(
                        "bool->=",
                        vec![(&1_i64).into_handle_ty(), (&1_i64).into_handle_ty()],
                    )
                    .eq(&true);

                    Unit::new()
                        .assert(and_tt)
                        .assert(and_tf)
                        .assert(or_tf)
                        .assert(or_tf_ne_false)
                        .assert(eq_11)
                        .assert(eq_mm)
                        .assert(eq_13)
                        .assert(eq_31)
                        .assert(lt_12)
                        .assert(lt_21)
                        .assert(lt_11)
                        .assert(le_12)
                        .assert(le_21)
                        .assert(le_11)
                        .assert(gt_12)
                        .assert(gt_21)
                        .assert(gt_11)
                        .assert(ge_12)
                        .assert(ge_21)
                        .assert(ge_11)
                },
                |_ctx, _pat| {},
            );

            let report = MyTxBool::run_ruleset(ruleset, RunConfig::Once);
            assert!(
                report
                    .num_matches_per_rule
                    .get("@upstream_bool_egg_primitives")
                    .copied()
                    .unwrap_or(0)
                    > 0
            );
        }

        #[test]
        fn upstream_bool_egg_tag_smoke() {
            let _ = env_logger::builder().is_test(true).try_init();

            tx_rx_vt_pr!(MyTxTag, MyPatRecTag);

            // Egglog snippet:
            // (relation R (i64))
            // (function F (i64) bool :no-merge)
            // (rule ((R i)) ((set (F i) true)))
            // (R 0) (run 3)

            let r0 = R::<MyTxTag>::new(0);
            r0.commit();

            let ruleset = MyTxTag::new_ruleset("upstream_bool_tag_rule");
            MyTxTag::add_rule(
                "upstream_bool_tag_rule",
                ruleset,
                || {
                    let r = R::query();
                    #[eggplant::pat_vars_catch]
                    struct Pat {
                        r: R,
                    }
                },
                |ctx, pat| {
                    let i = ctx.devalue(pat.r.i);
                    ctx.set_up_bool_tag_f(i, true);
                },
            );
            MyTxTag::run_ruleset(ruleset, RunConfig::Once);

            assert_eq!(UpBoolTagF::<MyTxTag>::get(&0), true);
        }
    }

    mod upstream_egglog_ported_merge {
        use super::*;

        #[eggplant::dsl]
        enum UpMergeBase {
            X {},
        }

        #[eggplant::dsl]
        enum UpMergeTree {
            Leaf { b: UpMergeBase },
            Leaf2 { b: UpMergeBase },
            C1 { a: UpMergeTree, b: UpMergeTree },
            C2 { a: UpMergeTree, b: UpMergeTree },
        }

        #[allow(non_camel_case_types)]
        #[eggplant::func(output = UpMergeTree, merge = "(C2 (C1 old new) (C2 old new))")]
        struct up_merge_f {
            x: i64,
        }

        #[test]
        fn upstream_complex_merge_func_egg() {
            let _ = env_logger::builder().is_test(true).try_init();

            tx_rx_vt_pr!(MyTxMerge, MyPatRecMerge);
            use egglog::ast::{Expr, Literal};

            // Port of `tests/complex-merge-func.egg`:
            // - function merge uses a constructor expression over `old/new`
            //
            // Upstream adds commutativity rewrites because egglog doesn't guarantee merge order
            // when there are multiple pending writes. In this port we keep the positive core
            // check: sequential `set` uses a deterministic `(old,new)` pair.

            let rebuild = MyTxMerge::new_ruleset("upstream_complex_merge_rebuild");

            let seed1 = MyTxMerge::new_ruleset("upstream_complex_merge_seed1");
            MyTxMerge::add_rule(
                "upstream_complex_merge_seed1",
                seed1,
                || {
                    #[eggplant::pat_vars_catch]
                    struct Unit {}
                },
                |ctx, _pat| {
                    let x = ctx.insert_x();
                    let leaf = ctx.insert_leaf(x);
                    ctx.set_up_merge_f(0, leaf);
                },
            );
            MyTxMerge::run_ruleset(seed1, RunConfig::Once);
            MyTxMerge::run_ruleset(rebuild, RunConfig::Once);

            {
                let egraph = MyTxMerge::egraph();
                let mut egraph = egraph.lock().unwrap();
                let sort = egraph.get_sort_by_name("UpMergeTree").unwrap().clone();

                let (_, got) = egraph
                    .eval_expr(&Expr::Call(
                        egglog::span!(),
                        "up_merge_f".to_string(),
                        vec![Expr::Lit(egglog::span!(), Literal::Int(0))],
                    ))
                    .unwrap();
                let (_, expected) = egraph
                    .eval_expr(&Expr::Call(
                        egglog::span!(),
                        "Leaf".to_string(),
                        vec![Expr::Call(egglog::span!(), "X".to_string(), vec![])],
                    ))
                    .unwrap();
                assert_eq!(
                    egraph.get_canonical_value(got, &sort),
                    egraph.get_canonical_value(expected, &sort)
                );
            }

            let seed2 = MyTxMerge::new_ruleset("upstream_complex_merge_seed2");
            MyTxMerge::add_rule(
                "upstream_complex_merge_seed2",
                seed2,
                || {
                    #[eggplant::pat_vars_catch]
                    struct Unit {}
                },
                |ctx, _pat| {
                    let x = ctx.insert_x();
                    let leaf2 = ctx.insert_leaf2(x);
                    ctx.set_up_merge_f(0, leaf2);
                },
            );
            MyTxMerge::run_ruleset(seed2, RunConfig::Once);
            MyTxMerge::run_ruleset(rebuild, RunConfig::Once);

            {
                let egraph = MyTxMerge::egraph();
                let mut egraph = egraph.lock().unwrap();
                let sort = egraph.get_sort_by_name("UpMergeTree").unwrap().clone();

                let (_, got) = egraph
                    .eval_expr(&Expr::Call(
                        egglog::span!(),
                        "up_merge_f".to_string(),
                        vec![Expr::Lit(egglog::span!(), Literal::Int(0))],
                    ))
                    .unwrap();
                let (_, expected) = egraph
                    .eval_expr(&Expr::Call(
                        egglog::span!(),
                        "C2".to_string(),
                        vec![
                            Expr::Call(
                                egglog::span!(),
                                "C1".to_string(),
                                vec![
                                    Expr::Call(
                                        egglog::span!(),
                                        "Leaf".to_string(),
                                        vec![Expr::Call(egglog::span!(), "X".to_string(), vec![])],
                                    ),
                                    Expr::Call(
                                        egglog::span!(),
                                        "Leaf2".to_string(),
                                        vec![Expr::Call(egglog::span!(), "X".to_string(), vec![])],
                                    ),
                                ],
                            ),
                            Expr::Call(
                                egglog::span!(),
                                "C2".to_string(),
                                vec![
                                    Expr::Call(
                                        egglog::span!(),
                                        "Leaf".to_string(),
                                        vec![Expr::Call(egglog::span!(), "X".to_string(), vec![])],
                                    ),
                                    Expr::Call(
                                        egglog::span!(),
                                        "Leaf2".to_string(),
                                        vec![Expr::Call(egglog::span!(), "X".to_string(), vec![])],
                                    ),
                                ],
                            ),
                        ],
                    ))
                    .unwrap();
                assert_eq!(
                    egraph.get_canonical_value(got, &sort),
                    egraph.get_canonical_value(expected, &sort)
                );
            }
        }

        #[eggplant::dsl]
        enum UpMergeN {
            Node { i: i64 },
        }

        #[allow(non_camel_case_types)]
        #[eggplant::func(output = i64, merge = "(min old new)")]
        struct up_merge_distance {
            a: UpMergeN,
            b: UpMergeN,
        }

        #[test]
        fn upstream_merge_during_rebuild_egg() {
            let _ = env_logger::builder().is_test(true).try_init();

            tx_rx_vt_pr!(MyTxDist, MyPatRecDist);
            use egglog::ast::{Expr, Literal};

            // Port of `tests/merge-during-rebuild.egg`.
            let a = Node::<MyTxDist>::new(0);
            let b = Node::<MyTxDist>::new(1);
            let x = Node::<MyTxDist>::new(2);
            let y = Node::<MyTxDist>::new(3);
            a.commit();
            b.commit();
            x.commit();
            y.commit();

            let a_v: Value<UpMergeN<(), NodeTy>> = Value::new(MyTxDist::value(&a).val);
            let b_v: Value<UpMergeN<(), NodeTy>> = Value::new(MyTxDist::value(&b).val);
            let x_v: Value<UpMergeN<(), NodeTy>> = Value::new(MyTxDist::value(&x).val);
            let y_v: Value<UpMergeN<(), NodeTy>> = Value::new(MyTxDist::value(&y).val);
            let a_v_seed = a_v.clone();
            let b_v_seed = b_v.clone();
            let x_v_seed = x_v.clone();
            let y_v_seed = y_v.clone();

            let seed = MyTxDist::new_ruleset("upstream_merge_during_rebuild_seed");
            MyTxDist::add_rule(
                "upstream_merge_during_rebuild_seed",
                seed,
                || {
                    #[eggplant::pat_vars_catch]
                    struct Unit {}
                },
                move |ctx, _pat| {
                    ctx.set_up_merge_distance(x_v_seed.clone(), y_v_seed.clone(), 1);
                    ctx.set_up_merge_distance(a_v_seed.clone(), b_v_seed.clone(), 2);
                    ctx.union(a_v_seed.clone(), x_v_seed.clone());
                    ctx.union(b_v_seed.clone(), y_v_seed.clone());
                },
            );

            // Force a rebuild/merge pass.
            let rebuild = MyTxDist::new_ruleset("upstream_merge_during_rebuild_rebuild");
            let report = MyTxDist::run_ruleset(seed, RunConfig::Once);
            assert!(report.updated);
            let report2 = MyTxDist::run_ruleset(rebuild, RunConfig::Once);
            assert!(report2.iterations.len() <= 2);

            // After unions, the min-merge should ensure the distance does not increase.
            let egraph = MyTxDist::egraph();
            let mut egraph = egraph.lock().unwrap();
            let eval_distance = |egraph: &mut egglog::EGraph, i0: i64, i1: i64| -> i64 {
                let (_, v) = egraph
                    .eval_expr(&Expr::Call(
                        egglog::span!(),
                        "up_merge_distance".to_string(),
                        vec![
                            Expr::Call(
                                egglog::span!(),
                                "Node".to_string(),
                                vec![Expr::Lit(egglog::span!(), Literal::Int(i0))],
                            ),
                            Expr::Call(
                                egglog::span!(),
                                "Node".to_string(),
                                vec![Expr::Lit(egglog::span!(), Literal::Int(i1))],
                            ),
                        ],
                    ))
                    .unwrap();
                egraph.value_to_base::<i64>(v)
            };
            assert_eq!(eval_distance(&mut egraph, 2, 3), 1);
            assert_eq!(eval_distance(&mut egraph, 0, 1), 1);
        }

        #[allow(non_camel_case_types)]
        #[eggplant::func(output = i64, merge = "(min old new)")]
        struct up_merge_saturates_foo {}

        #[test]
        fn upstream_merge_saturates_egg() {
            let _ = env_logger::builder().is_test(true).try_init();

            tx_rx_vt_pr!(MyTxSat, MyPatRecSat);

            // Port of `tests/merge-saturates.egg` (positive-only):
            // repeated `set` via rule should not cause updates when the merge keeps the old value.
            let seed = MyTxSat::new_ruleset("upstream_merge_saturates_seed0");
            MyTxSat::add_rule(
                "upstream_merge_saturates_seed0",
                seed,
                || {
                    #[eggplant::pat_vars_catch]
                    struct Unit {}
                },
                |ctx, _pat| {
                    ctx.set_up_merge_saturates_foo(0);
                },
            );
            MyTxSat::run_ruleset(seed, RunConfig::Once);

            let ruleset = MyTxSat::new_ruleset("upstream_merge_saturates_egg");
            MyTxSat::add_rule(
                "upstream_merge_saturates_egg",
                ruleset,
                || {
                    let f = up_merge_saturates_foo::query();
                    #[eggplant::pat_vars_catch]
                    struct Pat {
                        f: i64,
                    }
                },
                |ctx, _pat| {
                    // `(set (foo) 1)`; merged with `(min old new)` should preserve old=0.
                    ctx.set_up_merge_saturates_foo(1);
                },
            );

            let report = MyTxSat::run_ruleset(ruleset, RunConfig::Sat);
            assert_eq!(up_merge_saturates_foo::<MyTxSat>::get(()), 0);
            assert!(
                report
                    .num_matches_per_rule
                    .get("@upstream_merge_saturates_egg")
                    .copied()
                    .unwrap_or(0)
                    > 0
            );
            assert!(
                report.iterations.len() <= 2,
                "merge should prevent updates so saturate stops immediately"
            );
        }
    }

    mod upstream_egglog_ported_vec_set {
        use super::*;
        use egglog::ast::{Expr, Literal};

        fn int(i: i64) -> Expr {
            Expr::Lit(egglog::span!(), Literal::Int(i))
        }

        fn call(name: &str, args: Vec<Expr>) -> Expr {
            Expr::Call(egglog::span!(), name.to_string(), args)
        }

        fn assert_expr_eq(egraph: &mut egglog::EGraph, lhs: Expr, rhs: Expr) {
            let (lhs_sort, lhs_v) = egraph.eval_expr(&lhs).unwrap();
            let (rhs_sort, rhs_v) = egraph.eval_expr(&rhs).unwrap();
            assert_eq!(lhs_sort.name(), rhs_sort.name());
            assert_eq!(
                egraph.get_canonical_value(lhs_v, &lhs_sort),
                egraph.get_canonical_value(rhs_v, &lhs_sort)
            );
        }

        fn assert_expr_ne(egraph: &mut egglog::EGraph, lhs: Expr, rhs: Expr) {
            let (lhs_sort, lhs_v) = egraph.eval_expr(&lhs).unwrap();
            let (rhs_sort, rhs_v) = egraph.eval_expr(&rhs).unwrap();
            assert_eq!(lhs_sort.name(), rhs_sort.name());
            assert_ne!(
                egraph.get_canonical_value(lhs_v, &lhs_sort),
                egraph.get_canonical_value(rhs_v, &lhs_sort)
            );
        }

        #[eggplant::container]
        struct UpVecIVec {
            inner: VecContainer<i64>,
        }

        #[test]
        fn upstream_vec_egg_builtins_smoke() {
            let _ = env_logger::builder().is_test(true).try_init();

            tx_rx_vt_pr!(MyTxVecEgg, MyPatRecVecEgg);

            // Port of `tests/vec.egg` (positive-only core checks).
            let egraph = MyTxVecEgg::egraph();
            let mut egraph = egraph.lock().unwrap();

            // vec-of vs push/empty
            assert_expr_eq(
                &mut egraph,
                call("vec-of", vec![int(1), int(2)]),
                call(
                    "vec-push",
                    vec![
                        call("vec-push", vec![call("vec-empty", vec![]), int(1)]),
                        int(2),
                    ],
                ),
            );

            // vec-append
            assert_expr_eq(
                &mut egraph,
                call(
                    "vec-append",
                    vec![
                        call("vec-of", vec![int(1), int(2)]),
                        call("vec-of", vec![int(3), int(4)]),
                    ],
                ),
                call("vec-of", vec![int(1), int(2), int(3), int(4)]),
            );

            // vec-pop
            assert_expr_eq(
                &mut egraph,
                call(
                    "vec-pop",
                    vec![call("vec-of", vec![int(1), int(2), int(3)])],
                ),
                call("vec-of", vec![int(1), int(2)]),
            );

            // contains / not-contains
            // `vec-contains` / `vec-not-contains` are partial primitives (`-?> ()`): they succeed
            // with `()` when the predicate holds, and fail otherwise.
            assert!(
                egraph
                    .eval_expr(&call(
                        "vec-not-contains",
                        vec![call("vec-of", vec![int(1), int(2), int(3)]), int(4)],
                    ))
                    .is_ok()
            );
            assert!(
                egraph
                    .eval_expr(&call(
                        "vec-contains",
                        vec![call("vec-of", vec![int(1), int(2), int(3)]), int(2)],
                    ))
                    .is_ok()
            );

            // length
            let (_, v) = egraph
                .eval_expr(&call(
                    "vec-length",
                    vec![call("vec-of", vec![int(1), int(2), int(3)])],
                ))
                .unwrap();
            assert_eq!(egraph.value_to_base::<i64>(v), 3);

            // vec-get
            let (_, v) = egraph
                .eval_expr(&call(
                    "vec-get",
                    vec![call("vec-of", vec![int(1), int(2), int(3)]), int(1)],
                ))
                .unwrap();
            assert_eq!(egraph.value_to_base::<i64>(v), 2);

            // vec-set
            assert_expr_eq(
                &mut egraph,
                call(
                    "vec-set",
                    vec![call("vec-of", vec![int(1), int(2), int(3)]), int(1), int(4)],
                ),
                call("vec-of", vec![int(1), int(4), int(3)]),
            );
        }

        #[allow(non_camel_case_types)]
        #[eggplant::dsl]
        enum UpVecX {
            a {},
            b {},
        }

        #[eggplant::container]
        struct UpVecVX {
            inner: VecContainer<UpVecX>,
        }

        #[test]
        fn upstream_vec_egg_rebuild_smoke() {
            let _ = env_logger::builder().is_test(true).try_init();

            tx_rx_vt_pr!(MyTxVecRb, MyPatRecVecRb);

            // Port of `tests/vec.egg` rebuild check:
            // if elements become equal via union, the containing vectors rebuild to equal reps.
            let egraph = MyTxVecRb::egraph();
            let mut egraph = egraph.lock().unwrap();
            let p = call("vec-of", vec![call("a", vec![])]);
            let q = call("vec-of", vec![call("b", vec![])]);
            assert_expr_ne(&mut egraph, p.clone(), q.clone());
            drop(egraph);

            let seed = MyTxVecRb::new_ruleset("upstream_vec_rebuild_seed_union");
            MyTxVecRb::add_rule(
                "upstream_vec_rebuild_seed_union",
                seed,
                || {
                    #[eggplant::pat_vars_catch]
                    struct Unit {}
                },
                |ctx, _pat| {
                    let a = ctx.insert_a();
                    let b = ctx.insert_b();
                    ctx.union(a, b);
                },
            );

            // Running any ruleset triggers a rebuild pass; keep it explicit.
            let rebuild = MyTxVecRb::new_ruleset("upstream_vec_rebuild_rebuild");
            MyTxVecRb::run_ruleset(seed, RunConfig::Once);
            MyTxVecRb::run_ruleset(rebuild, RunConfig::Once);

            let egraph = MyTxVecRb::egraph();
            let mut egraph = egraph.lock().unwrap();
            assert_expr_eq(&mut egraph, p, q);
        }

        #[eggplant::container]
        struct UpSetISetBase {
            inner: SetContainer<i64>,
        }

        #[test]
        fn typed_container_primitive_helper_smoke() {
            let _ = env_logger::builder().is_test(true).try_init();

            tx_rx_vt_pr!(MyTxTypedPrim, MyPatRecTypedPrim);

            let egraph = MyTxTypedPrim::egraph();
            let mut egraph = egraph.lock().unwrap();
            let vec_push_expr = vec_empty::<UpVecIVec>()
                .vec_push(&1_i64)
                .vec_push(&2_i64)
                .to_resolved_expr(&egraph);
            let vec_of_expr = vec_of::<UpVecIVec, _, _>([&1_i64, &2_i64]).to_resolved_expr(&egraph);

            assert_expr_eq(&mut egraph, vec_push_expr, vec_of_expr);

            let set_insert_expr = set_empty::<UpSetISetBase>()
                .set_insert(&2_i64)
                .set_insert(&1_i64)
                .to_resolved_expr(&egraph);
            let set_of_expr =
                set_of::<UpSetISetBase, _, _>([&1_i64, &2_i64]).to_resolved_expr(&egraph);

            assert_expr_eq(&mut egraph, set_insert_expr, set_of_expr);

            let len_expr = set_of::<UpSetISetBase, _, _>([&1_i64, &1_i64, &2_i64])
                .set_len()
                .to_resolved_expr(&egraph);
            let (_, len) = egraph.eval_expr(&len_expr).unwrap();
            assert_eq!(egraph.value_to_base::<i64>(len), 2);
        }

        #[test]
        fn upstream_web_demo_set_egg_builtins_smoke() {
            let _ = env_logger::builder().is_test(true).try_init();

            tx_rx_vt_pr!(MyTxSetEgg, MyPatRecSetEgg);

            // Port of `tests/web-demo/set.egg` (builtin container ops + reify core).
            let egraph = MyTxSetEgg::egraph();
            let mut egraph = egraph.lock().unwrap();

            // set-of
            assert_expr_eq(
                &mut egraph,
                call("set-of", vec![int(1), int(2)]),
                call(
                    "set-insert",
                    vec![
                        call("set-insert", vec![call("set-empty", vec![]), int(1)]),
                        int(2),
                    ],
                ),
            );
            assert_expr_eq(
                &mut egraph,
                call("set-of", vec![int(1), int(2)]),
                call(
                    "set-insert",
                    vec![
                        call("set-insert", vec![call("set-empty", vec![]), int(2)]),
                        int(1),
                    ],
                ),
            );

            // set-union
            assert_expr_eq(
                &mut egraph,
                call(
                    "set-union",
                    vec![
                        call("set-of", vec![int(1), int(2)]),
                        call("set-of", vec![int(3), int(4)]),
                    ],
                ),
                call("set-of", vec![int(1), int(2), int(3), int(4)]),
            );

            // set-length
            let (_, v) = egraph
                .eval_expr(&call("set-length", vec![call("set-empty", vec![])]))
                .unwrap();
            assert_eq!(egraph.value_to_base::<i64>(v), 0);
            let (_, v) = egraph
                .eval_expr(&call(
                    "set-length",
                    vec![call("set-of", vec![int(1), int(1), int(1)])],
                ))
                .unwrap();
            assert_eq!(egraph.value_to_base::<i64>(v), 1);
            let (_, v) = egraph
                .eval_expr(&call(
                    "set-length",
                    vec![call("set-of", vec![int(1), int(-1), int(1), int(1)])],
                ))
                .unwrap();
            assert_eq!(egraph.value_to_base::<i64>(v), 2);

            // set-get
            for (idx, expected) in [(0, 1), (1, 2), (2, 4), (3, -1)] {
                let (_, got) = egraph
                    .eval_expr(&call(
                        "set-get",
                        vec![
                            call("set-of", vec![int(1), int(-1), int(2), int(4), int(1)]),
                            int(idx),
                        ],
                    ))
                    .unwrap();
                assert_eq!(egraph.value_to_base::<i64>(got), expected);
            }

            // set-remove
            assert_expr_eq(
                &mut egraph,
                call(
                    "set-remove",
                    vec![call("set-of", vec![int(1), int(2), int(3)]), int(3)],
                ),
                call("set-of", vec![int(1), int(2)]),
            );
        }

        #[test]
        fn upstream_web_demo_set_egg_reify_smoke() {
            let _ = env_logger::builder().is_test(true).try_init();

            tx_rx_vt_pr!(MyTxSetReify, MyPatRecSetReify);

            // Black-box port of the “reify set via rules” block from `tests/web-demo/set.egg`.
            // We keep this as native egglog for now because base-element container sorts are not
            // yet fully supported as first-class eggplant DSL fields/patterns.
            let program = r#"
(sort ISetBase (Set i64))

;; Reify set
(sort ISet)
(constructor IS (ISetBase) ISet)

(function ISet-get (ISet i64) i64 :no-merge)
(rule ((IS x) (> (set-length x) 0))
    ((set (ISet-get (IS x) 0) (set-get x 0))))
(rule ((ISet-get (IS x) j)
     (= i (+ j 1)) (< i (set-length x)))
    ((set (ISet-get (IS x) i) (set-get x i))))

(let $myset (IS (set-of 2 4 1 4 -1)))
(run 100)
(check (= 1 (ISet-get $myset 0)))
(check (= 2 (ISet-get $myset 1)))
(check (= 4 (ISet-get $myset 2)))
(check (= -1 (ISet-get $myset 3)))
"#;

            let egraph = MyTxSetReify::egraph();
            let mut egraph = egraph.lock().unwrap();
            let cmds = egraph.parse_program(None, program).unwrap();
            egraph.run_program(cmds).unwrap();
        }
    }

    mod fib_demo_function_query {
        use super::*;

        tx_rx_vt_pr!(MyTxFib, MyPatRecFib);

        #[allow(non_camel_case_types)]
        #[eggplant::func(output = i64, no_merge)]
        struct fib {
            x: i64,
        }

        #[eggplant::pat_vars]
        struct StepPat<PR: PatRecSgl> {
            x2: i64,
            f0: i64,
            f1: i64,
        }

        fn step_pat<PR: PatRecSgl>() -> StepPat<PR> {
            let x = fib::x();
            let x1 = fib::x().named("x1");
            let x2 = fib::x().named("x2");

            let x_plus_1 = x.handle() + (&1_i64).as_handle();
            let x_plus_2 = x.handle() + (&2_i64).as_handle();
            PR::on_new_constraint(x1.handle().eq(&x_plus_1));
            PR::on_new_constraint(x2.handle().eq(&x_plus_2));

            let f0 = fib::query(&x);
            let f1 = fib::query(&x1);

            StepPat::new(x2, f0, f1)
        }

        #[test]
        fn fib_demo_function_table_query_matches_egglog() {
            let _ = env_logger::builder().is_test(true).try_init();

            let seed = MyTxFib::new_ruleset("seed");
            MyTxFib::add_rule(
                "seed",
                seed,
                || {
                    #[eggplant::pat_vars_catch]
                    struct Unit {}
                },
                |ctx, _pat| {
                    ctx.set_fib(0, 0);
                    ctx.set_fib(1, 1);
                },
            );
            MyTxFib::run_ruleset(seed, RunConfig::Once);

            let step = MyTxFib::new_ruleset("step");
            MyTxFib::add_rule("step", step, step_pat, |ctx, pat| {
                let x2 = ctx.devalue(pat.x2);
                let f0 = ctx.devalue(pat.f0);
                let f1 = ctx.devalue(pat.f1);
                ctx.set_fib(x2, f0 + f1);
            });
            MyTxFib::run_ruleset(step, RunConfig::Times(7));

            assert_eq!(fib::<MyTxFib>::get(&7), 13);
        }
    }

    #[test]
    fn action_sample_recorder_attaches_runtime_effect_ids() {
        let _ = env_logger::builder().is_test(true).try_init();

        let root =
            TraceRoot::<SampleTx>::new(&TraceAdd::new(&TraceConst::new(2), &TraceConst::new(3)));
        root.commit();

        let ruleset = SampleTx::new_ruleset("sample_trace_rules");
        let recorder = ActionSampleRecorder::default();
        let handle = recorder.clone();
        SampleTx::add_rule_with_hook(
            "sample_trace_rule",
            ruleset,
            sample_pat,
            |ctx, pat| {
                let one = ctx.insert_trace_const(1);
                let sum = ctx.insert_trace_add(pat.expr, one);
                ctx.union(pat.expr, sum);
            },
            Box::new(recorder),
        );

        SampleTx::run_ruleset(ruleset, RunConfig::Once);

        let events = handle.snapshot();
        assert!(
            events.iter().any(|event| {
                matches!(
                    event,
                    ActionSampleEvent::Insert {
                        effect_id: Some(effect_id),
                        ..
                    } if effect_id.starts_with("effect@")
                )
            }),
            "{events:?}"
        );
        assert!(
            events.iter().any(|event| {
                matches!(
                    event,
                    ActionSampleEvent::Union {
                        effect_id: Some(effect_id),
                        ..
                    } if effect_id.starts_with("effect@")
                )
            }),
            "{events:?}"
        );
    }

    #[test]
    fn action_sample_recorder_emits_stable_event_ids_in_order() {
        let _ = env_logger::builder().is_test(true).try_init();

        let root =
            TraceRoot::<SampleTx>::new(&TraceAdd::new(&TraceConst::new(2), &TraceConst::new(3)));
        root.commit();

        let ruleset = SampleTx::new_ruleset("sample_trace_event_ids");
        let recorder = ActionSampleRecorder::default();
        let handle = recorder.clone();
        SampleTx::add_rule_with_hook(
            "sample_trace_event_ids",
            ruleset,
            sample_pat,
            |ctx, pat| {
                let one = ctx.insert_trace_const(1);
                let sum = ctx.insert_trace_add(pat.expr, one);
                ctx.union(pat.expr, sum);
            },
            Box::new(recorder),
        );

        SampleTx::run_ruleset(ruleset, RunConfig::Once);

        let trace = handle.trace();
        assert_eq!(trace.version, 1);
        assert!(trace.events.len() >= 3);
        assert!(matches!(
            trace.events.first(),
            Some(ActionSampleEvent::Insert { event_id, .. }) if event_id == "evt_0"
        ));
        assert!(matches!(
            trace.events.get(1),
            Some(ActionSampleEvent::Insert { event_id, .. }) if event_id == "evt_1"
        ));
        assert!(matches!(
            trace.events.get(2),
            Some(ActionSampleEvent::Union { event_id, .. }) if event_id == "evt_2"
        ));
    }
}

#[cfg(test)]
mod proofs_api_tests {
    use crate::{self as eggplant, instances::tx_rx_vt_pr::TxRxVTPR};
    use egglog::ast::Expr;
    use egglog::span;
    use eggplant::prelude::*;

    #[eggplant::dsl]
    pub enum ProofExpr {
        ProofConst { num: i64 },
        ProofMul { l: ProofExpr, r: ProofExpr },
    }

    pub struct MyTxProof {
        tx: TxRxVTPR,
    }

    impl SingletonGetter for MyTxProof {
        type RetTy = TxRxVTPR;
        fn sgl() -> &'static TxRxVTPR {
            static INSTANCE: std::sync::OnceLock<MyTxProof> = std::sync::OnceLock::new();
            &INSTANCE
                .get_or_init(|| MyTxProof {
                    tx: TxRxVTPR::new_with_proof(),
                })
                .tx
        }
    }

    impl eggplant::wrap::NonPatRecSgl for MyTxProof {
        fn egraph() -> std::sync::Arc<std::sync::Mutex<egglog::EGraph>> {
            <Self as crate::wrap::NonPatRecSgl>::egraph()
        }
    }

    eggplant::basic_patttern_recorder!(MyPatRec);
    impl eggplant::wrap::WithPatRecSgl for MyTxProof {
        type PatRecSgl = MyPatRec;
    }
    impl eggplant::wrap::WithRxSgl for MyPatRec {
        type RxSgl = MyTxProof;
    }

    #[test]
    fn proofs_mode_apis_smoke() {
        let _ = env_logger::builder().is_test(true).try_init();

        let mul: ProofExpr<MyTxProof, ProofMulTy> =
            ProofMul::new(&ProofConst::new(3), &ProofConst::new(2));
        mul.commit();
        let mul_value = MyTxProof::value(&mul).val;

        let expected: ProofExpr<MyTxProof, ProofConstTy> = ProofConst::new(6);
        expected.commit();
        let expected_value = MyTxProof::value(&expected).val;

        let ruleset = MyTxProof::new_ruleset("constant_prop_test");
        MyTxProof::add_rule(
            "MulPat",
            ruleset,
            || {
                let l = ProofConst::query();
                let r = ProofConst::query();
                let p = ProofMul::query(&l, &r);
                #[eggplant::pat_vars_catch]
                struct MulPat {
                    l: ProofConst,
                    r: ProofConst,
                    p: ProofMul,
                }
            },
            |ctx, pat| {
                let cal = ctx.devalue(pat.l.num) * ctx.devalue(pat.r.num);
                let op_value = ctx.insert_proof_const(cal);
                ctx.union(pat.p, op_value);
            },
        );

        let report = MyTxProof::run_ruleset(ruleset, RunConfig::Sat);
        assert!(
            report
                .num_matches_per_rule
                .get("@MulPat")
                .copied()
                .unwrap_or(0)
                > 0,
            "MulPat should match in proofs mode"
        );

        // 1) Value-based proof export must be non-empty and show rewrite rule name.
        let proof = MyTxProof::sgl()
            .prove_eq_pretty_raw("ProofExpr", mul_value, expected_value)
            .expect("prove_eq_pretty_raw should succeed");
        assert!(!proof.trim().is_empty());
        assert!(proof.contains("(name \"@MulPat\")"));

        // 2) Expr-AST-based APIs: call them with surface constructor ASTs.
        //
        // NOTE: In term-encoding mode, “surface AST -> committed Value” is not guaranteed to be
        // stable yet (tracked in #t42). These calls are best-effort and may return an error; the
        // regression we care about here is that they remain safe to call.
        let mul_ast = Expr::Call(
            span!(),
            "ProofMul".to_owned(),
            vec![
                Expr::Call(
                    span!(),
                    "ProofConst".to_owned(),
                    vec![Expr::Lit(span!(), egglog::ast::Literal::Int(3))],
                ),
                Expr::Call(
                    span!(),
                    "ProofConst".to_owned(),
                    vec![Expr::Lit(span!(), egglog::ast::Literal::Int(2))],
                ),
            ],
        );
        let const6_ast = Expr::Call(
            span!(),
            "ProofConst".to_owned(),
            vec![Expr::Lit(span!(), egglog::ast::Literal::Int(6))],
        );
        let _ =
            MyTxProof::sgl().value_equiv_expr_ast("ProofExpr", expected_value, const6_ast.clone());
        let _ = MyTxProof::sgl().prove_eq_pretty_expr_ast("ProofExpr", mul_ast, const6_ast);

        // 3) Regression: proof export should work for non-canonical values too (class-id/canon-rep keying).
        let (rep, non_rep) = {
            let egraph_handle = <MyTxProof as crate::wrap::NonPatRecSgl>::egraph();
            let egraph = egraph_handle.lock().unwrap();
            let sort = egraph.get_sort_by_name("ProofExpr").unwrap().clone();
            let rep = egraph.get_canonical_value(mul_value, &sort);
            let non_rep = if mul_value != rep {
                Some(mul_value)
            } else if expected_value != rep {
                Some(expected_value)
            } else {
                None
            };
            (rep, non_rep)
        };
        if let Some(non_rep) = non_rep {
            let proof_nonrep = MyTxProof::sgl()
                .prove_eq_pretty_raw("ProofExpr", non_rep, rep)
                .expect("prove_eq_pretty_raw(nonrep, rep) should succeed");
            assert!(!proof_nonrep.trim().is_empty());
            assert!(proof_nonrep.contains("(name \"@MulPat\")"));
        }
    }
}

#[cfg(test)]
mod egglog_rule_baseline_tests {
    #[test]
    fn egglog_native_rule_can_match_func_output_and_build_set_of() {
        // Baseline for: (rule ((= ?e (MAccum ?s))) ((set (MAccumSet) (set-of ?e))) ...)
        //
        // Eggplant `add_rule` cannot express "match over function output" yet; keep this
        // test as the semantic reference for the desired behavior.
        let mut egraph = egglog::EGraph::default();
        egraph
            .parse_and_run_program(
                None,
                r#"
(sort IntSet (Set i64))
(function MAccum (i64) i64 :merge old)
(function MAccumSet () IntSet :merge (set-union old new))

(set (MAccum 1) 42)
(ruleset ir-prop)
(rule
  ((= ?e (MAccum ?s)))
  ((set (MAccumSet) (set-of ?e)))
  :ruleset ir-prop)

(run-schedule (saturate (run ir-prop)))
(check (= (MAccumSet) (set-of 42)))
"#,
            )
            .expect("egglog baseline should succeed");
    }

    #[test]
    #[ignore = "TODO: eggplant add_rule needs function-output pattern support + container Insertable"]
    fn eggplant_add_rule_should_eventually_support_func_output_match_and_set_of_action() {
        // Intended future shape (pseudocode):
        // - `#[eggplant::func] struct MAccum { s: i64 } -> Expr`
        // - `MAccum::query(&s)` yields `e` such that fact `e = (MAccum s)` is recorded
        // - action: `ctx.set_m_accum_set(SetContainer::from(vec![e]))` or `ctx.set_m_accum_set_value(ctx.set_of(e))`
        //
        // Keep ignored until API exists; this is the spec we want to uphold.
        unimplemented!()
    }
}
// #[cfg(test)]
// mod test_container_of_base {
//     use crate::{self as eggplant};
//     use eggplant::prelude::*;
//     use eggplant::tx_rx_vt_pr;
//     #[eggplant::dsl(container =Array)]
//     pub enum Expr {
//         VecSum { exprs: Array },
//     }
//     #[eggplant::container]
//     struct Array {
//         inner: Vec<i64>,
//     }
//     #[eggplant::pat_vars]
//     struct SumVec {
//         vec_expr: VecSum,
//     }

//     tx_rx_vt_pr!(MyTx, MyPatRec);
//     fn main() {
//         env_logger::init();
//         let expr: Expr<MyTx, _> = VecSum::new(&Array::new(vec![3, 2, 1]));
//         expr.commit();

//         let ruleset = MyTx::new_ruleset("constant_prop");
//         MyTx::add_rule(
//             "sum_vec",
//             ruleset,
//             || {
//                 let vec_expr = Array::query_leaf();
//                 SumVec::new(VecSum::query(&vec_expr))
//             },
//             |ctx, values| {
//                 println!("{:?}", values);
//                 let v = ctx.devalue(values.vec_expr.exprs);
//                 for expr in v.iter() {
//                     println!("got expr {:?}", expr)
//                 }
//             },
//         );
//         let report = MyTx::run_ruleset(ruleset, RunConfig::Sat);
//         println!("{:#?}", report);
//         MyTx::table_view();

//         expr.pull();
//         MyTx::egraph_to_dot("egraph.dot".into());
//         MyTx::wag_to_dot("wag.dot".into());
//         // paterns to dot
//         MyPatRec::sgl().pats_to_dot("pats.dot".into());
//     }
// }
