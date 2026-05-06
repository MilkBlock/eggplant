use eggplant::artifact::{
    PersistedSnapshotRestoreError, build_persisted_snapshot_v1,
    persisted_snapshot_capability_summary, restore_persisted_snapshot_v1,
};
use eggplant::prelude::*;
use eggplant::tx_rx_vt_pr;

#[eggplant::dsl]
enum CorpusExpr {
    Const { num: i64 },
}

#[eggplant::dsl]
enum CorpusRoot {
    Root { node: CorpusExpr },
}

#[eggplant::relation]
struct CorpusEdge {
    src: i64,
    dst: i64,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
#[eggplant::base_ty]
struct CorpusHookedBase {
    n: i64,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
#[eggplant::base_ty]
struct CorpusUnhookedBase {
    n: i64,
}

static CORPUS_HOOKED_BASE_RESTORE_HOOK: eggplant::wrap::SerdeJsonUserBaseSortHook<
    CorpusHookedBase,
> = eggplant::wrap::SerdeJsonUserBaseSortHook::new("corpus-serde-json-boxed");

inventory::submit! {
    eggplant::wrap::PersistedSnapshotUserBaseSortHookRegistration::new(
        "CorpusHookedBase",
        &CORPUS_HOOKED_BASE_RESTORE_HOOK,
    )
}

tx_rx_vt_pr!(CorpusTx, CorpusPatRec);

#[test]
fn persisted_snapshot_corpus_common_path_round_trips() {
    CorpusTx::reset_for_bench();
    let root = Root::<CorpusTx>::new(&Const::new(7));
    root.commit();
    CorpusEdge::<CorpusTx>::insert(1, 2);

    let snapshot = {
        let egraph_handle = CorpusTx::egraph();
        let egraph = egraph_handle.lock().unwrap();
        build_persisted_snapshot_v1(&egraph, eggplant::egglog::SerializeConfig::default())
    };
    let summary = persisted_snapshot_capability_summary(&snapshot);

    CorpusTx::reset_for_bench();
    let report = {
        let egraph_handle = CorpusTx::egraph();
        let mut egraph = egraph_handle.lock().unwrap();
        restore_persisted_snapshot_v1(&mut egraph, &snapshot).unwrap()
    };

    assert!(
        summary
            .guaranteed_restorable
            .iter()
            .any(|entry| { entry.key == "state.function_rows" })
    );
    assert!(
        summary
            .guaranteed_restorable
            .iter()
            .any(|entry| { entry.key == "state.facts.eggplant_native_relations" })
    );
    assert_eq!(report.restored_facts, snapshot.state.facts.len());
    assert_eq!(
        report.restored_function_rows,
        snapshot.state.function_rows.len()
    );
}

#[test]
fn persisted_snapshot_corpus_plain_source_is_non_goal_but_currently_restorable() {
    let mut seeded = eggplant::egglog::EGraph::default();
    seeded
        .parse_and_run_program(
            None,
            r#"
(relation edge (i64 i64))
(edge 1 2)
"#,
        )
        .unwrap();

    let snapshot =
        build_persisted_snapshot_v1(&seeded, eggplant::egglog::SerializeConfig::default());
    let summary = persisted_snapshot_capability_summary(&snapshot);

    let mut restored = eggplant::egglog::EGraph::default();
    restored
        .parse_and_run_program(
            None,
            r#"
(relation edge (i64 i64))
"#,
        )
        .unwrap();
    let report = restore_persisted_snapshot_v1(&mut restored, &snapshot).unwrap();

    assert!(
        summary
            .non_goals
            .iter()
            .any(|entry| { entry.key == "state.plain_source_relation_like.edge" })
    );
    assert_eq!(
        report.restored_function_rows,
        snapshot.state.function_rows.len()
    );
}

#[test]
fn persisted_snapshot_corpus_hooked_user_base_round_trips() {
    let mut seeded = eggplant::egglog::EGraph::default();
    for sort in inventory::iter::<eggplant::wrap::UserBaseSort> {
        if sort.name == "CorpusHookedBase" {
            (sort.sort_insert_fn)(&mut seeded);
        }
    }
    seeded
        .parse_and_run_program(
            None,
            r#"
(datatype CorpusHookedExpr (CorpusHookedLeaf CorpusHookedBase))
"#,
        )
        .unwrap();
    run_ephemeral_rust_rule(
        &mut seeded,
        "seed_corpus_hooked_base",
        &[],
        eggplant::egglog::ast::Facts(Vec::new()),
        |ctx, _| {
            let base = ctx.base_to_value(eggplant::egglog::sort::Boxed::new(CorpusHookedBase {
                n: 17,
            }));
            let _ = ctx.lookup("CorpusHookedLeaf", &[base]);
            Some(())
        },
    )
    .unwrap();

    let snapshot =
        build_persisted_snapshot_v1(&seeded, eggplant::egglog::SerializeConfig::default());
    let summary = persisted_snapshot_capability_summary(&snapshot);

    let mut restored = eggplant::egglog::EGraph::default();
    for sort in inventory::iter::<eggplant::wrap::UserBaseSort> {
        if sort.name == "CorpusHookedBase" {
            (sort.sort_insert_fn)(&mut restored);
        }
    }
    restored
        .parse_and_run_program(
            None,
            r#"
(datatype CorpusHookedExpr (CorpusHookedLeaf CorpusHookedBase))
"#,
        )
        .unwrap();

    let report = restore_persisted_snapshot_v1(&mut restored, &snapshot).unwrap();

    assert!(summary.guaranteed_restorable.iter().any(|entry| {
        entry.key == "state.literal_sorts.CorpusHookedBase"
            && entry.detail.contains("corpus-serde-json-boxed")
    }));
    assert_eq!(
        report.restored_function_rows,
        snapshot.state.function_rows.len()
    );
}

#[test]
fn persisted_snapshot_corpus_unhooked_user_base_reports_gap_and_fails_restore() {
    let mut seeded = eggplant::egglog::EGraph::default();
    for sort in inventory::iter::<eggplant::wrap::UserBaseSort> {
        if sort.name == "CorpusUnhookedBase" {
            (sort.sort_insert_fn)(&mut seeded);
        }
    }
    seeded
        .parse_and_run_program(
            None,
            r#"
(datatype CorpusUnhookedExpr (CorpusUnhookedLeaf CorpusUnhookedBase))
"#,
        )
        .unwrap();
    run_ephemeral_rust_rule(
        &mut seeded,
        "seed_corpus_unhooked_base",
        &[],
        eggplant::egglog::ast::Facts(Vec::new()),
        |ctx, _| {
            let base = ctx.base_to_value(eggplant::egglog::sort::Boxed::new(CorpusUnhookedBase {
                n: 23,
            }));
            let _ = ctx.lookup("CorpusUnhookedLeaf", &[base]);
            Some(())
        },
    )
    .unwrap();

    let snapshot =
        build_persisted_snapshot_v1(&seeded, eggplant::egglog::SerializeConfig::default());
    let summary = persisted_snapshot_capability_summary(&snapshot);

    let mut restored = eggplant::egglog::EGraph::default();
    for sort in inventory::iter::<eggplant::wrap::UserBaseSort> {
        if sort.name == "CorpusUnhookedBase" {
            (sort.sort_insert_fn)(&mut restored);
        }
    }
    restored
        .parse_and_run_program(
            None,
            r#"
(datatype CorpusUnhookedExpr (CorpusUnhookedLeaf CorpusUnhookedBase))
"#,
        )
        .unwrap();

    let err = restore_persisted_snapshot_v1(&mut restored, &snapshot).unwrap_err();

    assert!(
        summary
            .missing_hooks
            .iter()
            .any(|entry| { entry.key == "state.literal_sorts.CorpusUnhookedBase" })
    );
    assert!(matches!(
        err,
        PersistedSnapshotRestoreError::UnsupportedLiteral { .. }
    ));
}
