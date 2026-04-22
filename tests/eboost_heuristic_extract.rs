#![cfg(feature = "fork-egglog")]

use eggplant::{prelude::*, tx_rx_vt_pr};

#[eggplant::dsl]
enum EBoostExpr {
    Leaf {
        n: i64,
    },
    #[cost(0)]
    CheapWrap {
        inner: EBoostExpr,
    },
    #[cost(5)]
    ExpensiveWrap {
        inner: EBoostExpr,
    },
}

tx_rx_vt_pr!(EBoostDemoTx, EBoostDemoPatRec);

#[eggplant::dsl]
enum EBoostCycleExpr {
    CycleLeaf {
        n: i64,
    },
    #[cost(10)]
    CycleWrap {
        inner: EBoostCycleExpr,
    },
}

tx_rx_vt_pr!(EBoostCycleTx, EBoostCyclePatRec);

#[eggplant::dsl]
enum EBoostCseExpr {
    CLeaf {
        n: i64,
    },
    CInc {
        inner: EBoostCseExpr,
    },
    CPair {
        lhs: EBoostCseExpr,
        rhs: EBoostCseExpr,
    },
}

tx_rx_vt_pr!(EBoostCseTx, EBoostCsePatRec);

#[test]
fn eboost_backend_matches_default_on_simple_acyclic_wrapper_expression() {
    let _ = env_logger::builder().is_test(true).try_init();
    EBoostDemoTx::sgl().reset_for_bench();

    let leaf = Leaf::<EBoostDemoTx>::new(7);
    let cheap = CheapWrap::<EBoostDemoTx>::new(&leaf);
    cheap.commit();
    let expensive = ExpensiveWrap::<EBoostDemoTx>::new(&leaf);
    expensive.commit();

    let ruleset = EBoostDemoTx::new_ruleset("union_wrapper_variants_for_eboost_test");
    EBoostDemoTx::add_rule(
        "union_wrapper_variants_for_eboost_test",
        ruleset,
        || {
            let leaf = Leaf::query();
            let cheap = CheapWrap::query(&leaf);
            let expensive = ExpensiveWrap::query(&leaf);
            #[eggplant::pat_vars_catch]
            struct Pat {
                cheap: CheapWrap,
                expensive: ExpensiveWrap,
            }
        },
        |ctx, pat| {
            ctx.union(pat.cheap, pat.expensive);
        },
    );
    EBoostDemoTx::run_ruleset(ruleset, RunConfig::Once);

    let default_result =
        EBoostDemoTx::extract_node_to_string(&cheap).expect("default extraction should succeed");
    let eboost_result = EBoostDemoTx::extract_node_to_string_with_backend(
        &cheap,
        ExtractBackend::<eggplant::egglog::extract::TreeAdditiveCostModel>::eboost_heuristic(
            EBoostExtractConfig::default(),
        ),
    )
    .expect("eboost backend should succeed on the simple acyclic wrapper case");

    assert_eq!(eboost_result.0, default_result.0);
    assert!(eboost_result.0.contains("CheapWrap"));
    assert!(
        eboost_result.1 <= default_result.1,
        "eboost heuristic should not report a higher cost than the legacy tree-additive extractor on the simple acyclic case"
    );
    assert_eq!(eboost_result.1, 1);
}

#[test]
fn eboost_backend_prefers_shared_subexpression_over_duplicated_tree_choice() {
    let _ = env_logger::builder().is_test(true).try_init();
    EBoostCseTx::sgl().reset_for_bench();

    let shared_leaf = CLeaf::<EBoostCseTx>::new(0);
    let shared_l1 = CInc::<EBoostCseTx>::new(&shared_leaf);
    let shared = CInc::<EBoostCseTx>::new(&shared_l1);
    let shared_root = CPair::<EBoostCseTx>::new(&shared, &shared);
    shared_root.commit();

    let left_leaf = CLeaf::<EBoostCseTx>::new(1);
    let right_leaf = CLeaf::<EBoostCseTx>::new(2);
    let left = CInc::<EBoostCseTx>::new(&left_leaf);
    let right = CInc::<EBoostCseTx>::new(&right_leaf);
    let duplicated_root = CPair::<EBoostCseTx>::new(&left, &right);
    duplicated_root.commit();

    let ruleset = EBoostCseTx::new_ruleset("union_common_subexpr_roots_for_eboost_test");
    EBoostCseTx::add_rule(
        "union_common_subexpr_roots_for_eboost_test",
        ruleset,
        || {
            let shared_leaf = CLeaf::query();
            let shared_l1 = CInc::query(&shared_leaf);
            let shared = CInc::query(&shared_l1);
            let shared_root = CPair::query(&shared, &shared);

            let left_leaf = CLeaf::query();
            let left = CInc::query(&left_leaf);
            let right_leaf = CLeaf::query();
            let right = CInc::query(&right_leaf);
            let duplicated_root = CPair::query(&left, &right);
            #[eggplant::pat_vars_catch]
            struct Pat {
                shared_leaf: CLeaf,
                shared_root: CPair,
                left_leaf: CLeaf,
                right_leaf: CLeaf,
                duplicated_root: CPair,
            }
        },
        |ctx, pat| {
            let shared_n = ctx.devalue(pat.shared_leaf.n);
            let left_n = ctx.devalue(pat.left_leaf.n);
            let right_n = ctx.devalue(pat.right_leaf.n);
            if shared_n == 0 && left_n == 1 && right_n == 2 {
                ctx.union(pat.shared_root, pat.duplicated_root);
            }
        },
    );
    EBoostCseTx::run_ruleset(ruleset, RunConfig::Once);

    let (legacy_rendered, legacy_cost) = EBoostCseTx::extract_node_to_string(&shared_root)
        .expect("legacy extraction should succeed");
    let (eboost_rendered, eboost_cost) = EBoostCseTx::extract_node_to_string_with_backend(
        &shared_root,
        ExtractBackend::<eggplant::egglog::extract::TreeAdditiveCostModel>::eboost_heuristic(
            EBoostExtractConfig::default(),
        ),
    )
    .expect("eboost backend should succeed on the common-subexpression case");

    assert!(
        legacy_rendered.contains("(CInc (CLeaf 1))")
            && legacy_rendered.contains("(CInc (CLeaf 2))"),
        "control check: legacy tree-additive extraction should over-count the shared subtree"
    );
    assert!(
        eboost_rendered.contains("(CInc (CInc (CLeaf 0)))"),
        "eboost heuristic should prefer the shared-subexpression variant"
    );
    assert!(
        legacy_cost > eboost_cost,
        "eboost heuristic should report a strictly smaller DAG-aware cost than legacy tree-additive extraction"
    );
}

#[test]
fn eboost_backend_keeps_acyclic_witness_in_cyclic_root_class() {
    let _ = env_logger::builder().is_test(true).try_init();
    EBoostCycleTx::sgl().reset_for_bench();

    let leaf = CycleLeaf::<EBoostCycleTx>::new(5);
    let wrap = CycleWrap::<EBoostCycleTx>::new(&leaf);
    wrap.commit();

    let ruleset = EBoostCycleTx::new_ruleset("union_cyclic_root_variants_for_eboost_test");
    EBoostCycleTx::add_rule(
        "union_cyclic_root_variants_for_eboost_test",
        ruleset,
        || {
            let leaf = CycleLeaf::query();
            let wrap = CycleWrap::query(&leaf);
            #[eggplant::pat_vars_catch]
            struct Pat {
                leaf: CycleLeaf,
                wrap: CycleWrap,
            }
        },
        |ctx, pat| {
            ctx.union(pat.leaf, pat.wrap);
        },
    );
    EBoostCycleTx::run_ruleset(ruleset, RunConfig::Once);

    let (legacy_rendered, _) =
        EBoostCycleTx::extract_node_to_string(&leaf).expect("legacy extraction should succeed");
    let (eboost_rendered, eboost_cost) = EBoostCycleTx::extract_node_to_string_with_backend(
        &leaf,
        ExtractBackend::<eggplant::egglog::extract::TreeAdditiveCostModel>::eboost_heuristic(
            EBoostExtractConfig::default(),
        ),
    )
    .expect("eboost backend should keep the acyclic witness even when the root class is cyclic");

    assert!(legacy_rendered.contains("Leaf"));
    assert!(eboost_rendered.contains("Leaf"));
    assert_eq!(eboost_cost, 1);
}
