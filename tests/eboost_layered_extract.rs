#![cfg(all(feature = "rustsat-extract", feature = "fork-egglog"))]

use eggplant::{prelude::*, tx_rx_vt_pr};

#[eggplant::dsl]
enum LayeredExpr {
    Leaf {
        n: i64,
    },
    #[cost(0)]
    CheapWrap {
        inner: LayeredExpr,
    },
    #[cost(5)]
    ExpensiveWrap {
        inner: LayeredExpr,
    },
}

tx_rx_vt_pr!(LayeredDemoTx, LayeredDemoPatRec);

#[eggplant::dsl]
enum LayeredCseExpr {
    CLeaf {
        n: i64,
    },
    CInc {
        inner: LayeredCseExpr,
    },
    CPair {
        lhs: LayeredCseExpr,
        rhs: LayeredCseExpr,
    },
}

tx_rx_vt_pr!(LayeredCseTx, LayeredCsePatRec);

#[test]
fn layered_backend_matches_rustsat_on_simple_acyclic_wrapper_expression() {
    let _ = env_logger::builder().is_test(true).try_init();
    LayeredDemoTx::sgl().reset_for_bench();

    let leaf = Leaf::<LayeredDemoTx>::new(7);
    let cheap = CheapWrap::<LayeredDemoTx>::new(&leaf);
    cheap.commit();
    let expensive = ExpensiveWrap::<LayeredDemoTx>::new(&leaf);
    expensive.commit();

    let ruleset = LayeredDemoTx::new_ruleset("union_wrapper_variants_for_layered_test");
    LayeredDemoTx::add_rule(
        "union_wrapper_variants_for_layered_test",
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
    LayeredDemoTx::run_ruleset(ruleset, RunConfig::Once);

    let rustsat = LayeredDemoTx::extract_node_to_string_with_backend(
        &cheap,
        ExtractBackend::<eggplant::egglog::extract::TreeAdditiveCostModel>::rustsat(
            RustsatExtractConfig::default(),
        ),
    )
    .expect("rustsat extraction should succeed");
    let layered = LayeredDemoTx::extract_node_to_string_with_backend(
        &cheap,
        ExtractBackend::<eggplant::egglog::extract::TreeAdditiveCostModel>::eboost_layered(
            EBoostLayeredConfig::default(),
        ),
    )
    .expect("layered extraction should succeed");

    assert_eq!(layered, rustsat);
    assert!(layered.0.contains("CheapWrap"));
}

#[test]
fn layered_backend_matches_rustsat_on_common_subexpression_case() {
    let _ = env_logger::builder().is_test(true).try_init();
    LayeredCseTx::sgl().reset_for_bench();

    let shared_leaf = CLeaf::<LayeredCseTx>::new(0);
    let shared_l1 = CInc::<LayeredCseTx>::new(&shared_leaf);
    let shared = CInc::<LayeredCseTx>::new(&shared_l1);
    let shared_root = CPair::<LayeredCseTx>::new(&shared, &shared);
    shared_root.commit();

    let left_leaf = CLeaf::<LayeredCseTx>::new(1);
    let right_leaf = CLeaf::<LayeredCseTx>::new(2);
    let left = CInc::<LayeredCseTx>::new(&left_leaf);
    let right = CInc::<LayeredCseTx>::new(&right_leaf);
    let duplicated_root = CPair::<LayeredCseTx>::new(&left, &right);
    duplicated_root.commit();

    let ruleset = LayeredCseTx::new_ruleset("union_common_subexpr_roots_for_layered_test");
    LayeredCseTx::add_rule(
        "union_common_subexpr_roots_for_layered_test",
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
    LayeredCseTx::run_ruleset(ruleset, RunConfig::Once);

    let rustsat = LayeredCseTx::extract_node_to_string_with_backend(
        &shared_root,
        ExtractBackend::<eggplant::egglog::extract::TreeAdditiveCostModel>::rustsat(
            RustsatExtractConfig::default(),
        ),
    )
    .expect("rustsat extraction should succeed");
    let layered = LayeredCseTx::extract_node_to_string_with_backend(
        &shared_root,
        ExtractBackend::<eggplant::egglog::extract::TreeAdditiveCostModel>::eboost_layered(
            EBoostLayeredConfig::default(),
        ),
    )
    .expect("layered extraction should succeed");

    assert_eq!(layered, rustsat);
    assert!(layered.0.contains("(CInc (CInc (CLeaf 0)))"));
}

#[test]
fn layered_backend_rejects_invalid_pruning_bound_below_one() {
    let _ = env_logger::builder().is_test(true).try_init();
    LayeredDemoTx::sgl().reset_for_bench();

    let leaf = Leaf::<LayeredDemoTx>::new(7);
    let cheap = CheapWrap::<LayeredDemoTx>::new(&leaf);
    cheap.commit();

    let err = LayeredDemoTx::extract_node_to_string_with_backend(
        &cheap,
        ExtractBackend::<eggplant::egglog::extract::TreeAdditiveCostModel>::eboost_layered(
            EBoostLayeredConfig {
                bound: 0.95,
                exact: RustsatExtractConfig::default(),
            },
        ),
    )
    .expect_err("layered backend should reject pruning bounds below one");

    assert!(
        err.to_string().contains("bound"),
        "error should mention the invalid pruning bound: {err}"
    );
}
