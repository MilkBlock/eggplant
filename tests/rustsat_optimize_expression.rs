#![cfg(feature = "rustsat-extract")]

use eggplant::{prelude::*, tx_rx_vt_pr};

#[eggplant::dsl]
enum RustsatExpr {
    Leaf {
        n: i64,
    },
    #[cost(0)]
    CheapWrap {
        inner: RustsatExpr,
    },
    #[cost(5)]
    ExpensiveWrap {
        inner: RustsatExpr,
    },
}

tx_rx_vt_pr!(RustsatDemoTx, RustsatDemoPatRec);

#[eggplant::dsl]
enum RustsatCycleExpr {
    CycleLeaf {
        n: i64,
    },
    #[cost(10)]
    CycleWrap {
        inner: RustsatCycleExpr,
    },
}

tx_rx_vt_pr!(RustsatCycleTx, RustsatCyclePatRec);

#[test]
fn rustsat_backend_optimizes_wrapper_expression_without_legacy_fallback() {
    let _ = env_logger::builder().is_test(true).try_init();
    RustsatDemoTx::reset_for_bench();

    let leaf = Leaf::<RustsatDemoTx>::new(7);
    let cheap = CheapWrap::<RustsatDemoTx>::new(&leaf);
    cheap.commit();
    let expensive = ExpensiveWrap::<RustsatDemoTx>::new(&leaf);
    expensive.commit();

    let ruleset = RustsatDemoTx::new_ruleset("union_wrapper_variants_for_rustsat_test");
    RustsatDemoTx::add_rule(
        "union_wrapper_variants_for_rustsat_test",
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
    RustsatDemoTx::run_ruleset(ruleset, RunConfig::Once);

    let (legacy_default, _) =
        RustsatDemoTx::extract_node_to_string(&cheap).expect("legacy extraction should succeed");
    let (rustsat_rendered, rustsat_cost) = RustsatDemoTx::extract_node_to_string_with_backend(
        &cheap,
        ExtractBackend::<eggplant::egglog::extract::TreeAdditiveCostModel>::rustsat(
            RustsatExtractConfig::default(),
        ),
    )
    .expect("rustsat backend should succeed on the acyclic wrapper case");

    assert!(
        legacy_default.contains("CheapWrap"),
        "control check: legacy default extractor should already prefer CheapWrap under #[cost]"
    );
    assert!(
        rustsat_rendered.contains("CheapWrap"),
        "rustsat backend should return the same optimal expression under the default additive objective"
    );
    assert_eq!(rustsat_cost, 1);
}

#[test]
fn rustsat_backend_keeps_acyclic_witness_in_cyclic_root_class() {
    let _ = env_logger::builder().is_test(true).try_init();
    RustsatCycleTx::reset_for_bench();

    let leaf = CycleLeaf::<RustsatCycleTx>::new(5);
    let wrap = CycleWrap::<RustsatCycleTx>::new(&leaf);
    wrap.commit();

    let ruleset = RustsatCycleTx::new_ruleset("union_cyclic_root_variants_for_rustsat_test");
    RustsatCycleTx::add_rule(
        "union_cyclic_root_variants_for_rustsat_test",
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
    RustsatCycleTx::run_ruleset(ruleset, RunConfig::Once);

    let (legacy_rendered, _) =
        RustsatCycleTx::extract_node_to_string(&leaf).expect("legacy extraction should succeed");
    let (rustsat_rendered, rustsat_cost) = RustsatCycleTx::extract_node_to_string_with_backend(
        &leaf,
        ExtractBackend::<eggplant::egglog::extract::TreeAdditiveCostModel>::rustsat(
            RustsatExtractConfig::default(),
        ),
    )
    .expect("rustsat backend should keep the acyclic witness even when the root class is cyclic");

    assert!(
        legacy_rendered.contains("Leaf"),
        "control check: legacy extractor should keep the acyclic leaf witness"
    );
    assert!(
        rustsat_rendered.contains("Leaf"),
        "rustsat backend should not blacklist the whole cyclic root class when an acyclic leaf witness exists"
    );
    assert_eq!(rustsat_cost, 1);
}
