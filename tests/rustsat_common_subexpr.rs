#![cfg(feature = "rustsat-extract")]

use eggplant::{prelude::*, tx_rx_vt_pr};

#[eggplant::dsl]
enum CseExpr {
    CLeaf { n: i64 },
    CInc { inner: CseExpr },
    CPair { lhs: CseExpr, rhs: CseExpr },
}

tx_rx_vt_pr!(RustsatCseTx, RustsatCsePatRec);

#[test]
fn rustsat_backend_prefers_shared_subexpression_when_tree_cost_overcounts() {
    let _ = env_logger::builder().is_test(true).try_init();
    RustsatCseTx::sgl().reset_for_bench();

    let shared_leaf = CLeaf::<RustsatCseTx>::new(0);
    let shared_l1 = CInc::<RustsatCseTx>::new(&shared_leaf);
    let shared = CInc::<RustsatCseTx>::new(&shared_l1);
    let shared_root = CPair::<RustsatCseTx>::new(&shared, &shared);
    shared_root.commit();

    let left_leaf = CLeaf::<RustsatCseTx>::new(1);
    let right_leaf = CLeaf::<RustsatCseTx>::new(2);
    let left = CInc::<RustsatCseTx>::new(&left_leaf);
    let right = CInc::<RustsatCseTx>::new(&right_leaf);
    let duplicated_root = CPair::<RustsatCseTx>::new(&left, &right);
    duplicated_root.commit();

    let ruleset = RustsatCseTx::new_ruleset("union_common_subexpr_roots_for_rustsat_test");
    RustsatCseTx::add_rule(
        "union_common_subexpr_roots_for_rustsat_test",
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
    RustsatCseTx::run_ruleset(ruleset, RunConfig::Once);

    let (legacy_rendered, legacy_cost) = RustsatCseTx::extract_node_to_string(&shared_root)
        .expect("legacy extraction should succeed");
    let (rustsat_rendered, rustsat_cost) = RustsatCseTx::extract_node_to_string_with_backend(
        &shared_root,
        ExtractBackend::<eggplant::egglog::extract::TreeAdditiveCostModel>::rustsat(
            RustsatExtractConfig::default(),
        ),
    )
    .expect("rustsat extraction should succeed on the common-subexpression case");

    assert!(
        legacy_rendered.contains("(CInc (CLeaf 1))")
            && legacy_rendered.contains("(CInc (CLeaf 2))"),
        "legacy tree-additive extraction should prefer the duplicated non-shared expression because it over-counts the shared subtree"
    );
    assert!(
        rustsat_rendered.contains("(CInc (CInc (CLeaf 0)))"),
        "rustsat backend should prefer the shared-subexpression variant"
    );
    assert!(
        legacy_cost > rustsat_cost,
        "legacy tree-additive extraction should over-count the shared subtree and therefore report a strictly larger cost than the rustsat backend"
    );
    assert_eq!(rustsat_cost, 4);
}
