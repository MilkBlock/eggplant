use eggplant::prelude::*;
use eggplant::tx_rx_vt_pr;

#[eggplant::dsl]
enum DynamicCostExpr {
    Leaf { n: i64 },
}

tx_rx_vt_pr!(MyTxDynamicCost, MyPatRecDynamicCost);

#[derive(Default, Clone)]
struct PreferSmallerLeafCostModel;

impl eggplant::egglog::extract::CostModel<eggplant::egglog::extract::DefaultCost>
    for PreferSmallerLeafCostModel
{
    fn fold(
        &self,
        _head: &str,
        children_cost: &[eggplant::egglog::extract::DefaultCost],
        head_cost: eggplant::egglog::extract::DefaultCost,
    ) -> eggplant::egglog::extract::DefaultCost {
        children_cost
            .iter()
            .fold(head_cost, |sum, child| sum.saturating_add(*child))
    }

    fn enode_cost(
        &self,
        egraph: &eggplant::egglog::EGraph,
        func: &eggplant::egglog::Function,
        row: &eggplant::egglog::FunctionRow,
    ) -> eggplant::egglog::extract::DefaultCost {
        match func.name() {
            "Leaf" => egraph.value_to_base::<i64>(row.vals[0]) as u64,
            _ => 1,
        }
    }
}

#[test]
fn typed_extract_node_with_dynamic_cost_can_prefer_specific_leaf_instance() {
    let _ = env_logger::builder().is_test(true).try_init();
    MyTxDynamicCost::sgl().reset_for_bench();

    let expensive_leaf = Leaf::<MyTxDynamicCost>::new(9);
    expensive_leaf.commit();
    let cheap_leaf = Leaf::<MyTxDynamicCost>::new(1);
    cheap_leaf.commit();

    let ruleset = MyTxDynamicCost::new_ruleset("union_dynamic_cost_leaves");
    MyTxDynamicCost::add_rule(
        "union_dynamic_cost_leaves",
        ruleset,
        || {
            let lhs = Leaf::query();
            let rhs = Leaf::query();
            #[eggplant::pat_vars_catch]
            struct Pat {
                lhs: Leaf,
                rhs: Leaf,
            }
        },
        |ctx, pat| {
            let lhs_n = ctx.devalue(pat.lhs.n);
            let rhs_n = ctx.devalue(pat.rhs.n);
            if (lhs_n == 9 && rhs_n == 1) || (lhs_n == 1 && rhs_n == 9) {
                ctx.union(pat.lhs, pat.rhs);
            }
        },
    );
    let report = MyTxDynamicCost::run_ruleset(ruleset, RunConfig::Once);
    assert!(
        report
            .num_matches_per_rule
            .get("@union_dynamic_cost_leaves")
            .copied()
            .unwrap_or(0)
            >= 1
    );

    let (dynamic_rendered, dynamic_cost) = MyTxDynamicCost::extract_node_to_string_with_cost_model(
        &expensive_leaf,
        PreferSmallerLeafCostModel,
    )
    .expect("dynamic node extraction should succeed");

    assert_eq!(dynamic_rendered, "(Leaf 1)");
    // Total extraction cost includes both the chosen `Leaf` row and its base `i64` input.
    assert_eq!(dynamic_cost, 2);
}
