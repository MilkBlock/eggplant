use eggplant::{prelude::*, tx_rx_vt_pr};

#[eggplant::dsl]
enum DynamicCostExpr {
    Leaf { n: i64 },
}

tx_rx_vt_pr!(DynamicCostTx, DynamicCostPatRec);

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
            "Leaf" => {
                // `row.vals[0]` is the concrete `n: i64` input for this specific `Leaf` row.
                // This makes the cost depend on the node instance, not just the constructor name.
                egraph.value_to_base::<i64>(row.vals[0]) as u64
            }
            _ => 1,
        }
    }
}

fn main() {
    let _ = env_logger::try_init();

    let expensive_leaf = Leaf::<DynamicCostTx>::new(9);
    expensive_leaf.commit();
    let cheap_leaf = Leaf::<DynamicCostTx>::new(1);
    cheap_leaf.commit();

    let ruleset = DynamicCostTx::new_ruleset("union_dynamic_cost_leaves");
    DynamicCostTx::add_rule(
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
    DynamicCostTx::run_ruleset(ruleset, RunConfig::Once);

    let (default_rendered, default_cost) = DynamicCostTx::extract_node_to_string(&expensive_leaf)
        .expect("default extraction should succeed");
    let (dynamic_rendered, dynamic_cost) = DynamicCostTx::extract_node_to_string_with_cost_model(
        &expensive_leaf,
        PreferSmallerLeafCostModel,
    )
    .expect("dynamic extraction should succeed");

    println!("default extracted term: {default_rendered}");
    println!("default extracted cost: {default_cost}");
    println!("dynamic extracted term: {dynamic_rendered}");
    println!("dynamic extracted cost: {dynamic_cost}");
}
