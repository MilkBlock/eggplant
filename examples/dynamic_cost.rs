use std::collections::HashMap;

use eggplant::{prelude::*, tx_rx_vt_pr};

#[eggplant::dsl]
enum DynamicCostExpr {
    Leaf {
        n: i64,
    },
    #[cost(0)]
    CheapWrap {
        inner: DynamicCostExpr,
    },
    #[cost(0)]
    ExpensiveWrap {
        inner: DynamicCostExpr,
    },
}

tx_rx_vt_pr!(DynamicCostTx, DynamicCostPatRec);

#[derive(Clone, Default)]
struct HashMapCostModel {
    node_costs: HashMap<&'static str, eggplant::egglog::extract::DefaultCost>,
}

impl HashMapCostModel {
    fn with_cost(
        mut self,
        head: &'static str,
        cost: eggplant::egglog::extract::DefaultCost,
    ) -> Self {
        self.node_costs.insert(head, cost);
        self
    }
}

impl eggplant::egglog::extract::CostModel<eggplant::egglog::extract::DefaultCost>
    for HashMapCostModel
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
        _egraph: &eggplant::egglog::EGraph,
        func: &eggplant::egglog::Function,
        _row: &eggplant::egglog::FunctionRow,
    ) -> eggplant::egglog::extract::DefaultCost {
        self.node_costs.get(func.name()).copied().unwrap_or(1)
    }
}

fn main() {
    let _ = env_logger::try_init();

    let leaf = Leaf::<DynamicCostTx>::new(7);
    leaf.commit();

    let cheap = CheapWrap::<DynamicCostTx>::new(&leaf);
    cheap.commit();
    let expensive = ExpensiveWrap::<DynamicCostTx>::new(&leaf);
    expensive.commit();

    let ruleset = DynamicCostTx::new_ruleset("union_dynamic_cost_variants");
    DynamicCostTx::add_rule(
        "union_dynamic_cost_variants",
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
    DynamicCostTx::run_ruleset(ruleset, RunConfig::Once);

    let prefer_cheap = HashMapCostModel::default()
        .with_cost("CheapWrap", 0)
        .with_cost("ExpensiveWrap", 100);
    let prefer_expensive = HashMapCostModel::default()
        .with_cost("CheapWrap", 100)
        .with_cost("ExpensiveWrap", 0);

    let (cheap_rendered, cheap_cost) =
        DynamicCostTx::extract_node_to_string_with_cost_model(&cheap, prefer_cheap)
            .expect("cheap-preferred extraction should succeed");
    let (expensive_rendered, expensive_cost) =
        DynamicCostTx::extract_node_to_string_with_cost_model(&cheap, prefer_expensive)
            .expect("expensive-preferred extraction should succeed");

    println!("prefer cheap => term: {cheap_rendered}, cost: {cheap_cost}");
    println!("prefer expensive => term: {expensive_rendered}, cost: {expensive_cost}");

    // Today this HashMap is keyed by constructor/function name.
    // If you want true per-node dynamic cost, the next step is to key by a more semantic,
    // stable identifier derived from the function row seen in `enode_cost(...)`.
}
