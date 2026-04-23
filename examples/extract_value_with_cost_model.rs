use eggplant::{prelude::*, tx_rx_vt_pr};

#[eggplant::dsl]
enum ExtractCostExpr {
    Leaf {
        n: i64,
    },
    #[cost(0)]
    DefaultWrap {
        inner: ExtractCostExpr,
    },
    #[cost(5)]
    CustomWrap {
        inner: ExtractCostExpr,
    },
}

tx_rx_vt_pr!(ExtractCostTx, ExtractCostPatRec);

#[derive(Default, Clone)]
struct PreferCustomWrapCostModel;

impl eggplant::egglog::extract::CostModel<eggplant::egglog::extract::DefaultCost>
    for PreferCustomWrapCostModel
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
        match func.name() {
            "DefaultWrap" => 100,
            "CustomWrap" => 0,
            _ => 1,
        }
    }
}

fn main() {
    let _ = env_logger::try_init();

    let leaf = Leaf::<ExtractCostTx>::new(7);
    leaf.commit();

    let default_wrap = DefaultWrap::<ExtractCostTx>::new(&leaf);
    default_wrap.commit();
    let custom_wrap = CustomWrap::<ExtractCostTx>::new(&leaf);
    custom_wrap.commit();

    let ruleset = ExtractCostTx::new_ruleset("union_wrappers_for_extract_cost_example");
    ExtractCostTx::add_rule(
        "union_wrappers_for_extract_cost_example",
        ruleset,
        || {
            let leaf = Leaf::query();
            let default_wrap = DefaultWrap::query(&leaf);
            let custom_wrap = CustomWrap::query(&leaf);
            #[eggplant::pat_vars_catch]
            struct Pat {
                default_wrap: DefaultWrap,
                custom_wrap: CustomWrap,
            }
        },
        |ctx, pat| {
            ctx.union(pat.default_wrap, pat.custom_wrap);
        },
    );
    ExtractCostTx::run_ruleset(ruleset, RunConfig::Once);

    let (default_rendered, default_cost) = ExtractCostTx::extract_node_to_string(&default_wrap)
        .expect("default extraction should succeed");
    let (custom_rendered, custom_cost) = ExtractCostTx::extract_node_to_string_with_cost_model(
        &default_wrap,
        PreferCustomWrapCostModel,
    )
    .expect("custom extraction should succeed");

    println!("default extracted term: {default_rendered}");
    println!("default extracted cost: {default_cost}");
    println!("custom extracted term: {custom_rendered}");
    println!("custom extracted cost: {custom_cost}");
}
