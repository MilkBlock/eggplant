#![allow(dead_code)]

#[path = "../benches/runners/eggplant_rewrite/math_microbenchmark.rs"]
mod typed_math_microbenchmark;

#[derive(Default, Clone)]
struct PreferDistributedDivisionCost;

impl eggplant::egglog::extract::CostModel<eggplant::egglog::extract::DefaultCost>
    for PreferDistributedDivisionCost
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
        row: &eggplant::egglog::FunctionRow,
    ) -> eggplant::egglog::extract::DefaultCost {
        match func.name() {
            "MDiv" if row.vals.len() >= 2 => 100,
            "MAdd" => 0,
            _ => 1,
        }
    }
}

#[cfg(feature = "rustsat-extract")]
#[test]
fn math_microbenchmark_rewrites_distribute_division_over_addition() {
    let rendered =
        typed_math_microbenchmark::run_div_add_rewrite_smoke(1, PreferDistributedDivisionCost);
    assert!(
        rendered.contains("MAdd") && rendered.matches("MDiv").count() >= 2,
        "expected division-over-addition rewrite to expose two MDiv terms, got: {rendered}"
    );
}
