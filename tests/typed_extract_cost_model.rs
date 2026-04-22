use eggplant::prelude::*;
use eggplant::tx_rx_vt_pr;

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

tx_rx_vt_pr!(MyTxExtractCost, MyPatRecExtractCost);

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

#[test]
fn typed_extract_value_with_cost_model_uses_custom_cost_model() {
    let _ = env_logger::builder().is_test(true).try_init();
    MyTxExtractCost::sgl().reset_for_bench();

    let leaf = Leaf::<MyTxExtractCost>::new(7);
    leaf.commit();

    let default_wrap = DefaultWrap::<MyTxExtractCost>::new(&leaf);
    default_wrap.commit();
    let custom_wrap = CustomWrap::<MyTxExtractCost>::new(&leaf);
    custom_wrap.commit();

    let ruleset = MyTxExtractCost::new_ruleset("union_wrappers_for_extract_cost");
    MyTxExtractCost::add_rule(
        "union_wrappers_for_extract_cost",
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
    let report = MyTxExtractCost::run_ruleset(ruleset, RunConfig::Once);
    assert_eq!(
        report
            .num_matches_per_rule
            .get("@union_wrappers_for_extract_cost")
            .copied()
            .unwrap_or(0),
        1
    );

    let canonical = MyTxExtractCost::canonical_raw(&default_wrap);

    let (default_dag, default_term, _) =
        MyTxExtractCost::extract_value(Value::<ExtractCostExpr<(), DefaultWrapTy>>::new(canonical))
            .expect("default typed extraction should succeed");
    let eggplant::egglog::Term::App(default_head, _) = default_dag.get(default_term) else {
        panic!("expected app node at extract root");
    };
    assert_eq!(default_head, "DefaultWrap");

    let (custom_dag, custom_term, _) = MyTxExtractCost::extract_value_with_cost_model(
        Value::<ExtractCostExpr<(), DefaultWrapTy>>::new(canonical),
        PreferCustomWrapCostModel,
    )
    .expect("custom typed extraction should succeed");
    let eggplant::egglog::Term::App(custom_head, _) = custom_dag.get(custom_term) else {
        panic!("expected app node at custom extract root");
    };
    assert_eq!(custom_head, "CustomWrap");

    let (custom_string, _) = MyTxExtractCost::extract_value_to_string_with_cost_model(
        Value::<ExtractCostExpr<(), DefaultWrapTy>>::new(canonical),
        PreferCustomWrapCostModel,
    )
    .expect("string extraction with custom cost model should succeed");
    assert!(custom_string.contains("CustomWrap"));

    let (default_node_string, _) = MyTxExtractCost::extract_node_to_string(&default_wrap)
        .expect("node extraction should work");
    assert!(default_node_string.contains("DefaultWrap"));

    let (custom_node_string, _) = MyTxExtractCost::extract_node_to_string_with_cost_model(
        &default_wrap,
        PreferCustomWrapCostModel,
    )
    .expect("node extraction with custom cost model should work");
    assert!(custom_node_string.contains("CustomWrap"));
}

#[test]
fn typed_extract_backend_cost_model_matches_existing_cost_model_api() {
    let _ = env_logger::builder().is_test(true).try_init();
    MyTxExtractCost::sgl().reset_for_bench();

    let leaf = Leaf::<MyTxExtractCost>::new(7);
    leaf.commit();

    let default_wrap = DefaultWrap::<MyTxExtractCost>::new(&leaf);
    default_wrap.commit();
    let custom_wrap = CustomWrap::<MyTxExtractCost>::new(&leaf);
    custom_wrap.commit();

    let ruleset = MyTxExtractCost::new_ruleset("union_wrappers_for_extract_backend");
    MyTxExtractCost::add_rule(
        "union_wrappers_for_extract_backend",
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
    MyTxExtractCost::run_ruleset(ruleset, RunConfig::Once);

    let existing = MyTxExtractCost::extract_node_to_string_with_cost_model(
        &default_wrap,
        PreferCustomWrapCostModel,
    )
    .expect("existing cost-model API should succeed");
    let via_backend = MyTxExtractCost::extract_node_to_string_with_backend(
        &default_wrap,
        ExtractBackend::cost_model(PreferCustomWrapCostModel),
    )
    .expect("backend cost-model API should succeed");

    assert_eq!(via_backend, existing);
}

#[test]
fn typed_extract_backend_rustsat_uses_variant_costs_over_custom_cost_model_bias() {
    let _ = env_logger::builder().is_test(true).try_init();
    MyTxExtractCost::sgl().reset_for_bench();

    let leaf = Leaf::<MyTxExtractCost>::new(7);
    leaf.commit();
    let default_wrap = DefaultWrap::<MyTxExtractCost>::new(&leaf);
    default_wrap.commit();
    let custom_wrap = CustomWrap::<MyTxExtractCost>::new(&leaf);
    custom_wrap.commit();

    let ruleset = MyTxExtractCost::new_ruleset("union_wrappers_for_rustsat_backend");
    MyTxExtractCost::add_rule(
        "union_wrappers_for_rustsat_backend",
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
    MyTxExtractCost::run_ruleset(ruleset, RunConfig::Once);

    let (legacy_custom, _) = MyTxExtractCost::extract_node_to_string_with_cost_model(
        &default_wrap,
        PreferCustomWrapCostModel,
    )
    .expect("custom cost-model path should succeed");
    let (rustsat_rendered, rustsat_cost) = MyTxExtractCost::extract_node_to_string_with_backend(
        &default_wrap,
        ExtractBackend::<PreferCustomWrapCostModel>::rustsat(RustsatExtractConfig::default()),
    )
    .expect("rustsat backend should succeed on the simple acyclic wrapper case");

    assert!(
        legacy_custom.contains("CustomWrap"),
        "control check: custom cost-model extractor should prefer CustomWrap"
    );
    assert!(
        rustsat_rendered.contains("DefaultWrap"),
        "rustsat v1 should currently minimize the default additive variant costs, so it should prefer DefaultWrap"
    );
    assert_eq!(rustsat_cost, 1);
}
