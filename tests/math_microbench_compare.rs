#[path = "../examples/math_microbenchmark_support.rs"]
mod rust_rule_math_microbenchmark;
#[path = "../benches/runners/eggplant_rewrite/math_microbenchmark.rs"]
mod typed_math_microbenchmark;

#[test]
fn typed_math_microbenchmark_stats_are_non_empty() {
    let stats = typed_math_microbenchmark::run_and_collect_stats(false);
    assert!(stats.total_num_tuples > 0);
    assert!(
        stats
            .table_sizes
            .iter()
            .any(|(name, size)| *name == "MAdd" && *size > 0)
    );
}

#[test]
fn rust_rule_math_microbenchmark_stats_are_non_empty() {
    let stats = rust_rule_math_microbenchmark::run_and_collect_stats();
    assert!(stats.total_num_tuples > 0);
    assert!(
        stats
            .table_sizes
            .iter()
            .any(|(name, size)| *name == "MAdd" && *size > 0)
    );
}
