#[cfg(feature = "rustsat-extract")]
#[path = "../src/benchmarks/mod.rs"]
mod benchmarks;

#[cfg(feature = "rustsat-extract")]
use crate::benchmarks::nncase_vectorize_microbenchmark;

#[cfg(feature = "rustsat-extract")]
use std::sync::{Mutex, OnceLock};

#[cfg(feature = "rustsat-extract")]
fn test_guard() -> &'static Mutex<()> {
    static GUARD: OnceLock<Mutex<()>> = OnceLock::new();
    GUARD.get_or_init(|| Mutex::new(()))
}

#[cfg(feature = "rustsat-extract")]
fn total_rule_matches(
    report: &nncase_vectorize_microbenchmark::ExtractTimelineReport,
    rule: &str,
) -> usize {
    let prefixed = format!("@{rule}");
    report
        .points
        .iter()
        .filter_map(|point| {
            point
                .rule_matches
                .get(rule)
                .or_else(|| point.rule_matches.get(prefixed.as_str()))
        })
        .sum()
}

#[cfg(feature = "rustsat-extract")]
#[test]
fn vectorize_extract_report_contains_all_backends_with_svg_outputs() {
    let _guard = test_guard().lock().unwrap_or_else(|err| err.into_inner());
    let rows =
        nncase_vectorize_microbenchmark::run_extract_comparison_with_iters_and_mem_cap(1, 20);
    let methods = rows.iter().map(|row| row.method).collect::<Vec<_>>();
    assert_eq!(methods, vec!["default", "eboost", "layered", "rustsat"]);
    for row in rows {
        assert!(!row.rendered.is_empty());
        assert!(row.extract_peak_memory_bytes.is_some());
        assert!(std::path::Path::new(&row.svg_path).exists());
    }
}

#[cfg(feature = "rustsat-extract")]
#[test]
fn vectorize_timeline_contains_points_for_iterations_zero_through_eight() {
    let _guard = test_guard().lock().unwrap_or_else(|err| err.into_inner());
    let report =
        nncase_vectorize_microbenchmark::run_extract_timeline_with_iters_and_mem_cap(9, 20);
    assert_eq!(report.points.len(), 9);
    assert_eq!(report.points.first().map(|p| p.iteration), Some(0));
    assert_eq!(report.points.last().map(|p| p.iteration), Some(8));
}

#[cfg(feature = "rustsat-extract")]
#[test]
fn vectorize_timeline_exercises_every_advertised_rule() {
    let _guard = test_guard().lock().unwrap_or_else(|err| err.into_inner());
    let report =
        nncase_vectorize_microbenchmark::run_extract_timeline_with_iters_and_mem_cap(9, 20);
    let spec = nncase_vectorize_microbenchmark::workload_spec();
    let witness_rules = spec
        .rule_witnesses
        .iter()
        .map(|(rule, _)| *rule)
        .collect::<Vec<_>>();
    assert_eq!(witness_rules, spec.rewrite_rules);
    let observed = report
        .points
        .iter()
        .flat_map(|point| {
            point
                .rule_matches
                .iter()
                .map(move |(rule, count)| (point.iteration, rule.clone(), *count))
        })
        .collect::<Vec<_>>();
    for rule in spec.rewrite_rules {
        let matches = total_rule_matches(&report, rule);
        assert!(
            matches > 0,
            "advertised rule `{rule}` was never exercised; observed matches: {observed:?}"
        );
    }
}

#[cfg(feature = "rustsat-extract")]
#[test]
fn vectorize_smoke_extract_prefers_packed_pipeline_without_pack_unpack_ping_pong() {
    let _guard = test_guard().lock().unwrap_or_else(|err| err.into_inner());
    let rendered = nncase_vectorize_microbenchmark::run_vectorize_rewrite_smoke(
        4,
        eggplant::egglog::extract::TreeAdditiveCostModel::default(),
    );
    assert!(rendered.contains("Packed"));
    assert!(!rendered.contains("Pack (Unpack"));
}
