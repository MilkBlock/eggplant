#[path = "../benches/runners/eggplant_rewrite/math_microbenchmark_no_calculus.rs"]
mod typed_math_microbenchmark_no_calculus;

use std::sync::{Mutex, OnceLock};

fn test_guard() -> &'static Mutex<()> {
    static GUARD: OnceLock<Mutex<()>> = OnceLock::new();
    GUARD.get_or_init(|| Mutex::new(()))
}

#[cfg(feature = "rustsat-extract")]
#[test]
fn typed_math_microbenchmark_no_calculus_stats_are_non_empty() {
    let _guard = test_guard().lock().unwrap();
    let stats = typed_math_microbenchmark_no_calculus::run_and_collect_stats(false);
    assert!(stats.total_num_tuples > 0);
    assert!(
        stats
            .table_sizes
            .iter()
            .any(|(name, size)| *name == "MAdd" && *size > 0)
    );
    assert!(
        stats
            .table_sizes
            .iter()
            .all(|(name, _)| *name != "MDiff" && *name != "MIntegral"),
        "no-calculus benchmark should not expose MDiff/MIntegral tables"
    );
}

#[cfg(feature = "rustsat-extract")]
#[test]
fn no_calculus_extract_report_omits_diff_and_integral_forms() {
    let _guard = test_guard().lock().unwrap();
    let rows =
        typed_math_microbenchmark_no_calculus::run_extract_comparison_with_iters_and_mem_cap(1, 20);
    assert_eq!(rows.len(), 4);
    for row in rows {
        assert!(
            !row.rendered.contains("MDiff") && !row.rendered.contains("MIntegral"),
            "no-calculus extraction should not contain MDiff/MIntegral: {}",
            row.rendered
        );
    }
}

#[cfg(feature = "rustsat-extract")]
#[test]
fn no_calculus_timeline_contains_one_point_per_iteration() {
    let _guard = test_guard().lock().unwrap();
    let report =
        typed_math_microbenchmark_no_calculus::run_extract_timeline_with_iters_and_mem_cap(2, 20);
    assert_eq!(report.requested_rewrite_iters, 2);
    assert_eq!(report.points.len(), 2);
    for point in report.points {
        assert!(point.iteration >= 1);
        assert_eq!(point.extracts.len(), 4);
        assert!(!point.rule_matches.is_empty());
        for metric in point.extracts {
            assert!(metric.cost.is_some());
        }
    }
}

#[cfg(feature = "rustsat-extract")]
#[test]
fn no_calculus_timeline_honors_selected_extractors() {
    let _guard = test_guard().lock().unwrap();
    let report = typed_math_microbenchmark_no_calculus::run_extract_timeline_with_options(
        1,
        20,
        &["default".to_string(), "layered".to_string()],
        Some(1),
    );
    assert_eq!(report.selected_extractors, vec!["default", "layered"]);
    assert_eq!(report.max_extract_time_secs, Some(1));
    assert_eq!(report.points.len(), 1);
    assert_eq!(report.points[0].extracts.len(), 2);
    assert_eq!(report.points[0].extracts[0].method, "default");
    assert_eq!(report.points[0].extracts[1].method, "layered");
}

#[cfg(feature = "rustsat-extract")]
#[test]
fn no_calculus_progress_callback_receives_rewrite_and_extract_events() {
    let _guard = test_guard().lock().unwrap();
    let mut saw_rewrite = false;
    let mut saw_extract = false;
    let _rows = typed_math_microbenchmark_no_calculus::run_extract_comparison_with_iters_and_mem_cap_and_progress(
        1,
        20,
        |event| match event {
            typed_math_microbenchmark_no_calculus::ProgressEvent::RewriteIterationComplete { .. } => {
                saw_rewrite = true;
            }
            typed_math_microbenchmark_no_calculus::ProgressEvent::ExtractPhaseStart { .. } => {
                saw_extract = true;
            }
            _ => {}
        },
    );
    assert!(saw_rewrite);
    assert!(saw_extract);
}

#[test]
fn no_calculus_cancel_neg_add_rewrites_to_zero() {
    let _guard = test_guard().lock().unwrap();
    let rendered = typed_math_microbenchmark_no_calculus::run_cancel_neg_add_rewrite_smoke(
        1,
        eggplant::egglog::extract::TreeAdditiveCostModel::default(),
    );
    assert_eq!(rendered, "(MConst 0)");
}
