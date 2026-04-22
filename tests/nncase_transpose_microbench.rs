#![cfg(feature = "rustsat-extract")]

#[cfg(feature = "rustsat-extract")]
#[path = "../benches/runners/eggplant_rewrite/nncase_transpose_microbenchmark.rs"]
mod nncase_transpose_microbenchmark;

use std::sync::{Mutex, OnceLock};

fn test_guard() -> &'static Mutex<()> {
    static GUARD: OnceLock<Mutex<()>> = OnceLock::new();
    GUARD.get_or_init(|| Mutex::new(()))
}

#[cfg(feature = "rustsat-extract")]
#[test]
fn transpose_extract_report_contains_all_backends_with_svg_outputs() {
    let _guard = test_guard().lock().unwrap_or_else(|err| err.into_inner());
    let rows =
        nncase_transpose_microbenchmark::run_extract_comparison_with_iters_and_mem_cap(1, 20);

    let methods = rows.iter().map(|row| row.method).collect::<Vec<_>>();
    assert_eq!(methods, vec!["default", "eboost", "layered", "rustsat"]);

    for row in rows {
        assert_eq!(row.requested_rewrite_iters, 1);
        assert!(row.executed_rewrite_iters <= 1);
        assert!(!row.rendered.is_empty());
        assert!(row.extract_peak_memory_bytes.is_some());
        assert!(!row.svg_path.is_empty());
        assert!(std::path::Path::new(&row.svg_path).exists());
    }
}

#[cfg(feature = "rustsat-extract")]
#[test]
fn transpose_timeline_contains_points_for_iterations_zero_through_eight() {
    let _guard = test_guard().lock().unwrap_or_else(|err| err.into_inner());
    let report =
        nncase_transpose_microbenchmark::run_extract_timeline_with_iters_and_mem_cap(9, 20);
    assert_eq!(report.requested_rewrite_iters, 9);
    assert_eq!(report.points.len(), 9);
    assert_eq!(report.points.first().map(|p| p.iteration), Some(0));
    assert_eq!(report.points.last().map(|p| p.iteration), Some(8));
    for point in report.points {
        assert_eq!(point.extracts.len(), 4);
        assert!(!point.rule_matches.is_empty());
    }
}

#[test]
fn transpose_smoke_extract_prefers_eliminating_redundant_transpose() {
    let _guard = test_guard().lock().unwrap_or_else(|err| err.into_inner());
    let rendered = nncase_transpose_microbenchmark::run_transpose_rewrite_smoke(
        3,
        eggplant::egglog::extract::TreeAdditiveCostModel::default(),
    );
    assert!(
        !rendered.contains("Transpose(Transpose"),
        "extracted form should fold nested transpose: {rendered}"
    );
}
