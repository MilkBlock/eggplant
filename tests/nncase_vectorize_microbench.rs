#![cfg(feature = "rustsat-extract")]

#[cfg(feature = "rustsat-extract")]
#[path = "../benches/runners/eggplant_rewrite/nncase_vectorize_microbenchmark.rs"]
mod nncase_vectorize_microbenchmark;

use std::sync::{Mutex, OnceLock};

fn test_guard() -> &'static Mutex<()> {
    static GUARD: OnceLock<Mutex<()>> = OnceLock::new();
    GUARD.get_or_init(|| Mutex::new(()))
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
