#![cfg(feature = "rustsat-extract")]

#[path = "../benches/runners/eggplant_rewrite/math_microbenchmark.rs"]
mod typed_math_microbenchmark;

#[cfg(feature = "rustsat-extract")]
#[test]
fn math_microbench_extract_report_contains_all_backends_with_rendered_terms() {
    let rows = typed_math_microbenchmark::run_extract_comparison_with_iters_and_mem_cap(1, 20);

    let methods = rows.iter().map(|row| row.method).collect::<Vec<_>>();
    assert_eq!(methods, vec!["default", "eboost", "layered", "rustsat"]);

    for row in rows {
        assert_eq!(row.requested_rewrite_iters, 1);
        assert!(row.executed_rewrite_iters <= row.requested_rewrite_iters);
        assert!(!row.run_ruleset_note.is_empty());
        assert_eq!(row.max_rewrite_mem_gib, 20);
        assert!(row.rewrite_peak_memory_bytes <= u64::MAX);
        assert!(row.extract_peak_memory_bytes.is_some());
        assert!(
            !row.rendered.is_empty(),
            "rendered expression should not be empty for {}",
            row.method
        );
        assert!(
            row.elapsed.is_some_and(|elapsed| elapsed.as_nanos() > 0),
            "elapsed time should be captured for {}",
            row.method
        );
        assert!(
            !row.svg_path.is_empty(),
            "svg path should be recorded for {}",
            row.method
        );
        assert!(
            std::path::Path::new(&row.svg_path).exists(),
            "svg file should exist for {} at {}",
            row.method,
            row.svg_path
        );
    }
}
