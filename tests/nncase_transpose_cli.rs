#[cfg(feature = "rustsat-extract")]
#[path = "../examples/nncase_transpose_extract_bench.rs"]
mod nncase_transpose_extract_bench;
#[cfg(feature = "rustsat-extract")]
#[path = "../examples/nncase_transpose_timeline_export.rs"]
mod nncase_transpose_timeline_export;

#[cfg(feature = "rustsat-extract")]
#[test]
fn transpose_extract_cli_accepts_iter_memory_and_extractors() {
    let parsed = nncase_transpose_extract_bench::parse_args([
        "extract-bench",
        "--max_iter=8",
        "--max_mem=9",
        "--extractor=default,layered",
        "--max-extract-time=3",
    ]);
    assert_eq!(parsed.rewrite_iters, 8);
    assert_eq!(parsed.max_rewrite_mem_gib, 9);
    assert_eq!(parsed.extractors, vec!["default", "layered"]);
    assert_eq!(parsed.max_extract_time_secs, Some(3));
}

#[cfg(feature = "rustsat-extract")]
#[test]
fn transpose_timeline_cli_accepts_version_nickname_and_json_out() {
    let parsed = nncase_transpose_timeline_export::parse_args([
        "timeline-export",
        "--version-nickname",
        "transpose-v1",
        "--json-out",
        "target/transpose.json",
        "--extractor",
        "default,layered",
        "--max-extract-time",
        "7",
    ]);
    assert_eq!(parsed.version_nickname.as_deref(), Some("transpose-v1"));
    assert_eq!(parsed.json_out, "target/transpose.json");
    assert_eq!(parsed.extractors, vec!["default", "layered"]);
    assert_eq!(parsed.max_extract_time_secs, Some(7));
}
