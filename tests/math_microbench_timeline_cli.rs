#[cfg(feature = "rustsat-extract")]
#[path = "../examples/math_microbenchmark_no_calculus_timeline_export.rs"]
mod math_microbenchmark_no_calculus_timeline_export;

#[cfg(feature = "rustsat-extract")]
#[test]
fn timeline_cli_accepts_version_nickname() {
    let parsed = math_microbenchmark_no_calculus_timeline_export::parse_args([
        "timeline-export",
        "--version-nickname",
        "cancel-neg-v1",
    ]);
    assert_eq!(parsed.version_nickname.as_deref(), Some("cancel-neg-v1"));
}

#[cfg(feature = "rustsat-extract")]
#[test]
fn timeline_cli_accepts_extractor_filter_and_timeout() {
    let parsed = math_microbenchmark_no_calculus_timeline_export::parse_args([
        "timeline-export",
        "--extractor",
        "default,layered",
        "--max-extract-time",
        "7",
    ]);
    assert_eq!(parsed.extractors, vec!["default", "layered"]);
    assert_eq!(parsed.max_extract_time_secs, Some(7));
}
