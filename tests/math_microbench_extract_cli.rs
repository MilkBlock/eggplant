#[cfg(feature = "rustsat-extract")]
#[path = "../examples/math_microbenchmark_no_calculus_extract_bench.rs"]
mod math_microbenchmark_extract_bench;

#[cfg(feature = "rustsat-extract")]
#[test]
fn parse_cli_iters_defaults_to_eleven() {
    let parsed = math_microbenchmark_extract_bench::parse_args(["extract-bench"]);
    assert_eq!(parsed.rewrite_iters, 11);
    assert_eq!(parsed.max_rewrite_mem_gib, 20);
}

#[cfg(feature = "rustsat-extract")]
#[test]
fn parse_cli_iters_accepts_explicit_flag() {
    let parsed =
        math_microbenchmark_extract_bench::parse_args(["extract-bench", "--max_iter", "3"]);
    assert_eq!(parsed.rewrite_iters, 3);
    assert_eq!(parsed.max_rewrite_mem_gib, 20);
}

#[cfg(feature = "rustsat-extract")]
#[test]
fn parse_cli_accepts_memory_cap_flag() {
    let parsed =
        math_microbenchmark_extract_bench::parse_args(["extract-bench", "--max_mem", "12"]);
    assert_eq!(parsed.rewrite_iters, 11);
    assert_eq!(parsed.max_rewrite_mem_gib, 12);
}

#[cfg(feature = "rustsat-extract")]
#[test]
fn parse_cli_accepts_long_clap_style_equals_syntax() {
    let parsed = math_microbenchmark_extract_bench::parse_args([
        "extract-bench",
        "--max_iter=7",
        "--max_mem=9",
    ]);
    assert_eq!(parsed.rewrite_iters, 7);
    assert_eq!(parsed.max_rewrite_mem_gib, 9);
}

#[cfg(feature = "rustsat-extract")]
#[test]
fn parse_cli_accepts_extractor_filter_and_timeout() {
    let parsed = math_microbenchmark_extract_bench::parse_args([
        "extract-bench",
        "--extractor=default,layered",
        "--max-extract-time=3",
    ]);
    assert_eq!(parsed.extractors, vec!["default", "layered"]);
    assert_eq!(parsed.max_extract_time_secs, Some(3));
}
