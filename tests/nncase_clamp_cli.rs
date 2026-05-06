#[cfg(feature = "rustsat-extract")]
#[path = "../src/benchmarks/mod.rs"]
mod benchmarks;

#[cfg(feature = "rustsat-extract")]
use crate::benchmarks::nncase_clamp_microbenchmark;

#[cfg(feature = "rustsat-extract")]
#[test]
fn clamp_extract_cli_accepts_iter_memory_and_extractors() {
    let parsed = nncase_clamp_microbenchmark::parse_extract_args([
        "extract-bench",
        "--max_iter=8",
        "--max_mem=9",
        "--extractor=default,layered",
        "--max-extract-time=3",
    ]);
    assert_eq!(parsed.rewrite_iters, 8);
    assert_eq!(parsed.max_rewrite_mem_gib, 9);
    assert_eq!(parsed.extractors, vec!["default", "layered"]);
}

#[cfg(feature = "rustsat-extract")]
#[test]
fn clamp_timeline_cli_accepts_version_nickname_and_json_out() {
    let parsed = nncase_clamp_microbenchmark::parse_timeline_args([
        "timeline-export",
        "--version-nickname",
        "clamp-v1",
        "--json-out",
        "target/clamp.json",
        "--extractor",
        "default,layered",
        "--max-extract-time",
        "7",
    ]);
    assert_eq!(parsed.version_nickname.as_deref(), Some("clamp-v1"));
    assert_eq!(parsed.json_out, "target/clamp.json");
    assert_eq!(parsed.extractors, vec!["default", "layered"]);
}

#[cfg(feature = "rustsat-extract")]
#[test]
fn clamp_example_uses_microbenchmark_workload_spec() {
    let spec = nncase_clamp_microbenchmark::workload_spec();
    assert_eq!(spec.positioning.benchmark_case, "clamp");
    assert_eq!(spec.target_root, "Relu(Conv2D(input, w0))");
    assert_eq!(
        spec.op_costs,
        &[
            ("Input", 0),
            ("Weight", 0),
            ("Conv2D", 12),
            ("Relu", 4),
            ("Relu6", 4),
            ("ClampZeroInf", 2),
            ("ClampZeroSix", 2),
            ("FusedConv2D", 1),
        ]
    );
    assert_eq!(
        spec.rewrite_rules,
        &[
            "relu_to_clamp",
            "relu6_to_clamp",
            "fold_nested_clamp",
            "fuse_clamp_conv2d",
        ]
    );
    assert_eq!(
        spec.rule_witnesses,
        &[
            ("relu_to_clamp", "Relu(Conv2D(input, w0))"),
            ("relu6_to_clamp", "Relu6(Conv2D(input, w0))"),
            (
                "fold_nested_clamp",
                "ClampZeroInf(ClampZeroInf(Conv2D(input, w0)))",
            ),
            ("fuse_clamp_conv2d", "ClampZeroInf(Conv2D(input, w0))"),
        ]
    );
}
