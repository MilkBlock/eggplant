#[cfg(feature = "rustsat-extract")]
#[path = "../src/benchmarks/mod.rs"]
mod benchmarks;

#[cfg(feature = "rustsat-extract")]
use crate::benchmarks::nncase_transpose_microbenchmark;

#[cfg(feature = "rustsat-extract")]
#[test]
fn transpose_extract_cli_accepts_iter_memory_and_extractors() {
    let parsed = nncase_transpose_microbenchmark::parse_extract_args([
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
    let parsed = nncase_transpose_microbenchmark::parse_timeline_args([
        "timeline-export",
        "--version-nickname",
        "transpose-v1",
        "--json-out",
        "target/transpose.json",
        "--md-out",
        "target/transpose.md",
        "--extractor",
        "default,layered",
        "--max-extract-time",
        "7",
    ]);
    assert_eq!(parsed.version_nickname.as_deref(), Some("transpose-v1"));
    assert_eq!(parsed.json_out, "target/transpose.json");
    assert_eq!(parsed.md_out.as_deref(), Some("target/transpose.md"));
    assert_eq!(parsed.extractors, vec!["default", "layered"]);
    assert_eq!(parsed.max_extract_time_secs, Some(7));
}

#[cfg(feature = "rustsat-extract")]
#[test]
fn transpose_example_uses_microbenchmark_workload_spec() {
    let spec = nncase_transpose_microbenchmark::workload_spec();
    assert_eq!(spec.positioning.benchmark_case, "transpose");
    assert_eq!(
        spec.target_root,
        "Transpose(Add(Transpose(lhs, swap), Transpose(UnaryExp(rhs), swap)), swap)"
    );
    assert_eq!(
        spec.op_costs,
        &[
            ("Input", 0),
            ("PermId", 0),
            ("PermSwap", 0),
            ("Add", 1),
            ("UnaryExp", 2),
            ("Transpose", 6),
        ]
    );
    assert_eq!(
        spec.rewrite_rules,
        &[
            "combine_binary_transpose",
            "combine_unary_transpose",
            "fold_two_transposes",
            "fold_nop_transpose",
        ]
    );
    assert_eq!(
        spec.rule_witnesses,
        &[
            (
                "combine_binary_transpose",
                "Add(Transpose(lhs, swap), Transpose(UnaryExp(rhs), swap))",
            ),
            ("combine_unary_transpose", "UnaryExp(Transpose(aux, swap))"),
            (
                "fold_two_transposes",
                "Transpose(Transpose(aux, swap), swap)"
            ),
            ("fold_nop_transpose", "Transpose(aux, id)"),
        ]
    );
}
