#[cfg(feature = "rustsat-extract")]
#[path = "../src/benchmarks/mod.rs"]
mod benchmarks;

#[cfg(feature = "rustsat-extract")]
use crate::benchmarks::nncase_vectorize_microbenchmark;

#[cfg(feature = "rustsat-extract")]
#[test]
fn vectorize_extract_cli_accepts_iter_memory_and_extractors() {
    let parsed = nncase_vectorize_microbenchmark::parse_extract_args([
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
fn vectorize_timeline_cli_accepts_version_nickname_and_json_out() {
    let parsed = nncase_vectorize_microbenchmark::parse_timeline_args([
        "timeline-export",
        "--version-nickname",
        "vectorize-v1",
        "--json-out",
        "target/vectorize.json",
        "--extractor",
        "default,layered",
        "--max-extract-time",
        "7",
    ]);
    assert_eq!(parsed.version_nickname.as_deref(), Some("vectorize-v1"));
    assert_eq!(parsed.json_out, "target/vectorize.json");
    assert_eq!(parsed.extractors, vec!["default", "layered"]);
}

#[cfg(feature = "rustsat-extract")]
#[test]
fn vectorize_example_uses_microbenchmark_workload_spec() {
    let spec = nncase_vectorize_microbenchmark::workload_spec();
    assert_eq!(spec.positioning.benchmark_case, "vectorize");
    assert_eq!(
        spec.target_root,
        "LogicalMatMul(LogicalExp(LogicalMatMul(q, k)), v)"
    );
    assert_eq!(
        spec.op_costs,
        &[
            ("Input", 0),
            ("Flat", 0),
            ("Blocked", 0),
            ("LogicalMatMul", 12),
            ("LogicalExp", 6),
            ("Pack", 3),
            ("Unpack", 3),
            ("PackedMatMul", 2),
            ("PackedExp", 1),
        ]
    );
    assert_eq!(
        spec.rewrite_rules,
        &[
            "meta_pack_matmul",
            "meta_pack_exp",
            "fold_nop_pack",
            "fold_nop_unpack",
        ]
    );
    assert_eq!(
        spec.rule_witnesses,
        &[
            ("meta_pack_matmul", "LogicalMatMul(q, k)"),
            ("meta_pack_exp", "LogicalExp(Unpack(q, blocked))"),
            ("fold_nop_pack", "Pack(Unpack(v, blocked), blocked)"),
            ("fold_nop_unpack", "Unpack(Pack(k, blocked), blocked)"),
        ]
    );
}
