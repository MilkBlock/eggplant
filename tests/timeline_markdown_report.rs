#[cfg(feature = "timeline-plot")]
use eggplant::prelude::write_timeline_plot_png;
use eggplant::prelude::{
    TimelineExportCliArgs, render_timeline_markdown_report,
    render_timeline_markdown_report_with_plot, timeline_markdown_output_path,
    timeline_plot_output_path,
};
use serde_json::json;

fn sample_timeline_report() -> serde_json::Value {
    json!({
        "version_nickname": "cancel-neg-v1",
        "selected_extractors": ["default", "layered"],
        "max_extract_time_secs": 7,
        "requested_rewrite_iters": 1,
        "executed_rewrite_iters": 1,
        "max_rewrite_mem_gib": 12,
        "stopped_early_due_to_memory_cap": false,
        "points": [
            {
                "iteration": 1,
                "tuple_count": 3,
                "rewrite_elapsed_ms": 1.0,
                "rewrite_peak_memory_bytes": 1048576,
                "rule_matches": {"@demo": 2},
                "extracts": [
                    {
                        "method": "default",
                        "cost": 0,
                        "elapsed_ms": 1.0,
                        "peak_memory_bytes": 1048576,
                        "svg_path": "target/default.svg",
                        "timed_out": false
                    },
                    {
                        "method": "layered",
                        "cost": null,
                        "elapsed_ms": null,
                        "peak_memory_bytes": null,
                        "svg_path": "n/a",
                        "timed_out": true
                    }
                ]
            }
        ]
    })
}

fn sample_nncase_timeline_report() -> serde_json::Value {
    json!({
        "version_nickname": "nncase-clamp",
        "benchmark_family": "nncase",
        "benchmark_case": "clamp",
        "baseline": "nncase egraph implementation",
        "comparison_target": "eggplant",
        "positioning": "This nncase workload is included because the original nncase egraph implementation is slower than eggplant on this rewrite/extract shape.",
        "selected_extractors": ["default"],
        "max_extract_time_secs": null,
        "requested_rewrite_iters": 1,
        "executed_rewrite_iters": 1,
        "max_rewrite_mem_gib": 12,
        "stopped_early_due_to_memory_cap": false,
        "points": []
    })
}

#[test]
fn timeline_markdown_report_renders_metadata_rules_and_svg_links() {
    let report = sample_timeline_report();
    let md = render_timeline_markdown_report("Math Microbenchmark Timeline", &report).unwrap();

    assert!(md.contains("# Math Microbenchmark Timeline - cancel-neg-v1"));
    assert!(md.contains("| Selected Extractors | default, layered |"));
    assert!(md.contains("| Max Extract Time | 7 s |"));
    assert!(md.contains("| @demo | 2 |"));
    assert!(md.contains("| 1 | 3 | 1.000 | 1.00 |"));
    assert!(md.contains("[target/default.svg](target/default.svg)"));
    assert!(md.contains("![default iteration 1](target/default.svg)"));
    assert!(md.contains("| layered | 3 | 1.000 | 1.00 | NaN | NaN | NaN | true | n/a | n/a |"));
}

#[test]
fn timeline_markdown_report_renders_nncase_positioning_notes() {
    let report = sample_nncase_timeline_report();
    let md = render_timeline_markdown_report("Nncase Clamp Timeline", &report).unwrap();

    assert!(md.contains("## Benchmark Positioning"));
    assert!(md.contains("| Benchmark Family | nncase |"));
    assert!(md.contains("| Benchmark Case | clamp |"));
    assert!(md.contains("| Baseline | nncase egraph implementation |"));
    assert!(md.contains("| Comparison Target | eggplant |"));
    assert!(
        md.contains(
            "the original nncase egraph implementation is slower than eggplant on this rewrite/extract shape"
        )
    );
}

#[test]
fn timeline_markdown_report_can_embed_plot_image() {
    let report = sample_timeline_report();
    let md = render_timeline_markdown_report_with_plot(
        "Math Microbenchmark Timeline",
        &report,
        Some("timeline_plot.png"),
    )
    .unwrap();

    assert!(md.contains("## Timeline Plot"));
    assert!(md.contains("![Timeline Plot](timeline_plot.png)"));
}

#[test]
fn timeline_markdown_output_path_defaults_to_json_path_with_md_extension() {
    let args = TimelineExportCliArgs {
        rewrite_iters: 1,
        max_rewrite_mem_gib: 12,
        json_out: "target/demo.timeline.json".to_string(),
        md_out: None,
        plot_out: None,
        version_nickname: None,
        extractors: vec!["default".to_string()],
        max_extract_time_secs: None,
    };

    assert_eq!(
        timeline_markdown_output_path(&args),
        std::path::PathBuf::from("target/demo.timeline.md")
    );
}

#[test]
fn timeline_plot_output_path_defaults_to_json_path_with_png_extension() {
    let args = TimelineExportCliArgs {
        rewrite_iters: 1,
        max_rewrite_mem_gib: 12,
        json_out: "target/demo.timeline.json".to_string(),
        md_out: None,
        plot_out: None,
        version_nickname: None,
        extractors: vec!["default".to_string()],
        max_extract_time_secs: None,
    };

    assert_eq!(
        timeline_plot_output_path(&args),
        std::path::PathBuf::from("target/demo.timeline.png")
    );
}

#[cfg(feature = "timeline-plot")]
#[test]
fn timeline_plot_png_writer_creates_png_file() {
    let report = sample_timeline_report();
    let output_dir = std::env::temp_dir().join(format!(
        "eggplant-timeline-plot-test-{}",
        std::process::id()
    ));
    std::fs::create_dir_all(&output_dir).unwrap();
    let output_path = output_dir.join("timeline_plot.png");

    write_timeline_plot_png("Math Microbenchmark Timeline", &report, &output_path).unwrap();

    let bytes = std::fs::read(&output_path).unwrap();
    assert!(bytes.starts_with(&[137, 80, 78, 71, 13, 10, 26, 10]));
    assert!(bytes.len() > 1_000);

    let _ = std::fs::remove_file(output_path);
    let _ = std::fs::remove_dir(output_dir);
}
