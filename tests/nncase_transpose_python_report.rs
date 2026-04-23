use std::path::PathBuf;
use std::process::Command;

#[test]
fn transpose_timeline_python_report_renders_metadata() {
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let json_path = manifest_dir.join("target/test_nncase_transpose_timeline.json");
    let out_dir = manifest_dir.join("target/test_nncase_transpose_report");
    let script = manifest_dir.join("tools/plot_nncase_transpose_timeline.py");

    std::fs::create_dir_all(json_path.parent().unwrap()).unwrap();
    std::fs::create_dir_all(&out_dir).unwrap();

    std::fs::write(
        &json_path,
        r#"{
  "version_nickname": "transpose-v1",
  "selected_extractors": ["default", "layered"],
  "max_extract_time_secs": 7,
  "requested_rewrite_iters": 9,
  "executed_rewrite_iters": 9,
  "max_rewrite_mem_gib": 12,
  "stopped_early_due_to_memory_cap": false,
  "points": [
    {
      "iteration": 0,
      "tuple_count": 3,
      "rewrite_elapsed_ms": 1.0,
      "rewrite_peak_memory_bytes": 1024,
      "rule_matches": {"@fold_nop_transpose": 1},
      "extracts": [
        {
          "method": "default",
          "cost": 0,
          "elapsed_ms": 1.0,
          "peak_memory_bytes": 1024,
          "svg_path": "dummy.svg",
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
}"#,
    )
    .unwrap();

    let status = Command::new("python3")
        .arg(script)
        .arg(&json_path)
        .arg("--out-dir")
        .arg(&out_dir)
        .status()
        .expect("python3 should launch transpose timeline report script");
    assert!(status.success());

    let html_path = out_dir.join("timeline_report.html");
    let html = std::fs::read_to_string(html_path).expect("timeline html should be readable");
    assert!(html.contains("transpose-v1"));
    assert!(html.contains("Report Metadata"));
    assert!(html.contains("Selected Extractors"));
    assert!(html.contains("default, layered"));
    assert!(html.contains("Max Extract Time"));
    assert!(html.contains("7 s"));
}
