use std::path::PathBuf;
use std::process::Command;

#[test]
fn timeline_python_report_renders_version_nickname() {
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let json_path = manifest_dir.join("target/test_timeline_version.json");
    let out_dir = manifest_dir.join("target/test_timeline_version_report");
    let script = manifest_dir.join("tools/plot_math_microbenchmark_timeline.py");

    std::fs::create_dir_all(json_path.parent().unwrap()).unwrap();
    std::fs::create_dir_all(&out_dir).unwrap();

    std::fs::write(
        &json_path,
        r#"{
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
      "rewrite_peak_memory_bytes": 1024,
      "rule_matches": {"@demo": 1},
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
          "cost": 0,
          "elapsed_ms": null,
          "peak_memory_bytes": 0,
          "svg_path": "dummy.svg",
          "timed_out": true
        }
      ]
    }
  ]
}"#,
    )
    .unwrap();

    let status = Command::new("/opt/homebrew/bin/python3.8")
        .arg(script)
        .arg(&json_path)
        .arg("--out-dir")
        .arg(&out_dir)
        .status()
        .expect("python3.8 should launch timeline report script");
    assert!(
        status.success(),
        "timeline report script should exit successfully"
    );

    let html_path = out_dir.join("timeline_report.html");
    let html = std::fs::read_to_string(html_path).expect("timeline html should be readable");
    assert!(html.contains("cancel-neg-v1"));
    assert!(html.contains("Version Nickname"));
    assert!(html.contains("Report Metadata"));
    assert!(html.contains("Selected Extractors"));
    assert!(html.contains("default, layered"));
    assert!(html.contains("Max Extract Time"));
    assert!(html.contains("7 s"));
    assert!(html.contains("NaN"));
}
