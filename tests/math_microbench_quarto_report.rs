use std::path::PathBuf;
use std::process::Command;

#[test]
fn quarto_report_builder_generates_qmd() {
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let json_path = manifest_dir.join("target/math_microbenchmark_no_calculus_timeline.json");
    let out_dir = manifest_dir.join("target/math_microbenchmark_no_calculus_quarto");
    let script = manifest_dir.join("tools/build_math_microbenchmark_quarto_report.py");

    let status = Command::new("python3")
        .arg(script)
        .arg(&json_path)
        .arg("--out-dir")
        .arg(&out_dir)
        .status()
        .expect("python3 should launch quarto report builder");
    assert!(
        status.success(),
        "quarto report builder should exit successfully"
    );

    let qmd_path = out_dir.join("report.qmd");
    assert!(qmd_path.exists(), "builder should create report.qmd");
    let qmd = std::fs::read_to_string(&qmd_path).expect("qmd should be readable");
    assert!(
        qmd.contains("Math Microbenchmark Timeline Report"),
        "qmd should contain the report title"
    );
}
