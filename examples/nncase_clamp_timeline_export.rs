#[cfg(not(feature = "rustsat-extract"))]
fn main() {
    eprintln!("nncase_clamp_timeline_export requires --features rustsat-extract");
}

#[cfg(feature = "rustsat-extract")]
mod real {
#[path = "../benches/runners/eggplant_rewrite/nncase_clamp_microbenchmark.rs"]
mod nncase_clamp_microbenchmark;

use eggplant::helpers::bench_cli::{TimelineExportCliArgs, parse_timeline_export_args};
use eggplant::helpers::progress::{BenchProgress, TimelineExtractMetric};

pub type CliArgs = TimelineExportCliArgs;

pub fn parse_args<I, T>(args: I) -> CliArgs
where
    I: IntoIterator<Item = T>,
    T: Into<std::ffi::OsString> + Clone,
{
    parse_timeline_export_args(
        "nncase_clamp_timeline_export",
        "Export nncase clamp timeline data with CLI progress bars",
        8,
        12,
        "target/nncase_clamp_timeline.json",
        args,
    )
}

pub fn main() -> Result<(), Box<dyn std::error::Error>> {
    let _ = env_logger::try_init();
    let args = parse_args(std::env::args_os());
    let progress = BenchProgress::new(
        (args.rewrite_iters + 1) as u64,
        args.extractors.len() as u64,
    );
    let mut report = nncase_clamp_microbenchmark::run_extract_timeline_with_options(
        args.rewrite_iters + 1,
        args.max_rewrite_mem_gib,
        &args.extractors,
        args.max_extract_time_secs,
    );
    report.version_nickname = args.version_nickname.clone();
    for point in &report.points {
        let metrics = point
            .extracts
            .iter()
            .map(|metric| TimelineExtractMetric {
                method: &metric.method,
                elapsed_ms: metric.elapsed_ms,
                peak_memory_bytes: metric.peak_memory_bytes,
            })
            .collect::<Vec<_>>();
        progress.timeline_iteration(
            (point.iteration + 1) as u64,
            point.iteration,
            point.tuple_count,
            point.rewrite_peak_memory_bytes,
            &metrics,
        );
    }
    progress.finish();
    let output_path = std::path::PathBuf::from(&args.json_out);
    if let Some(parent) = output_path.parent() {
        std::fs::create_dir_all(parent)?;
    }
    std::fs::write(&output_path, serde_json::to_string_pretty(&report)?)?;
    println!("wrote timeline JSON to {}", output_path.display());
    Ok(())
}
}

#[cfg(feature = "rustsat-extract")]
fn main() -> Result<(), Box<dyn std::error::Error>> {
    real::main()
}
