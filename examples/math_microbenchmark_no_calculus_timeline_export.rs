#[cfg(not(feature = "rustsat-extract"))]
fn main() {
    eprintln!(
        "math_microbenchmark_no_calculus_timeline_export requires --features rustsat-extract"
    );
}

#[path = "../benches/runners/eggplant_rewrite/math_microbenchmark_no_calculus.rs"]
mod typed_math_microbenchmark_no_calculus;

#[cfg(feature = "rustsat-extract")]
mod real {
    use super::typed_math_microbenchmark_no_calculus;
    use eggplant::helpers::bench_cli::{
        TimelineExportCliArgs, parse_timeline_export_args, timeline_markdown_output_path,
    };
    #[cfg(feature = "timeline-plot")]
    use eggplant::helpers::bench_cli::{timeline_markdown_asset_path, timeline_plot_output_path};
    use eggplant::helpers::progress::{BenchProgress, TimelineExtractMetric};
    #[cfg(not(feature = "timeline-plot"))]
    use eggplant::helpers::report::write_timeline_markdown_report;
    #[cfg(feature = "timeline-plot")]
    use eggplant::helpers::report::{
        write_timeline_markdown_report_with_plot, write_timeline_plot_png,
    };

    pub type CliArgs = TimelineExportCliArgs;

    pub fn parse_args<I, T>(args: I) -> CliArgs
    where
        I: IntoIterator<Item = T>,
        T: Into<std::ffi::OsString> + Clone,
    {
        parse_timeline_export_args(
            "math_microbenchmark_no_calculus_timeline_export",
            "Export no-calculus timeline data with CLI progress bars",
            20,
            12,
            "target/math_microbenchmark_no_calculus_timeline.json",
            args,
        )
    }

    pub fn main() -> Result<(), Box<dyn std::error::Error>> {
        let _ = env_logger::try_init();
        let args = parse_args(std::env::args_os());

        let progress = BenchProgress::new(args.rewrite_iters as u64, args.extractors.len() as u64);

        let mut report = typed_math_microbenchmark_no_calculus::run_extract_timeline_with_options(
            args.rewrite_iters,
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
                point.iteration as u64,
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

        let report_title = "Math Microbenchmark Timeline";
        let markdown_path = timeline_markdown_output_path(&args);
        #[cfg(feature = "timeline-plot")]
        {
            let plot_path = timeline_plot_output_path(&args);
            write_timeline_plot_png(report_title, &report, &plot_path)?;
            println!("wrote timeline PNG plot to {}", plot_path.display());
            let plot_link = timeline_markdown_asset_path(&markdown_path, &plot_path);
            write_timeline_markdown_report_with_plot(
                report_title,
                &report,
                &markdown_path,
                Some(&plot_link),
            )?;
        }
        #[cfg(not(feature = "timeline-plot"))]
        write_timeline_markdown_report(report_title, &report, &markdown_path)?;
        println!("wrote timeline Markdown to {}", markdown_path.display());
        Ok(())
    }
}

#[cfg(feature = "rustsat-extract")]
pub use real::*;

#[cfg(feature = "rustsat-extract")]
fn main() -> Result<(), Box<dyn std::error::Error>> {
    real::main()
}
