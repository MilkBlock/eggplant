#[cfg(not(feature = "rustsat-extract"))]
fn main() {
    eprintln!("nncase_vectorize_timeline_export requires --features rustsat-extract");
}

#[cfg(feature = "rustsat-extract")]
#[path = "../src/benchmarks/mod.rs"]
mod benchmarks;

#[cfg(feature = "rustsat-extract")]
mod real {
    use crate::benchmarks::nncase_vectorize_microbenchmark;
    use eggplant::helpers::bench_cli::{TimelineExportCliArgs, timeline_markdown_output_path};
    #[cfg(feature = "timeline-plot")]
    use eggplant::helpers::bench_cli::{timeline_markdown_asset_path, timeline_plot_output_path};
    use eggplant::helpers::progress::{BenchProgress, TimelineExtractMetric};
    #[cfg(not(feature = "timeline-plot"))]
    use eggplant::helpers::report::{
        BenchmarkWorkloadSpec, print_benchmark_workload_spec, write_timeline_markdown_report,
    };
    #[cfg(feature = "timeline-plot")]
    use eggplant::helpers::report::{
        BenchmarkWorkloadSpec, print_benchmark_workload_spec,
        write_timeline_markdown_report_with_plot, write_timeline_plot_png,
    };

    pub type CliArgs = TimelineExportCliArgs;

    pub fn workload_spec() -> BenchmarkWorkloadSpec<'static> {
        nncase_vectorize_microbenchmark::workload_spec()
    }

    pub fn parse_args<I, T>(args: I) -> CliArgs
    where
        I: IntoIterator<Item = T>,
        T: Into<std::ffi::OsString> + Clone,
    {
        nncase_vectorize_microbenchmark::parse_timeline_args(args)
    }

    pub fn main() -> Result<(), Box<dyn std::error::Error>> {
        let _ = env_logger::try_init();
        let args = parse_args(std::env::args_os());
        print_benchmark_workload_spec(&workload_spec());
        let progress = BenchProgress::new(
            (args.rewrite_iters + 1) as u64,
            args.extractors.len() as u64,
        );
        let mut report = nncase_vectorize_microbenchmark::run_extract_timeline_with_options(
            args.rewrite_iters + 1,
            args.max_rewrite_mem_gib,
            &args.extractors,
            args.max_extract_time_secs,
        );
        report.version_nickname = args.version_nickname.clone();
        report.requested_rewrite_iters = args.rewrite_iters;
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

        let report_title = "Nncase Vectorize Timeline";
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
fn main() -> Result<(), Box<dyn std::error::Error>> {
    real::main()
}
