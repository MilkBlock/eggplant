#[cfg(not(feature = "rustsat-extract"))]
fn main() {
    eprintln!("nncase_clamp_extract_bench requires --features rustsat-extract");
}

#[cfg(feature = "rustsat-extract")]
#[path = "../src/benchmarks/mod.rs"]
mod benchmarks;

#[cfg(feature = "rustsat-extract")]
mod real {
    use crate::benchmarks::nncase_clamp_microbenchmark;
    use eggplant::helpers::bench_cli::ExtractBenchCliArgs;
    use eggplant::helpers::progress::BenchProgress;
    use eggplant::helpers::report::{
        BenchmarkWorkloadSpec, ExtractReportRow, print_benchmark_workload_spec,
        print_extract_comparison_report, print_extract_run_configuration,
    };

    pub type CliArgs = ExtractBenchCliArgs;

    pub fn workload_spec() -> BenchmarkWorkloadSpec<'static> {
        nncase_clamp_microbenchmark::workload_spec()
    }

    pub fn parse_args<I, T>(args: I) -> CliArgs
    where
        I: IntoIterator<Item = T>,
        T: Into<std::ffi::OsString> + Clone,
    {
        nncase_clamp_microbenchmark::parse_extract_args(args)
    }

    pub fn main() -> Result<(), Box<dyn std::error::Error>> {
        let _ = env_logger::try_init();
        let args = parse_args(std::env::args_os());
        print_benchmark_workload_spec(&workload_spec());
        print_extract_run_configuration(&args);
        let progress = BenchProgress::new(args.rewrite_iters as u64, args.extractors.len() as u64);
        let rows = nncase_clamp_microbenchmark::run_extract_comparison_with_options_and_progress(
            args.rewrite_iters,
            args.max_rewrite_mem_gib,
            &args.extractors,
            args.max_extract_time_secs,
            |event| match event {
                nncase_clamp_microbenchmark::ProgressEvent::RewriteIterationComplete {
                    current,
                    total,
                    tuple_count,
                    peak_memory_bytes,
                } => {
                    progress.rewrite_iteration_complete(
                        current,
                        total,
                        tuple_count,
                        peak_memory_bytes,
                    );
                }
                nncase_clamp_microbenchmark::ProgressEvent::RewriteStoppedByMemoryCap {
                    current,
                    total,
                    peak_memory_bytes,
                    cap_bytes,
                } => {
                    progress.rewrite_stopped(current, total, peak_memory_bytes, cap_bytes);
                }
                nncase_clamp_microbenchmark::ProgressEvent::ExtractPhaseStart {
                    method,
                    current,
                    total,
                } => {
                    progress.extract_phase_start(method, current, total);
                }
                nncase_clamp_microbenchmark::ProgressEvent::ExtractPhaseComplete {
                    method,
                    current,
                    total,
                    elapsed_ms,
                    peak_memory_bytes,
                } => {
                    progress.extract_phase_complete(
                        method,
                        current,
                        total,
                        elapsed_ms,
                        peak_memory_bytes,
                    );
                }
            },
        );
        progress.finish();
        print_extract_comparison_report(
            "Nncase Clamp Extract Comparison",
            rows.into_iter().map(|row| ExtractReportRow {
                method: row.method.to_string(),
                requested_rewrite_iters: row.requested_rewrite_iters,
                executed_rewrite_iters: row.executed_rewrite_iters,
                max_rewrite_mem_gib: row.max_rewrite_mem_gib,
                run_ruleset_note: row.run_ruleset_note,
                rewrite_peak_memory_bytes: row.rewrite_peak_memory_bytes,
                extract_peak_memory_bytes: row.extract_peak_memory_bytes,
                elapsed: row.elapsed,
                rendered: row.rendered,
                svg_path: row.svg_path,
            }),
        );
        Ok(())
    }
}

#[cfg(feature = "rustsat-extract")]
fn main() -> Result<(), Box<dyn std::error::Error>> {
    real::main()
}
