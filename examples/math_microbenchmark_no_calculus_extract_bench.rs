#[cfg(not(feature = "rustsat-extract"))]
fn main() {
    eprintln!("math_microbenchmark_no_calculus_extract_bench requires --features rustsat-extract");
}

#[path = "../benches/runners/eggplant_rewrite/math_microbenchmark_no_calculus.rs"]
mod typed_math_microbenchmark;

#[cfg(feature = "rustsat-extract")]
mod real {
    use super::typed_math_microbenchmark;
    use eggplant::helpers::bench_cli::{ExtractBenchCliArgs, parse_extract_bench_args};
    use eggplant::helpers::progress::BenchProgress;
    use eggplant::helpers::report::{
        ExtractReportRow, print_extract_comparison_report, print_extract_run_configuration,
    };

    pub type CliArgs = ExtractBenchCliArgs;

    pub fn parse_args<I, T>(args: I) -> CliArgs
    where
        I: IntoIterator<Item = T>,
        T: Into<std::ffi::OsString> + Clone,
    {
        parse_extract_bench_args(
            "math_microbenchmark_no_calculus_extract_bench",
            "Run the no-calculus math microbenchmark with CLI progress bars",
            11,
            20,
            args,
        )
    }

    pub fn main() -> Result<(), Box<dyn std::error::Error>> {
        let _ = env_logger::try_init();
        let args = parse_args(std::env::args_os());

        println!("CPU-style DSL costs used for Math (no calculus):");
        println!("| Op | Cost |");
        println!("| --- | ---: |");
        println!("| MAdd | 1 |");
        println!("| MSub | 1 |");
        println!("| MMul | 3 |");
        println!("| MDiv | 10 |");
        println!("| MPow | 25 |");
        println!("| MLn | 18 |");
        println!("| MSqrt | 16 |");
        println!("| MSin | 18 |");
        println!("| MCos | 18 |");
        println!("| MConst | 0 |");
        println!("| MVar | 0 |");
        println!();

        println!("Target root:");
        println!("`Div(1, Sub(Div(Add(1, Sqrt(five)), 2), Div(Sub(1, Sqrt(five)), 2)))`");
        println!();
        print_extract_run_configuration(&args);

        let progress = BenchProgress::new(args.rewrite_iters as u64, args.extractors.len() as u64);

        let rows = typed_math_microbenchmark::run_extract_comparison_with_options_and_progress(
            args.rewrite_iters,
            args.max_rewrite_mem_gib,
            &args.extractors,
            args.max_extract_time_secs,
            |event| match event {
                typed_math_microbenchmark::ProgressEvent::RewriteIterationComplete {
                    current,
                    total,
                    tuple_count,
                    peak_memory_bytes,
                } => progress.rewrite_iteration_complete(
                    current,
                    total,
                    tuple_count,
                    peak_memory_bytes,
                ),
                typed_math_microbenchmark::ProgressEvent::RewriteStoppedByMemoryCap {
                    current,
                    total,
                    peak_memory_bytes,
                    cap_bytes,
                } => progress.rewrite_stopped(current, total, peak_memory_bytes, cap_bytes),
                typed_math_microbenchmark::ProgressEvent::ExtractPhaseStart {
                    method,
                    current,
                    total,
                } => progress.extract_phase_start(method, current, total),
                typed_math_microbenchmark::ProgressEvent::ExtractPhaseComplete {
                    method,
                    current,
                    total,
                    elapsed_ms,
                    peak_memory_bytes,
                } => progress.extract_phase_complete(
                    method,
                    current,
                    total,
                    elapsed_ms,
                    peak_memory_bytes,
                ),
            },
        );

        progress.finish();

        print_extract_comparison_report(
            "Math Microbenchmark No-Calculus Extract Comparison",
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
pub use real::*;

#[cfg(feature = "rustsat-extract")]
fn main() -> Result<(), Box<dyn std::error::Error>> {
    real::main()
}
