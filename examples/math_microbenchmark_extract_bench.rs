#[cfg(not(feature = "rustsat-extract"))]
fn main() {
    eprintln!("math_microbenchmark_extract_bench requires --features rustsat-extract");
}

#[path = "../benches/runners/eggplant_rewrite/math_microbenchmark.rs"]
mod typed_math_microbenchmark;

#[cfg(feature = "rustsat-extract")]
mod real {
    use super::typed_math_microbenchmark;
    use std::time::Duration;

    pub struct CliArgs {
        pub rewrite_iters: usize,
        pub max_rewrite_mem_gib: u64,
    }

    pub fn parse_args<I, S>(args: I) -> CliArgs
    where
        I: IntoIterator<Item = S>,
        S: AsRef<str>,
    {
        let mut rewrite_iters = 11usize;
        let mut max_rewrite_mem_gib = 20u64;
        let mut it = args.into_iter().map(|arg| arg.as_ref().to_string());
        let _program = it.next();
        while let Some(arg) = it.next() {
            match arg.as_str() {
                "--max_iter" => {
                    let value = it
                        .next()
                        .unwrap_or_else(|| panic!("missing value for --max_iter"));
                    rewrite_iters = value
                        .parse::<usize>()
                        .unwrap_or_else(|_| panic!("invalid usize for --max_iter: {value}"));
                }
                "--max_mem" => {
                    let value = it
                        .next()
                        .unwrap_or_else(|| panic!("missing value for --max_mem"));
                    max_rewrite_mem_gib = value
                        .parse::<u64>()
                        .unwrap_or_else(|_| panic!("invalid u64 for --max_mem: {value}"));
                }
                other => panic!("unknown argument: {other}"),
            }
        }
        CliArgs {
            rewrite_iters,
            max_rewrite_mem_gib,
        }
    }

    fn format_duration(duration: Duration) -> String {
        format!("{:.3} ms", duration.as_secs_f64() * 1000.0)
    }

    fn format_bytes(bytes: u64) -> String {
        format!("{:.2} MiB", bytes as f64 / (1024.0 * 1024.0))
    }

    pub fn main() -> Result<(), Box<dyn std::error::Error>> {
        let _ = env_logger::try_init();
        let args = parse_args(std::env::args());

        println!("CPU-style DSL costs used for Math:");
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
        println!("| MDiff | 32 |");
        println!("| MIntegral | 40 |");
        println!("| MConst | 0 |");
        println!("| MVar | 0 |");
        println!();

        println!("Target root:");
        println!("`Div(1, Sub(Div(Add(1, Sqrt(five)), 2), Div(Sub(1, Sqrt(five)), 2)))`");
        println!();

        println!("Requested rewrite iterations: {}", args.rewrite_iters);
        println!("Run ruleset memory cap: {} GiB", args.max_rewrite_mem_gib);
        println!();

        let rows = typed_math_microbenchmark::run_extract_comparison_with_iters_and_mem_cap(
            args.rewrite_iters,
            args.max_rewrite_mem_gib,
        );

        println!("Math Microbenchmark Extract Comparison");
        println!(
            "| Extract Method | Requested Iters | Executed Iters | Max Rewrite Mem | Run Ruleset Note | Run Ruleset Peak Mem | Extract Peak Mem | Time | Best Expression | SVG Path |"
        );
        println!("| --- | ---: | ---: | ---: | --- | ---: | ---: | ---: | --- | --- |");
        for row in rows {
            let rendered = row.rendered.replace('|', "\\|");
            let svg_path = row.svg_path.replace('|', "\\|");
            let note = row.run_ruleset_note.replace('|', "\\|");
            println!(
                "| {} | {} | {} | {} GiB | {} | {} | {} | {} | `{}` | `{}` |",
                row.method,
                row.requested_rewrite_iters,
                row.executed_rewrite_iters,
                row.max_rewrite_mem_gib,
                note,
                format_bytes(row.rewrite_peak_memory_bytes),
                format_bytes(row.extract_peak_memory_bytes),
                format_duration(row.elapsed),
                rendered,
                svg_path
            );
        }

        Ok(())
    }
}

#[cfg(feature = "rustsat-extract")]
pub use real::*;

#[cfg(feature = "rustsat-extract")]
fn main() -> Result<(), Box<dyn std::error::Error>> {
    real::main()
}
