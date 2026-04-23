use crate::helpers::bench_cli::ExtractBenchCliArgs;
use crate::helpers::progress::{format_bytes, format_optional_bytes, format_optional_duration};
use std::time::Duration;

#[derive(Debug, Clone)]
pub struct ExtractReportRow {
    pub method: String,
    pub requested_rewrite_iters: usize,
    pub executed_rewrite_iters: usize,
    pub max_rewrite_mem_gib: u64,
    pub run_ruleset_note: String,
    pub rewrite_peak_memory_bytes: u64,
    pub extract_peak_memory_bytes: Option<u64>,
    pub elapsed: Option<Duration>,
    pub rendered: String,
    pub svg_path: String,
}

pub fn print_extract_run_configuration(args: &ExtractBenchCliArgs) {
    println!("Requested rewrite iterations: {}", args.rewrite_iters);
    println!("Run ruleset memory cap: {} GiB", args.max_rewrite_mem_gib);
    println!("Selected extractors: {}", args.extractors.join(", "));
    println!(
        "Max extract time: {}",
        args.max_extract_time_secs
            .map(|secs| format!("{secs} s"))
            .unwrap_or_else(|| "unlimited".to_string())
    );
    println!();
}

pub fn print_extract_comparison_report<I>(title: &str, rows: I)
where
    I: IntoIterator<Item = ExtractReportRow>,
{
    println!("{title}");
    println!(
        "| Extract Method | Requested Iters | Executed Iters | Max Rewrite Mem | Run Ruleset Note | Run Ruleset Peak Mem | Extract Peak Mem | Time | Best Expression | SVG Path |"
    );
    println!("| --- | ---: | ---: | ---: | --- | ---: | ---: | ---: | --- | --- |");
    for row in rows {
        println!(
            "| {} | {} | {} | {} GiB | {} | {} | {} | {} | `{}` | `{}` |",
            row.method,
            row.requested_rewrite_iters,
            row.executed_rewrite_iters,
            row.max_rewrite_mem_gib,
            row.run_ruleset_note.replace('|', "\\|"),
            format_bytes(row.rewrite_peak_memory_bytes),
            format_optional_bytes(row.extract_peak_memory_bytes),
            format_optional_duration(row.elapsed),
            row.rendered.replace('|', "\\|"),
            row.svg_path.replace('|', "\\|")
        );
    }
}
