use crate::helpers::bench_cli::ExtractBenchCliArgs;
use crate::helpers::progress::{format_bytes, format_optional_bytes, format_optional_duration};
use serde::Serialize;
use serde_json::Value as JsonValue;
use std::collections::BTreeMap;
use std::path::Path;
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

pub struct BenchmarkPositioning<'a> {
    pub benchmark_family: &'a str,
    pub benchmark_case: &'a str,
    pub baseline: &'a str,
    pub comparison_target: &'a str,
    pub positioning: &'a str,
}

pub struct BenchmarkWorkloadSpec<'a> {
    pub positioning: BenchmarkPositioning<'a>,
    pub op_costs: &'a [(&'a str, u64)],
    pub target_root: &'a str,
    pub rewrite_rules: &'a [&'a str],
    pub rule_witnesses: &'a [(&'a str, &'a str)],
}

pub const NNCASE_EGRAPH_BASELINE: &str = "nncase egraph implementation";
pub const EGGPLANT_COMPARISON_TARGET: &str = "eggplant";
pub const NNCASE_EGRAPH_SLOWER_POSITIONING: &str = "This nncase workload is included because the original nncase egraph implementation is slower than eggplant on this rewrite/extract shape.";

pub fn nncase_benchmark_positioning(benchmark_case: &str) -> BenchmarkPositioning<'_> {
    BenchmarkPositioning {
        benchmark_family: "nncase",
        benchmark_case,
        baseline: NNCASE_EGRAPH_BASELINE,
        comparison_target: EGGPLANT_COMPARISON_TARGET,
        positioning: NNCASE_EGRAPH_SLOWER_POSITIONING,
    }
}

pub fn print_benchmark_positioning(positioning: &BenchmarkPositioning<'_>) {
    println!("Benchmark positioning:");
    println!("  Family: {}", positioning.benchmark_family);
    println!("  Case: {}", positioning.benchmark_case);
    println!("  Baseline: {}", positioning.baseline);
    println!("  Comparison target: {}", positioning.comparison_target);
    println!("  Note: {}", positioning.positioning);
    println!();
}

pub fn print_benchmark_workload_spec(spec: &BenchmarkWorkloadSpec<'_>) {
    print_benchmark_positioning(&spec.positioning);
    println!("Nncase workload:");
    println!("  Target root: {}", spec.target_root);
    println!("  Op costs:");
    for (op, cost) in spec.op_costs {
        println!("    {op}: {cost}");
    }
    println!("  Rewrite rules:");
    for rule in spec.rewrite_rules {
        println!("    {rule}");
    }
    println!("  Rule witnesses:");
    for (rule, witness) in spec.rule_witnesses {
        println!("    {rule}: {witness}");
    }
    println!();
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

pub fn render_timeline_markdown_report<T>(title: &str, report: &T) -> serde_json::Result<String>
where
    T: Serialize,
{
    render_timeline_markdown_report_with_plot(title, report, None)
}

pub fn render_timeline_markdown_report_with_plot<T>(
    title: &str,
    report: &T,
    plot_path: Option<&str>,
) -> serde_json::Result<String>
where
    T: Serialize,
{
    let value = serde_json::to_value(report)?;
    Ok(render_timeline_markdown_value(title, &value, plot_path))
}

pub fn write_timeline_markdown_report<T>(
    title: &str,
    report: &T,
    output_path: impl AsRef<Path>,
) -> std::io::Result<()>
where
    T: Serialize,
{
    let markdown = render_timeline_markdown_report(title, report)
        .map_err(|err| std::io::Error::new(std::io::ErrorKind::Other, err))?;
    write_markdown(output_path, markdown)
}

pub fn write_timeline_markdown_report_with_plot<T>(
    title: &str,
    report: &T,
    output_path: impl AsRef<Path>,
    plot_path: Option<&str>,
) -> std::io::Result<()>
where
    T: Serialize,
{
    let markdown = render_timeline_markdown_report_with_plot(title, report, plot_path)
        .map_err(|err| std::io::Error::new(std::io::ErrorKind::Other, err))?;
    write_markdown(output_path, markdown)
}

fn write_markdown(output_path: impl AsRef<Path>, markdown: String) -> std::io::Result<()> {
    let output_path = output_path.as_ref();
    if let Some(parent) = output_path
        .parent()
        .filter(|parent| !parent.as_os_str().is_empty())
    {
        std::fs::create_dir_all(parent)?;
    }
    std::fs::write(output_path, markdown)
}

#[cfg(feature = "timeline-plot")]
#[derive(Debug, Clone)]
struct TimelinePlotMetric {
    elapsed_ms: Option<f64>,
    peak_mib: Option<f64>,
    cost: Option<f64>,
}

#[cfg(feature = "timeline-plot")]
#[derive(Debug, Clone)]
struct TimelinePlotPoint {
    iteration: f64,
    tuple_count: Option<f64>,
    rewrite_elapsed_ms: Option<f64>,
    rewrite_peak_mib: Option<f64>,
    extracts: BTreeMap<String, TimelinePlotMetric>,
}

#[cfg(feature = "timeline-plot")]
#[derive(Debug, Clone)]
struct TimelinePlotSeries {
    label: Option<String>,
    points: Vec<(f64, f64)>,
}

#[cfg(feature = "timeline-plot")]
pub fn write_timeline_plot_png<T>(
    title: &str,
    report: &T,
    output_path: impl AsRef<Path>,
) -> std::io::Result<()>
where
    T: Serialize,
{
    let value = serde_json::to_value(report)
        .map_err(|err| std::io::Error::new(std::io::ErrorKind::Other, err))?;
    write_timeline_plot_png_value(title, &value, output_path.as_ref())
}

#[cfg(feature = "timeline-plot")]
fn write_timeline_plot_png_value(
    base_title: &str,
    report: &JsonValue,
    output_path: &Path,
) -> std::io::Result<()> {
    use plotters::coord::Shift;
    use plotters::prelude::*;

    fn plot_err<E>(err: DrawingAreaErrorKind<E>) -> std::io::Error
    where
        E: std::error::Error + Send + Sync + std::fmt::Debug + 'static,
    {
        std::io::Error::new(std::io::ErrorKind::Other, format!("{err:?}"))
    }

    fn draw_line_panel(
        area: &DrawingArea<BitMapBackend<'_>, Shift>,
        title: &str,
        y_label: &str,
        x_range: std::ops::Range<f64>,
        series: &[TimelinePlotSeries],
        show_legend: bool,
    ) -> std::io::Result<()> {
        let mut chart = ChartBuilder::on(area)
            .caption(title, ("sans-serif", 28))
            .margin(18)
            .x_label_area_size(46)
            .y_label_area_size(82)
            .build_cartesian_2d(x_range, plot_y_range(series))
            .map_err(plot_err)?;

        chart
            .configure_mesh()
            .x_desc("Iteration")
            .y_desc(y_label)
            .label_style(("sans-serif", 18))
            .axis_desc_style(("sans-serif", 20))
            .draw()
            .map_err(plot_err)?;

        for (idx, line) in series.iter().enumerate() {
            if line.points.is_empty() {
                continue;
            }
            let color = Palette99::pick(idx).mix(0.95);
            let line_style = ShapeStyle::from(&color).stroke_width(3);
            let points = line.points.clone();
            let drawn = chart
                .draw_series(LineSeries::new(points.clone(), line_style))
                .map_err(plot_err)?;
            if let Some(label) = &line.label {
                let legend_style = line_style;
                drawn.label(label.clone()).legend(move |(x, y)| {
                    PathElement::new(vec![(x, y), (x + 25, y)], legend_style)
                });
            }
            chart
                .draw_series(
                    points
                        .iter()
                        .map(|(x, y)| Circle::new((*x, *y), 5, color.filled())),
                )
                .map_err(plot_err)?;
        }

        if show_legend {
            chart
                .configure_series_labels()
                .background_style(WHITE.mix(0.85))
                .border_style(BLACK)
                .label_font(("sans-serif", 18))
                .draw()
                .map_err(plot_err)?;
        }

        Ok(())
    }

    if let Some(parent) = output_path
        .parent()
        .filter(|parent| !parent.as_os_str().is_empty())
    {
        std::fs::create_dir_all(parent)?;
    }

    let points = collect_timeline_plot_points(report);
    let methods = selected_timeline_plot_methods(report, &points);
    let x_range = plot_x_range(&points);
    let root = BitMapBackend::new(output_path, (2560, 2240)).into_drawing_area();
    root.fill(&WHITE).map_err(plot_err)?;
    let body = root
        .titled(
            &timeline_report_title(base_title, report),
            ("sans-serif", 44),
        )
        .map_err(plot_err)?;
    let areas = body.split_evenly((3, 2));

    draw_line_panel(
        &areas[0],
        "Tuple Count by Iteration",
        "Tuples",
        x_range.clone(),
        &[TimelinePlotSeries {
            label: None,
            points: points
                .iter()
                .filter_map(|point| point.tuple_count.map(|value| (point.iteration, value)))
                .collect(),
        }],
        false,
    )?;
    draw_line_panel(
        &areas[1],
        "Rewrite Time by Iteration",
        "ms",
        x_range.clone(),
        &[TimelinePlotSeries {
            label: Some("rewrite elapsed".to_string()),
            points: points
                .iter()
                .filter_map(|point| {
                    point
                        .rewrite_elapsed_ms
                        .map(|value| (point.iteration, value))
                })
                .collect(),
        }],
        true,
    )?;
    draw_line_panel(
        &areas[2],
        "Rewrite Peak Memory by Iteration",
        "MiB",
        x_range.clone(),
        &[TimelinePlotSeries {
            label: None,
            points: points
                .iter()
                .filter_map(|point| point.rewrite_peak_mib.map(|value| (point.iteration, value)))
                .collect(),
        }],
        false,
    )?;
    draw_line_panel(
        &areas[3],
        "Extraction Time by Iteration",
        "ms",
        x_range.clone(),
        &method_series(&points, &methods, |metric| metric.elapsed_ms),
        true,
    )?;
    draw_line_panel(
        &areas[4],
        "Extraction Peak Memory by Iteration",
        "MiB",
        x_range.clone(),
        &method_series(&points, &methods, |metric| metric.peak_mib),
        true,
    )?;
    draw_line_panel(
        &areas[5],
        "Extraction Cost by Iteration",
        "Cost",
        x_range,
        &method_series(&points, &methods, |metric| metric.cost),
        true,
    )?;

    root.present().map_err(plot_err)
}

#[cfg(feature = "timeline-plot")]
fn collect_timeline_plot_points(report: &JsonValue) -> Vec<TimelinePlotPoint> {
    report
        .get("points")
        .and_then(JsonValue::as_array)
        .into_iter()
        .flatten()
        .map(|point| {
            let extracts = point
                .get("extracts")
                .and_then(JsonValue::as_array)
                .into_iter()
                .flatten()
                .filter_map(|metric| {
                    let method = metric.get("method")?.as_str()?.to_string();
                    Some((
                        method,
                        TimelinePlotMetric {
                            elapsed_ms: metric.get("elapsed_ms").and_then(JsonValue::as_f64),
                            peak_mib: metric
                                .get("peak_memory_bytes")
                                .and_then(JsonValue::as_u64)
                                .map(bytes_to_mib),
                            cost: metric.get("cost").and_then(JsonValue::as_f64),
                        },
                    ))
                })
                .collect();

            TimelinePlotPoint {
                iteration: point
                    .get("iteration")
                    .and_then(JsonValue::as_f64)
                    .unwrap_or(0.0),
                tuple_count: point.get("tuple_count").and_then(JsonValue::as_f64),
                rewrite_elapsed_ms: point.get("rewrite_elapsed_ms").and_then(JsonValue::as_f64),
                rewrite_peak_mib: point
                    .get("rewrite_peak_memory_bytes")
                    .and_then(JsonValue::as_u64)
                    .map(bytes_to_mib),
                extracts,
            }
        })
        .collect()
}

#[cfg(feature = "timeline-plot")]
fn selected_timeline_plot_methods(report: &JsonValue, points: &[TimelinePlotPoint]) -> Vec<String> {
    let selected = report
        .get("selected_extractors")
        .and_then(JsonValue::as_array)
        .into_iter()
        .flatten()
        .filter_map(JsonValue::as_str)
        .map(ToOwned::to_owned)
        .collect::<Vec<_>>();
    if !selected.is_empty() {
        return selected;
    }

    points
        .first()
        .map(|point| point.extracts.keys().cloned().collect())
        .unwrap_or_default()
}

#[cfg(feature = "timeline-plot")]
fn method_series(
    points: &[TimelinePlotPoint],
    methods: &[String],
    value: impl Fn(&TimelinePlotMetric) -> Option<f64>,
) -> Vec<TimelinePlotSeries> {
    methods
        .iter()
        .map(|method| TimelinePlotSeries {
            label: Some(method.clone()),
            points: points
                .iter()
                .filter_map(|point| {
                    point
                        .extracts
                        .get(method)
                        .and_then(|metric| value(metric))
                        .map(|value| (point.iteration, value))
                })
                .collect(),
        })
        .collect()
}

#[cfg(feature = "timeline-plot")]
fn plot_x_range(points: &[TimelinePlotPoint]) -> std::ops::Range<f64> {
    let min = points
        .iter()
        .map(|point| point.iteration)
        .fold(f64::INFINITY, f64::min);
    let max = points
        .iter()
        .map(|point| point.iteration)
        .fold(f64::NEG_INFINITY, f64::max);
    if !min.is_finite() || !max.is_finite() {
        return 0.0..1.0;
    }
    if (max - min).abs() < f64::EPSILON {
        return (min - 1.0)..(max + 1.0);
    }
    min..max
}

#[cfg(feature = "timeline-plot")]
fn plot_y_range(series: &[TimelinePlotSeries]) -> std::ops::Range<f64> {
    let mut min = f64::INFINITY;
    let mut max = f64::NEG_INFINITY;
    for value in series
        .iter()
        .flat_map(|line| line.points.iter().map(|(_, y)| *y))
        .filter(|value| value.is_finite())
    {
        min = min.min(value);
        max = max.max(value);
    }

    if !min.is_finite() || !max.is_finite() {
        return 0.0..1.0;
    }

    if min >= 0.0 {
        min = 0.0;
    }
    if (max - min).abs() < f64::EPSILON {
        let pad = if max.abs() < 1.0 {
            1.0
        } else {
            max.abs() * 0.1
        };
        return min..(max + pad);
    }

    let pad = (max - min) * 0.1;
    let start = if min >= 0.0 { 0.0 } else { min - pad };
    start..(max + pad)
}

#[cfg(feature = "timeline-plot")]
fn bytes_to_mib(bytes: u64) -> f64 {
    bytes as f64 / (1024.0 * 1024.0)
}

fn render_timeline_markdown_value(
    base_title: &str,
    report: &JsonValue,
    plot_path: Option<&str>,
) -> String {
    let points = report
        .get("points")
        .and_then(JsonValue::as_array)
        .map(Vec::as_slice)
        .unwrap_or(&[]);
    let selected_extractors = report
        .get("selected_extractors")
        .and_then(JsonValue::as_array)
        .map(|values| {
            values
                .iter()
                .filter_map(JsonValue::as_str)
                .collect::<Vec<_>>()
                .join(", ")
        })
        .filter(|value| !value.is_empty())
        .unwrap_or_else(|| "n/a".to_string());
    let title = timeline_report_title(base_title, report);
    let mut rule_totals = BTreeMap::<String, u64>::new();
    for point in points {
        if let Some(rule_matches) = point.get("rule_matches").and_then(JsonValue::as_object) {
            for (rule, count) in rule_matches {
                *rule_totals.entry(rule.clone()).or_default() += count.as_u64().unwrap_or(0);
            }
        }
    }

    let mut lines = vec![
        format!("# {title}"),
        String::new(),
        "## Report Metadata".to_string(),
        String::new(),
        "| Key | Value |".to_string(),
        "| --- | --- |".to_string(),
        format!(
            "| Version Nickname | {} |",
            markdown_cell(
                report
                    .get("version_nickname")
                    .and_then(JsonValue::as_str)
                    .unwrap_or("n/a")
            )
        ),
        format!(
            "| Selected Extractors | {} |",
            markdown_cell(&selected_extractors)
        ),
        format!(
            "| Max Extract Time | {} |",
            report
                .get("max_extract_time_secs")
                .and_then(JsonValue::as_u64)
                .map(|secs| format!("{secs} s"))
                .unwrap_or_else(|| "n/a".to_string())
        ),
        format!(
            "| Requested Iters | {} |",
            json_u64_cell(report, "requested_rewrite_iters")
        ),
        format!(
            "| Executed Iters | {} |",
            json_u64_cell(report, "executed_rewrite_iters")
        ),
        format!(
            "| Max Rewrite Mem | {} GiB |",
            json_u64_cell(report, "max_rewrite_mem_gib")
        ),
        format!(
            "| Stopped Early | {} |",
            report
                .get("stopped_early_due_to_memory_cap")
                .and_then(JsonValue::as_bool)
                .map(|value| value.to_string())
                .unwrap_or_else(|| "n/a".to_string())
        ),
        String::new(),
    ];

    if has_benchmark_positioning(report) {
        lines.extend([
            "## Benchmark Positioning".to_string(),
            String::new(),
            "| Key | Value |".to_string(),
            "| --- | --- |".to_string(),
            format!(
                "| Benchmark Family | {} |",
                json_str_markdown_cell(report, "benchmark_family")
            ),
            format!(
                "| Benchmark Case | {} |",
                json_str_markdown_cell(report, "benchmark_case")
            ),
            format!(
                "| Baseline | {} |",
                json_str_markdown_cell(report, "baseline")
            ),
            format!(
                "| Comparison Target | {} |",
                json_str_markdown_cell(report, "comparison_target")
            ),
            format!(
                "| Positioning | {} |",
                json_str_markdown_cell(report, "positioning")
            ),
            String::new(),
        ]);
    }

    if let Some(plot_path) = plot_path.filter(|path| !path.is_empty()) {
        lines.extend([
            "## Timeline Plot".to_string(),
            String::new(),
            format!("![Timeline Plot]({plot_path})"),
            String::new(),
        ]);
    }

    lines.extend(["## Rule Match Totals".to_string(), String::new()]);

    if rule_totals.is_empty() {
        lines.push("No rule matches recorded.".to_string());
    } else {
        lines.push("| Rule | Total Matches |".to_string());
        lines.push("| --- | ---: |".to_string());
        for (rule, count) in &rule_totals {
            lines.push(format!("| {} | {count} |", markdown_cell(rule)));
        }
    }

    lines.extend([
        String::new(),
        "## Rule Matches Per Iteration".to_string(),
        String::new(),
    ]);
    if rule_totals.is_empty() {
        lines.push("No rule matches recorded.".to_string());
    } else {
        let rule_names = rule_totals.keys().cloned().collect::<Vec<_>>();
        lines.push(format!(
            "| Iteration | {} |",
            rule_names
                .iter()
                .map(|rule| markdown_cell(rule))
                .collect::<Vec<_>>()
                .join(" | ")
        ));
        lines.push(format!(
            "| ---: | {} |",
            rule_names
                .iter()
                .map(|_| "---:")
                .collect::<Vec<_>>()
                .join(" | ")
        ));
        for point in points {
            let rule_matches = point.get("rule_matches").and_then(JsonValue::as_object);
            let cells = rule_names
                .iter()
                .map(|rule| {
                    rule_matches
                        .and_then(|matches| matches.get(rule))
                        .and_then(JsonValue::as_u64)
                        .unwrap_or(0)
                        .to_string()
                })
                .collect::<Vec<_>>()
                .join(" | ");
            lines.push(format!(
                "| {} | {cells} |",
                value_u64_cell(point.get("iteration"))
            ));
        }
    }

    lines.extend([
        String::new(),
        "## Per-Iteration Metrics".to_string(),
        String::new(),
        "| Iteration | Tuples | Rewrite ms | Rewrite MiB |".to_string(),
        "| ---: | ---: | ---: | ---: |".to_string(),
    ]);
    for point in points {
        lines.push(format!(
            "| {} | {} | {} | {} |",
            value_u64_cell(point.get("iteration")),
            value_u64_cell(point.get("tuple_count")),
            value_f64_cell(point.get("rewrite_elapsed_ms"), 3),
            value_mib_cell(point.get("rewrite_peak_memory_bytes"))
        ));
    }

    lines.extend([
        String::new(),
        "## Per-Iteration SVGs".to_string(),
        String::new(),
        "| Iteration | Method | Tuples | Rewrite ms | Rewrite MiB | Extract ms | Cost | Extract MiB | Timed Out | SVG Path | Preview |".to_string(),
        "| ---: | --- | ---: | ---: | ---: | ---: | ---: | ---: | --- | --- | --- |".to_string(),
    ]);
    for point in points {
        let extracts = point
            .get("extracts")
            .and_then(JsonValue::as_array)
            .map(Vec::as_slice)
            .unwrap_or(&[]);
        for metric in extracts {
            let method = metric
                .get("method")
                .and_then(JsonValue::as_str)
                .unwrap_or("n/a");
            let svg_path = metric
                .get("svg_path")
                .and_then(JsonValue::as_str)
                .unwrap_or("n/a");
            let (svg_cell, preview_cell) = if svg_path == "n/a" || svg_path.is_empty() {
                ("n/a".to_string(), "n/a".to_string())
            } else {
                (
                    format!("[{}]({})", markdown_cell(svg_path), svg_path),
                    format!(
                        "![{} iteration {}]({})",
                        markdown_cell(method),
                        value_u64_cell(point.get("iteration")),
                        svg_path
                    ),
                )
            };
            lines.push(format!(
                "| {} | {} | {} | {} | {} | {} | {} | {} | {} | {} | {} |",
                value_u64_cell(point.get("iteration")),
                markdown_cell(method),
                value_u64_cell(point.get("tuple_count")),
                value_f64_cell(point.get("rewrite_elapsed_ms"), 3),
                value_mib_cell(point.get("rewrite_peak_memory_bytes")),
                metric
                    .get("elapsed_ms")
                    .and_then(JsonValue::as_f64)
                    .map(|value| format!("{value:.3} ms"))
                    .unwrap_or_else(|| "NaN".to_string()),
                value_number_cell(metric.get("cost")),
                value_mib_cell(metric.get("peak_memory_bytes")),
                metric
                    .get("timed_out")
                    .and_then(JsonValue::as_bool)
                    .map(|value| value.to_string())
                    .unwrap_or_else(|| "n/a".to_string()),
                svg_cell,
                preview_cell
            ));
        }
    }

    lines.push(String::new());
    lines.join("\n")
}

fn json_u64_cell(value: &JsonValue, key: &str) -> String {
    value_u64_cell(value.get(key))
}

fn json_str_markdown_cell(value: &JsonValue, key: &str) -> String {
    value
        .get(key)
        .and_then(JsonValue::as_str)
        .filter(|value| !value.is_empty())
        .map(markdown_cell)
        .unwrap_or_else(|| "n/a".to_string())
}

fn has_benchmark_positioning(report: &JsonValue) -> bool {
    [
        "benchmark_family",
        "benchmark_case",
        "baseline",
        "comparison_target",
        "positioning",
    ]
    .iter()
    .any(|key| {
        report
            .get(key)
            .and_then(JsonValue::as_str)
            .is_some_and(|value| !value.is_empty())
    })
}

fn timeline_report_title(base_title: &str, report: &JsonValue) -> String {
    report
        .get("version_nickname")
        .and_then(JsonValue::as_str)
        .filter(|value| !value.is_empty())
        .map(|nickname| format!("{base_title} - {nickname}"))
        .unwrap_or_else(|| base_title.to_string())
}

fn value_u64_cell(value: Option<&JsonValue>) -> String {
    value
        .and_then(JsonValue::as_u64)
        .map(|value| value.to_string())
        .unwrap_or_else(|| "n/a".to_string())
}

fn value_f64_cell(value: Option<&JsonValue>, precision: usize) -> String {
    value
        .and_then(JsonValue::as_f64)
        .map(|value| format!("{value:.precision$}"))
        .unwrap_or_else(|| "NaN".to_string())
}

fn value_mib_cell(value: Option<&JsonValue>) -> String {
    value
        .and_then(JsonValue::as_u64)
        .map(|bytes| format!("{:.2}", bytes as f64 / (1024.0 * 1024.0)))
        .unwrap_or_else(|| "NaN".to_string())
}

fn value_number_cell(value: Option<&JsonValue>) -> String {
    value
        .and_then(JsonValue::as_number)
        .map(ToString::to_string)
        .unwrap_or_else(|| "NaN".to_string())
}

fn markdown_cell(value: &str) -> String {
    value.replace('|', "\\|").replace('\n', "<br>")
}
