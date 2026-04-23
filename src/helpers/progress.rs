use indicatif::{MultiProgress, ProgressBar, ProgressStyle};
use std::time::Duration;

pub struct BenchProgress {
    rewrite_bar: ProgressBar,
    extract_bar: ProgressBar,
}

pub struct TimelineExtractMetric<'a> {
    pub method: &'a str,
    pub elapsed_ms: Option<f64>,
    pub peak_memory_bytes: Option<u64>,
}

impl BenchProgress {
    pub fn new(rewrite_len: u64, extract_len: u64) -> Self {
        let mp = MultiProgress::new();
        let rewrite_bar = make_bar(&mp, rewrite_len, "rewrite");
        let extract_bar = make_bar(&mp, extract_len, "extract");
        Self {
            rewrite_bar,
            extract_bar,
        }
    }

    pub fn rewrite_iteration_complete(
        &self,
        current: usize,
        total: usize,
        tuple_count: usize,
        peak_memory_bytes: u64,
    ) {
        self.rewrite_bar.set_length(total as u64);
        self.rewrite_bar.set_position(current as u64);
        self.rewrite_bar.set_message(format!(
            "tuples={} peak={}",
            tuple_count,
            format_bytes(peak_memory_bytes)
        ));
    }

    pub fn rewrite_stopped(
        &self,
        current: usize,
        total: usize,
        peak_memory_bytes: u64,
        cap_bytes: u64,
    ) {
        self.rewrite_bar.set_length(total as u64);
        self.rewrite_bar.set_position(current as u64);
        self.rewrite_bar.finish_with_message(format!(
            "stopped: peak={} cap={}",
            format_bytes(peak_memory_bytes),
            format_bytes(cap_bytes)
        ));
    }

    pub fn extract_phase_start(&self, method: &str, current: usize, total: usize) {
        self.extract_bar.set_length(total as u64);
        self.extract_bar
            .set_position((current.saturating_sub(1)) as u64);
        self.extract_bar.set_message(format!("starting {method}"));
    }

    pub fn extract_phase_complete(
        &self,
        method: &str,
        current: usize,
        total: usize,
        elapsed_ms: f64,
        peak_memory_bytes: u64,
    ) {
        self.extract_bar.set_length(total as u64);
        self.extract_bar.set_position(current as u64);
        self.extract_bar.set_message(format!(
            "{} done in {}, peak={}",
            method,
            if elapsed_ms.is_nan() {
                "NaN".to_string()
            } else {
                format!("{elapsed_ms:.3} ms")
            },
            if peak_memory_bytes == 0 {
                "NaN".to_string()
            } else {
                format_bytes(peak_memory_bytes)
            }
        ));
    }

    pub fn timeline_iteration(
        &self,
        rewrite_position: u64,
        iteration_label: usize,
        tuple_count: usize,
        rewrite_peak_memory_bytes: u64,
        extract_metrics: &[TimelineExtractMetric<'_>],
    ) {
        self.rewrite_bar.set_position(rewrite_position);
        self.rewrite_bar.set_message(format!(
            "iter={} tuples={} peak={}",
            iteration_label,
            tuple_count,
            format_bytes(rewrite_peak_memory_bytes)
        ));
        for (idx, metric) in extract_metrics.iter().enumerate() {
            self.extract_bar.set_position((idx + 1) as u64);
            self.extract_bar.set_message(format!(
                "iter {} {} {} peak={}",
                iteration_label,
                metric.method,
                format_optional_ms(metric.elapsed_ms),
                format_optional_bytes(metric.peak_memory_bytes)
            ));
        }
    }

    pub fn finish(&self) {
        if !self.rewrite_bar.is_finished() {
            self.rewrite_bar.finish_with_message("completed");
        }
        if !self.extract_bar.is_finished() {
            self.extract_bar.finish_with_message("completed");
        }
    }
}

pub fn format_duration(duration: Duration) -> String {
    format!("{:.3} ms", duration.as_secs_f64() * 1000.0)
}

pub fn format_optional_duration(duration: Option<Duration>) -> String {
    duration
        .map(format_duration)
        .unwrap_or_else(|| "NaN".to_string())
}

pub fn format_bytes(bytes: u64) -> String {
    format!("{:.2} MiB", bytes as f64 / (1024.0 * 1024.0))
}

pub fn format_optional_bytes(bytes: Option<u64>) -> String {
    bytes.map(format_bytes).unwrap_or_else(|| "NaN".to_string())
}

pub fn format_optional_ms(value: Option<f64>) -> String {
    value
        .map(|ms| format!("{ms:.3} ms"))
        .unwrap_or_else(|| "NaN ms".to_string())
}

fn make_bar(mp: &MultiProgress, len: u64, prefix: &str) -> ProgressBar {
    let bar = mp.add(ProgressBar::new(len));
    bar.set_prefix(prefix.to_string());
    bar.set_style(
        ProgressStyle::with_template("{prefix:>14} [{bar:40.cyan/blue}] {pos:>3}/{len:<3} {msg}")
            .unwrap()
            .progress_chars("=> "),
    );
    bar
}
