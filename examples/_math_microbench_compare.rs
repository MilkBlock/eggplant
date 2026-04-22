#[path = "math_microbenchmark_support.rs"]
mod math_microbenchmark_support;
#[path = "../benches/runners/eggplant_rewrite/math_microbenchmark.rs"]
mod typed_math_microbenchmark;

use std::time::{Duration, Instant};

use egglog::EGraph;

struct MathMicrobenchmarkStats {
    elapsed: Duration,
    total_num_tuples: usize,
    table_sizes: Vec<(&'static str, usize)>,
}

fn print_stats(label: &str, stats: &MathMicrobenchmarkStats) {
    println!("{label} time: {:?}", stats.elapsed);
    println!("[{label}] total num_tuples = {}", stats.total_num_tuples);
    for (display, size) in &stats.table_sizes {
        println!("[{label}] {display} = {size}");
    }
}

fn collect_egg_stats() -> MathMicrobenchmarkStats {
    let egglog_repo_root = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../stable/egglog_sync_serialize_raw");
    let filename = egglog_repo_root.join("tests/math-microbenchmark.egg");
    let tables = [
        ("Diff", "Diff"),
        ("Integral", "Integral"),
        ("Add", "Add"),
        ("Sub", "Sub"),
        ("Mul", "Mul"),
        ("Div", "Div"),
        ("Pow", "Pow"),
        ("Ln", "Ln"),
        ("Sqrt", "Sqrt"),
        ("Sin", "Sin"),
        ("Cos", "Cos"),
        ("Const", "Const"),
        ("Var", "Var"),
    ];
    let mut egg = EGraph::default();
    let program = std::fs::read_to_string(&filename).unwrap();
    let started = Instant::now();
    egg.parse_and_run_program(Some(filename.to_string_lossy().into_owned()), &program)
        .unwrap();

    MathMicrobenchmarkStats {
        elapsed: started.elapsed(),
        total_num_tuples: egg.num_tuples(),
        table_sizes: tables
            .iter()
            .map(|(display, table)| (*display, egg.get_size(table)))
            .collect(),
    }
}

fn relabel_stats(
    stats: Vec<(&'static str, usize)>,
    relabel: &[(&'static str, &'static str)],
) -> Vec<(&'static str, usize)> {
    relabel
        .iter()
        .map(|(display, source)| {
            let size = stats
                .iter()
                .find_map(|(name, size)| (*name == *source).then_some(*size))
                .unwrap();
            (*display, size)
        })
        .collect()
}

fn collect_rust_rule_stats() -> MathMicrobenchmarkStats {
    let stats = math_microbenchmark_support::run_and_collect_stats();
    MathMicrobenchmarkStats {
        elapsed: stats.elapsed,
        total_num_tuples: stats.total_num_tuples,
        table_sizes: relabel_stats(
            stats.table_sizes,
            &[
                ("Diff", "MDiff"),
                ("Integral", "MIntegral"),
                ("Add", "MAdd"),
                ("Sub", "MSub"),
                ("Mul", "MMul"),
                ("Div", "MDiv"),
                ("Pow", "MPow"),
                ("Ln", "MLn"),
                ("Sqrt", "MSqrt"),
                ("Sin", "MSin"),
                ("Cos", "MCos"),
                ("Const", "MConst"),
                ("Var", "MVar"),
            ],
        ),
    }
}

fn collect_typed_stats() -> MathMicrobenchmarkStats {
    let stats = typed_math_microbenchmark::run_and_collect_stats(false);
    MathMicrobenchmarkStats {
        elapsed: stats.elapsed,
        total_num_tuples: stats.total_num_tuples,
        table_sizes: relabel_stats(
            stats.table_sizes,
            &[
                ("Diff", "MDiff"),
                ("Integral", "MIntegral"),
                ("Add", "MAdd"),
                ("Sub", "MSub"),
                ("Mul", "MMul"),
                ("Div", "MDiv"),
                ("Pow", "MPow"),
                ("Ln", "MLn"),
                ("Sqrt", "MSqrt"),
                ("Sin", "MSin"),
                ("Cos", "MCos"),
                ("Const", "MConst"),
                ("Var", "MVar"),
            ],
        ),
    }
}

fn main() {
    let egg = collect_egg_stats();
    let rust = collect_rust_rule_stats();
    let typed = collect_typed_stats();

    print_stats("egg", &egg);
    print_stats("rust", &rust);
    print_stats("typed", &typed);
}
