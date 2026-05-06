use egglog_reports::RunReport;
use eggplant::prelude::*;
use eggplant::tx_rx_vt_pr;
use eggplant::wrap::NonPatRecSgl;
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::path::PathBuf;
use std::time::{Duration, Instant};

#[cfg(feature = "rustsat-extract")]
use eggplant::egglog::extract::TreeAdditiveCostModel;
#[cfg(feature = "rustsat-extract")]
use eggplant::wrap::EgglogTy;
#[cfg(feature = "rustsat-extract")]
use std::fs;

#[eggplant::dsl]
enum PermTag {
    #[typst("id")]
    #[precedence(100)]
    #[cost(0)]
    PermId {},
    #[typst("sigma")]
    #[precedence(100)]
    #[cost(0)]
    PermSwap {},
}

#[eggplant::dsl]
enum TransposeExpr {
    #[display("{name}")]
    #[typst("{name}")]
    #[precedence(100)]
    #[cost(0)]
    TransposeInput { name: String },
    #[typst("T_{perm}({inner})")]
    #[precedence(80)]
    #[cost(6)]
    Transpose { inner: TransposeExpr, perm: PermTag },
    #[typst("{lhs} + {rhs}")]
    #[precedence(40)]
    #[cost(1)]
    Add {
        lhs: TransposeExpr,
        rhs: TransposeExpr,
    },
    #[typst("exp({inner})")]
    #[precedence(90)]
    #[cost(2)]
    UnaryExp { inner: TransposeExpr },
}

tx_rx_vt_pr!(NncaseTransposeTx, NncaseTransposePatRec);

pub struct MathExtractComparisonRow {
    pub method: &'static str,
    pub requested_rewrite_iters: usize,
    pub executed_rewrite_iters: usize,
    pub max_rewrite_mem_gib: u64,
    pub run_ruleset_note: String,
    pub rewrite_peak_memory_bytes: u64,
    pub extract_peak_memory_bytes: Option<u64>,
    pub cost: Option<u64>,
    pub elapsed: Option<Duration>,
    pub rendered: String,
    pub svg_path: String,
    pub timed_out: bool,
}

#[derive(Debug, Clone)]
pub enum ProgressEvent {
    RewriteIterationComplete {
        current: usize,
        total: usize,
        tuple_count: usize,
        peak_memory_bytes: u64,
    },
    RewriteStoppedByMemoryCap {
        current: usize,
        total: usize,
        peak_memory_bytes: u64,
        cap_bytes: u64,
    },
    ExtractPhaseStart {
        method: &'static str,
        current: usize,
        total: usize,
    },
    ExtractPhaseComplete {
        method: &'static str,
        current: usize,
        total: usize,
        elapsed_ms: f64,
        peak_memory_bytes: u64,
    },
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExtractTimelineMetric {
    pub method: String,
    pub cost: Option<u64>,
    pub elapsed_ms: Option<f64>,
    pub peak_memory_bytes: Option<u64>,
    pub svg_path: String,
    pub timed_out: bool,
}

#[derive(Debug, Clone, Serialize)]
pub struct RewriteTimelinePoint {
    pub iteration: usize,
    pub tuple_count: usize,
    pub rewrite_elapsed_ms: f64,
    pub rewrite_peak_memory_bytes: u64,
    pub rule_matches: BTreeMap<String, usize>,
    pub extracts: Vec<ExtractTimelineMetric>,
}

#[derive(Debug, Clone, Serialize)]
pub struct ExtractTimelineReport {
    pub version_nickname: Option<String>,
    pub benchmark_family: &'static str,
    pub benchmark_case: &'static str,
    pub baseline: &'static str,
    pub comparison_target: &'static str,
    pub positioning: &'static str,
    pub selected_extractors: Vec<String>,
    pub max_extract_time_secs: Option<u64>,
    pub requested_rewrite_iters: usize,
    pub executed_rewrite_iters: usize,
    pub max_rewrite_mem_gib: u64,
    pub stopped_early_due_to_memory_cap: bool,
    pub points: Vec<RewriteTimelinePoint>,
}

pub struct NncaseTransposeStats {
    pub total_num_tuples: usize,
    pub max_rewrite_mem_gib: u64,
    pub rewrite_peak_memory_bytes: u64,
    pub requested_rewrite_iters: usize,
    pub executed_rewrite_iters: usize,
    pub rewrite_stopped_early_due_to_memory_cap: bool,
}

#[cfg(feature = "rustsat-extract")]
const ALL_EXTRACT_METHODS: &[&str] = &["default", "eboost", "layered", "rustsat"];

const DEFAULT_RUN_RULESET_MEMORY_CAP_GIB: u64 = 20;

const OP_COSTS: &[(&str, u64)] = &[
    ("Input", 0),
    ("PermId", 0),
    ("PermSwap", 0),
    ("Add", 1),
    ("UnaryExp", 2),
    ("Transpose", 6),
];

const REWRITE_RULES: &[&str] = &[
    "combine_binary_transpose",
    "combine_unary_transpose",
    "fold_two_transposes",
    "fold_nop_transpose",
];

const RULE_WITNESSES: &[(&str, &str)] = &[
    (
        "combine_binary_transpose",
        "Add(Transpose(lhs, swap), Transpose(UnaryExp(rhs), swap))",
    ),
    ("combine_unary_transpose", "UnaryExp(Transpose(aux, swap))"),
    (
        "fold_two_transposes",
        "Transpose(Transpose(aux, swap), swap)",
    ),
    ("fold_nop_transpose", "Transpose(aux, id)"),
];

pub fn workload_spec() -> eggplant::helpers::report::BenchmarkWorkloadSpec<'static> {
    eggplant::helpers::report::BenchmarkWorkloadSpec {
        positioning: eggplant::helpers::report::nncase_benchmark_positioning("transpose"),
        op_costs: OP_COSTS,
        target_root: "Transpose(Add(Transpose(lhs, swap), Transpose(UnaryExp(rhs), swap)), swap)",
        rewrite_rules: REWRITE_RULES,
        rule_witnesses: RULE_WITNESSES,
    }
}

pub type ExtractCliArgs = eggplant::helpers::bench_cli::ExtractBenchCliArgs;
pub type TimelineCliArgs = eggplant::helpers::bench_cli::TimelineExportCliArgs;

pub fn parse_extract_args<I, T>(args: I) -> ExtractCliArgs
where
    I: IntoIterator<Item = T>,
    T: Into<std::ffi::OsString> + Clone,
{
    eggplant::helpers::bench_cli::parse_extract_bench_args(
        "nncase_transpose_extract_bench",
        "Run the nncase transpose microbenchmark with CLI progress bars",
        8,
        DEFAULT_RUN_RULESET_MEMORY_CAP_GIB,
        args,
    )
}

pub fn parse_timeline_args<I, T>(args: I) -> TimelineCliArgs
where
    I: IntoIterator<Item = T>,
    T: Into<std::ffi::OsString> + Clone,
{
    eggplant::helpers::bench_cli::parse_timeline_export_args(
        "nncase_transpose_timeline_export",
        "Export nncase transpose timeline data with CLI progress bars",
        8,
        12,
        "target/nncase_transpose_timeline.json",
        args,
    )
}

fn register_rewrite_rules(rs: RuleSetId) {
    NncaseTransposeTx::add_rule(
        "combine_binary_transpose",
        rs,
        || {
            let lhs = TransposeExpr::query_leaf();
            let rhs = TransposeExpr::query_leaf();
            let perm = PermTag::query_leaf();
            let lhs_t = Transpose::query(&lhs, &perm);
            let rhs_t = Transpose::query(&rhs, &perm);
            let add = Add::query(&lhs_t, &rhs_t);
            #[eggplant::pat_vars]
            struct Pat {
                lhs: TransposeExpr,
                rhs: TransposeExpr,
                perm: PermTag,
                add: Add,
            }
            Pat::new(lhs, rhs, perm, add)
        },
        |ctx, pat| {
            let pushed_add = ctx.insert_add(pat.lhs, pat.rhs);
            let rhs = ctx.insert_transpose(pushed_add, pat.perm);
            ctx.union(pat.add, rhs);
        },
    );

    NncaseTransposeTx::add_rule(
        "combine_unary_transpose",
        rs,
        || {
            let inner = TransposeExpr::query_leaf();
            let perm = PermTag::query_leaf();
            let t = Transpose::query(&inner, &perm);
            let exp = UnaryExp::query(&t);
            #[eggplant::pat_vars]
            struct Pat {
                inner: TransposeExpr,
                perm: PermTag,
                exp: UnaryExp,
            }
            Pat::new(inner, perm, exp)
        },
        |ctx, pat| {
            let inner_exp = ctx.insert_unary_exp(pat.inner);
            let rhs = ctx.insert_transpose(inner_exp, pat.perm);
            ctx.union(pat.exp, rhs);
        },
    );

    NncaseTransposeTx::add_rule(
        "fold_two_transposes",
        rs,
        || {
            let inner = TransposeExpr::query_leaf();
            let perm = PermSwap::query();
            let t1 = Transpose::query(&inner, &perm);
            let t2 = Transpose::query(&t1, &perm);
            #[eggplant::pat_vars]
            struct Pat {
                inner: TransposeExpr,
                t2: Transpose,
            }
            Pat::new(inner, t2)
        },
        |ctx, pat| {
            ctx.union(pat.t2, pat.inner);
        },
    );

    NncaseTransposeTx::add_rule(
        "fold_nop_transpose",
        rs,
        || {
            let inner = TransposeExpr::query_leaf();
            let perm = PermId::query();
            let t = Transpose::query(&inner, &perm);
            #[eggplant::pat_vars]
            struct Pat {
                inner: TransposeExpr,
                t: Transpose,
            }
            Pat::new(inner, t)
        },
        |ctx, pat| {
            ctx.union(pat.t, pat.inner);
        },
    );
}

fn seed_domain(seed_name: &'static str) {
    let seed = NncaseTransposeTx::new_ruleset(seed_name);
    NncaseTransposeTx::add_rule(
        seed_name,
        seed,
        || {
            #[eggplant::pat_vars_catch]
            struct Unit {}
        },
        |ctx, _pat| {
            let lhs = ctx.insert_transpose_input("lhs".to_owned());
            let rhs = ctx.insert_transpose_input("rhs".to_owned());
            let aux = ctx.insert_transpose_input("aux".to_owned());
            let swap = ctx.insert_perm_swap();
            let id = ctx.insert_perm_id();

            let lhs_t = ctx.insert_transpose(lhs, swap);
            let rhs_exp = ctx.insert_unary_exp(rhs);
            let rhs_t = ctx.insert_transpose(rhs_exp, swap);
            let add = ctx.insert_add(lhs_t, rhs_t);
            ctx.insert_transpose(add, swap);

            let aux_id = ctx.insert_transpose(aux, id);
            ctx.insert_unary_exp(aux_id);
            let aux_swap = ctx.insert_transpose(aux, swap);
            ctx.insert_unary_exp(aux_swap);
            ctx.insert_transpose(aux_swap, swap);
        },
    );
    NncaseTransposeTx::run_ruleset(seed, RunConfig::Once);
}

pub fn run_and_collect_stats_iters_with_mem_cap_and_progress<F>(
    rewrite_iters: usize,
    max_rewrite_mem_gib: u64,
    mut on_progress: F,
) -> NncaseTransposeStats
where
    F: FnMut(ProgressEvent),
{
    let max_rewrite_mem_bytes = gib_to_bytes(max_rewrite_mem_gib);

    NncaseTransposeTx::reset_for_bench();
    seed_domain("nncase_transpose_seed");

    let rs = NncaseTransposeTx::new_ruleset("nncase_transpose_rules");
    register_rewrite_rules(rs);

    let rewrite_peak_before = current_peak_memory_bytes();
    let mut executed_rewrite_iters = 0usize;
    let mut rewrite_stopped_early_due_to_memory_cap = false;
    for _ in 0..rewrite_iters {
        NncaseTransposeTx::run_ruleset(rs, RunConfig::Once);
        let current_peak = current_peak_memory_bytes();
        let tuple_count = {
            let egraph = <NncaseTransposeTx as NonPatRecSgl>::egraph();
            let egraph = egraph.lock().unwrap();
            egraph.num_tuples()
        };
        on_progress(ProgressEvent::RewriteIterationComplete {
            current: executed_rewrite_iters + 1,
            total: rewrite_iters,
            tuple_count,
            peak_memory_bytes: current_peak,
        });
        executed_rewrite_iters += 1;
        if current_peak > max_rewrite_mem_bytes {
            rewrite_stopped_early_due_to_memory_cap = true;
            on_progress(ProgressEvent::RewriteStoppedByMemoryCap {
                current: executed_rewrite_iters,
                total: rewrite_iters,
                peak_memory_bytes: current_peak,
                cap_bytes: max_rewrite_mem_bytes,
            });
            break;
        }
    }
    let rewrite_peak_after = current_peak_memory_bytes();
    let rewrite_peak_memory_bytes = rewrite_peak_after.saturating_sub(rewrite_peak_before);
    let egraph = <NncaseTransposeTx as NonPatRecSgl>::egraph();
    let egraph = egraph.lock().unwrap();
    NncaseTransposeStats {
        total_num_tuples: egraph.num_tuples(),
        max_rewrite_mem_gib,
        rewrite_peak_memory_bytes,
        requested_rewrite_iters: rewrite_iters,
        executed_rewrite_iters,
        rewrite_stopped_early_due_to_memory_cap,
    }
}

#[cfg(feature = "rustsat-extract")]
fn build_extract_target() -> impl EgglogNode + EgglogTy + 'static {
    let lhs = TransposeInput::<NncaseTransposeTx>::new("lhs".to_owned());
    lhs.commit();
    let rhs = TransposeInput::<NncaseTransposeTx>::new("rhs".to_owned());
    rhs.commit();
    let swap = PermSwap::<NncaseTransposeTx>::new();
    swap.commit();

    let lhs_t = Transpose::<NncaseTransposeTx>::new(&lhs, &swap);
    lhs_t.commit();
    let rhs_exp = UnaryExp::<NncaseTransposeTx>::new(&rhs);
    rhs_exp.commit();
    let rhs_t = Transpose::<NncaseTransposeTx>::new(&rhs_exp, &swap);
    rhs_t.commit();
    let add = Add::<NncaseTransposeTx>::new(&lhs_t, &rhs_t);
    add.commit();
    let root = Transpose::<NncaseTransposeTx>::new(&add, &swap);
    root.commit();
    root
}

#[cfg(feature = "rustsat-extract")]
fn extract_backend_for_method(method: &'static str) -> ExtractBackend<TreeAdditiveCostModel> {
    match method {
        "default" => ExtractBackend::cost_model(TreeAdditiveCostModel::default()),
        "eboost" => {
            ExtractBackend::<TreeAdditiveCostModel>::eboost_heuristic(EBoostExtractConfig::default())
        }
        "layered" => {
            ExtractBackend::<TreeAdditiveCostModel>::eboost_layered(EBoostLayeredConfig::default())
        }
        "rustsat" => {
            ExtractBackend::<TreeAdditiveCostModel>::rustsat(RustsatExtractConfig::default())
        }
        other => panic!("unsupported extract method `{other}`"),
    }
}

#[cfg(feature = "rustsat-extract")]
fn parse_extract_methods(methods: &[String]) -> Vec<&'static str> {
    if methods.is_empty() {
        return ALL_EXTRACT_METHODS.to_vec();
    }
    methods
        .iter()
        .map(|method| match method.as_str() {
            "default" => "default",
            "eboost" => "eboost",
            "layered" => "layered",
            "rustsat" => "rustsat",
            other => panic!("unsupported extract method `{other}`"),
        })
        .collect()
}

#[cfg(feature = "rustsat-extract")]
fn render_svg_for_method<N>(target: &N, method: &'static str, svg_path: &PathBuf) -> u64
where
    N: EgglogNode + EgglogTy + 'static,
{
    NncaseTransposeTx::extract_node_to_svg_with_backend(
        target,
        extract_backend_for_method(method),
        svg_path,
    )
    .expect("svg rendering should succeed")
}

#[cfg(feature = "rustsat-extract")]
fn benchmark_extract_backend<N>(
    target: &N,
    method: &'static str,
    rewrite_peak_memory_bytes: u64,
) -> MathExtractComparisonRow
where
    N: EgglogNode + EgglogTy + 'static,
{
    let extract_peak_before = current_peak_memory_bytes();
    let started = Instant::now();
    let (rendered, cost) = NncaseTransposeTx::extract_node_to_string_with_backend(
        target,
        extract_backend_for_method(method),
    )
    .expect("transpose extraction should succeed");
    let svg_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("target")
        .join("nncase_transpose_extract_svgs");
    fs::create_dir_all(&svg_dir).expect("svg output directory should be creatable");
    let svg_path = svg_dir.join(format!("{method}.svg"));
    let _ = render_svg_for_method(target, method, &svg_path);
    let extract_peak_after = current_peak_memory_bytes();
    MathExtractComparisonRow {
        method,
        requested_rewrite_iters: 0,
        executed_rewrite_iters: 0,
        max_rewrite_mem_gib: 0,
        run_ruleset_note: String::new(),
        rewrite_peak_memory_bytes,
        extract_peak_memory_bytes: Some(extract_peak_after.saturating_sub(extract_peak_before)),
        cost: Some(cost),
        elapsed: Some(started.elapsed()),
        rendered,
        svg_path: svg_path.display().to_string(),
        timed_out: false,
    }
}

#[cfg(feature = "rustsat-extract")]
#[derive(Debug, Clone, Serialize, Deserialize)]
struct TimedExtractComparisonPayload {
    rendered: String,
    cost: u64,
    elapsed_ms: f64,
    peak_memory_bytes: u64,
    svg_path: String,
}

fn benchmark_extract_backend_with_timeout<N>(
    target: &N,
    method: &'static str,
    rewrite_peak_memory_bytes: u64,
    max_extract_time_secs: Option<u64>,
) -> MathExtractComparisonRow
where
    N: EgglogNode + EgglogTy + 'static,
{
    let payload = run_with_timeout_payload(
        max_extract_time_secs,
        || {
            let row = benchmark_extract_backend(target, method, rewrite_peak_memory_bytes);
            TimedExtractComparisonPayload {
                rendered: row.rendered,
                cost: row.cost.unwrap_or(0),
                elapsed_ms: row.elapsed.map(duration_to_ms).unwrap_or(0.0),
                peak_memory_bytes: row.extract_peak_memory_bytes.unwrap_or(0),
                svg_path: row.svg_path,
            }
        },
        || TimedExtractComparisonPayload {
            rendered: "NaN".to_string(),
            cost: 0,
            elapsed_ms: f64::NAN,
            peak_memory_bytes: 0,
            svg_path: "n/a".to_string(),
        },
    );

    if payload.elapsed_ms.is_nan() {
        timeout_row(method, rewrite_peak_memory_bytes)
    } else {
        MathExtractComparisonRow {
            method,
            requested_rewrite_iters: 0,
            executed_rewrite_iters: 0,
            max_rewrite_mem_gib: 0,
            run_ruleset_note: String::new(),
            rewrite_peak_memory_bytes,
            extract_peak_memory_bytes: Some(payload.peak_memory_bytes),
            cost: Some(payload.cost),
            elapsed: Some(duration_from_ms(payload.elapsed_ms)),
            rendered: payload.rendered,
            svg_path: payload.svg_path,
            timed_out: false,
        }
    }
}

#[cfg(feature = "rustsat-extract")]
fn timeout_row(method: &'static str, rewrite_peak_memory_bytes: u64) -> MathExtractComparisonRow {
    MathExtractComparisonRow {
        method,
        requested_rewrite_iters: 0,
        executed_rewrite_iters: 0,
        max_rewrite_mem_gib: 0,
        run_ruleset_note: String::new(),
        rewrite_peak_memory_bytes,
        extract_peak_memory_bytes: None,
        cost: None,
        elapsed: None,
        rendered: "NaN".to_string(),
        svg_path: "n/a".to_string(),
        timed_out: true,
    }
}

#[cfg(feature = "rustsat-extract")]
fn measure_extract_metric<N>(
    target: &N,
    iteration: usize,
    method: &'static str,
) -> ExtractTimelineMetric
where
    N: EgglogNode + EgglogTy + 'static,
{
    let extract_peak_before = current_peak_memory_bytes();
    let started = Instant::now();
    let (_rendered, cost) = NncaseTransposeTx::extract_node_to_string_with_backend(
        target,
        extract_backend_for_method(method),
    )
    .expect("timeline extract should succeed");
    let elapsed_ms = elapsed_ms(started);
    let svg_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("target")
        .join("nncase_transpose_timeline_svgs")
        .join(format!("iter_{iteration:03}"));
    fs::create_dir_all(&svg_dir).expect("timeline svg output directory should be creatable");
    let svg_path = svg_dir.join(format!("{method}.svg"));
    let _ = render_svg_for_method(target, method, &svg_path);
    let extract_peak_after = current_peak_memory_bytes();
    ExtractTimelineMetric {
        method: method.to_string(),
        cost: Some(cost),
        elapsed_ms: Some(elapsed_ms),
        peak_memory_bytes: Some(extract_peak_after.saturating_sub(extract_peak_before)),
        svg_path: svg_path.display().to_string(),
        timed_out: false,
    }
}

fn measure_extract_metric_with_timeout<N>(
    target: &N,
    iteration: usize,
    method: &'static str,
    max_extract_time_secs: Option<u64>,
) -> ExtractTimelineMetric
where
    N: EgglogNode + EgglogTy + 'static,
{
    run_with_timeout_payload(
        max_extract_time_secs,
        || measure_extract_metric(target, iteration, method),
        || ExtractTimelineMetric {
            method: method.to_string(),
            cost: None,
            elapsed_ms: None,
            peak_memory_bytes: None,
            svg_path: "n/a".to_string(),
            timed_out: true,
        },
    )
}

#[cfg(feature = "rustsat-extract")]
pub fn run_extract_comparison_with_iters_and_mem_cap(
    rewrite_iters: usize,
    max_rewrite_mem_gib: u64,
) -> Vec<MathExtractComparisonRow> {
    run_extract_comparison_with_options_and_progress(
        rewrite_iters,
        max_rewrite_mem_gib,
        &[],
        None,
        |_| {},
    )
}

#[cfg(feature = "rustsat-extract")]
pub fn run_extract_comparison_with_options_and_progress<F>(
    rewrite_iters: usize,
    max_rewrite_mem_gib: u64,
    selected_extractors: &[String],
    max_extract_time_secs: Option<u64>,
    mut on_progress: F,
) -> Vec<MathExtractComparisonRow>
where
    F: FnMut(ProgressEvent),
{
    let extract_methods = parse_extract_methods(selected_extractors);
    let stats = run_and_collect_stats_iters_with_mem_cap_and_progress(
        rewrite_iters,
        max_rewrite_mem_gib,
        |event| on_progress(event),
    );
    let target = build_extract_target();

    let mut rows = Vec::new();
    for (index, method) in extract_methods.iter().enumerate() {
        on_progress(ProgressEvent::ExtractPhaseStart {
            method,
            current: index + 1,
            total: extract_methods.len(),
        });
        let mut row = benchmark_extract_backend_with_timeout(
            &target,
            method,
            stats.rewrite_peak_memory_bytes,
            max_extract_time_secs,
        );
        row.requested_rewrite_iters = stats.requested_rewrite_iters;
        row.executed_rewrite_iters = stats.executed_rewrite_iters;
        row.max_rewrite_mem_gib = stats.max_rewrite_mem_gib;
        row.run_ruleset_note = if stats.rewrite_stopped_early_due_to_memory_cap {
            format!(
                "stopped early at {} / {} iterations because peak memory exceeded {} GiB",
                stats.executed_rewrite_iters,
                stats.requested_rewrite_iters,
                stats.max_rewrite_mem_gib
            )
        } else {
            "completed requested iterations".to_string()
        };
        on_progress(ProgressEvent::ExtractPhaseComplete {
            method,
            current: index + 1,
            total: extract_methods.len(),
            elapsed_ms: row.elapsed.map(duration_to_ms).unwrap_or(f64::NAN),
            peak_memory_bytes: row.extract_peak_memory_bytes.unwrap_or(0),
        });
        rows.push(row);
    }
    rows
}

#[cfg(feature = "rustsat-extract")]
pub fn run_extract_timeline_with_iters_and_mem_cap(
    rewrite_iters: usize,
    max_rewrite_mem_gib: u64,
) -> ExtractTimelineReport {
    run_extract_timeline_with_options(rewrite_iters, max_rewrite_mem_gib, &[], None)
}

#[cfg(feature = "rustsat-extract")]
pub fn run_extract_timeline_with_options(
    rewrite_iters: usize,
    max_rewrite_mem_gib: u64,
    selected_extractors: &[String],
    max_extract_time_secs: Option<u64>,
) -> ExtractTimelineReport {
    let max_rewrite_mem_bytes = gib_to_bytes(max_rewrite_mem_gib);
    let extract_methods = parse_extract_methods(selected_extractors);

    NncaseTransposeTx::reset_for_bench();
    seed_domain("nncase_transpose_timeline_seed");
    let rs = NncaseTransposeTx::new_ruleset("nncase_transpose_timeline_rules");
    register_rewrite_rules(rs);
    let target = build_extract_target();

    let mut points = Vec::new();
    let mut executed_rewrite_iters = 0usize;
    let mut stopped_early_due_to_memory_cap = false;
    let rewrite_peak_before = current_peak_memory_bytes();

    points.push(snapshot_timeline_point(
        0,
        &RunReport::default(),
        &target,
        &extract_methods,
        max_extract_time_secs,
        rewrite_peak_before,
    ));

    for iteration in 1..rewrite_iters {
        let started = Instant::now();
        let report = NncaseTransposeTx::run_ruleset(rs, RunConfig::Once);
        let rewrite_elapsed_ms = elapsed_ms(started);
        executed_rewrite_iters += 1;
        let current_peak = current_peak_memory_bytes();
        let tuple_count = {
            let egraph = <NncaseTransposeTx as NonPatRecSgl>::egraph();
            let egraph = egraph.lock().unwrap();
            egraph.num_tuples()
        };
        let rule_matches = report
            .num_matches_per_rule
            .iter()
            .map(|(name, count)| (name.to_string(), *count))
            .collect::<BTreeMap<_, _>>();
        let extracts = extract_methods
            .iter()
            .map(|method| {
                measure_extract_metric_with_timeout(
                    &target,
                    iteration,
                    method,
                    max_extract_time_secs,
                )
            })
            .collect::<Vec<_>>();
        points.push(RewriteTimelinePoint {
            iteration,
            tuple_count,
            rewrite_elapsed_ms,
            rewrite_peak_memory_bytes: current_peak.saturating_sub(rewrite_peak_before),
            rule_matches,
            extracts,
        });
        if current_peak > max_rewrite_mem_bytes {
            stopped_early_due_to_memory_cap = true;
            break;
        }
    }

    ExtractTimelineReport {
        version_nickname: None,
        benchmark_family: "nncase",
        benchmark_case: "transpose",
        baseline: eggplant::helpers::report::NNCASE_EGRAPH_BASELINE,
        comparison_target: eggplant::helpers::report::EGGPLANT_COMPARISON_TARGET,
        positioning: eggplant::helpers::report::NNCASE_EGRAPH_SLOWER_POSITIONING,
        selected_extractors: extract_methods.iter().map(|m| (*m).to_string()).collect(),
        max_extract_time_secs,
        requested_rewrite_iters: rewrite_iters,
        executed_rewrite_iters,
        max_rewrite_mem_gib,
        stopped_early_due_to_memory_cap,
        points,
    }
}

#[cfg(feature = "rustsat-extract")]
fn snapshot_timeline_point<N>(
    iteration: usize,
    report: &RunReport,
    target: &N,
    extract_methods: &[&'static str],
    max_extract_time_secs: Option<u64>,
    rewrite_peak_before: u64,
) -> RewriteTimelinePoint
where
    N: EgglogNode + EgglogTy + 'static,
{
    let tuple_count = {
        let egraph = <NncaseTransposeTx as NonPatRecSgl>::egraph();
        let egraph = egraph.lock().unwrap();
        egraph.num_tuples()
    };
    let current_peak = current_peak_memory_bytes();
    let mut rule_matches = report
        .num_matches_per_rule
        .iter()
        .map(|(name, count)| (name.to_string(), *count))
        .collect::<BTreeMap<_, _>>();
    if rule_matches.is_empty() && iteration == 0 {
        rule_matches.insert("@initial_state".to_string(), 1);
    }
    let extracts = extract_methods
        .iter()
        .map(|method| {
            measure_extract_metric_with_timeout(target, iteration, method, max_extract_time_secs)
        })
        .collect::<Vec<_>>();
    RewriteTimelinePoint {
        iteration,
        tuple_count,
        rewrite_elapsed_ms: 0.0,
        rewrite_peak_memory_bytes: current_peak.saturating_sub(rewrite_peak_before),
        rule_matches,
        extracts,
    }
}

pub fn run_transpose_rewrite_smoke<CM>(rewrite_iters: usize, cost_model: CM) -> String
where
    CM: eggplant::egglog::extract::CostModel<eggplant::egglog::extract::DefaultCost> + 'static,
{
    NncaseTransposeTx::reset_for_bench();

    let lhs = TransposeInput::<NncaseTransposeTx>::new("lhs".to_owned());
    lhs.commit();
    let rhs = TransposeInput::<NncaseTransposeTx>::new("rhs".to_owned());
    rhs.commit();
    let swap = PermSwap::<NncaseTransposeTx>::new();
    swap.commit();

    let lhs_t = Transpose::<NncaseTransposeTx>::new(&lhs, &swap);
    lhs_t.commit();
    let rhs_exp = UnaryExp::<NncaseTransposeTx>::new(&rhs);
    rhs_exp.commit();
    let rhs_t = Transpose::<NncaseTransposeTx>::new(&rhs_exp, &swap);
    rhs_t.commit();
    let add = Add::<NncaseTransposeTx>::new(&lhs_t, &rhs_t);
    add.commit();
    let root = Transpose::<NncaseTransposeTx>::new(&add, &swap);
    root.commit();

    let rs = NncaseTransposeTx::new_ruleset("nncase_transpose_smoke_rules");
    register_rewrite_rules(rs);

    for _ in 0..rewrite_iters {
        NncaseTransposeTx::run_ruleset(rs, RunConfig::Once);
    }

    let (rendered, _) =
        NncaseTransposeTx::extract_node_to_string_with_cost_model(&root, cost_model)
            .expect("smoke extract should succeed");
    rendered
}
