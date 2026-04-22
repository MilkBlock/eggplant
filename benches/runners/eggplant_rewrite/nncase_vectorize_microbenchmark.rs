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
enum LayoutTag {
    #[eggplant::typst("f")]
    #[eggplant::precedence(100)]
    #[cost(0)]
    Flat {},
    #[eggplant::typst("b")]
    #[eggplant::precedence(100)]
    #[cost(0)]
    Blocked {},
}

#[eggplant::dsl]
enum VectorizeExpr {
    #[eggplant::typst("{name}")]
    #[eggplant::precedence(100)]
    #[cost(0)]
    Input { name: String },
    #[eggplant::typst("({lhs} * {rhs})")]
    #[eggplant::precedence(50)]
    #[cost(12)]
    LogicalMatMul {
        lhs: VectorizeExpr,
        rhs: VectorizeExpr,
    },
    #[eggplant::typst("e^({inner})")]
    #[eggplant::precedence(90)]
    #[cost(6)]
    LogicalExp { inner: VectorizeExpr },
    #[eggplant::typst("({inner})_({layout})")]
    #[eggplant::precedence(90)]
    #[cost(3)]
    Pack {
        inner: VectorizeExpr,
        layout: LayoutTag,
    },
    #[eggplant::typst("({inner})^({layout})")]
    #[eggplant::precedence(90)]
    #[cost(3)]
    Unpack {
        inner: VectorizeExpr,
        layout: LayoutTag,
    },
    #[eggplant::typst("(({lhs} * {rhs}))_({layout})")]
    #[eggplant::precedence(50)]
    #[cost(2)]
    PackedMatMul {
        lhs: VectorizeExpr,
        rhs: VectorizeExpr,
        layout: LayoutTag,
    },
    #[eggplant::typst("(e^({inner}))_({layout})")]
    #[eggplant::precedence(90)]
    #[cost(1)]
    PackedExp {
        inner: VectorizeExpr,
        layout: LayoutTag,
    },
}

tx_rx_vt_pr!(NncaseVectorizeTx, NncaseVectorizePatRec);

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
    pub selected_extractors: Vec<String>,
    pub max_extract_time_secs: Option<u64>,
    pub requested_rewrite_iters: usize,
    pub executed_rewrite_iters: usize,
    pub max_rewrite_mem_gib: u64,
    pub stopped_early_due_to_memory_cap: bool,
    pub points: Vec<RewriteTimelinePoint>,
}

pub struct NncaseVectorizeStats {
    pub max_rewrite_mem_gib: u64,
    pub rewrite_peak_memory_bytes: u64,
    pub requested_rewrite_iters: usize,
    pub executed_rewrite_iters: usize,
    pub rewrite_stopped_early_due_to_memory_cap: bool,
}

#[cfg(feature = "rustsat-extract")]
const ALL_EXTRACT_METHODS: &[&str] = &["default", "eboost", "layered", "rustsat"];

fn register_rewrite_rules(rs: RuleSetId) {
    NncaseVectorizeTx::add_rule(
        "meta_pack_matmul",
        rs,
        || {
            let lhs = VectorizeExpr::query_leaf();
            let rhs = VectorizeExpr::query_leaf();
            let mm = LogicalMatMul::query(&lhs, &rhs);
            #[eggplant::pat_vars]
            struct Pat {
                lhs: VectorizeExpr,
                rhs: VectorizeExpr,
                mm: LogicalMatMul,
            }
            Pat::new(lhs, rhs, mm)
        },
        |ctx, pat| {
            let blocked = ctx.insert_blocked();
            let lhs_pack = ctx.insert_pack(pat.lhs, blocked);
            let rhs_pack = ctx.insert_pack(pat.rhs, blocked);
            let packed = ctx.insert_packed_mat_mul(lhs_pack, rhs_pack, blocked);
            let rhs = ctx.insert_unpack(packed, blocked);
            ctx.union(pat.mm, rhs);
        },
    );

    NncaseVectorizeTx::add_rule(
        "meta_pack_exp",
        rs,
        || {
            let inner = VectorizeExpr::query_leaf();
            let layout = LayoutTag::query_leaf();
            let unpack = Unpack::query(&inner, &layout);
            let exp = LogicalExp::query(&unpack);
            #[eggplant::pat_vars]
            struct Pat {
                inner: VectorizeExpr,
                layout: LayoutTag,
                exp: LogicalExp,
            }
            Pat::new(inner, layout, exp)
        },
        |ctx, pat| {
            let packed = ctx.insert_packed_exp(pat.inner, pat.layout);
            let rhs = ctx.insert_unpack(packed, pat.layout);
            ctx.union(pat.exp, rhs);
        },
    );

    NncaseVectorizeTx::add_rule(
        "fold_nop_pack",
        rs,
        || {
            let inner = VectorizeExpr::query_leaf();
            let layout = LayoutTag::query_leaf();
            let unpack = Unpack::query(&inner, &layout);
            let pack = Pack::query(&unpack, &layout);
            #[eggplant::pat_vars]
            struct Pat {
                inner: VectorizeExpr,
                pack: Pack,
            }
            Pat::new(inner, pack)
        },
        |ctx, pat| {
            ctx.union(pat.pack, pat.inner);
        },
    );

    NncaseVectorizeTx::add_rule(
        "fold_nop_unpack",
        rs,
        || {
            let inner = VectorizeExpr::query_leaf();
            let layout = LayoutTag::query_leaf();
            let pack = Pack::query(&inner, &layout);
            let unpack = Unpack::query(&pack, &layout);
            #[eggplant::pat_vars]
            struct Pat {
                inner: VectorizeExpr,
                unpack: Unpack,
            }
            Pat::new(inner, unpack)
        },
        |ctx, pat| {
            ctx.union(pat.unpack, pat.inner);
        },
    );
}

fn seed_domain(seed_name: &'static str) {
    let seed = NncaseVectorizeTx::new_ruleset(seed_name);
    NncaseVectorizeTx::add_rule(
        seed_name,
        seed,
        || {
            #[eggplant::pat_vars_catch]
            struct Unit {}
        },
        |ctx, _pat| {
            let q = ctx.insert_input("q".to_owned());
            let k = ctx.insert_input("k".to_owned());
            let v = ctx.insert_input("v".to_owned());
            let inner_mm = ctx.insert_logical_mat_mul(q, k);
            let exp = ctx.insert_logical_exp(inner_mm);
            ctx.insert_logical_mat_mul(exp, v);
        },
    );
    NncaseVectorizeTx::run_ruleset(seed, RunConfig::Once);
}

pub fn run_and_collect_stats_iters_with_mem_cap_and_progress<F>(
    rewrite_iters: usize,
    max_rewrite_mem_gib: u64,
    mut on_progress: F,
) -> NncaseVectorizeStats
where
    F: FnMut(ProgressEvent),
{
    let max_rewrite_mem_bytes = gib_to_bytes(max_rewrite_mem_gib);
    NncaseVectorizeTx::sgl().reset_for_bench();
    seed_domain("nncase_vectorize_seed");
    let rs = NncaseVectorizeTx::new_ruleset("nncase_vectorize_rules");
    register_rewrite_rules(rs);
    let rewrite_peak_before = current_peak_memory_bytes();
    let mut executed_rewrite_iters = 0usize;
    let mut rewrite_stopped_early_due_to_memory_cap = false;
    for _ in 0..rewrite_iters {
        NncaseVectorizeTx::run_ruleset(rs, RunConfig::Once);
        let current_peak = current_peak_memory_bytes();
        let tuple_count = {
            let egraph = <NncaseVectorizeTx as NonPatRecSgl>::egraph();
            let egraph = egraph.lock().unwrap();
            egraph.num_tuples()
        };
        executed_rewrite_iters += 1;
        on_progress(ProgressEvent::RewriteIterationComplete {
            current: executed_rewrite_iters,
            total: rewrite_iters,
            tuple_count,
            peak_memory_bytes: current_peak,
        });
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
    NncaseVectorizeStats {
        max_rewrite_mem_gib,
        rewrite_peak_memory_bytes: rewrite_peak_after.saturating_sub(rewrite_peak_before),
        requested_rewrite_iters: rewrite_iters,
        executed_rewrite_iters,
        rewrite_stopped_early_due_to_memory_cap,
    }
}

#[cfg(feature = "rustsat-extract")]
fn build_extract_target() -> impl EgglogNode + EgglogTy + 'static {
    let q = Input::<NncaseVectorizeTx>::new("q".to_owned());
    q.commit();
    let k = Input::<NncaseVectorizeTx>::new("k".to_owned());
    k.commit();
    let v = Input::<NncaseVectorizeTx>::new("v".to_owned());
    v.commit();
    let inner_mm = LogicalMatMul::<NncaseVectorizeTx>::new(&q, &k);
    inner_mm.commit();
    let exp = LogicalExp::<NncaseVectorizeTx>::new(&inner_mm);
    exp.commit();
    let root = LogicalMatMul::<NncaseVectorizeTx>::new(&exp, &v);
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
    NncaseVectorizeTx::extract_node_to_svg_with_backend(
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
    let (rendered, cost) = NncaseVectorizeTx::extract_node_to_string_with_backend(
        target,
        extract_backend_for_method(method),
    )
    .expect("vectorize extraction should succeed");
    let svg_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("target")
        .join("nncase_vectorize_extract_svgs");
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
    let (_rendered, cost) = NncaseVectorizeTx::extract_node_to_string_with_backend(
        target,
        extract_backend_for_method(method),
    )
    .expect("timeline extract should succeed");
    let elapsed_ms = elapsed_ms(started);
    let svg_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("target")
        .join("nncase_vectorize_timeline_svgs")
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
    NncaseVectorizeTx::sgl().reset_for_bench();
    seed_domain("nncase_vectorize_timeline_seed");
    let rs = NncaseVectorizeTx::new_ruleset("nncase_vectorize_timeline_rules");
    register_rewrite_rules(rs);
    let target = build_extract_target();
    let rewrite_peak_before = current_peak_memory_bytes();
    let mut points = Vec::new();
    points.push(snapshot_timeline_point(
        0,
        &RunReport::default(),
        &target,
        &extract_methods,
        max_extract_time_secs,
        rewrite_peak_before,
    ));
    let mut executed_rewrite_iters = 0usize;
    let mut stopped_early_due_to_memory_cap = false;
    for iteration in 1..rewrite_iters {
        let started = Instant::now();
        let report = NncaseVectorizeTx::run_ruleset(rs, RunConfig::Once);
        let rewrite_elapsed_ms = elapsed_ms(started);
        executed_rewrite_iters += 1;
        let current_peak = current_peak_memory_bytes();
        let tuple_count = {
            let egraph = <NncaseVectorizeTx as NonPatRecSgl>::egraph();
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
        let egraph = <NncaseVectorizeTx as NonPatRecSgl>::egraph();
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

pub fn run_vectorize_rewrite_smoke<CM>(rewrite_iters: usize, cost_model: CM) -> String
where
    CM: eggplant::egglog::extract::CostModel<eggplant::egglog::extract::DefaultCost> + 'static,
{
    NncaseVectorizeTx::sgl().reset_for_bench();
    let q = Input::<NncaseVectorizeTx>::new("q".to_owned());
    q.commit();
    let k = Input::<NncaseVectorizeTx>::new("k".to_owned());
    k.commit();
    let v = Input::<NncaseVectorizeTx>::new("v".to_owned());
    v.commit();
    let inner_mm = LogicalMatMul::<NncaseVectorizeTx>::new(&q, &k);
    inner_mm.commit();
    let exp = LogicalExp::<NncaseVectorizeTx>::new(&inner_mm);
    exp.commit();
    let root = LogicalMatMul::<NncaseVectorizeTx>::new(&exp, &v);
    root.commit();
    let rs = NncaseVectorizeTx::new_ruleset("nncase_vectorize_smoke_rules");
    register_rewrite_rules(rs);
    for _ in 0..rewrite_iters {
        NncaseVectorizeTx::run_ruleset(rs, RunConfig::Once);
    }
    let (rendered, _) =
        NncaseVectorizeTx::extract_node_to_string_with_cost_model(&root, cost_model)
            .expect("smoke extract should succeed");
    rendered
}
