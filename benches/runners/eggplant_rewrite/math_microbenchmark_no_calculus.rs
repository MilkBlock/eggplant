use egglog_reports::RunReport;
use eggplant::prelude::*;
use eggplant::tx_rx_vt_pr;
use eggplant::wrap::NonPatRecSgl;
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};

#[cfg(feature = "rustsat-extract")]
use eggplant::egglog::extract::TreeAdditiveCostModel;
#[cfg(feature = "rustsat-extract")]
use eggplant::wrap::EgglogTy;
#[cfg(feature = "rustsat-extract")]
use std::fs;
#[cfg(feature = "rustsat-extract")]
use std::path::PathBuf;

#[eggplant::dsl]
enum MathNoCalc {
    #[typst("{a} + {b}")]
    #[precedence(50)]
    #[cost(1)]
    MAdd { a: MathNoCalc, b: MathNoCalc },
    #[typst("{a} - {b}")]
    #[precedence(50)]
    #[cost(1)]
    MSub { a: MathNoCalc, b: MathNoCalc },
    #[typst("{a} * {b}")]
    #[precedence(60)]
    #[cost(3)]
    MMul { a: MathNoCalc, b: MathNoCalc },
    #[typst("frac({a}, {b}) ")]
    #[precedence(60)]
    #[cost(10)]
    MDiv { a: MathNoCalc, b: MathNoCalc },
    #[typst("{a}^{b}")]
    #[precedence(80)]
    #[cost(25)]
    MPow { a: MathNoCalc, b: MathNoCalc },
    #[typst("ln {a}")]
    #[precedence(90)]
    #[cost(18)]
    MLn { a: MathNoCalc },
    #[typst("sqrt({a})")]
    #[precedence(90)]
    #[cost(16)]
    MSqrt { a: MathNoCalc },

    #[typst("sin({a})")]
    #[precedence(90)]
    #[cost(18)]
    MSin { a: MathNoCalc },
    #[typst("cos({a})")]
    #[precedence(90)]
    #[cost(18)]
    MCos { a: MathNoCalc },

    #[typst("{n}")]
    #[precedence(100)]
    #[cost(1)]
    MConst { n: i64 },
    #[typst("{name}")]
    #[precedence(100)]
    #[cost(1)]
    MVar { name: String },
}

tx_rx_vt_pr!(MyTxMathNoCalc, MyPatRecMathNoCalcNoCalc);

#[derive(Default, Clone, Copy)]
struct CallbackTiming {
    total: Duration,
    calls: usize,
}

type SharedCallbackTimings = Arc<Mutex<BTreeMap<&'static str, CallbackTiming>>>;

pub struct MathMicrobenchmarkStats {
    pub elapsed: Duration,
    pub total_num_tuples: usize,
    pub table_sizes: Vec<(&'static str, usize)>,
    pub max_rewrite_mem_gib: u64,
    pub rewrite_peak_memory_bytes: u64,
    pub requested_rewrite_iters: usize,
    pub executed_rewrite_iters: usize,
    pub rewrite_stopped_early_due_to_memory_cap: bool,
}

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

#[cfg(feature = "rustsat-extract")]
const ALL_EXTRACT_METHODS: &[&str] = &["default", "eboost", "layered", "rustsat"];

const DEFAULT_RUN_RULESET_MEMORY_CAP_GIB: u64 = 20;

const MATH_TABLES: &[&str] = &[
    "MAdd", "MSub", "MMul", "MDiv", "MPow", "MLn", "MSqrt", "MSin", "MCos", "MConst", "MVar",
];

fn record_callback_timing(
    stats: &Option<SharedCallbackTimings>,
    rule_name: &'static str,
    elapsed: Duration,
) {
    let Some(stats) = stats else {
        return;
    };
    let mut stats = stats.lock().unwrap();
    let entry = stats.entry(rule_name).or_default();
    entry.total += elapsed;
    entry.calls += 1;
}

fn print_callback_timings(label: &str, stats: &Option<SharedCallbackTimings>, report: &RunReport) {
    let Some(stats) = stats else {
        return;
    };
    let stats = stats.lock().unwrap();
    let mut rows = stats
        .iter()
        .map(|(rule, timing)| (*rule, *timing))
        .collect::<Vec<_>>();
    rows.sort_by_key(|(_, timing)| std::cmp::Reverse(timing.total));
    let callback_total = rows
        .iter()
        .fold(Duration::ZERO, |acc, (_, t)| acc + t.total);
    eprintln!(
        "[bench-breakdown] {label} callback total: {:?}",
        callback_total
    );
    for (rule, timing) in rows {
        let matches = report
            .num_matches_per_rule
            .get(format!("@{rule}").as_str())
            .copied()
            .unwrap_or(0);
        let avg = if timing.calls == 0 {
            Duration::ZERO
        } else {
            Duration::from_secs_f64(timing.total.as_secs_f64() / timing.calls as f64)
        };
        eprintln!(
            "[bench-breakdown] {label} callback {rule}: total={:?}, calls={}, avg={:?}, matches={}",
            timing.total, timing.calls, avg, matches
        );
    }
}

fn register_rewrite_rules(
    rs: RuleSetId,
    sub_self: &'static str,
    mul_distrib: &'static str,
    add_factor: &'static str,
    mul_pow_combine: &'static str,
) {
    MyTxMathNoCalc::add_rule(
        "add_comm",
        rs,
        || {
            let a = MathNoCalc::query_leaf();
            let b = MathNoCalc::query_leaf();
            let add = MAdd::query(&a, &b);
            #[eggplant::pat_vars]
            struct Pat {
                a: MathNoCalc,
                b: MathNoCalc,
                add: MAdd,
            }
            Pat::new(a, b, add)
        },
        |ctx, pat| {
            let rhs = ctx.insert_m_add(pat.b, pat.a);
            ctx.union(pat.add, rhs);
        },
    );
    MyTxMathNoCalc::add_rule(
        "mul_comm",
        rs,
        || {
            let a = MathNoCalc::query_leaf();
            let b = MathNoCalc::query_leaf();
            let mul = MMul::query(&a, &b);
            #[eggplant::pat_vars]
            struct Pat {
                a: MathNoCalc,
                b: MathNoCalc,
                mul: MMul,
            }
            Pat::new(a, b, mul)
        },
        |ctx, pat| {
            let rhs = ctx.insert_m_mul(pat.b, pat.a);
            ctx.union(pat.mul, rhs);
        },
    );
    MyTxMathNoCalc::add_rule(
        "add_assoc",
        rs,
        || {
            let a = MathNoCalc::query_leaf();
            let b = MathNoCalc::query_leaf();
            let c = MathNoCalc::query_leaf();
            let add_inner = MAdd::query(&b, &c);
            let add_outer = MAdd::query(&a, &add_inner);
            #[eggplant::pat_vars]
            struct Pat {
                a: MathNoCalc,
                b: MathNoCalc,
                c: MathNoCalc,
                add_outer: MAdd,
            }
            Pat::new(a, b, c, add_outer)
        },
        |ctx, pat| {
            let ab = ctx.insert_m_add(pat.a, pat.b);
            let rhs = ctx.insert_m_add(ab, pat.c);
            ctx.union(pat.add_outer, rhs);
        },
    );
    MyTxMathNoCalc::add_rule(
        "mul_assoc",
        rs,
        || {
            let a = MathNoCalc::query_leaf();
            let b = MathNoCalc::query_leaf();
            let c = MathNoCalc::query_leaf();
            let mul_inner = MMul::query(&b, &c);
            let mul_outer = MMul::query(&a, &mul_inner);
            #[eggplant::pat_vars]
            struct Pat {
                a: MathNoCalc,
                b: MathNoCalc,
                c: MathNoCalc,
                mul_outer: MMul,
            }
            Pat::new(a, b, c, mul_outer)
        },
        |ctx, pat| {
            let ab = ctx.insert_m_mul(pat.a, pat.b);
            let rhs = ctx.insert_m_mul(ab, pat.c);
            ctx.union(pat.mul_outer, rhs);
        },
    );
    MyTxMathNoCalc::add_rule(
        "sub_to_add_neg",
        rs,
        || {
            let a = MathNoCalc::query_leaf();
            let b = MathNoCalc::query_leaf();
            let sub = MSub::query(&a, &b);
            #[eggplant::pat_vars]
            struct Pat {
                a: MathNoCalc,
                b: MathNoCalc,
                sub: MSub,
            }
            Pat::new(a, b, sub)
        },
        |ctx, pat| {
            let neg1 = ctx.insert_m_const(-1);
            let neg_b = ctx.insert_m_mul(neg1, pat.b);
            let rhs = ctx.insert_m_add(pat.a, neg_b);
            ctx.union(pat.sub, rhs);
        },
    );
    MyTxMathNoCalc::add_rule(
        "add_neg_self_zero",
        rs,
        || {
            let a = MathNoCalc::query_leaf();
            let b = MathNoCalc::query_leaf();
            let neg_one = MConst::query();
            let neg_b = MMul::query(&neg_one, &b);
            let add = MAdd::query(&a, &neg_b);
            let is_neg_one = neg_one.handle_n().eq(&-1_i64);
            let same_term = a.handle().eq(&b.handle());
            #[eggplant::pat_vars]
            struct Pat {
                a: MathNoCalc,
                b: MathNoCalc,
                neg_one: MConst,
                add: MAdd,
            }
            Pat::new(a, b, neg_one, add)
                .assert(is_neg_one)
                .assert(same_term)
        },
        |ctx, pat| {
            let z = ctx.insert_m_const(0);
            ctx.union(pat.add, z);
        },
    );
    MyTxMathNoCalc::add_rule(
        "add_zero",
        rs,
        || {
            let a = MathNoCalc::query_leaf();
            let z = MConst::query();
            let add = MAdd::query(&a, &z);
            let is_zero = z.handle_n().eq(&0_i64);
            #[eggplant::pat_vars]
            struct Pat {
                a: MathNoCalc,
                z: MConst,
                add: MAdd,
            }
            Pat::new(a, z, add).assert(is_zero)
        },
        |ctx, pat| {
            ctx.union(pat.add, pat.a);
        },
    );
    MyTxMathNoCalc::add_rule(
        "mul_zero",
        rs,
        || {
            let a = MathNoCalc::query_leaf();
            let z = MConst::query();
            let mul = MMul::query(&a, &z);
            let is_zero = z.handle_n().eq(&0_i64);
            #[eggplant::pat_vars]
            struct Pat {
                a: MathNoCalc,
                z: MConst,
                mul: MMul,
            }
            Pat::new(a, z, mul).assert(is_zero)
        },
        |ctx, pat| {
            ctx.union(pat.mul, pat.z);
        },
    );
    MyTxMathNoCalc::add_rule(
        "mul_one",
        rs,
        || {
            let a = MathNoCalc::query_leaf();
            let o = MConst::query();
            let mul = MMul::query(&a, &o);
            let is_one = o.handle_n().eq(&1_i64);
            #[eggplant::pat_vars]
            struct Pat {
                a: MathNoCalc,
                o: MConst,
                mul: MMul,
            }
            Pat::new(a, o, mul).assert(is_one)
        },
        |ctx, pat| {
            ctx.union(pat.mul, pat.a);
        },
    );
    MyTxMathNoCalc::add_rule(
        sub_self,
        rs,
        || {
            let a = MathNoCalc::query_leaf();
            let b = MathNoCalc::query_leaf();
            let sub = MSub::query(&a, &b);
            let same_terms = a.handle().eq(&b.handle());
            #[eggplant::pat_vars]
            struct Pat {
                a: MathNoCalc,
                sub: MSub,
            }
            Pat::new(a, sub).assert(same_terms)
        },
        |ctx, pat| {
            let z = ctx.insert_m_const(0);
            ctx.union(pat.sub, z);
        },
    );
    MyTxMathNoCalc::add_rule(
        mul_distrib,
        rs,
        || {
            let a = MathNoCalc::query_leaf();
            let b = MathNoCalc::query_leaf();
            let c = MathNoCalc::query_leaf();
            let add = MAdd::query(&b, &c);
            let mul = MMul::query(&a, &add);
            #[eggplant::pat_vars]
            struct Pat {
                a: MathNoCalc,
                b: MathNoCalc,
                c: MathNoCalc,
                mul: MMul,
            }
            Pat::new(a, b, c, mul)
        },
        |ctx, pat| {
            let ab = ctx.insert_m_mul(pat.a, pat.b);
            let ac = ctx.insert_m_mul(pat.a, pat.c);
            let rhs = ctx.insert_m_add(ab, ac);
            ctx.union(pat.mul, rhs);
        },
    );
    MyTxMathNoCalc::add_rule(
        add_factor,
        rs,
        || {
            let a = MathNoCalc::query_leaf();
            let b = MathNoCalc::query_leaf();
            let c = MathNoCalc::query_leaf();
            let mul1 = MMul::query(&a, &b);
            let mul2 = MMul::query(&a, &c);
            let add = MAdd::query(&mul1, &mul2);
            #[eggplant::pat_vars]
            struct Pat {
                a: MathNoCalc,
                b: MathNoCalc,
                c: MathNoCalc,
                add: MAdd,
            }
            Pat::new(a, b, c, add)
        },
        |ctx, pat| {
            let bc = ctx.insert_m_add(pat.b, pat.c);
            let rhs = ctx.insert_m_mul(pat.a, bc);
            ctx.union(pat.add, rhs);
        },
    );
    MyTxMathNoCalc::add_rule(
        mul_pow_combine,
        rs,
        || {
            let a = MathNoCalc::query_leaf();
            let b = MathNoCalc::query_leaf();
            let c = MathNoCalc::query_leaf();
            let p1 = MPow::query(&a, &b);
            let p2 = MPow::query(&a, &c);
            let mul = MMul::query(&p1, &p2);
            #[eggplant::pat_vars]
            struct Pat {
                a: MathNoCalc,
                b: MathNoCalc,
                c: MathNoCalc,
                mul: MMul,
            }
            Pat::new(a, b, c, mul)
        },
        |ctx, pat| {
            let bc = ctx.insert_m_add(pat.b, pat.c);
            let rhs = ctx.insert_m_pow(pat.a, bc);
            ctx.union(pat.mul, rhs);
        },
    );
    MyTxMathNoCalc::add_rule(
        "div_add_distrib",
        rs,
        || {
            let a = MathNoCalc::query_leaf();
            let b = MathNoCalc::query_leaf();
            let c = MathNoCalc::query_leaf();
            let add = MAdd::query(&a, &b);
            let div = MDiv::query(&add, &c);
            #[eggplant::pat_vars]
            struct Pat {
                a: MathNoCalc,
                b: MathNoCalc,
                c: MathNoCalc,
                div: MDiv,
            }
            Pat::new(a, b, c, div)
        },
        |ctx, pat| {
            let a_div_c = ctx.insert_m_div(pat.a, pat.c);
            let b_div_c = ctx.insert_m_div(pat.b, pat.c);
            let rhs = ctx.insert_m_add(a_div_c, b_div_c);
            ctx.union(pat.div, rhs);
        },
    );
    MyTxMathNoCalc::add_rule(
        "div_sub_distrib",
        rs,
        || {
            let a = MathNoCalc::query_leaf();
            let b = MathNoCalc::query_leaf();
            let c = MathNoCalc::query_leaf();
            let sub = MSub::query(&a, &b);
            let div = MDiv::query(&sub, &c);
            #[eggplant::pat_vars]
            struct Pat {
                a: MathNoCalc,
                b: MathNoCalc,
                c: MathNoCalc,
                div: MDiv,
            }
            Pat::new(a, b, c, div)
        },
        |ctx, pat| {
            let a_div_c = ctx.insert_m_div(pat.a, pat.c);
            let b_div_c = ctx.insert_m_div(pat.b, pat.c);
            let rhs = ctx.insert_m_sub(a_div_c, b_div_c);
            ctx.union(pat.div, rhs);
        },
    );
    MyTxMathNoCalc::add_rule(
        "add_frac_cross",
        rs,
        || {
            let a = MathNoCalc::query_leaf();
            let b = MathNoCalc::query_leaf();
            let c = MathNoCalc::query_leaf();
            let d = MathNoCalc::query_leaf();
            let lhs = MDiv::query(&a, &b);
            let rhs = MDiv::query(&c, &d);
            let add = MAdd::query(&lhs, &rhs);
            #[eggplant::pat_vars]
            struct Pat {
                a: MathNoCalc,
                b: MathNoCalc,
                c: MathNoCalc,
                d: MathNoCalc,
                add: MAdd,
            }
            Pat::new(a, b, c, d, add)
        },
        |ctx, pat| {
            let ad = ctx.insert_m_mul(pat.a, pat.d);
            let cb = ctx.insert_m_mul(pat.c, pat.b);
            let num = ctx.insert_m_add(ad, cb);
            let den = ctx.insert_m_mul(pat.b, pat.d);
            let rhs = ctx.insert_m_div(num, den);
            ctx.union(pat.add, rhs);
        },
    );
    MyTxMathNoCalc::add_rule(
        "sub_frac_cross",
        rs,
        || {
            let a = MathNoCalc::query_leaf();
            let b = MathNoCalc::query_leaf();
            let c = MathNoCalc::query_leaf();
            let d = MathNoCalc::query_leaf();
            let lhs = MDiv::query(&a, &b);
            let rhs = MDiv::query(&c, &d);
            let sub = MSub::query(&lhs, &rhs);
            #[eggplant::pat_vars]
            struct Pat {
                a: MathNoCalc,
                b: MathNoCalc,
                c: MathNoCalc,
                d: MathNoCalc,
                sub: MSub,
            }
            Pat::new(a, b, c, d, sub)
        },
        |ctx, pat| {
            let ad = ctx.insert_m_mul(pat.a, pat.d);
            let cb = ctx.insert_m_mul(pat.c, pat.b);
            let num = ctx.insert_m_sub(ad, cb);
            let den = ctx.insert_m_mul(pat.b, pat.d);
            let rhs = ctx.insert_m_div(num, den);
            ctx.union(pat.sub, rhs);
        },
    );
    MyTxMathNoCalc::add_rule(
        "pow_one",
        rs,
        || {
            let x = MathNoCalc::query_leaf();
            let o = MConst::query();
            let pow = MPow::query(&x, &o);
            let is_one = o.handle_n().eq(&1_i64);
            #[eggplant::pat_vars]
            struct Pat {
                x: MathNoCalc,
                o: MConst,
                pow: MPow,
            }
            Pat::new(x, o, pow).assert(is_one)
        },
        |ctx, pat| {
            ctx.union(pat.pow, pat.x);
        },
    );
    MyTxMathNoCalc::add_rule(
        "pow_two",
        rs,
        || {
            let x = MathNoCalc::query_leaf();
            let t = MConst::query();
            let pow = MPow::query(&x, &t);
            let is_two = t.handle_n().eq(&2_i64);
            #[eggplant::pat_vars]
            struct Pat {
                x: MathNoCalc,
                t: MConst,
                pow: MPow,
            }
            Pat::new(x, t, pow).assert(is_two)
        },
        |ctx, pat| {
            let rhs = ctx.insert_m_mul(pat.x, pat.x);
            ctx.union(pat.pow, rhs);
        },
    );
}

pub fn run_and_collect_stats(breakdown: bool) -> MathMicrobenchmarkStats {
    let t_total = Instant::now();
    let seed_callback_stats = breakdown.then(|| Arc::new(Mutex::new(BTreeMap::new())));
    let rewrite_callback_stats = breakdown.then(|| Arc::new(Mutex::new(BTreeMap::new())));

    let t = Instant::now();
    MyTxMathNoCalc::reset_for_bench();
    if breakdown {
        eprintln!(
            "[bench-breakdown] math-microbenchmark reset_for_bench: {:?}",
            t.elapsed()
        );
    }

    // Seed ground terms (ports `tests/math-microbenchmark.egg`).
    let t_seed_setup = Instant::now();
    let seed = MyTxMathNoCalc::new_ruleset("math_microbenchmark_no_calculus_seed");
    MyTxMathNoCalc::add_rule(
        "math_microbenchmark_no_calculus_seed",
        seed,
        || {
            #[eggplant::pat_vars_catch]
            struct Unit {}
        },
        |ctx, _pat| {
            let _x = ctx.insert_m_var("x".to_owned());
            let _y = ctx.insert_m_var("y".to_owned());
            let five = ctx.insert_m_var("five".to_owned());

            // // (Integral (Ln (Var "x")) (Var "x"))
            // let ln_x = ctx.insert_m_ln(x.clone());
            // ctx.insert_m_integral(ln_x, x.clone());

            // // (Integral (Add (Var "x") (Cos (Var "x"))) (Var "x"))
            // let cos_x = ctx.insert_m_cos(x.clone());
            // let add_x_cos_x = ctx.insert_m_add(x.clone(), cos_x);
            // ctx.insert_m_integral(add_x_cos_x, x.clone());

            // // (Integral (Mul (Cos (Var "x")) (Var "x")) (Var "x"))
            // let cos_x = ctx.insert_m_cos(x.clone());
            // let mul_cos_x_x = ctx.insert_m_mul(cos_x, x.clone());
            // ctx.insert_m_integral(mul_cos_x_x, x.clone());

            // // (Diff (Var "x") (Add (Const 1) (Mul (Const 2) (Var "x"))))
            // let c1 = ctx.insert_m_const(1);
            // let c2 = ctx.insert_m_const(2);
            // let mul_2_x = ctx.insert_m_mul(c2, x.clone());
            // let add_1_2x = ctx.insert_m_add(c1, mul_2_x);
            // ctx.insert_m_diff(x.clone(), add_1_2x);

            // // (Diff (Var "x") (Sub (Pow (Var "x") (Const 3))
            // //                      (Mul (Const 7) (Pow (Var "x") (Const 2)))))
            // let c3 = ctx.insert_m_const(3);
            // let c7 = ctx.insert_m_const(7);
            // let pow_x_3 = ctx.insert_m_pow(x.clone(), c3);
            // let pow_x_2 = ctx.insert_m_pow(x.clone(), ctx.insert_m_const(2));
            // let mul_7_pow = ctx.insert_m_mul(c7, pow_x_2);
            // let sub_pow = ctx.insert_m_sub(pow_x_3, mul_7_pow);
            // ctx.insert_m_diff(x.clone(), sub_pow);

            // // (Add (Mul (Var "y") (Add (Var "x") (Var "y")))
            // //      (Sub (Add (Var "x") (Const 2)) (Add (Var "x") (Var "x"))))
            // let add_x_y = ctx.insert_m_add(x.clone(), y.clone());
            // let mul_y_add = ctx.insert_m_mul(y.clone(), add_x_y);
            // let add_x_2 = ctx.insert_m_add(x.clone(), ctx.insert_m_const(2));
            // let add_x_x = ctx.insert_m_add(x.clone(), x.clone());
            // let sub_add = ctx.insert_m_sub(add_x_2, add_x_x);
            // ctx.insert_m_add(mul_y_add, sub_add);

            // // (Div (Const 1) (Sub (Div (Add (Const 1) (Sqrt (Var "five"))) (Const 2))
            // //                     (Div (Sub (Const 1) (Sqrt (Var "five"))) (Const 2))))
            let c1 = ctx.insert_m_const(1);
            let c2 = ctx.insert_m_const(2);
            let sqrt_five = ctx.insert_m_sqrt(five.clone());
            let add_1_sqrt = ctx.insert_m_add(c1, sqrt_five.clone());
            let div_add = ctx.insert_m_div(add_1_sqrt, c2);
            let sub_1_sqrt = ctx.insert_m_sub(ctx.insert_m_const(1), sqrt_five);
            let div_sub = ctx.insert_m_div(sub_1_sqrt, ctx.insert_m_const(2));
            let denom = ctx.insert_m_sub(div_add, div_sub);
            ctx.insert_m_div(ctx.insert_m_const(1), denom);
        },
    );
    if breakdown {
        eprintln!(
            "[bench-breakdown] math-microbenchmark seed add_rule: {:?}",
            t_seed_setup.elapsed()
        );
    }

    let t_seed_run = Instant::now();
    let seed_report = MyTxMathNoCalc::run_ruleset(seed, RunConfig::Once);
    if breakdown {
        eprintln!(
            "[bench-breakdown] math-microbenchmark seed run_ruleset: {:?}",
            t_seed_run.elapsed()
        );
        print_callback_timings(
            "math-microbenchmark seed",
            &seed_callback_stats,
            &seed_report,
        );
    }

    // Rewrite rules (ports `tests/math-microbenchmark.egg` rewrites).
    let t_rules_setup = Instant::now();
    let rs = MyTxMathNoCalc::new_ruleset("math_microbenchmark_no_calculus_rules");
    register_rewrite_rules(
        rs,
        "sub_self_zero",
        "mul_distrib",
        "add_factor",
        "mul_pow_combine",
    );

    if breakdown {
        eprintln!(
            "[bench-breakdown] math-microbenchmark rewrites add_rule: {:?}",
            t_rules_setup.elapsed()
        );
    }

    let t_rules_run = Instant::now();
    let rewrite_report = MyTxMathNoCalc::run_ruleset(rs, RunConfig::Times(11));
    if breakdown {
        eprintln!(
            "[bench-breakdown] math-microbenchmark rewrites run_ruleset: {:?}",
            t_rules_run.elapsed()
        );
        print_callback_timings(
            "math-microbenchmark rewrites",
            &rewrite_callback_stats,
            &rewrite_report,
        );
    }

    let egraph = <MyTxMathNoCalc as NonPatRecSgl>::egraph();
    let egraph = egraph.lock().unwrap();
    let stats = MathMicrobenchmarkStats {
        elapsed: t_total.elapsed(),
        total_num_tuples: egraph.num_tuples(),
        table_sizes: MATH_TABLES
            .iter()
            .map(|table| (*table, egraph.get_size(table)))
            .collect(),
        max_rewrite_mem_gib: DEFAULT_RUN_RULESET_MEMORY_CAP_GIB,
        rewrite_peak_memory_bytes: 0,
        requested_rewrite_iters: 11,
        executed_rewrite_iters: 11,
        rewrite_stopped_early_due_to_memory_cap: false,
    };
    if breakdown {
        eprintln!(
            "[bench-breakdown] math-microbenchmark total: {:?}",
            t_total.elapsed()
        );
    }
    stats
}

pub fn run_and_collect_stats_iters(
    breakdown: bool,
    rewrite_iters: usize,
) -> MathMicrobenchmarkStats {
    run_and_collect_stats_iters_with_mem_cap(
        breakdown,
        rewrite_iters,
        DEFAULT_RUN_RULESET_MEMORY_CAP_GIB,
    )
}

pub fn run_and_collect_stats_iters_with_mem_cap(
    breakdown: bool,
    rewrite_iters: usize,
    max_rewrite_mem_gib: u64,
) -> MathMicrobenchmarkStats {
    run_and_collect_stats_iters_with_mem_cap_and_progress(
        breakdown,
        rewrite_iters,
        max_rewrite_mem_gib,
        |_| {},
    )
}

pub fn run_and_collect_stats_iters_with_mem_cap_and_progress<F>(
    breakdown: bool,
    rewrite_iters: usize,
    max_rewrite_mem_gib: u64,
    mut on_progress: F,
) -> MathMicrobenchmarkStats
where
    F: FnMut(ProgressEvent),
{
    let t_total = Instant::now();
    let seed_callback_stats = breakdown.then(|| Arc::new(Mutex::new(BTreeMap::new())));
    let rewrite_callback_stats = breakdown.then(|| Arc::new(Mutex::new(BTreeMap::new())));
    let max_rewrite_mem_bytes = gib_to_bytes(max_rewrite_mem_gib);

    let t = Instant::now();
    MyTxMathNoCalc::reset_for_bench();
    if breakdown {
        eprintln!(
            "[bench-breakdown] math-microbenchmark reset_for_bench: {:?}",
            t.elapsed()
        );
    }

    let t_seed_setup = Instant::now();
    let seed = MyTxMathNoCalc::new_ruleset("math_microbenchmark_no_calculus_seed_iters");
    MyTxMathNoCalc::add_rule(
        "math_microbenchmark_no_calculus_seed_iters",
        seed,
        || {
            #[eggplant::pat_vars_catch]
            struct Unit {}
        },
        |ctx, _pat| {
            let x = ctx.insert_m_var("x".to_owned());
            let y = ctx.insert_m_var("y".to_owned());
            let five = ctx.insert_m_var("five".to_owned());

            let add_x_y = ctx.insert_m_add(x.clone(), y.clone());
            let mul_y_add = ctx.insert_m_mul(y.clone(), add_x_y);
            let add_x_2 = ctx.insert_m_add(x.clone(), ctx.insert_m_const(2));
            let add_x_x = ctx.insert_m_add(x.clone(), x.clone());
            let sub_add = ctx.insert_m_sub(add_x_2, add_x_x);
            ctx.insert_m_add(mul_y_add, sub_add);

            let c1 = ctx.insert_m_const(1);
            let c2 = ctx.insert_m_const(2);
            let sqrt_five = ctx.insert_m_sqrt(five.clone());
            let add_1_sqrt = ctx.insert_m_add(c1, sqrt_five.clone());
            let div_add = ctx.insert_m_div(add_1_sqrt, c2);
            let sub_1_sqrt = ctx.insert_m_sub(ctx.insert_m_const(1), sqrt_five);
            let div_sub = ctx.insert_m_div(sub_1_sqrt, ctx.insert_m_const(2));
            let denom = ctx.insert_m_sub(div_add, div_sub);
            ctx.insert_m_div(ctx.insert_m_const(1), denom);
        },
    );
    if breakdown {
        eprintln!(
            "[bench-breakdown] math-microbenchmark seed add_rule: {:?}",
            t_seed_setup.elapsed()
        );
    }

    let t_seed_run = Instant::now();
    let seed_report = MyTxMathNoCalc::run_ruleset(seed, RunConfig::Once);
    if breakdown {
        eprintln!(
            "[bench-breakdown] math-microbenchmark seed run_ruleset: {:?}",
            t_seed_run.elapsed()
        );
        print_callback_timings(
            "math-microbenchmark seed",
            &seed_callback_stats,
            &seed_report,
        );
    }

    let t_rules_setup = Instant::now();
    let rs = MyTxMathNoCalc::new_ruleset("math_microbenchmark_no_calculus_rules_iters");
    register_rewrite_rules(
        rs,
        "sub_self",
        "distribute_mul",
        "factor_mul",
        "pow_mul_same_base",
    );

    if breakdown {
        eprintln!(
            "[bench-breakdown] math-microbenchmark rewrites add_rule: {:?}",
            t_rules_setup.elapsed()
        );
    }

    let t_rules_run = Instant::now();
    let rewrite_peak_before = current_peak_memory_bytes();
    let mut rewrite_report = RunReport::default();
    let mut executed_rewrite_iters = 0usize;
    let mut rewrite_stopped_early_due_to_memory_cap = false;
    for _ in 0..rewrite_iters {
        let report = MyTxMathNoCalc::run_ruleset(rs, RunConfig::Once);
        rewrite_report.union(report);
        executed_rewrite_iters += 1;
        let current_peak = current_peak_memory_bytes();
        let tuple_count = {
            let egraph = <MyTxMathNoCalc as NonPatRecSgl>::egraph();
            let egraph = egraph.lock().unwrap();
            egraph.num_tuples()
        };
        eprintln!(
            "[math-microbenchmark] completed rewrite iteration {}/{}, tuples={}, peak_mem={:.2} MiB",
            executed_rewrite_iters,
            rewrite_iters,
            tuple_count,
            current_peak as f64 / (1024.0 * 1024.0),
        );
        on_progress(ProgressEvent::RewriteIterationComplete {
            current: executed_rewrite_iters,
            total: rewrite_iters,
            tuple_count,
            peak_memory_bytes: current_peak,
        });
        if current_peak > max_rewrite_mem_bytes {
            rewrite_stopped_early_due_to_memory_cap = true;
            eprintln!(
                "[math-microbenchmark] stopping early after iteration {} because peak memory {:.2} MiB exceeded configured cap {:.2} MiB",
                executed_rewrite_iters,
                current_peak as f64 / (1024.0 * 1024.0),
                max_rewrite_mem_bytes as f64 / (1024.0 * 1024.0),
            );
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
    if breakdown {
        eprintln!(
            "[bench-breakdown] math-microbenchmark rewrites run_ruleset: {:?}",
            t_rules_run.elapsed()
        );
        print_callback_timings(
            "math-microbenchmark rewrites",
            &rewrite_callback_stats,
            &rewrite_report,
        );
    }

    let egraph = <MyTxMathNoCalc as NonPatRecSgl>::egraph();
    let egraph = egraph.lock().unwrap();
    let stats = MathMicrobenchmarkStats {
        elapsed: t_total.elapsed(),
        total_num_tuples: egraph.num_tuples(),
        table_sizes: MATH_TABLES
            .iter()
            .map(|table| (*table, egraph.get_size(table)))
            .collect(),
        max_rewrite_mem_gib,
        rewrite_peak_memory_bytes,
        requested_rewrite_iters: rewrite_iters,
        executed_rewrite_iters,
        rewrite_stopped_early_due_to_memory_cap,
    };
    if breakdown {
        eprintln!(
            "[bench-breakdown] math-microbenchmark total: {:?}",
            t_total.elapsed()
        );
    }
    stats
}

pub fn bench() {
    let breakdown = std::env::var_os("EGGPLANT_BENCH_BREAKDOWN").is_some();
    let _ = run_and_collect_stats(breakdown);
}

#[cfg(feature = "rustsat-extract")]
fn build_extract_target() -> impl EgglogNode + EgglogTy + 'static {
    let five = MVar::<MyTxMathNoCalc>::new("five".to_owned());
    five.commit();
    let one_a = MConst::<MyTxMathNoCalc>::new(1);
    one_a.commit();
    let one_b = MConst::<MyTxMathNoCalc>::new(1);
    one_b.commit();
    let one_c = MConst::<MyTxMathNoCalc>::new(1);
    one_c.commit();
    let two_a = MConst::<MyTxMathNoCalc>::new(2);
    two_a.commit();
    let two_b = MConst::<MyTxMathNoCalc>::new(2);
    two_b.commit();

    let sqrt_five_a = MSqrt::<MyTxMathNoCalc>::new(&five);
    sqrt_five_a.commit();
    let add_1_sqrt = MAdd::<MyTxMathNoCalc>::new(&one_a, &sqrt_five_a);
    add_1_sqrt.commit();
    let div_add = MDiv::<MyTxMathNoCalc>::new(&add_1_sqrt, &two_a);
    div_add.commit();

    let sqrt_five_b = MSqrt::<MyTxMathNoCalc>::new(&five);
    sqrt_five_b.commit();
    let sub_1_sqrt = MSub::<MyTxMathNoCalc>::new(&one_b, &sqrt_five_b);
    sub_1_sqrt.commit();
    let div_sub = MDiv::<MyTxMathNoCalc>::new(&sub_1_sqrt, &two_b);
    div_sub.commit();

    let denom = MSub::<MyTxMathNoCalc>::new(&div_add, &div_sub);
    denom.commit();
    let root = MDiv::<MyTxMathNoCalc>::new(&one_c, &denom);
    root.commit();
    root
}

#[cfg(feature = "rustsat-extract")]
fn benchmark_extract_backend<N>(
    target: &N,
    method: &'static str,
    rewrite_peak_memory_bytes: u64,
    backend: ExtractBackend<TreeAdditiveCostModel>,
) -> MathExtractComparisonRow
where
    N: EgglogNode + EgglogTy + 'static,
{
    let extract_peak_before = current_peak_memory_bytes();
    let started = Instant::now();
    let (rendered, cost) = MyTxMathNoCalc::extract_node_to_string_with_backend(target, backend)
        .expect("math microbench extraction should succeed");
    let svg_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("target")
        .join("math_microbench_extract_svgs");
    fs::create_dir_all(&svg_dir).expect("svg output directory should be creatable");
    let svg_path = svg_dir.join(format!("{method}.svg"));
    let _ = MyTxMathNoCalc::extract_node_to_svg_with_backend(
        target,
        match method {
            "default" => ExtractBackend::cost_model(TreeAdditiveCostModel::default()),
            "eboost" => ExtractBackend::<TreeAdditiveCostModel>::eboost_heuristic(
                EBoostExtractConfig::default(),
            ),
            "layered" => ExtractBackend::<TreeAdditiveCostModel>::eboost_layered(
                EBoostLayeredConfig::default(),
            ),
            "rustsat" => {
                ExtractBackend::<TreeAdditiveCostModel>::rustsat(RustsatExtractConfig::default())
            }
            other => panic!("unsupported extract method `{other}`"),
        },
        &svg_path,
    )
    .expect("svg rendering should succeed for math microbench extract comparison");
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
            let row = benchmark_extract_backend(
                target,
                method,
                rewrite_peak_memory_bytes,
                extract_backend_for_method(method),
            );
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
    backend: ExtractBackend<TreeAdditiveCostModel>,
) -> ExtractTimelineMetric
where
    N: EgglogNode + EgglogTy + 'static,
{
    let extract_peak_before = current_peak_memory_bytes();
    let started = Instant::now();
    let (_rendered, cost) = MyTxMathNoCalc::extract_node_to_string_with_backend(target, backend)
        .expect("timeline extract should succeed");
    let elapsed_ms = elapsed_ms(started);
    let svg_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("target")
        .join("math_microbench_timeline_svgs")
        .join(format!("iter_{iteration:03}"));
    fs::create_dir_all(&svg_dir).expect("timeline svg output directory should be creatable");
    let svg_path = svg_dir.join(format!("{method}.svg"));
    let _ = MyTxMathNoCalc::extract_node_to_svg_with_backend(
        target,
        match method {
            "default" => ExtractBackend::cost_model(TreeAdditiveCostModel::default()),
            "eboost" => ExtractBackend::<TreeAdditiveCostModel>::eboost_heuristic(
                EBoostExtractConfig::default(),
            ),
            "layered" => ExtractBackend::<TreeAdditiveCostModel>::eboost_layered(
                EBoostLayeredConfig::default(),
            ),
            "rustsat" => {
                ExtractBackend::<TreeAdditiveCostModel>::rustsat(RustsatExtractConfig::default())
            }
            other => panic!("unsupported extract method `{other}`"),
        },
        &svg_path,
    )
    .expect("timeline svg rendering should succeed");
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
        || {
            measure_extract_metric(
                target,
                iteration,
                method,
                extract_backend_for_method(method),
            )
        },
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
    run_extract_comparison_with_iters_and_mem_cap_and_progress_detailed(
        rewrite_iters,
        max_rewrite_mem_gib,
        &extract_methods,
        max_extract_time_secs,
        |event| on_progress(event),
    )
}

#[cfg(feature = "rustsat-extract")]
pub fn run_extract_comparison_with_iters_and_mem_cap_and_progress<F>(
    rewrite_iters: usize,
    max_rewrite_mem_gib: u64,
    mut on_progress: F,
) -> Vec<MathExtractComparisonRow>
where
    F: FnMut(ProgressEvent),
{
    run_extract_comparison_with_iters_and_mem_cap_and_progress_detailed(
        rewrite_iters,
        max_rewrite_mem_gib,
        ALL_EXTRACT_METHODS,
        None,
        |event| on_progress(event),
    )
}

#[cfg(feature = "rustsat-extract")]
pub fn run_extract_comparison_with_iters_and_mem_cap_and_progress_detailed<F>(
    rewrite_iters: usize,
    max_rewrite_mem_gib: u64,
    extract_methods: &[&'static str],
    max_extract_time_secs: Option<u64>,
    mut on_progress: F,
) -> Vec<MathExtractComparisonRow>
where
    F: FnMut(ProgressEvent),
{
    let stats = run_and_collect_stats_iters_with_mem_cap_and_progress(
        false,
        rewrite_iters,
        max_rewrite_mem_gib,
        |event| on_progress(event),
    );
    let target = build_extract_target();

    let mut rows = Vec::new();
    let total_extract_methods = extract_methods.len();

    for (index, method) in extract_methods.iter().enumerate() {
        on_progress(ProgressEvent::ExtractPhaseStart {
            method,
            current: index + 1,
            total: total_extract_methods,
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
            total: total_extract_methods,
            elapsed_ms: row.elapsed.map(duration_to_ms).unwrap_or(f64::NAN),
            peak_memory_bytes: row.extract_peak_memory_bytes.unwrap_or(0),
        });
        rows.push(row);
    }

    rows
}

#[cfg(feature = "rustsat-extract")]
pub fn run_extract_comparison_with_iters(rewrite_iters: usize) -> Vec<MathExtractComparisonRow> {
    run_extract_comparison_with_iters_and_mem_cap(rewrite_iters, DEFAULT_RUN_RULESET_MEMORY_CAP_GIB)
}

#[cfg(feature = "rustsat-extract")]
pub fn run_extract_comparison() -> Vec<MathExtractComparisonRow> {
    run_extract_comparison_with_iters(11)
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

    MyTxMathNoCalc::reset_for_bench();

    let seed = MyTxMathNoCalc::new_ruleset("math_microbenchmark_no_calculus_timeline_seed");
    MyTxMathNoCalc::add_rule(
        "math_microbenchmark_no_calculus_timeline_seed",
        seed,
        || {
            #[eggplant::pat_vars_catch]
            struct Unit {}
        },
        |ctx, _pat| {
            let x = ctx.insert_m_var("x".to_owned());
            let y = ctx.insert_m_var("y".to_owned());
            let five = ctx.insert_m_var("five".to_owned());

            let add_x_y = ctx.insert_m_add(x.clone(), y.clone());
            let mul_y_add = ctx.insert_m_mul(y.clone(), add_x_y);
            let add_x_2 = ctx.insert_m_add(x.clone(), ctx.insert_m_const(2));
            let add_x_x = ctx.insert_m_add(x.clone(), x.clone());
            let sub_add = ctx.insert_m_sub(add_x_2, add_x_x);
            ctx.insert_m_add(mul_y_add, sub_add);

            let c1 = ctx.insert_m_const(1);
            let c2 = ctx.insert_m_const(2);
            let sqrt_five = ctx.insert_m_sqrt(five.clone());
            let add_1_sqrt = ctx.insert_m_add(c1, sqrt_five.clone());
            let div_add = ctx.insert_m_div(add_1_sqrt, c2);
            let sub_1_sqrt = ctx.insert_m_sub(ctx.insert_m_const(1), sqrt_five);
            let div_sub = ctx.insert_m_div(sub_1_sqrt, ctx.insert_m_const(2));
            let denom = ctx.insert_m_sub(div_add, div_sub);
            ctx.insert_m_div(ctx.insert_m_const(1), denom);
        },
    );
    MyTxMathNoCalc::run_ruleset(seed, RunConfig::Once);

    let target = build_extract_target();

    let rs = MyTxMathNoCalc::new_ruleset("math_microbenchmark_no_calculus_timeline_rules");
    register_rewrite_rules(
        rs,
        "sub_self",
        "distribute_mul",
        "factor_mul",
        "pow_mul_same_base",
    );

    let mut points = Vec::new();
    let mut executed_rewrite_iters = 0usize;
    let mut stopped_early_due_to_memory_cap = false;
    let rewrite_peak_before = current_peak_memory_bytes();
    for iteration in 1..=rewrite_iters {
        let started = Instant::now();
        let report = MyTxMathNoCalc::run_ruleset(rs, RunConfig::Once);
        let rewrite_elapsed_ms = elapsed_ms(started);
        executed_rewrite_iters += 1;
        let current_peak = current_peak_memory_bytes();
        let tuple_count = {
            let egraph = <MyTxMathNoCalc as NonPatRecSgl>::egraph();
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

pub fn run_div_add_rewrite_smoke<CM>(rewrite_iters: usize, cost_model: CM) -> String
where
    CM: eggplant::egglog::extract::CostModel<eggplant::egglog::extract::DefaultCost> + 'static,
{
    MyTxMathNoCalc::reset_for_bench();

    let one = MConst::<MyTxMathNoCalc>::new(1);
    one.commit();
    let x = MVar::<MyTxMathNoCalc>::new("x".to_owned());
    x.commit();
    let two = MConst::<MyTxMathNoCalc>::new(2);
    two.commit();
    let add = MAdd::<MyTxMathNoCalc>::new(&one, &x);
    add.commit();
    let root = MDiv::<MyTxMathNoCalc>::new(&add, &two);
    root.commit();

    let rs = MyTxMathNoCalc::new_ruleset("math_microbenchmark_div_add_smoke");
    {
        MyTxMathNoCalc::add_rule(
            "div_add_distrib_smoke",
            rs,
            || {
                let a = MathNoCalc::query_leaf();
                let b = MathNoCalc::query_leaf();
                let c = MathNoCalc::query_leaf();
                let add = MAdd::query(&a, &b);
                let div = MDiv::query(&add, &c);
                #[eggplant::pat_vars]
                struct Pat {
                    a: MathNoCalc,
                    b: MathNoCalc,
                    c: MathNoCalc,
                    div: MDiv,
                }
                Pat::new(a, b, c, div)
            },
            |ctx, pat| {
                let a_div_c = ctx.insert_m_div(pat.a, pat.c);
                let b_div_c = ctx.insert_m_div(pat.b, pat.c);
                let rhs = ctx.insert_m_add(a_div_c, b_div_c);
                ctx.union(pat.div, rhs);
            },
        );
    };

    for _ in 0..rewrite_iters {
        MyTxMathNoCalc::run_ruleset(rs, RunConfig::Once);
    }

    let (rendered, _) = MyTxMathNoCalc::extract_node_to_string_with_cost_model(&root, cost_model)
        .expect("smoke extract should succeed");
    rendered
}

pub fn run_cancel_neg_add_rewrite_smoke<CM>(rewrite_iters: usize, cost_model: CM) -> String
where
    CM: eggplant::egglog::extract::CostModel<eggplant::egglog::extract::DefaultCost> + 'static,
{
    MyTxMathNoCalc::reset_for_bench();

    let x = MVar::<MyTxMathNoCalc>::new("x".to_owned());
    x.commit();
    let neg_one = MConst::<MyTxMathNoCalc>::new(-1);
    neg_one.commit();
    let neg_x = MMul::<MyTxMathNoCalc>::new(&neg_one, &x);
    neg_x.commit();
    let root = MAdd::<MyTxMathNoCalc>::new(&x, &neg_x);
    root.commit();

    let rs = MyTxMathNoCalc::new_ruleset("math_microbenchmark_cancel_neg_add_smoke");
    MyTxMathNoCalc::add_rule(
        "add_neg_self_zero_smoke",
        rs,
        || {
            let a = MathNoCalc::query_leaf();
            let b = MathNoCalc::query_leaf();
            let neg_one = MConst::query();
            let neg_b = MMul::query(&neg_one, &b);
            let add = MAdd::query(&a, &neg_b);
            let is_neg_one = neg_one.handle_n().eq(&-1_i64);
            let same_term = a.handle().eq(&b.handle());
            #[eggplant::pat_vars]
            struct Pat {
                a: MathNoCalc,
                b: MathNoCalc,
                neg_one: MConst,
                add: MAdd,
            }
            Pat::new(a, b, neg_one, add)
                .assert(is_neg_one)
                .assert(same_term)
        },
        |ctx, pat| {
            let z = ctx.insert_m_const(0);
            ctx.union(pat.add, z);
        },
    );

    for _ in 0..rewrite_iters {
        MyTxMathNoCalc::run_ruleset(rs, RunConfig::Once);
    }

    let (rendered, _) = MyTxMathNoCalc::extract_node_to_string_with_cost_model(&root, cost_model)
        .expect("cancel-neg smoke extract should succeed");
    rendered
}
