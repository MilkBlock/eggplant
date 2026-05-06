use egglog_reports::RunReport;
use eggplant::prelude::*;
use eggplant::tx_rx_vt_pr;
use eggplant::wrap::NonPatRecSgl;
use libc::{RUSAGE_SELF, getrusage, rusage};
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
enum Math {
    #[typst("{f}'({x})")]
    #[precedence(90)]
    #[cost(32)]
    MDiff { x: Math, f: Math },
    #[typst("integral {f} quad d {x}")]
    #[precedence(90)]
    #[cost(40)]
    MIntegral { f: Math, x: Math },

    #[typst("{a} + {b}")]
    #[precedence(50)]
    #[cost(1)]
    MAdd { a: Math, b: Math },
    #[typst("{a} - {b}")]
    #[precedence(50)]
    #[cost(1)]
    MSub { a: Math, b: Math },
    #[typst("{a} * {b}")]
    #[precedence(60)]
    #[cost(3)]
    MMul { a: Math, b: Math },
    #[typst("frac({a}, {b}) ")]
    #[precedence(60)]
    #[cost(10)]
    MDiv { a: Math, b: Math },
    #[typst("{a}^{b}")]
    #[precedence(80)]
    #[cost(25)]
    MPow { a: Math, b: Math },
    #[typst("ln {a}")]
    #[precedence(90)]
    #[cost(18)]
    MLn { a: Math },
    #[typst("sqrt({a})")]
    #[precedence(90)]
    #[cost(16)]
    MSqrt { a: Math },

    #[typst("sin({a})")]
    #[precedence(90)]
    #[cost(18)]
    MSin { a: Math },
    #[typst("cos({a})")]
    #[precedence(90)]
    #[cost(18)]
    MCos { a: Math },

    #[typst("{n}")]
    #[precedence(100)]
    #[cost(0)]
    MConst { n: i64 },
    #[typst("{name}")]
    #[precedence(100)]
    #[cost(0)]
    MVar { name: String },
}

tx_rx_vt_pr!(MyTxMath, MyPatRecMath);

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
    pub extract_peak_memory_bytes: u64,
    pub elapsed: Duration,
    pub rendered: String,
    pub svg_path: String,
}

const DEFAULT_RUN_RULESET_MEMORY_CAP_GIB: u64 = 20;

fn current_peak_memory_bytes() -> u64 {
    let mut usage = std::mem::MaybeUninit::<rusage>::uninit();
    let rc = unsafe { getrusage(RUSAGE_SELF, usage.as_mut_ptr()) };
    if rc != 0 {
        return 0;
    }
    let usage = unsafe { usage.assume_init() };
    #[cfg(target_os = "macos")]
    {
        usage.ru_maxrss as u64
    }
    #[cfg(not(target_os = "macos"))]
    {
        (usage.ru_maxrss as u64) * 1024
    }
}

const MATH_TABLES: &[&str] = &[
    "MDiff",
    "MIntegral",
    "MAdd",
    "MSub",
    "MMul",
    "MDiv",
    "MPow",
    "MLn",
    "MSqrt",
    "MSin",
    "MCos",
    "MConst",
    "MVar",
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
    MyTxMath::add_rule(
        "add_comm",
        rs,
        || {
            let a = Math::query_leaf();
            let b = Math::query_leaf();
            let add = MAdd::query(&a, &b);
            #[eggplant::pat_vars]
            struct Pat {
                a: Math,
                b: Math,
                add: MAdd,
            }
            Pat::new(a, b, add)
        },
        |ctx, pat| {
            let rhs = ctx.insert_m_add(pat.b, pat.a);
            ctx.union(pat.add, rhs);
        },
    );
    MyTxMath::add_rule(
        "mul_comm",
        rs,
        || {
            let a = Math::query_leaf();
            let b = Math::query_leaf();
            let mul = MMul::query(&a, &b);
            #[eggplant::pat_vars]
            struct Pat {
                a: Math,
                b: Math,
                mul: MMul,
            }
            Pat::new(a, b, mul)
        },
        |ctx, pat| {
            let rhs = ctx.insert_m_mul(pat.b, pat.a);
            ctx.union(pat.mul, rhs);
        },
    );
    MyTxMath::add_rule(
        "add_assoc",
        rs,
        || {
            let a = Math::query_leaf();
            let b = Math::query_leaf();
            let c = Math::query_leaf();
            let add_inner = MAdd::query(&b, &c);
            let add_outer = MAdd::query(&a, &add_inner);
            #[eggplant::pat_vars]
            struct Pat {
                a: Math,
                b: Math,
                c: Math,
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
    MyTxMath::add_rule(
        "mul_assoc",
        rs,
        || {
            let a = Math::query_leaf();
            let b = Math::query_leaf();
            let c = Math::query_leaf();
            let mul_inner = MMul::query(&b, &c);
            let mul_outer = MMul::query(&a, &mul_inner);
            #[eggplant::pat_vars]
            struct Pat {
                a: Math,
                b: Math,
                c: Math,
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
    MyTxMath::add_rule(
        "sub_to_add_neg",
        rs,
        || {
            let a = Math::query_leaf();
            let b = Math::query_leaf();
            let sub = MSub::query(&a, &b);
            #[eggplant::pat_vars]
            struct Pat {
                a: Math,
                b: Math,
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
    MyTxMath::add_rule(
        "add_zero",
        rs,
        || {
            let a = Math::query_leaf();
            let z = MConst::query();
            let add = MAdd::query(&a, &z);
            let is_zero = z.handle_n().eq(&0_i64);
            #[eggplant::pat_vars]
            struct Pat {
                a: Math,
                z: MConst,
                add: MAdd,
            }
            Pat::new(a, z, add).assert(is_zero)
        },
        |ctx, pat| {
            ctx.union(pat.add, pat.a);
        },
    );
    MyTxMath::add_rule(
        "mul_zero",
        rs,
        || {
            let a = Math::query_leaf();
            let z = MConst::query();
            let mul = MMul::query(&a, &z);
            let is_zero = z.handle_n().eq(&0_i64);
            #[eggplant::pat_vars]
            struct Pat {
                a: Math,
                z: MConst,
                mul: MMul,
            }
            Pat::new(a, z, mul).assert(is_zero)
        },
        |ctx, pat| {
            ctx.union(pat.mul, pat.z);
        },
    );
    MyTxMath::add_rule(
        "mul_one",
        rs,
        || {
            let a = Math::query_leaf();
            let o = MConst::query();
            let mul = MMul::query(&a, &o);
            let is_one = o.handle_n().eq(&1_i64);
            #[eggplant::pat_vars]
            struct Pat {
                a: Math,
                o: MConst,
                mul: MMul,
            }
            Pat::new(a, o, mul).assert(is_one)
        },
        |ctx, pat| {
            ctx.union(pat.mul, pat.a);
        },
    );
    MyTxMath::add_rule(
        sub_self,
        rs,
        || {
            let a = Math::query_leaf();
            let b = Math::query_leaf();
            let sub = MSub::query(&a, &b);
            let same_terms = a.handle().eq(&b.handle());
            #[eggplant::pat_vars]
            struct Pat {
                a: Math,
                sub: MSub,
            }
            Pat::new(a, sub).assert(same_terms)
        },
        |ctx, pat| {
            let z = ctx.insert_m_const(0);
            ctx.union(pat.sub, z);
        },
    );
    MyTxMath::add_rule(
        mul_distrib,
        rs,
        || {
            let a = Math::query_leaf();
            let b = Math::query_leaf();
            let c = Math::query_leaf();
            let add = MAdd::query(&b, &c);
            let mul = MMul::query(&a, &add);
            #[eggplant::pat_vars]
            struct Pat {
                a: Math,
                b: Math,
                c: Math,
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
    MyTxMath::add_rule(
        add_factor,
        rs,
        || {
            let a = Math::query_leaf();
            let b = Math::query_leaf();
            let c = Math::query_leaf();
            let mul1 = MMul::query(&a, &b);
            let mul2 = MMul::query(&a, &c);
            let add = MAdd::query(&mul1, &mul2);
            #[eggplant::pat_vars]
            struct Pat {
                a: Math,
                b: Math,
                c: Math,
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
    MyTxMath::add_rule(
        mul_pow_combine,
        rs,
        || {
            let a = Math::query_leaf();
            let b = Math::query_leaf();
            let c = Math::query_leaf();
            let p1 = MPow::query(&a, &b);
            let p2 = MPow::query(&a, &c);
            let mul = MMul::query(&p1, &p2);
            #[eggplant::pat_vars]
            struct Pat {
                a: Math,
                b: Math,
                c: Math,
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
    MyTxMath::add_rule(
        "div_add_distrib",
        rs,
        || {
            let a = Math::query_leaf();
            let b = Math::query_leaf();
            let c = Math::query_leaf();
            let add = MAdd::query(&a, &b);
            let div = MDiv::query(&add, &c);
            #[eggplant::pat_vars]
            struct Pat {
                a: Math,
                b: Math,
                c: Math,
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
    MyTxMath::add_rule(
        "div_sub_distrib",
        rs,
        || {
            let a = Math::query_leaf();
            let b = Math::query_leaf();
            let c = Math::query_leaf();
            let sub = MSub::query(&a, &b);
            let div = MDiv::query(&sub, &c);
            #[eggplant::pat_vars]
            struct Pat {
                a: Math,
                b: Math,
                c: Math,
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
    MyTxMath::add_rule(
        "pow_one",
        rs,
        || {
            let x = Math::query_leaf();
            let o = MConst::query();
            let pow = MPow::query(&x, &o);
            let is_one = o.handle_n().eq(&1_i64);
            #[eggplant::pat_vars]
            struct Pat {
                x: Math,
                o: MConst,
                pow: MPow,
            }
            Pat::new(x, o, pow).assert(is_one)
        },
        |ctx, pat| {
            ctx.union(pat.pow, pat.x);
        },
    );
    MyTxMath::add_rule(
        "pow_two",
        rs,
        || {
            let x = Math::query_leaf();
            let t = MConst::query();
            let pow = MPow::query(&x, &t);
            let is_two = t.handle_n().eq(&2_i64);
            #[eggplant::pat_vars]
            struct Pat {
                x: Math,
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
    MyTxMath::add_rule(
        "diff_add",
        rs,
        || {
            let x = Math::query_leaf();
            let a = Math::query_leaf();
            let b = Math::query_leaf();
            let add = MAdd::query(&a, &b);
            let diff = MDiff::query(&x, &add);
            #[eggplant::pat_vars]
            struct Pat {
                x: Math,
                a: Math,
                b: Math,
                diff: MDiff,
            }
            Pat::new(x, a, b, diff)
        },
        |ctx, pat| {
            let da = ctx.insert_m_diff(pat.x, pat.a);
            let db = ctx.insert_m_diff(pat.x, pat.b);
            let rhs = ctx.insert_m_add(da, db);
            ctx.union(pat.diff, rhs);
        },
    );
    MyTxMath::add_rule(
        "diff_mul",
        rs,
        || {
            let x = Math::query_leaf();
            let a = Math::query_leaf();
            let b = Math::query_leaf();
            let mul = MMul::query(&a, &b);
            let diff = MDiff::query(&x, &mul);
            #[eggplant::pat_vars]
            struct Pat {
                x: Math,
                a: Math,
                b: Math,
                diff: MDiff,
            }
            Pat::new(x, a, b, diff)
        },
        |ctx, pat| {
            let db = ctx.insert_m_diff(pat.x, pat.b);
            let da = ctx.insert_m_diff(pat.x, pat.a);
            let a_db = ctx.insert_m_mul(pat.a, db);
            let b_da = ctx.insert_m_mul(pat.b, da);
            let rhs = ctx.insert_m_add(a_db, b_da);
            ctx.union(pat.diff, rhs);
        },
    );
    MyTxMath::add_rule(
        "diff_sin",
        rs,
        || {
            let x = Math::query_leaf();
            let sin = MSin::query(&x);
            let diff = MDiff::query(&x, &sin);
            #[eggplant::pat_vars]
            struct Pat {
                x: Math,
                diff: MDiff,
            }
            Pat::new(x, diff)
        },
        |ctx, pat| {
            let rhs = ctx.insert_m_cos(pat.x);
            ctx.union(pat.diff, rhs);
        },
    );
    MyTxMath::add_rule(
        "diff_cos",
        rs,
        || {
            let x = Math::query_leaf();
            let cos = MCos::query(&x);
            let diff = MDiff::query(&x, &cos);
            #[eggplant::pat_vars]
            struct Pat {
                x: Math,
                diff: MDiff,
            }
            Pat::new(x, diff)
        },
        |ctx, pat| {
            let neg1 = ctx.insert_m_const(-1);
            let sin = ctx.insert_m_sin(pat.x);
            let rhs = ctx.insert_m_mul(neg1, sin);
            ctx.union(pat.diff, rhs);
        },
    );
    MyTxMath::add_rule(
        "int_one",
        rs,
        || {
            let x = Math::query_leaf();
            let one = MConst::query();
            let integ = MIntegral::query(&one, &x);
            let is_one = one.handle_n().eq(&1_i64);
            #[eggplant::pat_vars]
            struct Pat {
                x: Math,
                one: MConst,
                integ: MIntegral,
            }
            Pat::new(x, one, integ).assert(is_one)
        },
        |ctx, pat| {
            ctx.union(pat.integ, pat.x);
        },
    );
    MyTxMath::add_rule(
        "int_cos",
        rs,
        || {
            let x = Math::query_leaf();
            let cos = MCos::query(&x);
            let integ = MIntegral::query(&cos, &x);
            #[eggplant::pat_vars]
            struct Pat {
                x: Math,
                integ: MIntegral,
            }
            Pat::new(x, integ)
        },
        |ctx, pat| {
            let rhs = ctx.insert_m_sin(pat.x);
            ctx.union(pat.integ, rhs);
        },
    );
    MyTxMath::add_rule(
        "int_sin",
        rs,
        || {
            let x = Math::query_leaf();
            let sin = MSin::query(&x);
            let integ = MIntegral::query(&sin, &x);
            #[eggplant::pat_vars]
            struct Pat {
                x: Math,
                integ: MIntegral,
            }
            Pat::new(x, integ)
        },
        |ctx, pat| {
            let neg1 = ctx.insert_m_const(-1);
            let cos = ctx.insert_m_cos(pat.x);
            let rhs = ctx.insert_m_mul(neg1, cos);
            ctx.union(pat.integ, rhs);
        },
    );
    MyTxMath::add_rule(
        "int_add",
        rs,
        || {
            let f = Math::query_leaf();
            let g = Math::query_leaf();
            let x = Math::query_leaf();
            let add = MAdd::query(&f, &g);
            let integ = MIntegral::query(&add, &x);
            #[eggplant::pat_vars]
            struct Pat {
                f: Math,
                g: Math,
                x: Math,
                integ: MIntegral,
            }
            Pat::new(f, g, x, integ)
        },
        |ctx, pat| {
            let i_f = ctx.insert_m_integral(pat.f, pat.x);
            let i_g = ctx.insert_m_integral(pat.g, pat.x);
            let rhs = ctx.insert_m_add(i_f, i_g);
            ctx.union(pat.integ, rhs);
        },
    );
    MyTxMath::add_rule(
        "int_sub",
        rs,
        || {
            let f = Math::query_leaf();
            let g = Math::query_leaf();
            let x = Math::query_leaf();
            let sub = MSub::query(&f, &g);
            let integ = MIntegral::query(&sub, &x);
            #[eggplant::pat_vars]
            struct Pat {
                f: Math,
                g: Math,
                x: Math,
                integ: MIntegral,
            }
            Pat::new(f, g, x, integ)
        },
        |ctx, pat| {
            let i_f = ctx.insert_m_integral(pat.f, pat.x);
            let i_g = ctx.insert_m_integral(pat.g, pat.x);
            let rhs = ctx.insert_m_sub(i_f, i_g);
            ctx.union(pat.integ, rhs);
        },
    );
    MyTxMath::add_rule(
        "int_mul",
        rs,
        || {
            let a = Math::query_leaf();
            let b = Math::query_leaf();
            let x = Math::query_leaf();
            let mul = MMul::query(&a, &b);
            let integ = MIntegral::query(&mul, &x);
            #[eggplant::pat_vars]
            struct Pat {
                a: Math,
                b: Math,
                x: Math,
                integ: MIntegral,
            }
            Pat::new(a, b, x, integ)
        },
        |ctx, pat| {
            let i_b = ctx.insert_m_integral(pat.b, pat.x);
            let a_i_b = ctx.insert_m_mul(pat.a, i_b);
            let dxa = ctx.insert_m_diff(pat.x, pat.a);
            let mul = ctx.insert_m_mul(dxa, i_b);
            let i2 = ctx.insert_m_integral(mul, pat.x);
            let rhs = ctx.insert_m_sub(a_i_b, i2);
            ctx.union(pat.integ, rhs);
        },
    );
}

pub fn run_and_collect_stats(breakdown: bool) -> MathMicrobenchmarkStats {
    let t_total = Instant::now();
    let seed_callback_stats = breakdown.then(|| Arc::new(Mutex::new(BTreeMap::new())));
    let rewrite_callback_stats = breakdown.then(|| Arc::new(Mutex::new(BTreeMap::new())));

    let t = Instant::now();
    MyTxMath::reset_for_bench();
    if breakdown {
        eprintln!(
            "[bench-breakdown] math-microbenchmark reset_for_bench: {:?}",
            t.elapsed()
        );
    }

    // Seed ground terms (ports `tests/math-microbenchmark.egg`).
    let t_seed_setup = Instant::now();
    let seed = MyTxMath::new_ruleset("math_microbenchmark_seed");
    MyTxMath::add_rule(
        "math_microbenchmark_seed",
        seed,
        || {
            #[eggplant::pat_vars_catch]
            struct Unit {}
        },
        |ctx, _pat| {
            let x = ctx.insert_m_var("x".to_owned());
            let y = ctx.insert_m_var("y".to_owned());
            let five = ctx.insert_m_var("five".to_owned());

            // (Integral (Ln (Var "x")) (Var "x"))
            let ln_x = ctx.insert_m_ln(x.clone());
            ctx.insert_m_integral(ln_x, x.clone());

            // (Integral (Add (Var "x") (Cos (Var "x"))) (Var "x"))
            let cos_x = ctx.insert_m_cos(x.clone());
            let add_x_cos_x = ctx.insert_m_add(x.clone(), cos_x);
            ctx.insert_m_integral(add_x_cos_x, x.clone());

            // (Integral (Mul (Cos (Var "x")) (Var "x")) (Var "x"))
            let cos_x = ctx.insert_m_cos(x.clone());
            let mul_cos_x_x = ctx.insert_m_mul(cos_x, x.clone());
            ctx.insert_m_integral(mul_cos_x_x, x.clone());

            // (Diff (Var "x") (Add (Const 1) (Mul (Const 2) (Var "x"))))
            let c1 = ctx.insert_m_const(1);
            let c2 = ctx.insert_m_const(2);
            let mul_2_x = ctx.insert_m_mul(c2, x.clone());
            let add_1_2x = ctx.insert_m_add(c1, mul_2_x);
            ctx.insert_m_diff(x.clone(), add_1_2x);

            // (Diff (Var "x") (Sub (Pow (Var "x") (Const 3))
            //                      (Mul (Const 7) (Pow (Var "x") (Const 2)))))
            let c3 = ctx.insert_m_const(3);
            let c7 = ctx.insert_m_const(7);
            let pow_x_3 = ctx.insert_m_pow(x.clone(), c3);
            let pow_x_2 = ctx.insert_m_pow(x.clone(), ctx.insert_m_const(2));
            let mul_7_pow = ctx.insert_m_mul(c7, pow_x_2);
            let sub_pow = ctx.insert_m_sub(pow_x_3, mul_7_pow);
            ctx.insert_m_diff(x.clone(), sub_pow);

            // (Add (Mul (Var "y") (Add (Var "x") (Var "y")))
            //      (Sub (Add (Var "x") (Const 2)) (Add (Var "x") (Var "x"))))
            let add_x_y = ctx.insert_m_add(x.clone(), y.clone());
            let mul_y_add = ctx.insert_m_mul(y.clone(), add_x_y);
            let add_x_2 = ctx.insert_m_add(x.clone(), ctx.insert_m_const(2));
            let add_x_x = ctx.insert_m_add(x.clone(), x.clone());
            let sub_add = ctx.insert_m_sub(add_x_2, add_x_x);
            ctx.insert_m_add(mul_y_add, sub_add);

            // (Div (Const 1) (Sub (Div (Add (Const 1) (Sqrt (Var "five"))) (Const 2))
            //                     (Div (Sub (Const 1) (Sqrt (Var "five"))) (Const 2))))
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
    let seed_report = MyTxMath::run_ruleset(seed, RunConfig::Once);
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
    let rs = MyTxMath::new_ruleset("math_microbenchmark_rules");
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
    let rewrite_report = MyTxMath::run_ruleset(rs, RunConfig::Times(11));
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

    let egraph = <MyTxMath as NonPatRecSgl>::egraph();
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
    let t_total = Instant::now();
    let seed_callback_stats = breakdown.then(|| Arc::new(Mutex::new(BTreeMap::new())));
    let rewrite_callback_stats = breakdown.then(|| Arc::new(Mutex::new(BTreeMap::new())));
    let max_rewrite_mem_bytes = max_rewrite_mem_gib
        .saturating_mul(1024)
        .saturating_mul(1024)
        .saturating_mul(1024);

    let t = Instant::now();
    MyTxMath::reset_for_bench();
    if breakdown {
        eprintln!(
            "[bench-breakdown] math-microbenchmark reset_for_bench: {:?}",
            t.elapsed()
        );
    }

    let t_seed_setup = Instant::now();
    let seed = MyTxMath::new_ruleset("math_microbenchmark_seed");
    MyTxMath::add_rule(
        "math_microbenchmark_seed",
        seed,
        || {
            #[eggplant::pat_vars_catch]
            struct Unit {}
        },
        |ctx, _pat| {
            let x = ctx.insert_m_var("x".to_owned());
            let y = ctx.insert_m_var("y".to_owned());
            let five = ctx.insert_m_var("five".to_owned());

            let ln_x = ctx.insert_m_ln(x.clone());
            ctx.insert_m_integral(ln_x, x.clone());

            let cos_x = ctx.insert_m_cos(x.clone());
            let add_x_cos_x = ctx.insert_m_add(x.clone(), cos_x);
            ctx.insert_m_integral(add_x_cos_x, x.clone());

            let cos_x = ctx.insert_m_cos(x.clone());
            let mul_cos_x_x = ctx.insert_m_mul(cos_x, x.clone());
            ctx.insert_m_integral(mul_cos_x_x, x.clone());

            let c1 = ctx.insert_m_const(1);
            let c2 = ctx.insert_m_const(2);
            let mul_2_x = ctx.insert_m_mul(c2, x.clone());
            let add_1_2x = ctx.insert_m_add(c1, mul_2_x);
            ctx.insert_m_diff(x.clone(), add_1_2x);

            let c3 = ctx.insert_m_const(3);
            let c7 = ctx.insert_m_const(7);
            let pow_x_3 = ctx.insert_m_pow(x.clone(), c3);
            let pow_x_2 = ctx.insert_m_pow(x.clone(), ctx.insert_m_const(2));
            let mul_7_pow = ctx.insert_m_mul(c7, pow_x_2);
            let sub_pow = ctx.insert_m_sub(pow_x_3, mul_7_pow);
            ctx.insert_m_diff(x.clone(), sub_pow);

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
    let seed_report = MyTxMath::run_ruleset(seed, RunConfig::Once);
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
    let rs = MyTxMath::new_ruleset("math_microbenchmark_rules");
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
        let report = MyTxMath::run_ruleset(rs, RunConfig::Once);
        rewrite_report.union(report);
        executed_rewrite_iters += 1;
        let current_peak = current_peak_memory_bytes();
        let tuple_count = {
            let egraph = <MyTxMath as NonPatRecSgl>::egraph();
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
        if current_peak > max_rewrite_mem_bytes {
            rewrite_stopped_early_due_to_memory_cap = true;
            eprintln!(
                "[math-microbenchmark] stopping early after iteration {} because peak memory {:.2} MiB exceeded configured cap {:.2} MiB",
                executed_rewrite_iters,
                current_peak as f64 / (1024.0 * 1024.0),
                max_rewrite_mem_bytes as f64 / (1024.0 * 1024.0),
            );
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

    let egraph = <MyTxMath as NonPatRecSgl>::egraph();
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
    let five = MVar::<MyTxMath>::new("five".to_owned());
    five.commit();
    let one_a = MConst::<MyTxMath>::new(1);
    one_a.commit();
    let one_b = MConst::<MyTxMath>::new(1);
    one_b.commit();
    let one_c = MConst::<MyTxMath>::new(1);
    one_c.commit();
    let two_a = MConst::<MyTxMath>::new(2);
    two_a.commit();
    let two_b = MConst::<MyTxMath>::new(2);
    two_b.commit();

    let sqrt_five_a = MSqrt::<MyTxMath>::new(&five);
    sqrt_five_a.commit();
    let add_1_sqrt = MAdd::<MyTxMath>::new(&one_a, &sqrt_five_a);
    add_1_sqrt.commit();
    let div_add = MDiv::<MyTxMath>::new(&add_1_sqrt, &two_a);
    div_add.commit();

    let sqrt_five_b = MSqrt::<MyTxMath>::new(&five);
    sqrt_five_b.commit();
    let sub_1_sqrt = MSub::<MyTxMath>::new(&one_b, &sqrt_five_b);
    sub_1_sqrt.commit();
    let div_sub = MDiv::<MyTxMath>::new(&sub_1_sqrt, &two_b);
    div_sub.commit();

    let denom = MSub::<MyTxMath>::new(&div_add, &div_sub);
    denom.commit();
    let root = MDiv::<MyTxMath>::new(&one_c, &denom);
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
    let (rendered, _cost) = MyTxMath::extract_node_to_string_with_backend(target, backend)
        .expect("math microbench extraction should succeed");
    let svg_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("target")
        .join("math_microbench_extract_svgs");
    fs::create_dir_all(&svg_dir).expect("svg output directory should be creatable");
    let svg_path = svg_dir.join(format!("{method}.svg"));
    let _ = MyTxMath::extract_node_to_svg_with_backend(
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
        extract_peak_memory_bytes: extract_peak_after.saturating_sub(extract_peak_before),
        elapsed: started.elapsed(),
        rendered,
        svg_path: svg_path.display().to_string(),
    }
}

#[cfg(feature = "rustsat-extract")]
pub fn run_extract_comparison_with_iters_and_mem_cap(
    rewrite_iters: usize,
    max_rewrite_mem_gib: u64,
) -> Vec<MathExtractComparisonRow> {
    let stats = run_and_collect_stats_iters_with_mem_cap(false, rewrite_iters, max_rewrite_mem_gib);
    let target = build_extract_target();

    let mut rows = Vec::new();

    let mut default_row = benchmark_extract_backend(
        &target,
        "default",
        stats.rewrite_peak_memory_bytes,
        ExtractBackend::cost_model(TreeAdditiveCostModel::default()),
    );
    default_row.requested_rewrite_iters = stats.requested_rewrite_iters;
    default_row.executed_rewrite_iters = stats.executed_rewrite_iters;
    default_row.max_rewrite_mem_gib = stats.max_rewrite_mem_gib;
    default_row.run_ruleset_note = if stats.rewrite_stopped_early_due_to_memory_cap {
        format!(
            "stopped early at {} / {} iterations because peak memory exceeded {} GiB",
            stats.executed_rewrite_iters, stats.requested_rewrite_iters, stats.max_rewrite_mem_gib
        )
    } else {
        "completed requested iterations".to_string()
    };
    rows.push(default_row);

    let mut eboost_row = benchmark_extract_backend(
        &target,
        "eboost",
        stats.rewrite_peak_memory_bytes,
        ExtractBackend::<TreeAdditiveCostModel>::eboost_heuristic(EBoostExtractConfig::default()),
    );
    eboost_row.requested_rewrite_iters = stats.requested_rewrite_iters;
    eboost_row.executed_rewrite_iters = stats.executed_rewrite_iters;
    eboost_row.max_rewrite_mem_gib = stats.max_rewrite_mem_gib;
    eboost_row.run_ruleset_note = if stats.rewrite_stopped_early_due_to_memory_cap {
        format!(
            "stopped early at {} / {} iterations because peak memory exceeded {} GiB",
            stats.executed_rewrite_iters, stats.requested_rewrite_iters, stats.max_rewrite_mem_gib
        )
    } else {
        "completed requested iterations".to_string()
    };
    rows.push(eboost_row);

    let mut layered_row = benchmark_extract_backend(
        &target,
        "layered",
        stats.rewrite_peak_memory_bytes,
        ExtractBackend::<TreeAdditiveCostModel>::eboost_layered(EBoostLayeredConfig::default()),
    );
    layered_row.requested_rewrite_iters = stats.requested_rewrite_iters;
    layered_row.executed_rewrite_iters = stats.executed_rewrite_iters;
    layered_row.max_rewrite_mem_gib = stats.max_rewrite_mem_gib;
    layered_row.run_ruleset_note = if stats.rewrite_stopped_early_due_to_memory_cap {
        format!(
            "stopped early at {} / {} iterations because peak memory exceeded {} GiB",
            stats.executed_rewrite_iters, stats.requested_rewrite_iters, stats.max_rewrite_mem_gib
        )
    } else {
        "completed requested iterations".to_string()
    };
    rows.push(layered_row);

    let mut rustsat_row = benchmark_extract_backend(
        &target,
        "rustsat",
        stats.rewrite_peak_memory_bytes,
        ExtractBackend::<TreeAdditiveCostModel>::rustsat(RustsatExtractConfig::default()),
    );
    rustsat_row.requested_rewrite_iters = stats.requested_rewrite_iters;
    rustsat_row.executed_rewrite_iters = stats.executed_rewrite_iters;
    rustsat_row.max_rewrite_mem_gib = stats.max_rewrite_mem_gib;
    rustsat_row.run_ruleset_note = if stats.rewrite_stopped_early_due_to_memory_cap {
        format!(
            "stopped early at {} / {} iterations because peak memory exceeded {} GiB",
            stats.executed_rewrite_iters, stats.requested_rewrite_iters, stats.max_rewrite_mem_gib
        )
    } else {
        "completed requested iterations".to_string()
    };
    rows.push(rustsat_row);

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

pub fn run_div_add_rewrite_smoke<CM>(rewrite_iters: usize, cost_model: CM) -> String
where
    CM: eggplant::egglog::extract::CostModel<eggplant::egglog::extract::DefaultCost> + 'static,
{
    MyTxMath::reset_for_bench();

    let one = MConst::<MyTxMath>::new(1);
    let x = MVar::<MyTxMath>::new("x".to_owned());
    let two = MConst::<MyTxMath>::new(2);
    let add = MAdd::<MyTxMath>::new(&one, &x);
    let root = MDiv::<MyTxMath>::new(&add, &two);
    root.commit();

    let rs = MyTxMath::new_ruleset("math_microbenchmark_div_add_smoke");
    {
        MyTxMath::add_rule(
            "div_add_distrib_smoke",
            rs,
            || {
                let a = Math::query_leaf();
                let b = Math::query_leaf();
                let c = Math::query_leaf();
                let add = MAdd::query(&a, &b);
                let div = MDiv::query(&add, &c);
                #[eggplant::pat_vars]
                struct Pat {
                    a: Math,
                    b: Math,
                    c: Math,
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
        MyTxMath::run_ruleset(rs, RunConfig::Once);
    }

    let (rendered, _) = MyTxMath::extract_node_to_string_with_cost_model(&root, cost_model)
        .expect("smoke extract should succeed");
    rendered
}
