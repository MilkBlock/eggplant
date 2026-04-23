use eggplant::prelude::*;
use eggplant::tx_rx_vt_pr;
use std::time::Instant;

#[eggplant::dsl]
enum Math {
    #[eggplant::typst("{f}'({x})")]
    #[eggplant::precedence(90)]
    MDiff { x: Math, f: Math },
    #[eggplant::typst("integral {f} quad d {x}")]
    #[eggplant::precedence(90)]
    MIntegral { f: Math, x: Math },

    #[eggplant::typst("{a} + {b}")]
    #[eggplant::precedence(50)]
    MAdd { a: Math, b: Math },
    #[eggplant::typst("{a} - {b}")]
    #[eggplant::precedence(50)]
    MSub { a: Math, b: Math },
    #[eggplant::typst("{a} * {b}")]
    #[eggplant::precedence(60)]
    MMul { a: Math, b: Math },
    #[eggplant::typst("frac({a}, {b}) ")]
    #[eggplant::precedence(60)]
    MDiv { a: Math, b: Math },
    #[eggplant::typst("{a}^{b}")]
    #[eggplant::precedence(80)]
    MPow { a: Math, b: Math },
    #[eggplant::typst("ln {a}")]
    #[eggplant::precedence(90)]
    MLn { a: Math },
    #[eggplant::typst("sqrt({a})")]
    #[eggplant::precedence(90)]
    MSqrt { a: Math },

    #[eggplant::typst("sin({a})")]
    #[eggplant::precedence(90)]
    MSin { a: Math },
    #[eggplant::typst("cos({a})")]
    #[eggplant::precedence(90)]
    MCos { a: Math },

    #[eggplant::typst("{n}")]
    #[eggplant::precedence(100)]
    MConst { n: i64 },
    #[eggplant::typst("{name}")]
    #[eggplant::precedence(100)]
    MVar { name: String },
}

tx_rx_vt_pr!(MyTxMath, MyPatRecMath);

#[eggplant::pat_vars]
struct AddCommPat<PR: PatRecSgl> {
    a: Math,
    b: Math,
    add: MAdd,
}

fn add_comm_pat<PR: PatRecSgl>() -> AddCommPat<PR> {
    let a = Math::query_leaf();
    let b = Math::query_leaf();
    let add = MAdd::query(&a, &b);
    AddCommPat::new(a, b, add)
}

#[eggplant::pat_vars]
struct MulCommPat<PR: PatRecSgl> {
    a: Math,
    b: Math,
    mul: MMul,
}

fn mul_comm_pat<PR: PatRecSgl>() -> MulCommPat<PR> {
    let a = Math::query_leaf();
    let b = Math::query_leaf();
    let mul = MMul::query(&a, &b);
    MulCommPat::new(a, b, mul)
}

#[eggplant::pat_vars]
struct AddAssocPat<PR: PatRecSgl> {
    a: Math,
    b: Math,
    c: Math,
    add_outer: MAdd,
}

fn add_assoc_pat<PR: PatRecSgl>() -> AddAssocPat<PR> {
    let a = Math::query_leaf();
    let b = Math::query_leaf();
    let c = Math::query_leaf();
    let add_inner = MAdd::query(&b, &c);
    let add_outer = MAdd::query(&a, &add_inner);
    AddAssocPat::new(a, b, c, add_outer)
}

#[eggplant::pat_vars]
struct MulAssocPat<PR: PatRecSgl> {
    a: Math,
    b: Math,
    c: Math,
    mul_outer: MMul,
}

fn mul_assoc_pat<PR: PatRecSgl>() -> MulAssocPat<PR> {
    let a = Math::query_leaf();
    let b = Math::query_leaf();
    let c = Math::query_leaf();
    let mul_inner = MMul::query(&b, &c);
    let mul_outer = MMul::query(&a, &mul_inner);
    MulAssocPat::new(a, b, c, mul_outer)
}

#[eggplant::pat_vars]
struct SubToAddNegPat<PR: PatRecSgl> {
    a: Math,
    b: Math,
    sub: MSub,
}

fn sub_to_add_neg_pat<PR: PatRecSgl>() -> SubToAddNegPat<PR> {
    let a = Math::query_leaf();
    let b = Math::query_leaf();
    let sub = MSub::query(&a, &b);
    SubToAddNegPat::new(a, b, sub)
}

#[eggplant::pat_vars]
struct AddZeroPat<PR: PatRecSgl> {
    a: Math,
    z: MConst,
    add: MAdd,
}

fn add_zero_pat<PR: PatRecSgl>() -> AddZeroPat<PR> {
    let a = Math::query_leaf();
    let z = MConst::query();
    let add = MAdd::query(&a, &z);
    let constraint = z.handle_n().eq(&0_i64);
    AddZeroPat::new(a, z, add).assert(constraint)
}

#[eggplant::pat_vars]
struct MulZeroPat<PR: PatRecSgl> {
    a: Math,
    z: MConst,
    mul: MMul,
}

fn mul_zero_pat<PR: PatRecSgl>() -> MulZeroPat<PR> {
    let a = Math::query_leaf();
    let z = MConst::query();
    let mul = MMul::query(&a, &z);
    let constraint = z.handle_n().eq(&0_i64);
    MulZeroPat::new(a, z, mul).assert(constraint)
}

#[eggplant::pat_vars]
struct MulOnePat<PR: PatRecSgl> {
    a: Math,
    o: MConst,
    mul: MMul,
}

fn mul_one_pat<PR: PatRecSgl>() -> MulOnePat<PR> {
    let a = Math::query_leaf();
    let o = MConst::query();
    let mul = MMul::query(&a, &o);
    let constraint = o.handle_n().eq(&1_i64);
    MulOnePat::new(a, o, mul).assert(constraint)
}

#[eggplant::pat_vars]
struct SubSelfZeroPat<PR: PatRecSgl> {
    a: Math,
    sub: MSub,
}

fn sub_self_zero_pat<PR: PatRecSgl>() -> SubSelfZeroPat<PR> {
    let a = Math::query_leaf();
    let b = Math::query_leaf();
    let sub = MSub::query(&a, &b);
    let constraint = a.handle().eq(&b.handle());
    SubSelfZeroPat::new(a, sub).assert(constraint)
}

#[eggplant::pat_vars]
struct MulDistribPat<PR: PatRecSgl> {
    a: Math,
    b: Math,
    c: Math,
    mul: MMul,
}

fn mul_distrib_pat<PR: PatRecSgl>() -> MulDistribPat<PR> {
    let a = Math::query_leaf();
    let b = Math::query_leaf();
    let c = Math::query_leaf();
    let add = MAdd::query(&b, &c);
    let mul = MMul::query(&a, &add);
    MulDistribPat::new(a, b, c, mul)
}

#[eggplant::pat_vars]
struct AddFactorPat<PR: PatRecSgl> {
    a: Math,
    b: Math,
    c: Math,
    add: MAdd,
}

fn add_factor_pat<PR: PatRecSgl>() -> AddFactorPat<PR> {
    let a = Math::query_leaf();
    let b = Math::query_leaf();
    let c = Math::query_leaf();
    let mul1 = MMul::query(&a, &b);
    let mul2 = MMul::query(&a, &c);
    let add = MAdd::query(&mul1, &mul2);
    AddFactorPat::new(a, b, c, add)
}

#[eggplant::pat_vars]
struct MulPowCombinePat<PR: PatRecSgl> {
    a: Math,
    b: Math,
    c: Math,
    mul: MMul,
}

fn mul_pow_combine_pat<PR: PatRecSgl>() -> MulPowCombinePat<PR> {
    let a = Math::query_leaf();
    let b = Math::query_leaf();
    let c = Math::query_leaf();
    let p1 = MPow::query(&a, &b);
    let p2 = MPow::query(&a, &c);
    let mul = MMul::query(&p1, &p2);
    MulPowCombinePat::new(a, b, c, mul)
}

#[eggplant::pat_vars]
struct PowOnePat<PR: PatRecSgl> {
    x: Math,
    o: MConst,
    pow: MPow,
}

fn pow_one_pat<PR: PatRecSgl>() -> PowOnePat<PR> {
    let x = Math::query_leaf();
    let o = MConst::query();
    let pow = MPow::query(&x, &o);
    let constraint = o.handle_n().eq(&1_i64);
    PowOnePat::new(x, o, pow).assert(constraint)
}

#[eggplant::pat_vars]
struct PowTwoPat<PR: PatRecSgl> {
    x: Math,
    t: MConst,
    pow: MPow,
}

fn pow_two_pat<PR: PatRecSgl>() -> PowTwoPat<PR> {
    let x = Math::query_leaf();
    let t = MConst::query();
    let pow = MPow::query(&x, &t);
    let constraint = t.handle_n().eq(&2_i64);
    PowTwoPat::new(x, t, pow).assert(constraint)
}

#[eggplant::pat_vars]
struct DiffAddPat<PR: PatRecSgl> {
    x: Math,
    a: Math,
    b: Math,
    diff: MDiff,
}

fn diff_add_pat<PR: PatRecSgl>() -> DiffAddPat<PR> {
    let x = Math::query_leaf();
    let a = Math::query_leaf();
    let b = Math::query_leaf();
    let add = MAdd::query(&a, &b);
    let diff = MDiff::query(&x, &add);
    DiffAddPat::new(x, a, b, diff)
}

#[eggplant::pat_vars]
struct DiffMulPat<PR: PatRecSgl> {
    x: Math,
    a: Math,
    b: Math,
    diff: MDiff,
}

fn diff_mul_pat<PR: PatRecSgl>() -> DiffMulPat<PR> {
    let x = Math::query_leaf();
    let a = Math::query_leaf();
    let b = Math::query_leaf();
    let mul = MMul::query(&a, &b);
    let diff = MDiff::query(&x, &mul);
    DiffMulPat::new(x, a, b, diff)
}

#[eggplant::pat_vars]
struct DiffSinPat<PR: PatRecSgl> {
    x: Math,
    diff: MDiff,
}

fn diff_sin_pat<PR: PatRecSgl>() -> DiffSinPat<PR> {
    let x = Math::query_leaf();
    let sin = MSin::query(&x);
    let diff = MDiff::query(&x, &sin);
    DiffSinPat::new(x, diff)
}

#[eggplant::pat_vars]
struct DiffCosPat<PR: PatRecSgl> {
    x: Math,
    diff: MDiff,
}

fn diff_cos_pat<PR: PatRecSgl>() -> DiffCosPat<PR> {
    let x = Math::query_leaf();
    let cos = MCos::query(&x);
    let diff = MDiff::query(&x, &cos);
    DiffCosPat::new(x, diff)
}

#[eggplant::pat_vars]
struct IntOnePat<PR: PatRecSgl> {
    x: Math,
    one: MConst,
    integ: MIntegral,
}

fn int_one_pat<PR: PatRecSgl>() -> IntOnePat<PR> {
    let x = Math::query_leaf();
    let one = MConst::query();
    let integ = MIntegral::query(&one, &x);
    let constraint = one.handle_n().eq(&1_i64);
    IntOnePat::new(x, one, integ).assert(constraint)
}

#[eggplant::pat_vars]
struct IntCosPat<PR: PatRecSgl> {
    x: Math,
    integ: MIntegral,
}

fn int_cos_pat<PR: PatRecSgl>() -> IntCosPat<PR> {
    let x = Math::query_leaf();
    let cos = MCos::query(&x);
    let integ = MIntegral::query(&cos, &x);
    IntCosPat::new(x, integ)
}

#[eggplant::pat_vars]
struct IntSinPat<PR: PatRecSgl> {
    x: Math,
    integ: MIntegral,
}

fn int_sin_pat<PR: PatRecSgl>() -> IntSinPat<PR> {
    let x = Math::query_leaf();
    let sin = MSin::query(&x);
    let integ = MIntegral::query(&sin, &x);
    IntSinPat::new(x, integ)
}

#[eggplant::pat_vars]
struct IntAddPat<PR: PatRecSgl> {
    f: Math,
    g: Math,
    x: Math,
    integ: MIntegral,
}

fn int_add_pat<PR: PatRecSgl>() -> IntAddPat<PR> {
    let f = Math::query_leaf();
    let g = Math::query_leaf();
    let x = Math::query_leaf();
    let add = MAdd::query(&f, &g);
    let integ = MIntegral::query(&add, &x);
    IntAddPat::new(f, g, x, integ)
}

#[eggplant::pat_vars]
struct IntSubPat<PR: PatRecSgl> {
    f: Math,
    g: Math,
    x: Math,
    integ: MIntegral,
}

fn int_sub_pat<PR: PatRecSgl>() -> IntSubPat<PR> {
    let f = Math::query_leaf();
    let g = Math::query_leaf();
    let x = Math::query_leaf();
    let sub = MSub::query(&f, &g);
    let integ = MIntegral::query(&sub, &x);
    IntSubPat::new(f, g, x, integ)
}

#[eggplant::pat_vars]
struct IntMulPat<PR: PatRecSgl> {
    a: Math,
    b: Math,
    x: Math,
    integ: MIntegral,
}

fn int_mul_pat<PR: PatRecSgl>() -> IntMulPat<PR> {
    let a = Math::query_leaf();
    let b = Math::query_leaf();
    let x = Math::query_leaf();
    let mul = MMul::query(&a, &b);
    let integ = MIntegral::query(&mul, &x);
    IntMulPat::new(a, b, x, integ)
}

pub fn bench() {
    let breakdown = std::env::var_os("EGGPLANT_BENCH_BREAKDOWN").is_some();
    let t_total = Instant::now();

    let t = Instant::now();
    MyTxMath::sgl().reset_for_bench();
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
    MyTxMath::run_ruleset(seed, RunConfig::Once);
    if breakdown {
        eprintln!(
            "[bench-breakdown] math-microbenchmark seed run_ruleset: {:?}",
            t_seed_run.elapsed()
        );
    }

    // Rewrite rules (ports `tests/math-microbenchmark.egg` rewrites).
    let t_rules_setup = Instant::now();
    let rs = MyTxMath::new_ruleset("math_microbenchmark_rules");

    MyTxMath::add_rule("add_comm", rs, add_comm_pat, |ctx, pat| {
        let rhs = ctx.insert_m_add(pat.b, pat.a);
        ctx.union(pat.add, rhs);
    });
    MyTxMath::add_rule("mul_comm", rs, mul_comm_pat, |ctx, pat| {
        let rhs = ctx.insert_m_mul(pat.b, pat.a);
        ctx.union(pat.mul, rhs);
    });
    MyTxMath::add_rule("add_assoc", rs, add_assoc_pat, |ctx, pat| {
        let ab = ctx.insert_m_add(pat.a, pat.b);
        let rhs = ctx.insert_m_add(ab, pat.c);
        ctx.union(pat.add_outer, rhs);
    });
    MyTxMath::add_rule("mul_assoc", rs, mul_assoc_pat, |ctx, pat| {
        let ab = ctx.insert_m_mul(pat.a, pat.b);
        let rhs = ctx.insert_m_mul(ab, pat.c);
        ctx.union(pat.mul_outer, rhs);
    });

    MyTxMath::add_rule("sub_to_add_neg", rs, sub_to_add_neg_pat, |ctx, pat| {
        let neg1 = ctx.insert_m_const(-1);
        let neg_b = ctx.insert_m_mul(neg1, pat.b);
        let rhs = ctx.insert_m_add(pat.a, neg_b);
        ctx.union(pat.sub, rhs);
    });

    MyTxMath::add_rule("add_zero", rs, add_zero_pat, |ctx, pat| {
        ctx.union(pat.add, pat.a);
    });
    MyTxMath::add_rule("mul_zero", rs, mul_zero_pat, |ctx, pat| {
        ctx.union(pat.mul, pat.z);
    });
    MyTxMath::add_rule("mul_one", rs, mul_one_pat, |ctx, pat| {
        ctx.union(pat.mul, pat.a);
    });

    MyTxMath::add_rule("sub_self_zero", rs, sub_self_zero_pat, |ctx, pat| {
        let z = ctx.insert_m_const(0);
        ctx.union(pat.sub, z);
    });

    MyTxMath::add_rule("mul_distrib", rs, mul_distrib_pat, |ctx, pat| {
        let ab = ctx.insert_m_mul(pat.a, pat.b);
        let ac = ctx.insert_m_mul(pat.a, pat.c);
        let rhs = ctx.insert_m_add(ab, ac);
        ctx.union(pat.mul, rhs);
    });
    MyTxMath::add_rule("add_factor", rs, add_factor_pat, |ctx, pat| {
        let bc = ctx.insert_m_add(pat.b, pat.c);
        let rhs = ctx.insert_m_mul(pat.a, bc);
        ctx.union(pat.add, rhs);
    });

    MyTxMath::add_rule("mul_pow_combine", rs, mul_pow_combine_pat, |ctx, pat| {
        let bc = ctx.insert_m_add(pat.b, pat.c);
        let rhs = ctx.insert_m_pow(pat.a, bc);
        ctx.union(pat.mul, rhs);
    });
    MyTxMath::add_rule("pow_one", rs, pow_one_pat, |ctx, pat| {
        ctx.union(pat.pow, pat.x);
    });
    MyTxMath::add_rule("pow_two", rs, pow_two_pat, |ctx, pat| {
        let rhs = ctx.insert_m_mul(pat.x, pat.x);
        ctx.union(pat.pow, rhs);
    });

    MyTxMath::add_rule("diff_add", rs, diff_add_pat, |ctx, pat| {
        let da = ctx.insert_m_diff(pat.x, pat.a);
        let db = ctx.insert_m_diff(pat.x, pat.b);
        let rhs = ctx.insert_m_add(da, db);
        ctx.union(pat.diff, rhs);
    });
    MyTxMath::add_rule("diff_mul", rs, diff_mul_pat, |ctx, pat| {
        let db = ctx.insert_m_diff(pat.x, pat.b);
        let da = ctx.insert_m_diff(pat.x, pat.a);
        let a_db = ctx.insert_m_mul(pat.a, db);
        let b_da = ctx.insert_m_mul(pat.b, da);
        let rhs = ctx.insert_m_add(a_db, b_da);
        ctx.union(pat.diff, rhs);
    });
    MyTxMath::add_rule("diff_sin", rs, diff_sin_pat, |ctx, pat| {
        let rhs = ctx.insert_m_cos(pat.x);
        ctx.union(pat.diff, rhs);
    });
    MyTxMath::add_rule("diff_cos", rs, diff_cos_pat, |ctx, pat| {
        let neg1 = ctx.insert_m_const(-1);
        let sin = ctx.insert_m_sin(pat.x);
        let rhs = ctx.insert_m_mul(neg1, sin);
        ctx.union(pat.diff, rhs);
    });

    MyTxMath::add_rule("int_one", rs, int_one_pat, |ctx, pat| {
        ctx.union(pat.integ, pat.x);
    });
    MyTxMath::add_rule("int_cos", rs, int_cos_pat, |ctx, pat| {
        let rhs = ctx.insert_m_sin(pat.x);
        ctx.union(pat.integ, rhs);
    });
    MyTxMath::add_rule("int_sin", rs, int_sin_pat, |ctx, pat| {
        let neg1 = ctx.insert_m_const(-1);
        let cos = ctx.insert_m_cos(pat.x);
        let rhs = ctx.insert_m_mul(neg1, cos);
        ctx.union(pat.integ, rhs);
    });
    MyTxMath::add_rule("int_add", rs, int_add_pat, |ctx, pat| {
        let i_f = ctx.insert_m_integral(pat.f, pat.x);
        let i_g = ctx.insert_m_integral(pat.g, pat.x);
        let rhs = ctx.insert_m_add(i_f, i_g);
        ctx.union(pat.integ, rhs);
    });
    MyTxMath::add_rule("int_sub", rs, int_sub_pat, |ctx, pat| {
        let i_f = ctx.insert_m_integral(pat.f, pat.x);
        let i_g = ctx.insert_m_integral(pat.g, pat.x);
        let rhs = ctx.insert_m_sub(i_f, i_g);
        ctx.union(pat.integ, rhs);
    });
    MyTxMath::add_rule("int_mul", rs, int_mul_pat, |ctx, pat| {
        let i_b = ctx.insert_m_integral(pat.b, pat.x);
        let a_i_b = ctx.insert_m_mul(pat.a, i_b);
        let dxa = ctx.insert_m_diff(pat.x, pat.a);
        let mul = ctx.insert_m_mul(dxa, i_b);
        let i2 = ctx.insert_m_integral(mul, pat.x);
        let rhs = ctx.insert_m_sub(a_i_b, i2);
        ctx.union(pat.integ, rhs);
    });

    if breakdown {
        eprintln!(
            "[bench-breakdown] math-microbenchmark rewrites add_rule: {:?}",
            t_rules_setup.elapsed()
        );
    }

    let t_rules_run = Instant::now();
    MyTxMath::run_ruleset(rs, RunConfig::Times(11));
    if breakdown {
        eprintln!(
            "[bench-breakdown] math-microbenchmark rewrites run_ruleset: {:?}",
            t_rules_run.elapsed()
        );
    }

    let t_serialize = Instant::now();
    let egraph = MyTxMath::egraph();
    let egraph = egraph.lock().unwrap();
    egraph.serialize(egglog::SerializeConfig::default());
    if breakdown {
        eprintln!(
            "[bench-breakdown] math-microbenchmark serialize: {:?}",
            t_serialize.elapsed()
        );
        eprintln!(
            "[bench-breakdown] math-microbenchmark total: {:?}",
            t_total.elapsed()
        );
    }
}
