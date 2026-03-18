use eggplant::prelude::*;
use eggplant::tx_rx_vt_pr;

#[eggplant::dsl]
enum Expr {
    Mul { l: Expr, r: Expr },
    Var { name: String },
    Lit { n: i64 },
}

tx_rx_vt_pr!(MyTxUnify, MyPatRecUnify);

#[eggplant::pat_vars]
struct MulInjPat<PR: PatRecSgl> {
    a: Expr,
    b: Expr,
    c: Expr,
    d: Expr,
    _m1: Mul,
    _m2: Mul,
}

fn mul_inj_pat<PR: PatRecSgl>() -> MulInjPat<PR> {
    let a = Expr::query_leaf();
    let b = Expr::query_leaf();
    let c = Expr::query_leaf();
    let d = Expr::query_leaf();
    let m1 = Mul::query(&a, &b);
    let m2 = Mul::query(&c, &d);
    let constraint = m1.handle().eq(&m2.handle());
    MulInjPat::new(a, b, c, d, m1, m2).assert(constraint)
}

#[eggplant::pat_vars]
struct LitEqMulPat<PR: PatRecSgl> {
    a: Expr,
    b: Expr,
    _lit: Lit,
    _mul: Mul,
}

fn lit_eq_mul_pat<PR: PatRecSgl>() -> LitEqMulPat<PR> {
    let a = Expr::query_leaf();
    let b = Expr::query_leaf();
    let lit = Lit::query();
    let mul = Mul::query(&a, &b);
    let constraint = lit.handle().eq(&mul.handle());
    LitEqMulPat::new(a, b, lit, mul).assert(constraint)
}

pub fn bench() {
    MyTxUnify::sgl().reset_for_bench();

    let seed = MyTxUnify::new_ruleset("unify_seed");
    MyTxUnify::add_rule(
        "unify_seed",
        seed,
        || {
            #[eggplant::pat_vars_catch]
            struct Unit {}
        },
        |ctx, _pat| {
            // Matches:
            // (union (Mul (Var "a") (Var "a")) (Mul (Lit 1) (Lit 2)))
            let a = ctx.insert_var("a".to_owned());
            let mul_aa = ctx.insert_mul(a.clone(), a);
            let mul_12 = ctx.insert_mul(ctx.insert_lit(1), ctx.insert_lit(2));
            ctx.union(mul_aa, mul_12);
        },
    );

    let step = MyTxUnify::new_ruleset("unify_step");
    MyTxUnify::add_rule("mul_injective", step, mul_inj_pat, |ctx, pat| {
        ctx.union(pat.a, pat.c);
        ctx.union(pat.b, pat.d);
    });
    MyTxUnify::add_rule("lit_eq_mul_panic", step, lit_eq_mul_pat, |_ctx, _pat| {
        panic!("Literal cannot be equal to a product");
    });

    MyTxUnify::run_ruleset(seed, RunConfig::Once);
    MyTxUnify::run_ruleset(step, RunConfig::Times(3));

    let var_a: Expr<MyTxUnify, _> = Var::new("a".to_owned());
    var_a.commit();
    let lit1: Expr<MyTxUnify, _> = Lit::new(1);
    lit1.commit();
    let lit2: Expr<MyTxUnify, _> = Lit::new(2);
    lit2.commit();

    assert_eq!(
        MyTxUnify::canonical_raw(&var_a),
        MyTxUnify::canonical_raw(&lit1)
    );
    assert_eq!(
        MyTxUnify::canonical_raw(&lit2),
        MyTxUnify::canonical_raw(&lit1)
    );

    let egraph = MyTxUnify::egraph();
    let mut egraph = egraph.lock().unwrap();
    egraph.serialize(egglog::SerializeConfig::default());
}
