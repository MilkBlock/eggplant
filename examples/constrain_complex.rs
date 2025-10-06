use eggplant::prelude::*;
use eggplant::tx_rx_vt_pr_fp;
#[eggplant::dsl]
pub enum Expr {
    Const { num: i64 },
    Mul { l: Expr, r: Expr },
    Sub { l: Expr, r: Expr },
    Add { l: Expr, r: Expr },
    Div { l: Expr, r: Expr },
}

tx_rx_vt_pr_fp!(MyTx, MyPatRec);
fn main() {
    env_logger::init();
    let expr: Expr<MyTx, _> = Add::new(&Mul::new(&Const::new(3), &Const::new(3)), &Const::new(3));
    expr.commit();

    let ruleset = MyTx::new_ruleset("constant_prop");
    #[eggplant::pat_vars]
    struct AddPat {
        l: Const,
        r: Const,
        p: Add,
    }
    MyTx::add_rule(
        stringify!(AddPat),
        ruleset,
        || {
            let l = Const::query();
            let r = Const::query();
            let p = Add::query(&l, &r);
            let l_h = l.handle();
            let r_h = r.handle();
            AddPat::new(l, r, p).assert(l_h.eq(&r_h))
        },
        |ctx, pat| {
            let cal = ctx.devalue(pat.l.num) + ctx.devalue(pat.r.num);
            let op_value = ctx.insert_const(cal);
            ctx.union(pat.p, op_value);
        },
    );
    #[eggplant::pat_vars]
    struct MulPat {
        l: Const,
        r: Const,
        p: Mul,
    }
    MyTx::add_rule(
        stringify!(MulPat),
        ruleset,
        || {
            let l = Const::query();
            let r = Const::query();
            let p = Mul::query(&l, &r);
            let l_h = l.handle();
            let r_h = r.handle();
            MulPat::new(l, r, p).assert(l_h.eq(&r_h))
        },
        |ctx, pat| {
            let cal = ctx.devalue(pat.l.num) * ctx.devalue(pat.r.num);
            let op_value = ctx.insert_const(cal);
            ctx.union(pat.p, op_value);
        },
    );
    let report = MyTx::run_ruleset(ruleset, RunConfig::Sat);
    println!("{:#?}", report);
    MyTx::table_view();
    // MyTx::explain_eq_raw(6, 4);
    // MyTx::explain_raw(5);

    expr.pull();
    MyTx::egraph_to_dot("egraph.dot".into());
    MyTx::wag_to_dot("wag.dot".into());
    // paterns to dot
    MyPatRec::sgl().pats_to_dot("pats.dot".into());
}
