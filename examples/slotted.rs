use eggplant::prelude::*;
use eggplant::tx_rx_vt_pr_fp;
#[eggplant::dsl]
pub enum Expr {
    Const { num: i64 },
    Var {},
    Mul { l: Expr, r: Expr },
    Sub { l: Expr, r: Expr },
    Add { l: Expr, r: Expr },
    Div { l: Expr, r: Expr },
}

tx_rx_vt_pr_fp!(MyTx, MyPatRec);

fn main() {
    env_logger::init();

    // let expr: Expr<MyTx, MulTy> = Mul::new(
    //     &Sub::new(&Var::new(), &Var::new()),
    //     &Sub::new(&Var::new(), &Var::new()),
    // );
    let expr: Expr<MyTx, _> = Add::new(
        &Add::new(&Var::new(), &Var::new()),
        &Add::new(&Var::new(), &Var::new()),
    );
    let r = MyTx::new_ruleset("r");

    #[eggplant::pat_vars]
    struct AddPat {
        l_l: Expr,
        l_r: Expr,
        r: Expr,
        l: Add,
        p: Add,
    }
    MyTx::add_rule(
        stringify!(AddPat),
        r,
        || {
            let l_l = Expr::query_leaf();
            let l_r = Expr::query_leaf();
            let r = Expr::query_leaf();
            let l = Add::query(&l_l, &l_r);
            let p = Add::query(&l, &r);
            AddPat::new(l_l, l_r, r, l, p)
        },
        |ctx, pat| {
            let r = ctx.insert_add(pat.l_r, pat.r);
            let p = ctx.insert_add(pat.l_l, r);
            ctx.union(p, pat.p);
        },
    );

    expr.commit();

    // println!("{:#?}", report);
    MyTx::table_view();

    MyTx::run_ruleset(r, RunConfig::Sat);
    expr.pull();
    MyTx::egraph_to_dot("egraph.dot");
    MyTx::wag_to_dot("wag.dot");
    // paterns to dot
    MyPatRec::sgl().pats_to_dot("pats.dot");
}

// let ruleset = MyTx::new_ruleset("constant_prop");
// let report = MyTx::run_ruleset(ruleset, RunConfig::Sat);
// #[eggplant::pat_vars]
// struct AddPat {
//     l: Const,
//     r: Const,
//     p: Add,
// }
// MyTx::add_rule(
//     stringify!(AddPat),
//     ruleset,
//     || {
//         let l = Const::query();
//         let r = Const::query();
//         let p = Add::query(&l, &r);
//         let l_h_eq_r_h = l.handle().eq(&r.handle());
//         AddPat::new(l, r, p).assert(l_h_eq_r_h)
//     },
//     |ctx, pat| {
//         let cal = ctx.devalue(pat.l.num) + ctx.devalue(pat.r.num);
//         let op_value = ctx.insert_const(cal);
//         ctx.union(pat.p, op_value);
//     },
// );
// #[eggplant::pat_vars]
// struct MulPat {
//     l: Const,
//     r: Const,
//     p: Mul,
// }
// MyTx::add_rule(
//     stringify!(MulPat),
//     ruleset,
//     || {
//         let l = Const::query();
//         let r = Const::query();
//         let p = Mul::query(&l, &r);
//         let l_r_eq = { l.handle().eq(&r.handle()) };
//         MulPat::new(l, r, p).assert(l_r_eq)
//     },
//     |ctx, pat| {
//         let cal = ctx.devalue(pat.l.num) * ctx.devalue(pat.r.num);
//         let op_value = ctx.insert_const(cal);
//         ctx.union(pat.p, op_value);
//     },
// );
