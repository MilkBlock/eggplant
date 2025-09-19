use eggplant::prelude::*;
use eggplant::tx_rx_vt_pr_fp;
use serde::{Deserialize, Serialize};

#[eggplant::dsl(base=Op)]
pub enum Expr {
    Const { num: i64 },
    Mul { l: Expr, r: Expr },
    Sub { l: Expr, r: Expr },
    Add { l: Expr, r: Expr },
    Div { l: Expr, r: Expr },
    Binary { op: Op, l: Expr, r: Expr },
}

#[eggplant::base_ty]
#[derive(Serialize, Deserialize, Debug, Clone, Hash, PartialEq, Eq)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Unknown,
}

tx_rx_vt_pr_fp!(MyTx, MyPatRec);
fn main() {
    env_logger::init();
    let expr: Expr<MyTx, _> = Binary::new(Op::Mul, &Const::new(3), &Const::new(2));
    expr.commit();

    #[eggplant::pat_vars]
    struct SubPat {
        l: Expr,
        r: Expr,
        p: Binary,
    }
    let ruleset = MyTx::new_ruleset("sss");
    MyTx::add_rule(
        stringify!(SubPat),
        ruleset,
        || {
            let l = Expr::query_leaf();
            let r = Expr::query_leaf();
            let b = Binary::query(&l, &r);
            SubPat::new(l, r, b)
        },
        |ctx, pat| {
            let s = ctx.devalue(pat.p.op);
            match s {
                Op::Add => todo!(),
                Op::Sub => todo!(),
                Op::Mul => {
                    let mul = ctx.insert_mul(pat.l, pat.r);
                    ctx.union(mul, pat.p);
                }
                Op::Div => todo!(),
                Op::Unknown => todo!(),
            }
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
