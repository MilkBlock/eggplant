use eggplant::prelude::*;
use eggplant::tx_rx_vt_pr;
#[eggplant::dsl]
pub enum Expr {
    Const { num: i64 },
    Mul { l: Expr, r: Expr },
    Sub { l: Expr, r: Expr },
    Add { l: Expr, r: Expr },
    Div { l: Expr, r: Expr },
}

tx_rx_vt_pr!(MyTx, MyPatRec);

#[eggplant::pat_vars]
struct AddPat {
    l: Const,
    r: Const,
    p: Add,
}

fn main() {
    let ruleset = MyTx::new_ruleset("constant_prop");
    let expr: Expr<MyTx, _> = Add::new(&Add::new(&Const::new(3), &Const::new(2)), &Const::new(4));
    expr.commit();
    MyTx::add_rule(
        stringify!(AddPat),
        ruleset,
        || {
            let l = Const::query().num(&3);
            let r = Const::query();
            let p = Add::query(&l, &r);
            AddPat::new(l, r, p)
        },
        |ctx, pat| {
            let cal = ctx.devalue(pat.l.num) + ctx.devalue(pat.r.num);
            let op_value = ctx.insert_const(cal);
            ctx.union(pat.p, op_value);
        },
    );
    MyTx::run_ruleset(ruleset, RunConfig::Sat);
    MyTx::egraph_to_dot("egraph.dot");
}
