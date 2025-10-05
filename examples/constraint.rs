use eggplant::prelude::*;
use eggplant::tx_rx_vt_pr_fp;
use eggplant::wrap::IntoConstraintAtoms;
use eggplant::wrap::PEq;
#[eggplant::dsl]
pub enum Expr {
    Const { num: i64 },
    Mul { l: Expr, r: Expr },
    Sub { l: Expr, r: Expr },
    Add { l: Expr, r: Expr },
    Div { l: Expr, r: Expr },
}

tx_rx_vt_pr_fp!(MyTx, MyPatRec);

#[eggplant::pat_vars]
struct AddPat {
    l: Const,
    r: Const,
    p: Add,
}
impl<PR: PatRecSgl> AddPat<PR> {
    fn assert(self, constraint: impl IntoConstraintAtoms) -> Self {
        PR::on_new_constraint(constraint);
        self
    }
}

fn main() {
    let ruleset = MyTx::new_ruleset("constant_prop");
    MyTx::add_rule(
        stringify!(AddPat),
        ruleset,
        || {
            let l = Const::query();
            let r = Const::query();
            let p = Add::query(&l, &r);
            let num = l.handle_num();
            AddPat::new(l, r, p).assert(num.eq(3)).assert(num.eq(&num))
        },
        |ctx, pat| {
            let cal = ctx.devalue(pat.l.num) + ctx.devalue(pat.r.num);
            let op_value = ctx.insert_const(cal);
            ctx.union(pat.p, op_value);
        },
    )
}
