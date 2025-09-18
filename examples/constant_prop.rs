use eggplant::prelude::*;
use eggplant::tx_rx_vt_pr_pf;
#[eggplant::dsl]
pub enum Expr {
    Const { num: i64 },
    Mul { l: Expr, r: Expr },
    Sub { l: Expr, r: Expr },
    Add { l: Expr, r: Expr },
    Div { l: Expr, r: Expr },
    VecSum { exprs: VecExpr },
}
#[eggplant::dsl]
struct VecExpr {
    exprs: Vec<Expr>,
}

tx_rx_vt_pr_pf!(MyTx, MyPatRec);
macro_rules! prop {
    ($ty:ident,$op:tt,$pat_name:ident,$ruleset:ident) => {
        #[eggplant::pat_vars]
        struct $pat_name {
            l: Const,
            r: Const,
            p: $ty,
        }
        MyTx::add_rule(
            stringify!($pat_name),
            $ruleset,
            || {
                let l = Const::query();
                let r = Const::query();
                let p = $ty::query(&l, &r);
                $pat_name::new(l, r, p)
            },
            |ctx, values| {
                let cal = ctx.devalue(values.l.num) $op ctx.devalue(values.r.num);
                let op_value = ctx.insert_const(cal);
                ctx.union(values.p, op_value);
            },
        );
    };
}
fn main() {
    env_logger::init();
    let expr: Expr<MyTx, _> = Add::new(&Mul::new(&Const::new(3), &Const::new(2)), &Const::new(4));
    expr.commit();

    let ruleset = MyTx::new_ruleset("constant_prop");
    prop!(Add,+,AddPat,ruleset);
    prop!(Sub,-,SubPat,ruleset);
    prop!(Mul,*,MulPat,ruleset);
    prop!(Div,/,DivPat,ruleset);
    let report = MyTx::run_ruleset(ruleset, RunConfig::Sat);
    println!("{:#?}", report);
    MyTx::table_view();
    MyTx::explain_eq_raw(6, 4);
    MyTx::explain_raw(5);

    expr.pull();
    MyTx::egraph_to_dot("egraph.dot".into());
    MyTx::wag_to_dot("wag.dot".into());
    // paterns to dot
    MyPatRec::sgl().pats_to_dot("pats.dot".into());
}
