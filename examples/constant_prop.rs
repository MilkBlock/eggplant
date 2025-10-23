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
macro_rules! prop {
    ($ty:ident,$op:tt,$pat_name:ident,$ruleset:ident) => {
        MyTx::add_rule(
            stringify!($pat_name),
            $ruleset,
            || {
                let l = Const::query();
                let r = Const::query();
                let p = $ty::query(&l, &r);
                #[eggplant::pat_vars_catch]
                struct $pat_name {
                    l: Const,
                    r: Const,
                    p: $ty,
                }
            },
            |ctx, pat| {
                let cal = ctx.devalue(pat.l.num) $op ctx.devalue(pat.r.num);
                let op_value = ctx.insert_const(cal);
                ctx.union(pat.p, op_value);
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

    let c: Expr<MyTx, ConstTy> = Const::new(10);
    c.commit();
    if MyTx::canonical_raw(&expr) != MyTx::canonical_raw(&c) {
        panic!("should infer to 10");
    }

    expr.pull();
    MyTx::egraph_to_dot("egraph.dot");
    MyTx::wag_to_dot("wag.dot");
    // paterns to dot
    MyPatRec::sgl().pats_to_dot("pats.dot");
}
