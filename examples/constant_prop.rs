use eggplant::{Commit, PatRecSgl, RuleRunnerSgl, RunConfig, SingletonGetter, tx_rx_vt_pr};
#[eggplant::ty]
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
        #[eggplant::pat_vars]
        struct $pat_name<PR: PatRecSgl> {
            l: Const<PR>,
            r: Const<PR>,
            p: $ty<PR>,
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
            |ctx: &mut eggplant::RuleCtx<'_, '_, '_>, values| {
                let cal = ctx.devalue(values.l.num) $op ctx.devalue(values.r.num);
                let mul_value = ctx.insert_const(cal);
                ctx.union(values.p.itself, mul_value);
            },
        );
    };
}
fn main() {
    let expr: Expr<MyTx, AddTy> =
        Add::new(&Mul::new(&Const::new(3), &Const::new(2)), &Const::new(4));
    expr.commit();

    let ruleset = MyTx::new_ruleset("constant_prop");
    prop!(Add,+,AddPat,ruleset);
    prop!(Sub,-,SubPat,ruleset);
    prop!(Mul,*,MulPat,ruleset);
    prop!(Div,/,DivPat,ruleset);
    for _ in 0..3 {
        let rst = MyTx::run_ruleset(ruleset, RunConfig::None);
        println!("{:?}", rst);
    }
    MyTx::sgl().egraph_to_dot("egraph.dot".into());
}
