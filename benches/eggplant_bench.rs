use eggplant::prelude::*;
use eggplant::tx_rx_vt_pr;

#[eggplant::dsl]
pub enum Expr {
    #[eggplant::typst("{num}")]
    #[eggplant::precedence(100)]
    Const { num: i64 },
    #[eggplant::typst("{l} * {r}")]
    #[eggplant::precedence(60)]
    Mul { l: Expr, r: Expr },
    #[eggplant::typst("{l} - {r}")]
    #[eggplant::precedence(50)]
    Sub { l: Expr, r: Expr },
    #[eggplant::typst("{l} + {r}")]
    #[eggplant::precedence(50)]
    Add { l: Expr, r: Expr },
    #[eggplant::typst("frac({l}, {r})")]
    #[eggplant::precedence(60)]
    Div { l: Expr, r: Expr },
}

tx_rx_vt_pr!(BenchTx, BenchPatRec);

macro_rules! prop {
    ($ty:ident,$op:tt,$pat_name:ident,$ruleset:ident) => {
        BenchTx::add_rule(
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

#[divan::bench]
fn constant_propagation() {
    let expr: Expr<BenchTx, _> =
        Add::new(&Mul::new(&Const::new(3), &Const::new(2)), &Const::new(4));
    expr.commit();

    let ruleset = BenchTx::new_ruleset("constant_prop");
    prop!(Add, +, AddPat, ruleset);
    prop!(Sub, -, SubPat, ruleset);
    prop!(Mul, *, MulPat, ruleset);
    prop!(Div, /, DivPat, ruleset);
    BenchTx::run_ruleset(ruleset, RunConfig::Sat);
}

#[divan::bench]
fn dsl_insert_and_commit() {
    let expr: Expr<BenchTx, _> = Add::new(
        &Mul::new(&Const::new(3), &Const::new(2)),
        &Sub::new(&Const::new(10), &Const::new(4)),
    );
    expr.commit();
}

fn main() {
    divan::main();
}
