use eggplant::{prelude::*, tx_rx_vt_pr};
#[eggplant::dsl]
enum Expr {
    Add {
        l: Expr,
        r: Expr,
    },
    Mul {
        l: Expr,
        r: Expr,
    },
    Neg {
        e: Expr,
    },
    Var {
        val: f64,
    },
    /// -xy + r
    Fnamadd {
        x: Expr,
        y: Expr,
        r: Expr,
    },
}
tx_rx_vt_pr!(MyTx, MyPatRec);

fn main() {
    env_logger::init();
    let expr: Expr<MyTx, _> = Add::new(
        &Neg::new(&Mul::new(&Var::new(3.0), &Var::new(4.0))),
        &Var::new(5.0),
    );
    let ruleset = MyTx::new_ruleset("intrinsic_recognize");
    MyTx::add_rule(
        "fnamadd rule",
        ruleset,
        || {
            let l = Var::query();
            let r = Var::query();
            let product = Mul::query(&l, &r);
            let neg = Neg::query(&product);
            let added = Var::query();
            let root = Add::query(&neg, &added);
            #[pat_vars]
            struct Pat {
                neg: Neg,
                l: Var,
                r: Var,
                added: Var,
                root: Add,
            }
            Pat::new(neg, l, r, added, root)
        },
        |ctx, pat| {
            println!("Fnamadd values detected {:#?}", pat);
            let fnamadd = ctx.insert_fnamadd(pat.l, pat.r, pat.added);
            ctx.union(fnamadd, pat.root);
        },
    );
    expr.commit();
    MyTx::run_ruleset(ruleset, RunConfig::Sat);
    MyTx::egraph_to_dot("egraph.dot");
}
