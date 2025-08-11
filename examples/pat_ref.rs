use eggplant::{prelude::*, tx_rx_vt_pr};
#[eggplant::ty]
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
#[pat_vars]
struct FnamaddVars {
    neg_product: NegProduct,
    added: Var,
    root: Add,
}
fn fnamadd_pat<PR: PatRecSgl>() -> FnamaddVars<PR> {
    let neg_product = neg_product_pat();
    let r = Var::query();
    let root = Add::query(&neg_product.neg, &r);
    FnamaddVars::new(neg_product, r, root)
}
#[pat_vars]
struct NegProduct {
    neg: Neg,
    l: Var,
    r: Var,
}
fn neg_product_pat<PR: PatRecSgl>() -> NegProduct<PR> {
    let l = Var::query();
    let r = Var::query();
    let product = Mul::query(&l, &r);
    let neg = Neg::query(&product);
    NegProduct::new(neg, l, r)
}

fn main() {
    env_logger::init();
    let expr: Expr<MyTx, _> = Add::new(
        &Neg::new(&Mul::new(&Var::new(3.0), &Var::new(4.0))),
        &Var::new(5.0),
    );
    let ruleset = MyTx::new_ruleset("intrinsic_recognize");
    MyTx::add_rule("fnamadd rule", ruleset, fnamadd_pat, |ctx, values| {
        println!("Fnamadd values detected {:?}", values);
        let fnamadd = ctx.insert_fnamadd(values.neg_product.l, values.neg_product.r, values.added);
        ctx.union(fnamadd, values.root);
    });
    expr.commit();
    for _ in 1..10 {
        MyTx::run_ruleset(ruleset, RunConfig::None);
    }
    MyTx::sgl().egraph_to_dot("egraph.dot".into());
}
