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
#[pat_vars]
struct FnamaddPat {
    neg_product: NegProdPat,
    added: Var,
    root: Add,
}
fn fnamadd_pat<PR: PatRecSgl>() -> FnamaddPat<PR> {
    let neg_product = neg_product_pat();
    let r = Var::query();
    let root = Add::query(&neg_product.neg, &r);
    FnamaddPat::new(neg_product, r, root)
}
#[pat_vars]
struct NegProdPat {
    neg: Neg,
    l: Var,
    r: Var,
}
fn neg_product_pat<PR: PatRecSgl>() -> NegProdPat<PR> {
    let l = Var::query();
    let r = Var::query();
    let product = Mul::query(&l, &r);
    let neg = Neg::query(&product);
    NegProdPat::new(neg, l, r)
}

fn main() {
    env_logger::init();
    let expr: Expr<MyTx, _> = Add::new(
        &Neg::new(&Mul::new(&Var::new(3.0), &Var::new(4.0))),
        &Var::new(5.0),
    );
    let ruleset = MyTx::new_ruleset("intrinsic_recognize");
    MyTx::add_rule("fnamadd rule", ruleset, fnamadd_pat, |ctx, pat| {
        println!("Fnamadd values detected {:#?}", pat);
        let fnamadd = ctx.insert_fnamadd(pat.neg_product.l, pat.neg_product.r, pat.added);
        ctx.union(fnamadd, pat.root);
    });
    expr.commit();
    MyTx::run_ruleset(ruleset, RunConfig::Sat);
    MyTx::egraph_to_dot("egraph.dot".into());
}
