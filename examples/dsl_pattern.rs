use eggplant::{prelude::*, tx_rx_vt_pr};

#[eggplant::dsl]
enum Expr {
    Const { num: i64 },
    Mul { l: Expr, r: Expr },
}
#[eggplant::dsl]
enum GraphRoot {
    Root { node: Expr },
}
tx_rx_vt_pr!(MyTx, MyPatRec);
// bind pattern recorder for MyTx
#[eggplant::pat_vars]
struct MulCommuPat {
    l_expr: Expr,
    r_expr: Expr,
    p_expr: Mul,
}

fn main() {
    env_logger::init();
    let root = Root::<MyTx>::new(&Mul::new(&Const::new(3), &Const::new(5)));
    root.commit();

    let ruleset = MyTx::new_ruleset("my_rule_set");
    MyTx::add_rule(
        "this rule",
        ruleset,
        || {
            let l = Expr::query_leaf();
            let r = Expr::query_leaf();
            let a = Mul::query(&l, &r);
            MulCommuPat::new(l, r, a)
        },
        |ctx, pat| {
            println!("Commun values detected {:?}", pat);
            let mul = ctx.insert_mul(pat.l_expr, pat.r_expr);
            ctx.union(mul, pat.p_expr);
        },
    );

    MyTx::run_ruleset(ruleset, RunConfig::Once);
    MyTx::egraph_to_dot("egraph.dot");
}
