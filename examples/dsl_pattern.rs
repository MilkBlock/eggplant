use eggplant::{prelude::*, tx_rx_vt_pr};

#[eggplant::ty]
enum Expr {
    Const { num: i64 },
    Mul { l: Expr, r: Expr },
}
#[eggplant::ty]
enum GraphRoot {
    Root { node: Expr },
}
tx_rx_vt_pr!(MyTx, MyPatRec);
// bind pattern recorder for MyTx
#[eggplant::pat_vars]
struct MulCommuVars {
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
            MulCommuVars::new(l, r, a)
        },
        |ctx, values| {
            println!("Commun values detected {:?}", values);
            let mul = ctx.insert_mul(values.l_expr, values.r_expr);
            ctx.union(mul, values.p_expr);
        },
    );

    MyTx::run_ruleset(ruleset, RunConfig::Once);
    MyTx::sgl().egraph_to_dot("egraph.dot".into());
}
