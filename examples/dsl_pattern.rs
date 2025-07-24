use eggplant::{Commit, PH, PatRecSgl, RuleRunnerSgl, RunConfig, SingletonGetter, tx_rx_vt_pr};

#[eggplant::ty]
enum Expr {
    Const { num: i64 },
}
#[eggplant::ty]
enum GraphRoot {
    Root { node: Expr },
}
tx_rx_vt_pr!(MyTx, MyPatRec);
// bind pattern recorder for MyTx

#[eggplant::patttern_vars]
struct MyPatternVars<PS: PatRecSgl> {
    #[allow(unused)]
    expr: PH<Expr<PS>>,
}
fn my_pat<PS: PatRecSgl>() -> MyPatternVars<PS> {
    let expr_var = Expr::new_ph();
    let _root = GraphRoot::new(&expr_var);
    MyPatternVars::new(expr_var)
}

fn main() {
    env_logger::init();
    let root = Root::<MyTx>::new(&Const::new(3));
    root.commit();

    let ruleset = MyTx::new_rule_set("my_rule_set");
    MyTx::add_rule(ruleset, my_pat, |ctx, values| {
        println!("hello here is a Node Root pair {:?}", values);
        let con_node = ctx.insert_const(5);
        ctx.insert_root(con_node);
        // rule_ctx.insert(table, row);
        // _ctx.rule_ctx.insert(table, row);
        // _ctx.rule_ctx.lookup(table, key)
        Some(())
    });
    MyTx::run_ruleset(ruleset, RunConfig::None);
    MyTx::sgl().egraph_to_dot("egraph.dot".into());
}
