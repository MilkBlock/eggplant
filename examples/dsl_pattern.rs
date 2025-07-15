use eggplant::{
    Commit, PH, PatRecSgl, RuleRunnerSgl, RunConfig, SingletonGetter, WithPatRecSgl,
    basic_patttern_recorder, basic_tx_rx_vt_pr, tx_rx_vt_pr,
};

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
    expr: PH<Expr<PS>>,
}
fn my_pat<PR: PatRecSgl>() -> MyPatternVars<PR> {
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
        Some(())
    });
    MyTx::run_ruleset(ruleset, RunConfig::None);
    MyTx::sgl().egraph_to_dot("egraph.dot".into());
}
