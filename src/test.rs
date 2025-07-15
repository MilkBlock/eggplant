#[cfg(test)]
mod tests {
    use crate::{self as eggplant, RuleRunnerSgl, tx_rx_vt_pr};
    use eggplant::{Commit, PH, PatRecSgl, RunConfig};
    use std::sync::{Arc, Mutex};

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

    #[test]
    fn pattern_test() {
        env_logger::init();
        let root = Root::<MyTx>::new(&Const::new(3));
        root.commit();

        let ruleset = MyTx::new_rule_set("my_rule_set");
        let executed = Arc::new(Mutex::new(false));
        let cloned_flag = executed.clone();
        MyTx::add_rule(ruleset, my_pat, move |_ctx, my_pattern_vars| {
            println!("{:?}", my_pattern_vars.expr);
            let mut locked = cloned_flag.lock().unwrap();
            *locked = true;
            Some(())
        });
        MyTx::run_ruleset(ruleset, RunConfig::None);
        assert_eq!(*executed.lock().unwrap(), true);
    }
}
