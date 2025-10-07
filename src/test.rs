#[cfg(test)]
mod tests {
    use crate::{self as eggplant, tx_rx_vt_pr};
    use eggplant::prelude::*;
    use std::sync::{Arc, Mutex};

    #[eggplant::dsl]
    enum Expr {
        #[cost(3)]
        Const { num: i64 },
    }
    #[eggplant::dsl]
    enum GraphRoot {
        Root { node: Expr },
    }
    tx_rx_vt_pr!(MyTx, MyPatRec);
    // bind pattern recorder for MyTx

    #[eggplant::pat_vars]
    struct MyPatternVars<PR: PatRecSgl> {
        expr: Expr<PR>,
    }
    fn my_pat<PR: PatRecSgl>() -> MyPatternVars<PR> {
        let expr_var = Expr::query_leaf();
        let _root = GraphRoot::query(&expr_var);
        MyPatternVars::new(expr_var)
    }

    #[test]
    fn pattern_test() {
        env_logger::init();
        let root = Root::<MyTx>::new(&Const::new(3));
        root.commit();

        let ruleset = MyTx::new_ruleset("my_rule_set");
        let executed = Arc::new(Mutex::new(false));
        let cloned_flag = executed.clone();
        MyTx::add_rule("my_rule", ruleset, my_pat, move |_ctx, my_pattern_vars| {
            println!("{:?}", my_pattern_vars.expr);
            let mut locked = cloned_flag.lock().unwrap();
            *locked = true;
        });
        MyTx::run_ruleset(ruleset, RunConfig::Once);
        assert_eq!(*executed.lock().unwrap(), true);
    }
}
// #[cfg(test)]
// mod test_container_of_base {
//     use crate::{self as eggplant};
//     use eggplant::prelude::*;
//     use eggplant::tx_rx_vt_pr_fp;
//     #[eggplant::dsl(container =Array)]
//     pub enum Expr {
//         VecSum { exprs: Array },
//     }
//     #[eggplant::container]
//     struct Array {
//         inner: Vec<i64>,
//     }
//     #[eggplant::pat_vars]
//     struct SumVec {
//         vec_expr: VecSum,
//     }

//     tx_rx_vt_pr_fp!(MyTx, MyPatRec);
//     fn main() {
//         env_logger::init();
//         let expr: Expr<MyTx, _> = VecSum::new(&Array::new(vec![3, 2, 1]));
//         expr.commit();

//         let ruleset = MyTx::new_ruleset("constant_prop");
//         MyTx::add_rule(
//             "sum_vec",
//             ruleset,
//             || {
//                 let vec_expr = Array::query_leaf();
//                 SumVec::new(VecSum::query(&vec_expr))
//             },
//             |ctx, values| {
//                 println!("{:?}", values);
//                 let v = ctx.devalue(values.vec_expr.exprs);
//                 for expr in v.iter() {
//                     println!("got expr {:?}", expr)
//                 }
//             },
//         );
//         let report = MyTx::run_ruleset(ruleset, RunConfig::Sat);
//         println!("{:#?}", report);
//         MyTx::table_view();

//         expr.pull();
//         MyTx::egraph_to_dot("egraph.dot".into());
//         MyTx::wag_to_dot("wag.dot".into());
//         // paterns to dot
//         MyPatRec::sgl().pats_to_dot("pats.dot".into());
//     }
// }
