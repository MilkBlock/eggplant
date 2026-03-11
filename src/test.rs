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

#[cfg(test)]
mod proofs_api_tests {
    use crate::{self as eggplant, instances::tx_rx_vt_pr::TxRxVTPR};
    use eggplant::prelude::*;
    use egglog::prelude::exprs;

    #[eggplant::dsl]
    pub enum ProofExpr {
        ProofConst { num: i64 },
        ProofMul { l: ProofExpr, r: ProofExpr },
    }

    pub struct MyTxProof {
        tx: TxRxVTPR,
    }

    impl SingletonGetter for MyTxProof {
        type RetTy = TxRxVTPR;
        fn sgl() -> &'static TxRxVTPR {
            static INSTANCE: std::sync::OnceLock<MyTxProof> = std::sync::OnceLock::new();
            &INSTANCE
                .get_or_init(|| MyTxProof {
                    tx: TxRxVTPR::new_with_proof(),
                })
                .tx
        }
    }

    impl eggplant::wrap::NonPatRecSgl for MyTxProof {
        fn egraph() -> std::sync::Arc<std::sync::Mutex<egglog::EGraph>> {
            Self::sgl().egraph.clone()
        }
    }

    eggplant::basic_patttern_recorder!(MyPatRec);
    impl eggplant::wrap::WithPatRecSgl for MyTxProof {
        type PatRecSgl = MyPatRec;
    }
    impl eggplant::wrap::WithRxSgl for MyPatRec {
        type RxSgl = MyTxProof;
    }

    #[test]
    fn proofs_mode_apis_smoke() {
        let _ = env_logger::builder().is_test(true).try_init();

        let mul: ProofExpr<MyTxProof, ProofMulTy> =
            ProofMul::new(&ProofConst::new(3), &ProofConst::new(2));
        mul.commit();
        let mul_value = MyTxProof::value(&mul).val;

        let expected: ProofExpr<MyTxProof, ProofConstTy> = ProofConst::new(6);
        expected.commit();
        let expected_value = MyTxProof::value(&expected).val;

        let ruleset = MyTxProof::new_ruleset("constant_prop_test");
        MyTxProof::add_rule(
            "MulPat",
            ruleset,
            || {
                let l = ProofConst::query();
                let r = ProofConst::query();
                let p = ProofMul::query(&l, &r);
                #[eggplant::pat_vars_catch]
                struct MulPat {
                    l: ProofConst,
                    r: ProofConst,
                    p: ProofMul,
                }
            },
            |ctx, pat| {
                let cal = ctx.devalue(pat.l.num) * ctx.devalue(pat.r.num);
                let op_value = ctx.insert_proof_const(cal);
                ctx.union(pat.p, op_value);
            },
        );

        let report = MyTxProof::run_ruleset(ruleset, RunConfig::Sat);
        assert!(
            report
                .num_matches_per_rule
                .get("@MulPat")
                .copied()
                .unwrap_or(0)
                > 0,
            "MulPat should match in proofs mode"
        );

        // 1) Value-based proof export must be non-empty and show rewrite rule name.
        let proof = MyTxProof::sgl()
            .prove_eq_pretty_raw("ProofExpr", mul_value, expected_value)
            .expect("prove_eq_pretty_raw should succeed");
        assert!(!proof.trim().is_empty());
        assert!(proof.contains("(name \"@MulPat\")"));

        // NOTE: `value_equiv_expr_ast` depends on proof-mode `eval_expr` returning a stable witness
        // value for a surface AST. That path is currently unstable under term encoding, so we
        // test it separately once egglog exposes a “lookup committed value for AST” primitive.
    }
}
// #[cfg(test)]
// mod test_container_of_base {
//     use crate::{self as eggplant};
//     use eggplant::prelude::*;
//     use eggplant::tx_rx_vt_pr;
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

//     tx_rx_vt_pr!(MyTx, MyPatRec);
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
