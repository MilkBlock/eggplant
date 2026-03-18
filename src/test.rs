#[cfg(test)]
mod tests {
    use crate::{self as eggplant, tx_rx_vt_pr};
    use eggplant::prelude::*;
    use std::sync::{Arc, Mutex, atomic::{AtomicBool, Ordering}};

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

    #[eggplant::dsl]
    enum FuncS {
        SConst { n: i64 },
    }
    #[eggplant::dsl]
    enum FuncE {
        EConst { n: i64 },
    }
    #[eggplant::func(output=FuncE)]
    struct MAccumQ {
        s: FuncS,
    }

    #[test]
    fn func_query_pattern_smoke() {
        let _ = env_logger::builder().is_test(true).try_init();

        tx_rx_vt_pr!(MyTxFunc, MyPatRecFunc);

        let init = MyTxFunc::new_ruleset("func_query_init");
        MyTxFunc::add_rule(
            "seed_func",
            init,
            || {
                #[eggplant::pat_vars_catch]
                struct Unit {}
            },
            |ctx, _pat| {
                let s = SConst::<MyTxFunc>::new(1);
                s.commit();
                let e = EConst::<MyTxFunc>::new(2);
                e.commit();
                let sv = MyTxFunc::value(&s);
                let ev = MyTxFunc::value(&e);
                ctx.set_m_accum_q(sv, ev);
            },
        );
        MyTxFunc::run_ruleset(init, RunConfig::Once);

        let ruleset = MyTxFunc::new_ruleset("func_query_pattern");
        let hit = Arc::new(AtomicBool::new(false));
        let hit2 = Arc::clone(&hit);
        MyTxFunc::add_rule(
            "match_func_output",
            ruleset,
            || {
                let s = FuncS::query_leaf();
                let e = MAccumQ::query(&s);
                #[eggplant::pat_vars_catch]
                struct Pat {
                    s: FuncS,
                    e: FuncE,
                }
            },
            move |_ctx, _pat| {
                hit2.store(true, Ordering::SeqCst);
            },
        );
        MyTxFunc::run_ruleset(ruleset, RunConfig::Once);
        assert!(hit.load(Ordering::SeqCst));
    }

    #[test]
    fn func_ctx_read_smoke() {
        tx_rx_vt_pr!(MyTxRead, MyPatRecRead);

        #[eggplant::func(output=i64)]
        struct FibRead {
            x: i64,
        }

        let init = MyTxRead::new_ruleset("init");
        MyTxRead::add_rule(
            "init",
            init,
            || {
                #[eggplant::pat_vars_catch]
                struct Unit {}
            },
            |ctx, _pat| {
                ctx.set_fib_read(1, 1);
                ctx.set_fib_read(2, 2);
            },
        );
        MyTxRead::run_ruleset(init, RunConfig::Once);

        let use_read = MyTxRead::new_ruleset("use_read");
        MyTxRead::add_rule(
            "use_read",
            use_read,
            || {
                #[eggplant::pat_vars_catch]
                struct Unit {}
            },
            |ctx, _pat| {
                let v1 = ctx.read_fib_read(1);
                let v2 = ctx.devalue(ctx.read_fib_read_value(2));
                ctx.set_fib_read(3, v1 + v2);
            },
        );
        MyTxRead::run_ruleset(use_read, RunConfig::Once);

        assert_eq!(FibRead::<MyTxRead>::get(&3), 3);
    }
}

#[cfg(test)]
mod proofs_api_tests {
    use crate::{self as eggplant, instances::tx_rx_vt_pr::TxRxVTPR};
    use eggplant::prelude::*;
    use egglog::ast::Expr;
    use egglog::span;

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

        // 2) Expr-AST-based APIs: call them with surface constructor ASTs.
        //
        // NOTE: In term-encoding mode, “surface AST -> committed Value” is not guaranteed to be
        // stable yet (tracked in #t42). These calls are best-effort and may return an error; the
        // regression we care about here is that they remain safe to call.
        let mul_ast = Expr::Call(
            span!(),
            "ProofMul".to_owned(),
            vec![
                Expr::Call(
                    span!(),
                    "ProofConst".to_owned(),
                    vec![Expr::Lit(span!(), egglog::ast::Literal::Int(3))],
                ),
                Expr::Call(
                    span!(),
                    "ProofConst".to_owned(),
                    vec![Expr::Lit(span!(), egglog::ast::Literal::Int(2))],
                ),
            ],
        );
        let const6_ast = Expr::Call(
            span!(),
            "ProofConst".to_owned(),
            vec![Expr::Lit(span!(), egglog::ast::Literal::Int(6))],
        );
        let _ = MyTxProof::sgl().value_equiv_expr_ast("ProofExpr", expected_value, const6_ast.clone());
        let _ = MyTxProof::sgl().prove_eq_pretty_expr_ast("ProofExpr", mul_ast, const6_ast);

        // 3) Regression: proof export should work for non-canonical values too (class-id/canon-rep keying).
        let (rep, non_rep) = {
            let egraph = MyTxProof::sgl().egraph.lock().unwrap();
            let sort = egraph.get_sort_by_name("ProofExpr").unwrap().clone();
            let rep = egraph.get_canonical_value(mul_value, &sort);
            let non_rep = if mul_value != rep {
                Some(mul_value)
            } else if expected_value != rep {
                Some(expected_value)
            } else {
                None
            };
            (rep, non_rep)
        };
        if let Some(non_rep) = non_rep {
            let proof_nonrep = MyTxProof::sgl()
                .prove_eq_pretty_raw("ProofExpr", non_rep, rep)
                .expect("prove_eq_pretty_raw(nonrep, rep) should succeed");
            assert!(!proof_nonrep.trim().is_empty());
            assert!(proof_nonrep.contains("(name \"@MulPat\")"));
        }
    }
}

#[cfg(test)]
mod egglog_rule_baseline_tests {
    #[test]
    fn egglog_native_rule_can_match_func_output_and_build_set_of() {
        // Baseline for: (rule ((= ?e (MAccum ?s))) ((set (MAccumSet) (set-of ?e))) ...)
        //
        // Eggplant `add_rule` cannot express "match over function output" yet; keep this
        // test as the semantic reference for the desired behavior.
        let mut egraph = egglog::EGraph::default();
        egraph
            .parse_and_run_program(
                None,
                r#"
(sort IntSet (Set i64))
(function MAccum (i64) i64 :merge old)
(function MAccumSet () IntSet :merge (set-union old new))

(set (MAccum 1) 42)
(ruleset ir-prop)
(rule
  ((= ?e (MAccum ?s)))
  ((set (MAccumSet) (set-of ?e)))
  :ruleset ir-prop)

(run-schedule (saturate (run ir-prop)))
(check (= (MAccumSet) (set-of 42)))
"#,
            )
            .expect("egglog baseline should succeed");
    }

    #[test]
    #[ignore = "TODO: eggplant add_rule needs function-output pattern support + container Insertable"]
    fn eggplant_add_rule_should_eventually_support_func_output_match_and_set_of_action() {
        // Intended future shape (pseudocode):
        // - `#[eggplant::func] struct MAccum { s: i64 } -> Expr`
        // - `MAccum::query(&s)` yields `e` such that fact `e = (MAccum s)` is recorded
        // - action: `ctx.set_m_accum_set(SetContainer::from(vec![e]))` or `ctx.set_m_accum_set_value(ctx.set_of(e))`
        //
        // Keep ignored until API exists; this is the spec we want to uphold.
        unimplemented!()
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
