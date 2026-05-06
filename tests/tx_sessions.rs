use eggplant::{prelude::*, tx_rx_vt_pr};
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering;

#[eggplant::dsl]
enum SessionExpr {
    Const { num: i64 },
    Mul { l: SessionExpr, r: SessionExpr },
    Add { l: SessionExpr, r: SessionExpr },
}

tx_rx_vt_pr!(SessionTx, SessionPatRec);
tx_rx_vt_pr!(AltSessionTx, AltSessionPatRec);

macro_rules! prop {
    ($rule_name:expr, $ty:ident, $op:tt, $pat_name:ident, $ruleset:ident) => {{
        SessionTx::add_rule(
            $rule_name,
            $ruleset,
            || {
                let l = Const::query();
                let r = Const::query();
                let p = $ty::query(&l, &r);
                #[eggplant::pat_vars_catch]
                struct $pat_name {
                    l: Const,
                    r: Const,
                    p: $ty,
                }
            },
            |ctx, pat| {
                let value = ctx.devalue(pat.l.num) $op ctx.devalue(pat.r.num);
                let folded = ctx.insert_const(value);
                ctx.union(pat.p, folded);
            },
        );
    }};
}

fn register_rules() -> RuleSetId {
    static NEXT_RULESET_ID: AtomicUsize = AtomicUsize::new(0);
    let id = NEXT_RULESET_ID.fetch_add(1, Ordering::Relaxed);
    let name: &'static str =
        Box::leak(format!("session_aware_constant_prop_{id}").into_boxed_str());
    let ruleset = SessionTx::new_ruleset(name);
    prop!("AddPat", Add, +, AddPat, ruleset);
    prop!("MulPat", Mul, *, MulPat, ruleset);
    ruleset
}

fn canonical_eq(lhs: &SessionExpr<SessionTx>, rhs_const: i64) -> bool {
    let rhs: SessionExpr<SessionTx, ConstTy> = Const::new(rhs_const);
    rhs.commit();
    SessionTx::canonical_raw(lhs) == SessionTx::canonical_raw(&rhs)
}

fn current_has_const(needle: i64) -> bool {
    let egraph = SessionTx::egraph();
    let egraph = egraph.lock().unwrap();

    egraph.function_rows("Const").into_iter().any(|row| {
        !row.subsumed
            && row
                .vals
                .first()
                .is_some_and(|value| egraph.value_to_base::<i64>(*value) == needle)
    })
}

fn fold_active_mul_add_expr(lhs: i64, rhs: i64, addend: i64) -> i64 {
    let expr: SessionExpr<SessionTx, AddTy> = Add::new(
        &Mul::new(&Const::new(lhs), &Const::new(rhs)),
        &Const::new(addend),
    );
    expr.commit();

    let ruleset = register_rules();
    let _ = SessionTx::run_ruleset(ruleset, RunConfig::Sat);

    let expected = lhs * rhs + addend;
    assert!(canonical_eq(&expr, expected));
    expected
}

fn fold_active_mul_add_expr_alt(lhs: i64, rhs: i64, addend: i64) -> i64 {
    let expr: SessionExpr<AltSessionTx, AddTy> = Add::new(
        &Mul::new(&Const::new(lhs), &Const::new(rhs)),
        &Const::new(addend),
    );
    expr.commit();

    let ruleset = {
        static NEXT_RULESET_ID: AtomicUsize = AtomicUsize::new(0);
        let id = NEXT_RULESET_ID.fetch_add(1, Ordering::Relaxed);
        let name: &'static str =
            Box::leak(format!("alt_session_aware_constant_prop_{id}").into_boxed_str());
        let ruleset = AltSessionTx::new_ruleset(name);
        AltSessionTx::add_rule(
            "AddPatAlt",
            ruleset,
            || {
                let l = Const::query();
                let r = Const::query();
                let p = Add::query(&l, &r);
                #[eggplant::pat_vars_catch]
                struct AddPatAlt {
                    l: Const,
                    r: Const,
                    p: Add,
                }
            },
            |ctx, pat| {
                let value = ctx.devalue(pat.l.num) + ctx.devalue(pat.r.num);
                let folded = ctx.insert_const(value);
                ctx.union(pat.p, folded);
            },
        );
        AltSessionTx::add_rule(
            "MulPatAlt",
            ruleset,
            || {
                let l = Const::query();
                let r = Const::query();
                let p = Mul::query(&l, &r);
                #[eggplant::pat_vars_catch]
                struct MulPatAlt {
                    l: Const,
                    r: Const,
                    p: Mul,
                }
            },
            |ctx, pat| {
                let value = ctx.devalue(pat.l.num) * ctx.devalue(pat.r.num);
                let folded = ctx.insert_const(value);
                ctx.union(pat.p, folded);
            },
        );
        ruleset
    };
    let _ = AltSessionTx::run_ruleset(ruleset, RunConfig::Sat);

    let expected = lhs * rhs + addend;
    let rhs: SessionExpr<AltSessionTx, ConstTy> = Const::new(expected);
    rhs.commit();
    assert_eq!(
        AltSessionTx::canonical_raw(&expr),
        AltSessionTx::canonical_raw(&rhs)
    );
    expected
}

#[test]
fn default_generated_tx_uses_default_session_when_no_explicit_session_is_bound() {
    assert_eq!(fold_active_mul_add_expr(3, 2, 4), 10);
    assert!(current_has_const(10));
}

#[test]
fn generated_tx_can_isolate_two_explicit_sessions() {
    let left = SessionTx::new_session();
    let right = SessionTx::new_session();

    left.run(|| {
        assert_eq!(fold_active_mul_add_expr(3, 2, 4), 10);
        assert!(current_has_const(10));
    });

    right.run(|| {
        assert_eq!(fold_active_mul_add_expr(5, 5, 1), 26);
        assert!(current_has_const(26));
    });

    left.run(|| {
        assert!(!current_has_const(26));
    });

    right.run(|| {
        assert!(!current_has_const(10));
    });
}

#[test]
fn generated_tx_can_route_explicit_async_sessions() {
    let runtime = tokio::runtime::Builder::new_multi_thread()
        .worker_threads(2)
        .enable_all()
        .build()
        .unwrap();

    runtime.block_on(async {
        let left = SessionTx::new_session();
        let right = SessionTx::new_session();

        let left_value = left
            .run_async(async {
                tokio::task::yield_now().await;
                fold_active_mul_add_expr(3, 2, 4)
            })
            .await;
        assert_eq!(left_value, 10);

        let right_join = right.spawn_async(async {
            tokio::task::yield_now().await;
            fold_active_mul_add_expr(5, 5, 1)
        });
        assert_eq!(right_join.await.unwrap(), 26);
    });
}

#[test]
fn reset_for_bench_clears_session_ruleset_cache() {
    let session = SessionTx::new_session();

    session.run(|| {
        assert_eq!(fold_active_mul_add_expr(3, 2, 4), 10);
        SessionTx::reset_for_bench();
        assert_eq!(fold_active_mul_add_expr(2, 8, 1), 17);
    });
}

#[test]
fn async_session_entry_preserves_other_thread_bound_tx_bindings() {
    let runtime = tokio::runtime::Builder::new_multi_thread()
        .worker_threads(2)
        .enable_all()
        .build()
        .unwrap();

    runtime.block_on(async {
        let left = SessionTx::new_session();
        let right = AltSessionTx::new_session();

        let seen_left_inside_right = left.run(|| {
            assert_eq!(fold_active_mul_add_expr(3, 2, 4), 10);
            right.spawn_async(async {
                tokio::task::yield_now().await;
                current_has_const(10)
            })
        });
        assert!(seen_left_inside_right.await.unwrap());

        right.run(|| {
            assert_eq!(fold_active_mul_add_expr_alt(5, 5, 1), 26);
        });
    });
}
