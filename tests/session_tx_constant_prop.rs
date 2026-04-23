use eggplant::{prelude::*, tx_rx_vt_pr};
use std::sync::{Arc, Barrier, mpsc};
use std::time::Duration;

#[eggplant::dsl]
pub enum Expr {
    Const { num: i64 },
    Mul { l: Expr, r: Expr },
    Add { l: Expr, r: Expr },
}

tx_rx_vt_pr!(MyTx, MyPatRec);

macro_rules! prop {
    ($rule_name:expr, $ty:ident, $op:tt, $pat_name:ident, $ruleset:ident) => {{
        MyTx::add_rule(
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

fn register_constant_prop_rules() -> RuleSetId {
    MyTx::get_or_register_ruleset("constant_prop", || {
        let ruleset = MyTx::new_ruleset("constant_prop_sessions_test");
        prop!("AddPat", Add, +, AddPat, ruleset);
        prop!("MulPat", Mul, *, MulPat, ruleset);
        ruleset
    })
}

fn current_snapshot() -> PersistedSnapshot {
    let egraph = MyTx::egraph();
    let egraph = egraph.lock().unwrap();
    build_persisted_snapshot_v1(&egraph, egglog::SerializeConfig::default())
}

fn snapshot_has_const(snapshot: &PersistedSnapshot, needle: i64) -> bool {
    let Some(const_decl) = snapshot
        .schema
        .constructor_decls
        .iter()
        .find(|decl| decl.name == "Const")
    else {
        return false;
    };

    snapshot.state.function_rows.iter().any(|row| {
        row.op_id == const_decl.op_id
            && matches!(
                row.inputs.first(),
                Some(PersistedSnapshotValue::Lit { value, .. }) if value.value == needle.to_string()
            )
    })
}

fn current_session_has_const(needle: i64) -> bool {
    snapshot_has_const(&current_snapshot(), needle)
}

fn canonical_eq(lhs: &Expr<MyTx>, rhs_const: i64) -> bool {
    let rhs: Expr<MyTx, ConstTy> = Const::new(rhs_const);
    rhs.commit();
    MyTx::canonical_raw(lhs) == MyTx::canonical_raw(&rhs)
}

fn fold_active_mul_add_expr(lhs: i64, rhs: i64, addend: i64) -> i64 {
    let expr: Expr<MyTx, AddTy> = Add::new(
        &Mul::new(&Const::new(lhs), &Const::new(rhs)),
        &Const::new(addend),
    );
    expr.commit();

    let ruleset = register_constant_prop_rules();
    let _ = MyTx::run_ruleset(ruleset, RunConfig::Sat);

    let expected = lhs * rhs + addend;
    assert!(canonical_eq(&expr, expected));
    expected
}

fn fold_mul_add_expr(session: &Session<MyTx>, lhs: i64, rhs: i64, addend: i64) -> i64 {
    session.run(|| fold_active_mul_add_expr(lhs, rhs, addend))
}

fn session_has_const(session: &Session<MyTx>, needle: i64) -> bool {
    session.run(|| current_session_has_const(needle))
}

#[test]
fn tx_sessions_keep_egraphs_isolated() {
    let left = MyTx::new_session();
    let right = MyTx::new_session();

    assert_eq!(fold_mul_add_expr(&left, 3, 2, 4), 10);
    assert_eq!(fold_mul_add_expr(&right, 5, 5, 1), 26);
    assert!(session_has_const(&left, 10));
    assert!(!session_has_const(&left, 26));
    assert!(session_has_const(&right, 26));
    assert!(!session_has_const(&right, 10));
}

#[test]
fn tx_sessions_can_register_rules_twice_per_session() {
    let session = MyTx::new_session();

    session.run(|| {
        let ruleset1 = session.get_or_register_ruleset("constant_prop", || {
            let ruleset = MyTx::new_ruleset("constant_prop_sessions_test");
            prop!("AddPat", Add, +, AddPat, ruleset);
            prop!("MulPat", Mul, *, MulPat, ruleset);
            ruleset
        });
        let ruleset2 = session.get_or_register_ruleset("constant_prop", || unreachable!());

        assert_eq!(ruleset1.0, ruleset2.0);

        let expr: Expr<MyTx, AddTy> =
            Add::new(&Mul::new(&Const::new(2), &Const::new(8)), &Const::new(1));
        expr.commit();

        let _ = MyTx::run_ruleset(ruleset1, RunConfig::Sat);
        let _ = MyTx::run_ruleset(ruleset2, RunConfig::Sat);
        assert!(canonical_eq(&expr, 17));
    });
}

#[test]
fn tx_sessions_support_explicit_cross_thread_reentry() {
    let left = MyTx::new_session();
    let right = MyTx::new_session();

    let left_thread = left.spawn(move || fold_active_mul_add_expr(3, 2, 4));
    let right_thread = right.spawn(move || fold_active_mul_add_expr(5, 5, 1));

    assert_eq!(left_thread.join().unwrap(), 10);
    assert_eq!(right_thread.join().unwrap(), 26);
    assert!(session_has_const(&left, 10));
    assert!(!session_has_const(&left, 26));
    assert!(session_has_const(&right, 26));
    assert!(!session_has_const(&right, 10));
}

#[test]
fn tx_sessions_support_async_task_local_routing() {
    let runtime = tokio::runtime::Builder::new_multi_thread()
        .worker_threads(2)
        .enable_all()
        .build()
        .unwrap();

    runtime.block_on(async {
        let left = MyTx::new_session();
        let right = MyTx::new_session();

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

        assert!(left.run_async(async { current_session_has_const(10) }).await);
        assert!(!left.run_async(async { current_session_has_const(26) }).await);
        assert!(right.run_async(async { current_session_has_const(26) }).await);
        assert!(!right.run_async(async { current_session_has_const(10) }).await);
    });
}

#[test]
fn tx_sessions_sync_run_can_override_outer_async_session() {
    let runtime = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()
        .unwrap();

    runtime.block_on(async {
        let outer = MyTx::new_session();
        let inner = MyTx::new_session();

        outer
            .run_async(async {
                tokio::task::yield_now().await;
                assert_eq!(fold_mul_add_expr(&inner, 5, 5, 1), 26);
                assert!(session_has_const(&inner, 26));
                assert!(!session_has_const(&outer, 26));
            })
            .await;
    });
}

#[test]
fn tx_sessions_same_session_concurrent_registration_is_safe() {
    let session = MyTx::new_session();
    let barrier = Arc::new(Barrier::new(2));

    let left_session = session.clone();
    let left_barrier = Arc::clone(&barrier);
    let left = session.spawn(move || {
        left_barrier.wait();
        let ruleset = left_session.get_or_register_ruleset("constant_prop", || {
            let ruleset = MyTx::new_ruleset("constant_prop_sessions_test");
            prop!("AddPat", Add, +, AddPat, ruleset);
            prop!("MulPat", Mul, *, MulPat, ruleset);
            ruleset
        });
        let folded = fold_active_mul_add_expr(3, 2, 4);
        (
            ruleset.0.to_owned(),
            folded,
            session_has_const(&left_session, 10),
        )
    });

    let right_session = session.clone();
    let right_barrier = Arc::clone(&barrier);
    let right = session.spawn(move || {
        right_barrier.wait();
        let ruleset = right_session.get_or_register_ruleset("constant_prop", || {
            let ruleset = MyTx::new_ruleset("constant_prop_sessions_test");
            prop!("AddPat", Add, +, AddPat, ruleset);
            prop!("MulPat", Mul, *, MulPat, ruleset);
            ruleset
        });
        let folded = fold_active_mul_add_expr(2, 8, 1);
        (
            ruleset.0.to_owned(),
            folded,
            session_has_const(&right_session, 17),
        )
    });

    let left = left.join().unwrap();
    let right = right.join().unwrap();
    assert_eq!(left.0, right.0);
    assert_eq!(left.1, 10);
    assert_eq!(right.1, 17);
    assert!(left.2);
    assert!(right.2);
}

#[test]
fn tx_sessions_nested_ruleset_registration_is_safe() {
    let session = MyTx::new_session();
    let (sender, receiver) = mpsc::sync_channel(1);

    let sender_thread = sender.clone();
    let session_for_thread = session.clone();
    let handle = session.spawn(move || {
        let outer = session_for_thread.get_or_register_ruleset("outer_constant_prop", || {
            let inner = session_for_thread.get_or_register_ruleset("inner_constant_prop", || {
                MyTx::new_ruleset("inner_constant_prop_ruleset")
            });
            let outer = MyTx::new_ruleset("outer_constant_prop_ruleset");
            assert_ne!(inner.0, outer.0);
            outer
        });
        sender_thread.send(outer.0.to_owned()).unwrap();
    });

    let outer_ruleset = receiver.recv_timeout(Duration::from_secs(1)).unwrap();
    assert_eq!(outer_ruleset, "outer_constant_prop_ruleset");
    handle.join().unwrap();
}

#[test]
fn tx_sessions_same_key_nested_registration_fails_fast() {
    let session = MyTx::new_session();

    let panic = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        session.run(|| {
            let _ = session.get_or_register_ruleset("same_key_ruleset", || {
                let _ = session.get_or_register_ruleset("same_key_ruleset", || {
                    MyTx::new_ruleset("same_key_inner_ruleset")
                });
                MyTx::new_ruleset("same_key_outer_ruleset")
            });
        });
    }))
    .expect_err("same-key nested registration should fail fast");

    let message = if let Some(message) = panic.downcast_ref::<&str>() {
        (*message).to_owned()
    } else if let Some(message) = panic.downcast_ref::<String>() {
        message.clone()
    } else {
        String::new()
    };

    assert!(message.contains("reentrant ruleset registration"));
}
