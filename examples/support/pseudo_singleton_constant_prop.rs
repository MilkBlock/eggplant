#[path = "pseudo_singleton_runtime.rs"]
mod pseudo_singleton_runtime;

use eggplant::prelude::*;
use std::sync::{Arc, Barrier};

pub use pseudo_singleton_runtime::{MyTx, Session, new_session};

#[eggplant::dsl]
pub enum Expr {
    Const { num: i64 },
    Mul { l: Expr, r: Expr },
    Add { l: Expr, r: Expr },
}

macro_rules! prop {
    ($rule_name:expr, $ty:ident, $op:tt, $pat_name:ident, $ruleset:ident) => {{
        let rule_name = $rule_name;
        MyTx::add_rule(
            &rule_name,
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

pub fn register_constant_prop_rules() -> RuleSetId {
    const RULESET_CACHE_KEY: &str = "constant_prop";
    const RULESET_NAME: &str = "constant_prop_pseudo_singleton";

    pseudo_singleton_runtime::get_or_register_ruleset(RULESET_CACHE_KEY, || {
        let ruleset = MyTx::new_ruleset(RULESET_NAME);
        prop!("AddPat", Add, +, AddPat, ruleset);
        prop!("MulPat", Mul, *, MulPat, ruleset);
        ruleset
    })
}

fn current_snapshot() -> PersistedSnapshot {
    let egraph = <MyTx as eggplant::wrap::NonPatRecSgl>::egraph();
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
    assert!(
        canonical_eq(&expr, expected),
        "expected {lhs} * {rhs} + {addend} to fold to {expected}"
    );
    expected
}

pub fn fold_mul_add_expr(session: &Session, lhs: i64, rhs: i64, addend: i64) -> i64 {
    session.run(|| fold_active_mul_add_expr(lhs, rhs, addend))
}

pub fn session_has_const(session: &Session, needle: i64) -> bool {
    session.run(|| current_session_has_const(needle))
}

pub fn registers_rules_twice_in_same_session() {
    let session = new_session();

    session.run(|| {
        let ruleset1 = register_constant_prop_rules();
        let ruleset2 = register_constant_prop_rules();

        assert_eq!(
            ruleset1.0, ruleset2.0,
            "re-registering in one session should reuse the cached ruleset"
        );

        let expr: Expr<MyTx, AddTy> =
            Add::new(&Mul::new(&Const::new(2), &Const::new(8)), &Const::new(1));
        expr.commit();

        let _ = MyTx::run_ruleset(ruleset1, RunConfig::Sat);
        let _ = MyTx::run_ruleset(ruleset2, RunConfig::Sat);

        assert!(
            canonical_eq(&expr, 17),
            "re-registering rules in one session should remain usable"
        );
    });
}

pub fn concurrent_sessions_can_register_rules_on_different_threads() {
    let left = new_session();
    let right = new_session();

    let left_thread = left.spawn(move || fold_active_mul_add_expr(3, 2, 4));
    let right_thread = right.spawn(move || fold_active_mul_add_expr(5, 5, 1));

    assert_eq!(left_thread.join().unwrap(), 10);
    assert_eq!(right_thread.join().unwrap(), 26);

    assert!(
        session_has_const(&left, 10),
        "left session should keep its folded constant after threaded execution"
    );
    assert!(
        !session_has_const(&left, 26),
        "left session should remain isolated from the right session"
    );
    assert!(
        session_has_const(&right, 26),
        "right session should keep its folded constant after threaded execution"
    );
    assert!(
        !session_has_const(&right, 10),
        "right session should remain isolated from the left session"
    );
}

pub fn same_session_concurrent_registration_is_safe() {
    let session = new_session();
    let barrier = Arc::new(Barrier::new(2));

    let left_session = session.clone();
    let left_barrier = Arc::clone(&barrier);
    let left = session.spawn(move || {
        left_barrier.wait();
        let ruleset = register_constant_prop_rules();
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
        let ruleset = register_constant_prop_rules();
        let folded = fold_active_mul_add_expr(2, 8, 1);
        (
            ruleset.0.to_owned(),
            folded,
            session_has_const(&right_session, 17),
        )
    });

    let left = left.join().unwrap();
    let right = right.join().unwrap();

    assert_eq!(
        left.0, right.0,
        "same-session concurrent registration should converge on one cached ruleset"
    );
    assert_eq!(left.1, 10);
    assert_eq!(right.1, 17);
    assert!(left.2);
    assert!(right.2);
}

pub fn async_sessions_can_survive_yield_and_spawn() {
    let runtime = tokio::runtime::Builder::new_multi_thread()
        .worker_threads(2)
        .enable_all()
        .build()
        .unwrap();

    runtime.block_on(async {
        let left = new_session();
        let right = new_session();

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

        let left_has_10 = left
            .run_async(async { current_session_has_const(10) })
            .await;
        let left_has_26 = left
            .run_async(async { current_session_has_const(26) })
            .await;
        let right_has_26 = right
            .run_async(async { current_session_has_const(26) })
            .await;
        let right_has_10 = right
            .run_async(async { current_session_has_const(10) })
            .await;

        assert!(left_has_10, "left async session should retain Const 10");
        assert!(
            !left_has_26,
            "left async session should remain isolated from the right async session"
        );
        assert!(right_has_26, "right async session should retain Const 26");
        assert!(
            !right_has_10,
            "right async session should remain isolated from the left async session"
        );
    });
}

pub fn sync_run_can_override_outer_async_session() {
    let runtime = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()
        .unwrap();

    runtime.block_on(async {
        let outer = new_session();
        let inner = new_session();

        outer
            .run_async(async {
                tokio::task::yield_now().await;

                assert_eq!(fold_mul_add_expr(&inner, 5, 5, 1), 26);

                assert!(
                    session_has_const(&inner, 26),
                    "inner session should receive the sync override work"
                );
                assert!(
                    !session_has_const(&outer, 26),
                    "outer async session should not accidentally receive inner sync work"
                );
            })
            .await;
    });
}

pub fn two_isolated_sessions_keep_separate_egraphs_with_handles() {
    let left = new_session();
    let right = new_session();

    assert_eq!(fold_mul_add_expr(&left, 3, 2, 4), 10);
    assert_eq!(fold_mul_add_expr(&right, 5, 5, 1), 26);

    assert!(
        session_has_const(&left, 10),
        "left session should keep its own derived constant"
    );
    assert!(
        !session_has_const(&left, 26),
        "left session should not see the right session's folded constant"
    );
    assert!(
        session_has_const(&right, 26),
        "right session should keep its own derived constant"
    );
    assert!(
        !session_has_const(&right, 10),
        "right session should not inherit the left session's folded constant"
    );
}
