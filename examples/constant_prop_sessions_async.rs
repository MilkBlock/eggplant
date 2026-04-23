use eggplant::{prelude::*, tx_rx_vt_pr};

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
        let ruleset = MyTx::new_ruleset("constant_prop_sessions_async");
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

fn main() {
    env_logger::init();
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

        let outer = MyTx::new_session();
        let inner = MyTx::new_session();
        outer
            .run_async(async {
                tokio::task::yield_now().await;
                let inner_value = inner.run(|| fold_active_mul_add_expr(5, 5, 1));
                assert_eq!(inner_value, 26);
                assert!(inner.run(|| current_session_has_const(26)));
                assert!(!outer.run(|| current_session_has_const(26)));
            })
            .await;
    });

    println!("constant_prop_sessions_async passed");
}
