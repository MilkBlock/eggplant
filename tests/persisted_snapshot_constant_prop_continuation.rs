use std::fs;
use std::path::PathBuf;
use std::sync::{Mutex, MutexGuard, OnceLock};
use std::time::{SystemTime, UNIX_EPOCH};

use eggplant::prelude::*;
use eggplant::tx_rx_vt_pr;

#[eggplant::dsl]
enum Expr {
    Const { num: i64 },
    Mul { l: Expr, r: Expr },
    Add { l: Expr, r: Expr },
}

tx_rx_vt_pr!(ContTx, ContPatRec);

macro_rules! prop {
    ($ty:ident,$op:tt,$pat_name:ident,$ruleset:ident) => {
        ContTx::add_rule(
            stringify!($pat_name),
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
                let cal = ctx.devalue(pat.l.num) $op ctx.devalue(pat.r.num);
                let op_value = ctx.insert_const(cal);
                ctx.union(pat.p, op_value);
            },
        );
    };
}

fn test_guard() -> MutexGuard<'static, ()> {
    static LOCK: OnceLock<Mutex<()>> = OnceLock::new();
    LOCK.get_or_init(|| Mutex::new(()))
        .lock()
        .unwrap_or_else(|err| err.into_inner())
}

fn register_constant_prop_rules() -> RuleSetId {
    let ruleset = ContTx::new_ruleset("constant_prop_continuation_test");
    prop!(Add, +, AddPat, ruleset);
    prop!(Mul, *, MulPat, ruleset);
    ruleset
}

fn snapshot_path() -> PathBuf {
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_nanos();
    std::env::temp_dir().join(format!(
        "persisted_snapshot_constant_prop_continuation_test_{nanos}.json"
    ))
}

fn dump_snapshot_to_disk(path: &PathBuf) {
    let snapshot = {
        let egraph_handle = ContTx::egraph();
        let egraph = egraph_handle.lock().unwrap();
        build_persisted_snapshot_v1(&egraph, egglog::SerializeConfig::default())
    };
    fs::write(path, serde_json::to_string_pretty(&snapshot).unwrap()).unwrap();
}

fn load_snapshot_from_disk(path: &PathBuf) -> PersistedSnapshot {
    serde_json::from_str(&fs::read_to_string(path).unwrap()).unwrap()
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

fn canonical_eq(lhs: &Expr<ContTx>, rhs_const: i64) -> bool {
    let rhs: Expr<ContTx, ConstTy> = Const::new(rhs_const);
    rhs.commit();
    ContTx::canonical_raw(lhs) == ContTx::canonical_raw(&rhs)
}

#[test]
fn persisted_snapshot_constant_prop_continues_after_restore() {
    let _guard = test_guard();

    ContTx::reset_for_bench();
    let ruleset = register_constant_prop_rules();

    let mul: Expr<ContTx, MulTy> = Mul::new(&Const::new(3), &Const::new(2));
    let expr: Expr<ContTx, AddTy> = Add::new(&mul, &Const::new(4));
    expr.commit();

    let first = ContTx::run_ruleset(ruleset, RunConfig::Once);
    assert!(
        first.updated,
        "first run should derive the inner multiplication"
    );
    assert!(canonical_eq(&mul, 6));
    assert!(
        !canonical_eq(&expr, 10),
        "one non-saturating run should not yet derive the outer addition"
    );

    let path = snapshot_path();
    dump_snapshot_to_disk(&path);

    ContTx::reset_for_bench();
    let ruleset = register_constant_prop_rules();
    let snapshot = load_snapshot_from_disk(&path);
    {
        let egraph_handle = ContTx::egraph();
        let mut egraph = egraph_handle.lock().unwrap();
        restore_persisted_snapshot_v1(&mut egraph, &snapshot).unwrap();
    }
    let restored_snapshot = {
        let egraph_handle = ContTx::egraph();
        let egraph = egraph_handle.lock().unwrap();
        build_persisted_snapshot_v1(&egraph, egglog::SerializeConfig::default())
    };
    assert!(
        snapshot_has_const(&restored_snapshot, 6),
        "the derived constant row should survive dump/restore"
    );

    let mul_after: Expr<ContTx, MulTy> = Mul::new(&Const::new(3), &Const::new(2));
    let expr_after: Expr<ContTx, AddTy> = Add::new(&mul_after, &Const::new(4));
    expr_after.commit();

    let second = ContTx::run_ruleset(ruleset, RunConfig::Once);
    assert!(
        second.updated,
        "second run after restore should continue the derivation"
    );
    assert!(
        canonical_eq(&mul_after, 6),
        "one more run should reconnect the restored multiplication to the surviving `Const 6` row"
    );
    assert!(
        canonical_eq(&expr_after, 10),
        "the surviving inner constant should let one more run derive the outer `Const 10`"
    );

    let _ = fs::remove_file(path);
}
