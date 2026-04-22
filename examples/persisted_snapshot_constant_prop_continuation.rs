use std::fs;
use std::path::PathBuf;
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

fn register_constant_prop_rules() -> RuleSetId {
    let ruleset = ContTx::new_ruleset("constant_prop_continuation");
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
        "persisted_snapshot_constant_prop_continuation_{nanos}.json"
    ))
}

fn dump_snapshot_to_disk(path: &PathBuf) {
    let snapshot = {
        let egraph = ContTx::sgl().egraph.lock().unwrap();
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

fn main() {
    env_logger::init();

    ContTx::sgl().reset_for_bench();
    let ruleset = register_constant_prop_rules();

    let mul: Expr<ContTx, MulTy> = Mul::new(&Const::new(3), &Const::new(2));
    let expr: Expr<ContTx, AddTy> = Add::new(&mul, &Const::new(4));
    expr.commit();

    let first = ContTx::run_ruleset(ruleset, RunConfig::Once);
    println!("first run: {first:#?}");
    assert!(canonical_eq(&mul, 6));
    assert!(!canonical_eq(&expr, 10));

    let path = snapshot_path();
    dump_snapshot_to_disk(&path);
    println!("snapshot: {}", path.display());

    ContTx::sgl().reset_for_bench();
    let ruleset = register_constant_prop_rules();
    let snapshot = load_snapshot_from_disk(&path);
    {
        let mut egraph = ContTx::sgl().egraph.lock().unwrap();
        restore_persisted_snapshot_v1(&mut egraph, &snapshot).unwrap();
    }
    let restored_snapshot = {
        let egraph = ContTx::sgl().egraph.lock().unwrap();
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
    println!("second run: {second:#?}");
    assert!(canonical_eq(&mul_after, 6));
    assert!(
        canonical_eq(&expr_after, 10),
        "the surviving `Const 6` row should let one more run finish the outer addition"
    );

    let _ = fs::remove_file(path);
}
