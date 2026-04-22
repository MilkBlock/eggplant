use egglog::EGraph;
use eggplant::prelude::*;

#[eggplant::dsl]
pub enum Expr {
    Const { num: i64 },
    Mul { l: Expr, r: Expr },
    Sub { l: Expr, r: Expr },
    Add { l: Expr, r: Expr },
    Div { l: Expr, r: Expr },
}

pub struct MyTxProof {
    tx: eggplant::instances::tx_rx_vt_pr::TxRxVTPR,
}

impl SingletonGetter for MyTxProof {
    type RetTy = eggplant::instances::tx_rx_vt_pr::TxRxVTPR;
    fn sgl() -> &'static eggplant::instances::tx_rx_vt_pr::TxRxVTPR {
        static INSTANCE: std::sync::OnceLock<MyTxProof> = std::sync::OnceLock::new();
        &INSTANCE
            .get_or_init(|| MyTxProof {
                tx: eggplant::instances::tx_rx_vt_pr::TxRxVTPR::new_with_proof(),
            })
            .tx
    }
}

impl eggplant::wrap::NonPatRecSgl for MyTxProof {
    fn egraph() -> std::sync::Arc<std::sync::Mutex<EGraph>> {
        <Self as eggplant::wrap::NonPatRecSgl>::egraph()
    }
}

eggplant::basic_patttern_recorder!(MyPatRec);
impl eggplant::wrap::WithPatRecSgl for MyTxProof {
    type PatRecSgl = MyPatRec;
}
impl eggplant::wrap::WithRxSgl for MyPatRec {
    type RxSgl = MyTxProof;
}

macro_rules! prop {
    ($ty:ident,$op:tt,$pat_name:ident,$ruleset:ident) => {
        MyTxProof::add_rule(
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

fn main() -> Result<(), egglog::Error> {
    env_logger::init();

    let mul: Expr<MyTxProof, MulTy> = Mul::new(&Const::new(3), &Const::new(2));
    let expr: Expr<MyTxProof, _> = Add::new(&mul, &Const::new(4));
    expr.commit();
    // Capture the *pre-rewrite* value ids so `prove` won't short-circuit on `lhs==rhs` after
    // rewrites merge them into the same canonical value.
    let expr_value = MyTxProof::value(&expr).val;
    let mul_value = MyTxProof::value(&mul).val;

    let expected: Expr<MyTxProof, ConstTy> = Const::new(10);
    expected.commit();
    let expected_value = MyTxProof::value(&expected).val;

    let expected_mul: Expr<MyTxProof, ConstTy> = Const::new(6);
    expected_mul.commit();
    let expected_mul_value = MyTxProof::value(&expected_mul).val;

    let ruleset = MyTxProof::new_ruleset("constant_prop");
    prop!(Add, +, AddPat, ruleset);
    prop!(Sub, -, SubPat, ruleset);
    prop!(Mul, *, MulPat, ruleset);
    prop!(Div, /, DivPat, ruleset);
    MyTxProof::add_rule(
        "AddMulConstPat",
        ruleset,
        || {
            let l = Const::query();
            let r = Const::query();
            let m = Mul::query(&l, &r);
            let c = Const::query();
            let p = Add::query(&m, &c);
            #[eggplant::pat_vars_catch]
            struct AddMulConstPat {
                l: Const,
                r: Const,
                c: Const,
                p: Add,
            }
        },
        |ctx, pat| {
            let cal = ctx.devalue(pat.l.num) * ctx.devalue(pat.r.num) + ctx.devalue(pat.c.num);
            let op_value = ctx.insert_const(cal);
            ctx.union(pat.p, op_value);
        },
    );
    let report = MyTxProof::run_ruleset(ruleset, RunConfig::Sat);
    println!("{:#?}", report);

    assert!(
        report
            .num_matches_per_rule
            .get("@MulPat")
            .copied()
            .unwrap_or(0)
            > 0,
        "MulPat should match in proofs mode"
    );

    let proof_mul = MyTxProof::sgl().prove_eq_pretty_raw("Expr", mul_value, expected_mul_value)?;
    println!("{proof_mul}");

    let proof_expr = MyTxProof::sgl().prove_eq_pretty_raw("Expr", expr_value, expected_value)?;
    println!("{proof_expr}");
    println!("constant_prop_proof passed");
    Ok(())
}
