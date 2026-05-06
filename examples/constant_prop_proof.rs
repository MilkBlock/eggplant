use eggplant::prelude::*;
use std::{fs, path::Path};

#[eggplant::dsl]
pub enum Expr {
    #[typst("{name}")]
    #[precedence(100)]
    Var { name: String },
    #[typst("{num}")]
    #[precedence(100)]
    Const { num: i64 },
    #[typst("{l} * {r}")]
    #[precedence(60)]
    Mul { l: Expr, r: Expr },
    #[typst("{l} - {r}")]
    #[precedence(50)]
    Sub { l: Expr, r: Expr },
    #[typst("{l} + {r}")]
    #[precedence(50)]
    Add { l: Expr, r: Expr },
    #[typst("frac({l}, {r})")]
    #[precedence(60)]
    Div { l: Expr, r: Expr },
}

tx_rx_vt_pr_pf!(MyTxProof, MyPatRec);

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
    let expr_value = MyTxProof::value(&expr).val;
    let mul_value = MyTxProof::value(&mul).val;

    let expected: Expr<MyTxProof, ConstTy> = Const::new(10);
    expected.commit();
    let expected_value = MyTxProof::value(&expected).val;

    let expected_mul: Expr<MyTxProof, ConstTy> = Const::new(6);
    expected_mul.commit();
    let expected_mul_value = MyTxProof::value(&expected_mul).val;

    let ruleset = MyTxProof::new_ruleset("constant_prop");
    MyTxProof::add_rule(
        "ConstPropAddPat",
        ruleset,
        || {
            let l = Const::query();
            let r = Const::query();
            let p = Add::query(&l, &r);
            #[eggplant::pat_vars_catch]
            struct ConstPropAddPat {
                l: Const,
                r: Const,
                p: Add,
            }
        },
        |ctx, pat| {
            let cal = ctx.devalue(pat.l.num) + ctx.devalue(pat.r.num);
            let op_value = ctx.insert_const(cal);
            ctx.union(pat.p, op_value);
        },
    );
    prop!(Sub, -, SubPat, ruleset);
    MyTxProof::add_rule(
        "ConstPropMulPat",
        ruleset,
        || {
            let l = Const::query();
            let r = Const::query();
            let p = Mul::query(&l, &r);
            #[eggplant::pat_vars_catch]
            struct ConstPropMulPat {
                l: Const,
                r: Const,
                p: Mul,
            }
        },
        |ctx, pat| {
            let cal = ctx.devalue(pat.l.num) * ctx.devalue(pat.r.num);
            let op_value = ctx.insert_const(cal);
            ctx.union(pat.p, op_value);
        },
    );
    prop!(Div, /, DivPat, ruleset);
    let report = MyTxProof::run_ruleset(ruleset, RunConfig::Sat);
    println!("{:#?}", report);

    assert!(
        report
            .num_matches_per_rule
            .get("@ConstPropMulPat")
            .copied()
            .unwrap_or(0)
            > 0,
        "ConstPropMulPat should match in proofs mode"
    );

    let templates = ProofRulesTemplateIndex::from_default_path()?;
    let concise = true;

    let proof_mul = MyTxProof::sgl().prove_eq_pretty_raw("Expr", mul_value, expected_mul_value)?;
    println!("{proof_mul}");
    let proof_mul_typst = render_proof_text_typst_with_options(&proof_mul, &templates, concise);
    write_typst("target/constant_prop_proof_mul.typ", &proof_mul_typst)?;
    let proof_mul_svg = render_proof_text_svg_with_options(&proof_mul, &templates, concise)?;
    write_svg("target/constant_prop_proof_mul.svg", &proof_mul_svg)?;

    let proof_mul_value_typst = MyTxProof::sgl().prove_typst_raw_default_template_with_options(
        "Expr",
        expected_mul_value,
        concise,
    )?;
    write_typst(
        "target/constant_prop_proof_mul_value.typ",
        &proof_mul_value_typst,
    )?;
    let proof_mul_value_svg = MyTxProof::sgl().prove_svg_raw_default_template_with_options(
        "Expr",
        expected_mul_value,
        concise,
    )?;
    write_svg(
        "target/constant_prop_proof_mul_value.svg",
        &proof_mul_value_svg,
    )?;

    let proof_expr = MyTxProof::sgl().prove_eq_pretty_raw("Expr", expr_value, expected_value)?;
    println!("{proof_expr}");
    let proof_expr_typst = render_proof_text_typst_with_options(&proof_expr, &templates, concise);
    write_typst("target/constant_prop_proof_expr.typ", &proof_expr_typst)?;
    let proof_expr_svg = render_proof_text_svg_with_options(&proof_expr, &templates, concise)?;
    write_svg("target/constant_prop_proof_expr.svg", &proof_expr_svg)?;

    let add_comm_ruleset = MyTxProof::new_ruleset("add_commutativity");
    MyTxProof::add_rule(
        "ConstPropAddCommPat",
        add_comm_ruleset,
        || {
            let a = Expr::query_leaf();
            let b = Expr::query_leaf();
            let p = Add::query(&a, &b);
            #[eggplant::pat_vars_catch]
            struct ConstPropAddCommPat {
                a: Expr,
                b: Expr,
                p: Add,
            }
        },
        |ctx, pat| {
            let swapped = ctx.insert_add(pat.b, pat.a);
            ctx.union(pat.p, swapped);
        },
    );

    let a = Var::<MyTxProof>::new("a".to_owned());
    let b = Var::<MyTxProof>::new("b".to_owned());
    let comm_lhs = Add::new(&a, &b);
    comm_lhs.commit();
    let comm_rhs = Add::new(&b, &a);
    comm_rhs.commit();
    let _ = MyTxProof::run_ruleset(add_comm_ruleset, RunConfig::Sat);
    let proof_comm = MyTxProof::sgl().prove_eq_pretty_raw(
        "Expr",
        MyTxProof::value(&comm_lhs).val,
        MyTxProof::value(&comm_rhs).val,
    )?;
    println!("{proof_comm}");
    let proof_comm_typst = render_proof_text_typst_with_options(&proof_comm, &templates, concise);
    write_typst("target/constant_prop_proof_add_comm.typ", &proof_comm_typst)?;
    let proof_comm_svg = render_proof_text_svg_with_options(&proof_comm, &templates, concise)?;
    write_svg("target/constant_prop_proof_add_comm.svg", &proof_comm_svg)?;
    println!("constant_prop_proof passed");
    Ok(())
}

fn write_typst(path: impl AsRef<Path>, typst: &str) -> Result<(), egglog::Error> {
    let path = path.as_ref();
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).map_err(|err| {
            egglog::Error::BackendError(format!("failed to create {}: {err}", parent.display()))
        })?;
    }
    fs::write(path, typst).map_err(|err| {
        egglog::Error::BackendError(format!("failed to write {}: {err}", path.display()))
    })?;
    Ok(())
}

fn write_svg(path: impl AsRef<Path>, svg: &str) -> Result<(), egglog::Error> {
    let path = path.as_ref();
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).map_err(|err| {
            egglog::Error::BackendError(format!("failed to create {}: {err}", parent.display()))
        })?;
    }
    fs::write(path, svg).map_err(|err| {
        egglog::Error::BackendError(format!("failed to write {}: {err}", path.display()))
    })?;
    Ok(())
}
