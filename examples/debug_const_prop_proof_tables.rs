use egglog::{ArcSort, EGraph, Error, Value};
use eggplant::prelude::*;
use std::collections::BTreeSet;
use std::fmt::Write as _;
use std::fs;
use std::path::Path;

#[eggplant::dsl]
pub enum Expr {
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

tx_rx_vt_pr_pf!(DebugTxProof, DebugPatRec);

macro_rules! prop {
    ($ty:ident,$op:tt,$pat_name:ident,$ruleset:ident) => {
        DebugTxProof::add_rule(
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

fn main() -> Result<(), Error> {
    let mut egg = EGraph::new_with_proofs();
    egg.parse_and_run_program(Some("debug_const_prop_egg".to_owned()), egg_program())?;
    dump_egraph("egg", &egg, "target/debug_const_prop_tables_egg.txt")?;

    run_rust_api_program()?;
    let egraph = DebugTxProof::sgl().egraph.lock().unwrap();
    dump_egraph(
        "rust-api",
        &egraph,
        "target/debug_const_prop_tables_rust.txt",
    )?;

    Ok(())
}

fn egg_program() -> &'static str {
    r#"
(datatype Expr
  (Const i64)
  (Mul Expr Expr)
  (Sub Expr Expr)
  (Add Expr Expr)
  (Div Expr Expr))

(ruleset constant_prop)

(rule ((= l (Const x))
       (= r (Const y))
       (= p (Add l r)))
      ((union p (Const (+ x y))))
      :ruleset constant_prop
      :name "AddPat")

(rule ((= l (Const x))
       (= r (Const y))
       (= p (Sub l r)))
      ((union p (Const (- x y))))
      :ruleset constant_prop
      :name "SubPat")

(rule ((= l (Const x))
       (= r (Const y))
       (= p (Mul l r)))
      ((union p (Const (* x y))))
      :ruleset constant_prop
      :name "MulPat")

(rule ((= l (Const x))
       (= r (Const y))
       (= p (Div l r)))
      ((union p (Const (/ x y))))
      :ruleset constant_prop
      :name "DivPat")

(rule ((= l (Const x))
       (= r (Const y))
       (= m (Mul l r))
       (= c (Const z))
       (= p (Add m c)))
      ((union p (Const (+ (* x y) z))))
      :ruleset constant_prop
      :name "AddMulConstPat")

(let $mul (Mul (Const 3) (Const 2)))
(let $expr (Add $mul (Const 4)))
(let $expected (Const 10))
(let $expected_mul (Const 6))

(run constant_prop 10)
"#
}

fn run_rust_api_program() -> Result<(), Error> {
    let mul: Expr<DebugTxProof, MulTy> = Mul::new(&Const::new(3), &Const::new(2));
    let expr: Expr<DebugTxProof, _> = Add::new(&mul, &Const::new(4));
    expr.commit();

    let expected: Expr<DebugTxProof, ConstTy> = Const::new(10);
    expected.commit();

    let expected_mul: Expr<DebugTxProof, ConstTy> = Const::new(6);
    expected_mul.commit();

    {
        let egraph = DebugTxProof::sgl().egraph.lock().unwrap();
        dump_egraph(
            "rust-api-after-commits",
            &egraph,
            "target/debug_const_prop_tables_rust_after_commits.txt",
        )?;
    }

    let ruleset = DebugTxProof::new_ruleset("constant_prop");
    DebugTxProof::add_rule(
        "DebugConstPropAddPat",
        ruleset,
        || {
            let l = Const::query();
            let r = Const::query();
            let p = Add::query(&l, &r);
            #[eggplant::pat_vars_catch]
            struct DebugConstPropAddPat {
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
    DebugTxProof::add_rule(
        "DebugConstPropMulPat",
        ruleset,
        || {
            let l = Const::query();
            let r = Const::query();
            let p = Mul::query(&l, &r);
            #[eggplant::pat_vars_catch]
            struct DebugConstPropMulPat {
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

    {
        let egraph = DebugTxProof::sgl().egraph.lock().unwrap();
        dump_egraph(
            "rust-api-after-rules",
            &egraph,
            "target/debug_const_prop_tables_rust_after_rules.txt",
        )?;
    }

    let _ = DebugTxProof::run_ruleset(ruleset, RunConfig::Sat);
    Ok(())
}

fn dump_egraph(label: &str, egraph: &EGraph, path: impl AsRef<Path>) -> Result<(), Error> {
    let mut out = String::new();
    writeln!(out, "label: {label}").unwrap();

    let mut selected = BTreeSet::new();
    for logical_name in ["Const", "Mul", "Add", "Sub", "Div"] {
        selected.insert(logical_name.to_owned());
        if let Ok(view_name) = egraph.proof_view_name(logical_name) {
            selected.insert(view_name);
        }
    }
    if let Ok(uf_name) = egraph.uf_proof_name("Expr") {
        selected.insert(uf_name);
    }

    let mut all_names = egraph.get_function_names();
    all_names.sort();
    for name in all_names {
        let Some(function) = egraph.get_function(&name) else {
            continue;
        };
        let schema = function.schema();
        let output_sort = schema.output.name();
        let input_has_interesting_sort = schema.input.iter().any(|sort| {
            let sort_name = sort.name();
            sort_name.contains("Expr")
                || sort_name.contains("Proof")
                || sort_name.contains("Ast")
                || sort_name.contains("UFPair")
        });
        let name_interesting = [
            "Expr", "Const", "Mul", "Add", "Sub", "Div", "Proof", "UF", "View", "Fiat", "Rule",
            "Merge", "Trans", "Sym", "Congr", "PCons", "PNil", "commit",
        ]
        .iter()
        .any(|needle| name.contains(needle));

        if name_interesting
            || input_has_interesting_sort
            || output_sort.contains("Expr")
            || output_sort.contains("Proof")
            || output_sort.contains("Ast")
            || output_sort.contains("UFPair")
        {
            selected.insert(name);
        }
    }

    for name in selected {
        let Some(function) = egraph.get_function(&name) else {
            continue;
        };
        let schema = function.schema();
        let input_sorts = schema
            .input
            .iter()
            .map(|sort| sort.name().to_owned())
            .collect::<Vec<_>>();
        let output_sort = schema.output.name().to_owned();
        let mut rows = Vec::new();
        egraph.function_for_each(&name, |row| {
            rows.push((row.vals.to_vec(), row.subsumed));
        })?;
        if rows.is_empty() {
            continue;
        }

        writeln!(
            out,
            "\n## {name}\ninputs: ({}) -> {output_sort}\nrows: {}",
            input_sorts.join(" "),
            rows.len()
        )
        .unwrap();

        for (idx, (vals, subsumed)) in rows.iter().take(200).enumerate() {
            let mut formatted = Vec::new();
            for (pos, value) in vals.iter().enumerate() {
                if let Some(sort) = schema.get_by_pos(pos) {
                    formatted.push(format_value(egraph, *value, sort));
                } else {
                    formatted.push(format!("{value:?}:<unknown>"));
                }
            }
            writeln!(
                out,
                "  [{idx:03}] {}{}",
                formatted.join(", "),
                if *subsumed { " ; subsumed" } else { "" }
            )
            .unwrap();
        }
        if rows.len() > 200 {
            writeln!(out, "  ... {} more rows", rows.len() - 200).unwrap();
        }
    }

    let path = path.as_ref();
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).map_err(|err| {
            Error::BackendError(format!("failed to create {}: {err}", parent.display()))
        })?;
    }
    fs::write(path, out)
        .map_err(|err| Error::BackendError(format!("failed to write {}: {err}", path.display())))?;
    Ok(())
}

fn format_value(egraph: &EGraph, value: Value, sort: &ArcSort) -> String {
    let sort_name = sort.name();
    let rendered = match sort_name {
        "i64" => egraph.value_to_base::<i64>(value).to_string(),
        "bool" => egraph.value_to_base::<bool>(value).to_string(),
        "String" => format!("{:?}", egraph.value_to_base::<egglog::sort::S>(value)),
        "Unit" | "()" => "()".to_owned(),
        _ => format!("{value:?}"),
    };
    format!("{rendered}:{sort_name}")
}
