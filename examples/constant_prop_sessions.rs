use eggplant::{prelude::*, tx_rx_vt_pr};

#[eggplant::dsl]
pub enum Expr {
    #[typst("{num}")]
    #[precedence(100)]
    Const { num: i64 },
    #[typst("{l} * {r}")]
    #[precedence(60)]
    Mul { l: Expr, r: Expr },
    #[typst("{l} + {r}")]
    #[precedence(50)]
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
        let ruleset = MyTx::new_ruleset("constant_prop_sessions");
        prop!("AddPat", Add, +, AddPat, ruleset);
        prop!("MulPat", Mul, *, MulPat, ruleset);
        ruleset
    })
}

fn current_session_has_const(needle: i64) -> bool {
    let egraph = MyTx::egraph();
    let egraph = egraph.lock().unwrap();

    egraph.function_rows("Const").into_iter().any(|row| {
        !row.subsumed
            && row
                .vals
                .first()
                .is_some_and(|value| egraph.value_to_base::<i64>(*value) == needle)
    })
}

fn canonical_eq_const(lhs: &Expr<MyTx>, rhs_const: i64) -> bool {
    let rhs: Expr<MyTx, ConstTy> = Const::new(rhs_const);
    rhs.commit();
    MyTx::canonical_raw(lhs) == MyTx::canonical_raw(&rhs)
}

fn fold_mul_add_expr(session: &Session<MyTx>, lhs: i64, rhs: i64, addend: i64) -> i64 {
    session.run(|| {
        let expr: Expr<MyTx, AddTy> = Add::new(
            &Mul::new(&Const::new(lhs), &Const::new(rhs)),
            &Const::new(addend),
        );
        expr.commit();

        let ruleset = register_constant_prop_rules();
        let _ = MyTx::run_ruleset(ruleset, RunConfig::Sat);

        let expected = lhs * rhs + addend;
        assert!(canonical_eq_const(&expr, expected));
        expected
    })
}

fn session_has_const(session: &Session<MyTx>, needle: i64) -> bool {
    session.run(|| current_session_has_const(needle))
}

fn main() {
    env_logger::init();

    let left = MyTx::new_session();
    let right = MyTx::new_session();

    assert_eq!(fold_mul_add_expr(&left, 3, 2, 4), 10);
    assert_eq!(fold_mul_add_expr(&right, 5, 5, 1), 26);
    assert!(session_has_const(&left, 10));
    assert!(!session_has_const(&left, 26));
    assert!(session_has_const(&right, 26));
    assert!(!session_has_const(&right, 10));

    println!("constant_prop_sessions passed");
}
