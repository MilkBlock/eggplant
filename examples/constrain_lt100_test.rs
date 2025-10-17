use eggplant::prelude::*;
use eggplant::tx_rx_vt_pr;

#[eggplant::dsl]
pub enum Expr {
    Const { num: i64 },
    Add { l: Expr, r: Expr },
}

tx_rx_vt_pr!(MyTx, MyPatRec);

fn main() {
    env_logger::init();

    // Create some test expressions: 30 + 40 = 70 (should satisfy < 100 constraint)
    let expr1: Expr<MyTx, _> = Add::new(&Const::new(30), &Const::new(40));
    expr1.commit();

    // Create another test expression: 120 + 80 = 200 (at least one operand >= 100, should not satisfy constraint)
    let expr2: Expr<MyTx, _> = Add::new(&Const::new(120), &Const::new(80));
    expr2.commit();

    let ruleset = MyTx::new_ruleset("constant_prop_with_constraint");

    // Define addition pattern with constraint less than 100
    #[eggplant::pat_vars]
    struct AddPat {
        l: Const,
        r: Const,
        p: Add,
    }

    MyTx::add_rule(
        stringify!(AddPat),
        ruleset,
        || {
            let l = Const::query();
            let r = Const::query();
            let p = Add::query(&l, &r);
            let l_h = l.handle_num();
            let r_h = r.handle_num();
            // Constraint: both operands are less than 100
            AddPat::new(l, r, p)
                .assert(l_h.lt(&100))
                .assert(r_h.lt(&100))
        },
        |ctx, pat| {
            let left_val = ctx.devalue(pat.l.num);
            let right_val = ctx.devalue(pat.r.num);
            let sum = left_val + right_val;

            println!(
                "Executing constant folding: {} + {} = {}",
                left_val, right_val, sum
            );
            let op_value = ctx.insert_const(sum);
            ctx.union(pat.p, op_value);
        },
    );

    println!("Starting to run ruleset...");
    let report = MyTx::run_ruleset(ruleset, RunConfig::Sat);
    println!("Ruleset execution report: {:#?}", report);

    println!("\nTable view:");
    MyTx::table_view();

    // Pull results
    expr1.pull();
    expr2.pull();

    // Generate graph files
    MyTx::egraph_to_dot("egraph_lt100.dot");
    MyTx::wag_to_dot("wag_lt100.dot");
    MyPatRec::sgl().pats_to_dot("pats_lt100.dot");

    println!("\nTest completed!");
    println!("- expr1 (30 + 40): should be constant folded to 70, because both operands are < 100");
    println!(
        "- expr2 (120 + 80): should remain as addition expression, because at least one operand is >= 100"
    );
}
