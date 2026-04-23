use eggplant::prelude::*;
use eggplant::tx_rx_vt_pr;

tx_rx_vt_pr!(MyTx, MyPatRec);

#[eggplant::dsl]
enum Expr {
    Add { l: Expr, r: Expr },
    Const { n: i64 },
}

#[eggplant::func(output = Expr)]
struct LeadTo {
    e: Expr,
}

fn main() {
    env_logger::init();

    // We'll seed a function-table row in a rule callback (tx_rx_vt_pr doesn't support `LeadTo::set` yet):
    //   LeadTo(Add(1,2)) = Const(3)
    let one = Const::new(1);
    let two = Const::new(2);
    let three = Const::new(3);
    let add_1_2 = Add::<MyTx>::new(&one, &two);
    add_1_2.commit();
    three.commit();

    // Also create a key that has no LeadTo row, to demo try_read_*.
    let add_1_3 = Add::<MyTx>::new(&one, &three);
    add_1_3.commit();

    // In rule callbacks, func ctx helpers take `Insertable<Expr<(), _>>`.
    // A practical way to pass an existing node is to capture its canonical `egglog::Value` handle.
    let add_1_2_key: Value<Expr<(), AddTy>> = Value::new(MyTx::canonical_raw(&add_1_2));
    let add_1_3_key: Value<Expr<(), AddTy>> = Value::new(MyTx::canonical_raw(&add_1_3));
    let three_key: Value<Expr<(), ConstTy>> = Value::new(MyTx::canonical_raw(&three));

    let seed_ruleset = MyTx::new_ruleset("seed_complex_output");
    MyTx::add_rule(
        "seed_complex_output",
        seed_ruleset,
        || {
            #[eggplant::pat_vars_catch]
            struct Unit {}
        },
        move |ctx, _pat| {
            ctx.set_lead_to(add_1_2_key, three_key);
        },
    );

    let ruleset = MyTx::new_ruleset("read_complex_output");
    MyTx::add_rule(
        "read_complex_output",
        ruleset,
        || {
            #[eggplant::pat_vars_catch]
            struct Unit {}
        },
        move |ctx, _pat| {
            // Complex-output read: returns Value<Expr> (opaque handle), not a concrete Rust AST.
            let out_v = ctx.read_lead_to(add_1_2_key);
            println!("LeadTo(Add(1,2)) -> {:?}", out_v);

            // Optional: non-panicking read for missing rows.
            let missing = ctx.try_read_lead_to(add_1_3_key);
            println!("try_read LeadTo(Add(1,3)) -> {}", missing.is_some());
        },
    );

    MyTx::run_ruleset(seed_ruleset, RunConfig::Once);
    MyTx::run_ruleset(ruleset, RunConfig::Once);

    MyTx::egraph_to_dot("func_read_complex_egraph.dot");
}
