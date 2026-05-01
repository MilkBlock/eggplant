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
    let one: Expr<MyTx, ConstTy> = Const::new(1);
    let two: Expr<MyTx, ConstTy> = Const::new(2);
    let three: Expr<MyTx, ConstTy> = Const::new(3);
    let add_1_2 = Add::<MyTx>::new(&one, &two);
    add_1_2.commit();
    three.commit();

    // In rule callbacks, func ctx helpers take `Insertable<Expr<(), _>>`.
    // A practical way to pass an existing node is to capture its canonical `egglog::Value` handle.
    let add_1_2_key: Value<Expr<(), AddTy>> = Value::new(MyTx::canonical_raw(&add_1_2));
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
            let one = Const::query().n(&1);
            let two = Const::query().n(&2);
            let add_1_2 = Add::query(&one, &two);
            let out = LeadTo::query(&add_1_2);
            #[eggplant::pat_vars]
            struct Pat {
                out: Expr,
            }
            Pat::new(out)
        },
        move |_ctx, pat| {
            println!("LeadTo(Add(1,2)) -> {:?}", pat.out);
        },
    );

    MyTx::run_ruleset(seed_ruleset, RunConfig::Once);
    MyTx::run_ruleset(ruleset, RunConfig::Once);

    MyTx::egraph_to_dot("func_read_complex_egraph.dot");
}
