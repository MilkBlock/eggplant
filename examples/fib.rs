use eggplant::prelude::*;
use eggplant::tx_rx_vt_pr;

tx_rx_vt_pr!(MyTx, MyPatRec);

#[eggplant::func(output=i64)]
struct Fib {
    x: i64,
}
fn main() {
    let seed_ruleset = MyTx::new_ruleset("fib_seed");
    MyTx::add_rule(
        "fib_seed",
        seed_ruleset,
        || {
            #[eggplant::pat_vars_catch]
            struct Unit {}
        },
        |ctx, _pat| {
            ctx.set_fib(1, 1);
            ctx.set_fib(2, 3);
        },
    );

    let read_ruleset = MyTx::new_ruleset("fib_read");
    MyTx::add_rule(
        "fib_read",
        read_ruleset,
        || {
            #[eggplant::pat_vars_catch]
            struct Unit {}
        },
        |ctx, _pat| {
            let fib_val = ctx.read_fib(2);
            println!("{}", fib_val);
        },
    );

    MyTx::run_ruleset(seed_ruleset, RunConfig::Once);
    MyTx::run_ruleset(read_ruleset, RunConfig::Once);
    Fib::<MyTx>::get(&2);
    MyTx::egraph_to_dot("egraph.dot");
}
