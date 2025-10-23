use eggplant::prelude::*;
use eggplant::tx_rx_vt_pr;

tx_rx_vt_pr!(MyTx, MyPatRec);

#[eggplant::func(output=i64)]
struct Fib {
    x: i64,
}
fn main() {
    let ruleset = MyTx::new_ruleset("hello");
    MyTx::add_rule(
        "hello",
        ruleset,
        || {
            #[eggplant::pat_vars_catch]
            struct Unit {}
        },
        |ctx, _pat| {
            ctx.set_fib(1, 1);
            ctx.set_fib(2, 3);
        },
    );
    Fib::<MyTx>::get(&2);
    MyTx::run_ruleset(ruleset, RunConfig::Once);
    MyTx::egraph_to_dot("egraph.dot");
}
