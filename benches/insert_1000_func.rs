use eggplant::prelude::*;
use eggplant::tx_rx_vt_pr;

tx_rx_vt_pr!(MyTx, MyPatRec);

#[eggplant::func(output = i64, no_merge)]
struct f {
    x: i64,
}

#[divan::bench(sample_count = 10)]
fn eggplant_insert_1000_func() {
    MyTx::reset_for_bench();

    let seed = MyTx::new_ruleset("insert_1000_func_seed");
    MyTx::add_rule(
        "insert_1000_func_seed",
        seed,
        || {
            #[eggplant::pat_vars_catch]
            struct Unit {}
        },
        |ctx, _pat| {
            for x in 0_i64..1000 {
                ctx.set_f(x, x);
            }
        },
    );

    MyTx::run_ruleset(seed, RunConfig::Once);

    // Sanity: ensure the last insert is visible after the callback completes.
    assert_eq!(f::<MyTx>::get(&999), 999);
}

fn main() {
    divan::main();
}
