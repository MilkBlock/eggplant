use eggplant::{PatRecSgl, RuleRunnerSgl, SingletonGetter, Tx, TxCommand, tx_rx_vt_pr};

tx_rx_vt_pr!(MyTx, MyPatRec);

#[eggplant::func(output = i64)]
struct F {
    x: i64,
}

#[eggplant::patttern_vars]
struct Unit<PS: PatRecSgl> {}

fn unit_pat<PS: PatRecSgl>() -> Unit<PS> {
    Unit::new()
}

#[divan::bench(sample_count = 10)]
fn eggplant_insert_1000_func() {
    // Minimal reset: clear the egraph and re-register types before each sample.
    // This keeps the benchmark stable across repeated runs without requiring
    // new singleton instances.
    {
        let tx = MyTx::sgl();
        *tx.egraph.lock().unwrap() = egglog::EGraph::default();
        tx.map.clear();
        tx.staged_set_map.clear();
        tx.staged_new_map.lock().unwrap().clear();
    }
    for def in eggplant::EgglogTypeRegistry::collect_type_defs() {
        MyTx::sgl().send(TxCommand::NativeCommand { command: def });
    }

    let ruleset = MyTx::new_rule_set("insert_1000_func_seed");
    MyTx::add_rule(
        ruleset,
        unit_pat,
        |ctx, _values| {
            for i in 0_i64..1000 {
                // Insert a row into a user-defined function table: F(i) = i+1.
                // This targets rust_rule's `ctx.insert/lookup` hot path.
                let x = ctx.rule_ctx.base_to_value(i);
                let y = ctx.rule_ctx.base_to_value(i + 1);
                ctx.rule_ctx.insert("F", [x, y].into_iter());
            }
            Some(())
        },
    );
    MyTx::run_ruleset(ruleset, eggplant::RunConfig::None);
}

fn main() {
    divan::main();
}
