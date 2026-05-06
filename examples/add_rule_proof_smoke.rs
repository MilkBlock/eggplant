use eggplant::prelude::*;

tx_rx_vt_pr_pf!(MyTxProof, MyPatRec);

fn main() {
    let ruleset = MyTxProof::new_ruleset("proof_smoke");
    MyTxProof::add_rule(
        "proof_smoke_rule",
        ruleset,
        || {
            #[eggplant::pat_vars_catch]
            struct Unit {}
        },
        |_ctx, _pat| {},
    );
    let _report = MyTxProof::run_ruleset(ruleset, RunConfig::Once);
    println!("add_rule_proof_smoke passed");
}
