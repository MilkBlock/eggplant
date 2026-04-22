use egglog::EGraph;
use eggplant::prelude::*;

pub struct MyTxProof {
    tx: eggplant::instances::tx_rx_vt_pr::TxRxVTPR,
}

impl SingletonGetter for MyTxProof {
    type RetTy = eggplant::instances::tx_rx_vt_pr::TxRxVTPR;
    fn sgl() -> &'static eggplant::instances::tx_rx_vt_pr::TxRxVTPR {
        static INSTANCE: std::sync::OnceLock<MyTxProof> = std::sync::OnceLock::new();
        &INSTANCE
            .get_or_init(|| MyTxProof {
                tx: eggplant::instances::tx_rx_vt_pr::TxRxVTPR::new_with_proof(),
            })
            .tx
    }
}

impl eggplant::wrap::NonPatRecSgl for MyTxProof {
    fn egraph() -> std::sync::Arc<std::sync::Mutex<EGraph>> {
        <Self as eggplant::wrap::NonPatRecSgl>::egraph()
    }
}

eggplant::basic_patttern_recorder!(MyPatRec);
impl eggplant::wrap::WithPatRecSgl for MyTxProof {
    type PatRecSgl = MyPatRec;
}
impl eggplant::wrap::WithRxSgl for MyPatRec {
    type RxSgl = MyTxProof;
}

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
