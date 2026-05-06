use eggplant::prelude::*;

tx_rx_vt_pr_pf!(MacroTxProof, MacroPatRec);

#[test]
fn tx_rx_vt_pr_pf_macro_builds_a_proof_tx_singleton() {
    let _ = MacroTxProof::sgl();
    let _ = MacroPatRec::sgl();
    let egraph = MacroTxProof::egraph();
    assert!(egraph.lock().unwrap().are_proofs_enabled());
}
