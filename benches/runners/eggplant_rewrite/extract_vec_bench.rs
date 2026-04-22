use eggplant::prelude::*;
use eggplant::tx_rx_vt_pr;

tx_rx_vt_pr!(MyTxExtract, MyPatRecExtract);

include!("../generated/extract_vec_bench.rs");

pub fn bench() {
    MyTxExtract::reset_for_bench();

    let large_expr = build_large_expr();
    large_expr.commit();
    large_expr.pull();

    let egraph = MyTxExtract::egraph();
    let egraph = egraph.lock().unwrap();
    egraph.serialize(egglog::SerializeConfig::default());
}
