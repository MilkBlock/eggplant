use eggplant::{prelude::*, tx_rx_vt_pr};

#[eggplant::dsl]
enum ExtractExpr {
    Leaf { n: i64 },
    Wrap { inner: ExtractExpr },
}

tx_rx_vt_pr!(ExtractTx, ExtractPatRec);

fn main() {
    let _ = env_logger::try_init();

    let leaf = Leaf::<ExtractTx>::new(7);
    leaf.commit();
    let wrapped = Wrap::<ExtractTx>::new(&leaf);
    wrapped.commit();

    let (rendered, cost) =
        ExtractTx::extract_node_to_string(&wrapped).expect("default extraction should succeed");

    println!("default extracted term: {rendered}");
    println!("default extracted cost: {cost}");
}
