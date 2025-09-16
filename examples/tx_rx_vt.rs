use eggplant::basic_tx_rx_vt;
use eggplant::func;
use eggplant::prelude::*;

#[eggplant::dsl]
enum Cons {
    Value { v: i64, con: Cons },
    End {},
}

#[eggplant::dsl]
struct VecCon {
    v: Vec<Cons>,
}

#[eggplant::dsl]
enum Root {
    V { v: VecCon },
}

#[func(output=Root)]
struct Selected {}

fn main() {
    env_logger::init();
    let end = End::new();
    let node1 = Value::new(1, &end);
    let mut node2 = Value::new(2, &node1);
    let _node3 = Value::new(3, &node2);
    let mut root = V::new(&VecCon::new(vec![&node2]));
    println!("node2's current version is {}", node2.cur_sym());
    // node2.set_v(4).stage();
    root.commit();

    println!("node2's current version is {}", node2.cur_sym());
    node2.set_v(6).stage();
    root.commit();
    root.pull();
    // let old_root = root.clone();
    Selected::<MyTx>::set((), &root);
    root.locate_latest();
    Selected::<MyTx>::set((), &root);
    // MyTx::on_union(&root, &old_root);
    // MyTx::on_union(&node2, &end);
    // MyTx::on_union(&node1, &end);
    MyTx::egraph_to_dot("egraph0.dot".into());
    MyTx::wag_to_dot("wag0.dot".into());
    let selected = Selected::<MyTx>::get(());
    println!("selected is {:?}", selected.cur_sym());
    MyTx::egraph_to_dot("egraph1.dot".into());
    MyTx::wag_to_dot("wag1.dot".into());
}

basic_tx_rx_vt!(MyTx);
