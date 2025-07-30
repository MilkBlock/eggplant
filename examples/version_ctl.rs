use eggplant::basic_tx_vt;
use eggplant::func;
use eggplant::prelude::*;

#[eggplant::ty]
enum Cons {
    Value { v: i64, con: Cons },
    End {},
}

#[eggplant::ty]
struct VecCon {
    v: Vec<Cons>,
}

#[eggplant::ty]
enum Root {
    V { v: VecCon },
}

#[func(output=Root)]
struct Selected {}

fn main() {
    env_logger::init();
    let node1 = Value::new(1, &Cons::<MyTx>::new_end());
    let mut node2 = Value::new(2, &node1);
    let _node3 = Value::new(3, &node2);
    let mut root = V::new(&VecCon::new(vec![&node2]));
    println!("node2's current version is {}", node2.cur_sym());
    node2.set_v(4).stage();
    root.commit();

    println!("node2's current version is {}", node2.cur_sym());
    node2.set_v(6).stage();
    root.commit();
    Selected::<MyTx>::set((), &root);
    MyTx::sgl().to_dot("egraph.dot".into());
    root.locate_latest();
    Selected::<MyTx>::set((), &root);
    MyTx::sgl().to_dot("egraph1.dot".into());
}

basic_tx_vt!(MyTx);
