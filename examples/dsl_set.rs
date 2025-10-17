use eggplant::basic_tx_no_vt;
use eggplant::prelude::*;

#[eggplant::dsl]
enum Cons {
    Value { v: i64, con: Cons },
    End {},
}

#[eggplant::dsl]
struct VecCon {
    v: VecContainer<Cons>,
}

#[eggplant::dsl]
enum Root {
    V { v: VecCon },
}

fn main() {
    env_logger::init();
    let node1 = Value::new(3, &Cons::<MyTx>::new_end());
    let mut node2 = Value::new(2, &node1);
    let node3 = Value::new(1, &node2);
    let _root = V::new(&VecCon::new(vec![&node1, &node2, &node3]));
    node2.set_v(5);
    MyTx::sgl().to_dot("egraph.dot");
}

basic_tx_no_vt!(MyTx);
