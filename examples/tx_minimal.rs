use eggplant::basic_tx_minimal;
use eggplant::func;
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
#[func(output= Root)]
struct F {}

fn main() {
    env_logger::init();
    let node1 = Value::new(3, &End::new());
    let node2 = Value::new(2, &node1);
    let node3 = Value::new(1, &node2);
    let _root = V::new(&VecCon::new(vec![&node2]));
    let root = V::<MyTx>::new(&VecCon::new(vec![&node3]));
    F::set((), &root);
    MyTx::sgl().to_dot("egraph.dot");
}

basic_tx_minimal!(MyTx);
