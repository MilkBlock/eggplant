use eggplant::{SingletonGetter, basic_tx_minimal};
use eggplant::{eggplant_func, eggplant_ty};

#[eggplant_ty]
enum Cons {
    Value { v: i64, con: Box<Cons> },
    End {},
}

#[eggplant_ty]
struct VecCon {
    v: Vec<Cons>,
}

#[eggplant_ty]
enum Root {
    V { v: VecCon },
}
#[eggplant_func(output= Root)]
struct F {}

fn main() {
    env_logger::init();
    let node1 = Value::new(3, &End::new());
    let node2 = Value::new(2, &node1);
    let node3 = Value::new(1, &node2);
    let _root = V::new(&VecCon::new(vec![&node2]));
    let root = V::<MyTx>::new(&VecCon::new(vec![&node3]));
    F::set((), &root);
    MyTx::sgl().to_dot("egraph.dot".into());
}

basic_tx_minimal!(MyTx);
