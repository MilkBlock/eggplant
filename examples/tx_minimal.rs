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
    let node1 = Cons::new_value(3, &Cons::new_end());
    let node2 = Cons::new_value(2, &node1);
    let node3 = Cons::new_value(1, &node2);
    let _root = Root::new_v(&VecCon::new(vec![&node2]));
    let root = Root::<MyTx>::new_v(&VecCon::new(vec![&node3]));
    F::set((), &root);
    MyTx::sgl().to_dot("egraph.dot".into());
}

basic_tx_minimal!(MyTx);
