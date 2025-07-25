use eggplant::{EgglogNode, SingletonGetter, basic_tx_no_vt, func};

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

#[func(output= Root)]
struct F {}

fn main() {
    env_logger::init();
    let node1 = Value::new(3, &End::<MyTx>::new());
    let mut node2 = Value::new(2, &node1);
    let node3 = Value::new(1, &node2);
    let _root = V::new(&VecCon::new(vec![&node2]));
    println!("node2's current version is {}", node2.cur_sym());
    node2.set_v(4);
    println!("node2's current version is {}", node2.cur_sym());
    let root = V::new(&VecCon::new(vec![&node3]));
    node2.set_v(6);
    println!("node2's current version is {}", node2.cur_sym());
    F::set((), &root);
    MyTx::sgl().to_dot("egraph.dot".into());
}

basic_tx_no_vt!(MyTx);
