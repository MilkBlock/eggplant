use eggplant::{Commit, EgglogNode, LocateVersion, SingletonGetter, basic_tx_vt};
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

#[eggplant_func(output=Root)]
struct Selected {}

fn main() {
    env_logger::init();
    let node1 = Cons::new_value(1, &Cons::<MyTx>::new_end());
    let mut node2 = Cons::new_value(2, &node1);
    let _node3 = Cons::new_value(3, &node2);
    let mut root = Root::new_v(&VecCon::new(vec![&node2]));
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
