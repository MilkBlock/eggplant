use eggplant::{SingletonGetter, basic_tx_rx_vt, eggplant_ty};

#[eggplant_ty]
enum Node {
    Value { v: i64 },
}
#[eggplant_ty]
enum _Root {
    Root { node: Node },
}

basic_tx_rx_vt!(MyTx);

// #[eggplant_pattern]
// fn xxx_rule() -> (Node,Node,Root){
//    let n= Node::place_holder();
//    let b= Node::place_holder_xxxx();
//    let a = Root::new_nod(node);
//     // PlaceHolder variant once more
//    let r= Root::new(n).with_predicate(|xxx|xxxx)
//    (n,b,r)
// }

fn main() {
    env_logger::init();
    let _root = Root::<MyTx>::new(&Value::new(3));

    MyTx::sgl().egraph_to_dot("egraph.dot".into());
}
