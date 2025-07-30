use eggplant::basic_tx_no_vt;
use eggplant::prelude::*;
use std::path::PathBuf;

#[eggplant::ty]
enum A {
    ACon { b: B },
}

#[eggplant::ty]
enum B {
    BCon { a: A },
    Empty {},
}

/// NB: this should PANIC because Cycle is not allowed.
/// Only DAG is supported.
fn main() {
    env_logger::init();
    let mut a = ACon::new(&B::<MyTx>::new_empty());
    a.set_b(&BCon::new(&a));
    MyTx::sgl().to_dot(PathBuf::from("egraph"));
}

basic_tx_no_vt!(MyTx);
