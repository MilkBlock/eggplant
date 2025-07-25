use eggplant::Commit;
use eggplant::EgglogNode;
use eggplant::SingletonGetter;
use eggplant::basic_tx_vt;
use std::time::Instant;

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
    RootCons { v: VecCon },
}

fn main() {
    env_logger::init();
    let a = Instant::now();

    let mut count = 0;
    let root = loop {
        let node1 = Value::new(5, &End::new());
        let node2 = Value::new(5 + 1, &node1);
        let node3 = Value::new(2, &node2);
        let root = RootCons::<MyTx>::new(&VecCon::new(vec![&node1, &node2, &node3]));
        let _m = root.cur_sym();
        if count == 99 {
            break root;
        }
        count += 1;
    };
    root.commit();
    println!("push elpased: {:?}", a.elapsed());
    MyTx::sgl().to_dot("egraph.dot".into());
}

basic_tx_vt!(MyTx);
