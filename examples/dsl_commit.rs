use eggplant::Commit;
use eggplant::SingletonGetter;
use eggplant::basic_tx_vt;
use std::{path::PathBuf, str::FromStr};

#[eggplant::ty]
enum Compare {
    Less { v1: Var, v2: Var },
}
#[eggplant::ty]
enum Var {
    VarItem { num: i64 },
    Expr { eq: Compare },
}

fn main() {
    env_logger::init();
    let mut v0 = VarItem::new(1);
    let mut v1 = VarItem::new(2);
    let eq0 = Less::<MyTx>::new(&v0, &v1);
    eq0.commit();
    MyTx::sgl().to_dot(PathBuf::from_str("egraph0").unwrap());

    v1.set_num(4).stage();
    eq0.commit();
    MyTx::sgl().to_dot(PathBuf::from_str("egraph1").unwrap());

    v0.set_num(4).stage();
    eq0.commit();
    MyTx::sgl().to_dot(PathBuf::from_str("egraph2").unwrap());
}

basic_tx_vt!(MyTx);
