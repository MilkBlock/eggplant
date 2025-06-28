use std::{path::PathBuf, str::FromStr};

use eggplant::Commit;
use eggplant::SingletonGetter;
use eggplant::basic_tx_vt;
use eggplant::eggplant_ty;

#[eggplant_ty]
enum Eq {
    Less { v1: Var, v2: Var },
}
#[eggplant_ty]
enum Var {
    VarItem { num: i64 },
}

fn main() {
    env_logger::init();
    let _less0 = Less::<MyTx>::new(&VarItem::new(3), &VarItem::new(4));
    let less1 = Less::<MyTx>::new(&VarItem::new(4), &VarItem::new(5));
    let less2 = Less::<MyTx>::new(&VarItem::new(3),&VarItem::new(5));
    less2.commit();
    less1.commit();

    MyTx::sgl().to_dot(PathBuf::from_str("egraph").unwrap());
}

basic_tx_vt!(MyTx);
