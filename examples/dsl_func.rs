use eggplant::{Commit, SingletonGetter, basic_tx_rx_vt, eggplant_func, eggplant_ty};
// #[eggplant_ty]
// enum Expr{
//     Add { a:Box<Expr>, b:Box<Expr>},
//     Const { s:i64}
// }

#[eggplant_ty]
enum Expr {
    Add { v: Box<Expr>, con: Box<Expr> },
    Const { s: i64 },
}

#[eggplant_func(output=Expr)]
struct LeadTo {
    e: Expr,
}

fn main() {
    let a = Const::new(3);
    let b = Const::new(5);
    let add = Add::<MyTx>::new(&a, &b);
    let c = Const::new(8);
    add.commit();
    LeadTo::set(&add, &c);
    MyTx::sgl().wag_to_dot("dsl_func.dot".into());
    MyTx::sgl().egraph_to_dot("dsl_func_egraph.dot".into());
}

basic_tx_rx_vt!(MyTx);
