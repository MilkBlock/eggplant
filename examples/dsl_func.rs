use eggplant::{basic_tx_rx_vt, prelude::*};

#[eggplant::dsl]
enum Expr {
    Add { v: Expr, con: Expr },

    Const { s: i64 },
}

#[eggplant::func(output=Expr)]
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
    MyTx::wag_to_dot("dsl_func.dot");
    MyTx::egraph_to_dot("dsl_func_egraph.dot");
}

basic_tx_rx_vt!(MyTx);
