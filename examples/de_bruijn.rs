use eggplant::prelude::*;
use eggplant::slotted_tx_rx_vt_pr;
use eggplant::wrap::NodeDropperSgl;

#[eggplant::slotted_dsl]
pub enum Expr {
    Var {},
    Tri { t1: Expr, t2: Expr, t3: Expr },
}

slotted_tx_rx_vt_pr!(MyTx, MyPatRec);
fn main() {
    env_logger::init();
    // let expr: Expr<MyTx, _> = Add::new(&Mul::new(&Var::new(), &Var::new()), &Const::new(4));
    let expr: Expr<MyTx, _> = Tri::new(&Var::new(), &Var::new(), &Var::new());
    expr.commit();

    let ruleset = MyTx::new_ruleset("constant_prop");
    MyTx::add_rule(
        stringify!("add commutative"),
        ruleset,
        || {
            let x = Var::query_slot("x");
            let y = Var::query_slot("y");
            let z = Var::query_slot("z");
            let tri = Tri::query(&x, &y, &z);
            #[eggplant::slotted_pat_vars_catch]
            struct AddPat {
                x: Var,
                y: Var,
                z: Var,
                tri: Tri,
            }
        },
        |ctx, pat| {
            println!("{:?}", pat);
            // ctx.remove_add(&pat.y, &pat.x, ctx.devalue(pat.add.0.ver));
            let tri_l_r_exchange = ctx.insert_tri(&pat.y, &pat.x, &pat.z);
            // context should be passed from query to action
            println!("{:#?}", tri_l_r_exchange.1.tensor());
            ctx.union(&pat.tri, tri_l_r_exchange);
            println!("end")
        },
    );
    println!("first");
    let report = MyTx::run_ruleset(ruleset, RunConfig::Once);
    println!("second");
    let report = MyTx::run_ruleset(ruleset, RunConfig::Once);
    println!("third");
    let report = MyTx::run_ruleset(ruleset, RunConfig::Once);
    println!("{:#?}", report);
    MyTx::table_view();

    // let c: Expr<MyTx, ConstTy> = Const::new(10);
    // c.commit();
    // if MyTx::canonical_raw(&expr) != MyTx::canonical_raw(&c) {
    //     panic!("should infer to 10");
    // }

    // expr
    //     .pull();
    MyTx::egraph_to_dot("egraph.dot");
    MyTx::wag_to_dot("wag.dot");
    // paterns to dot
    MyPatRec::sgl().pats_to_dot("pats.dot");
}

impl<T: TxSgl + NodeDropperSgl + SlottedPatRecSgl> QuerySlot for Var<T> {
    fn query_slot(var_id: SlotVarID) -> Self {
        let node = Var::query();
        T::on_new_query_slot(&node, var_id);
        node
    }
}
