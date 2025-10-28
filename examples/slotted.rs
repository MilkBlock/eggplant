use eggplant::prelude::*;
use eggplant::slotted_tx_rx_vt_pr;
use eggplant::wrap::NodeDropperSgl;
use eggplant::wrap::RuleCtxHook;
#[eggplant::slotted_dsl]
pub enum Expr {
    Var {},
    Const { num: i64 },
    Mul { l: Expr, r: Expr },
    Add { l: Expr, r: Expr },
}

slotted_tx_rx_vt_pr!(MyTx, MyPatRec);
fn main() {
    env_logger::init();
    let expr: Expr<MyTx, _> = Add::new(&Mul::new(&Const::new(3), &Const::new(2)), &Const::new(4));
    expr.commit();

    let ruleset = MyTx::new_ruleset("constant_prop");
    MyTx::add_rule(
        stringify!(MulPat),
        ruleset,
        || {
            let x = Var::query_slot("x");
            let y = Var::query_slot("y");
            let add = Add::query(&x, &y);

            #[eggplant::slotted_pat_vars]
            struct MulPat {
                x: Var,
                y: Var,
                add: Add,
            }
            MulPat::new(x, y, add)
        },
        |ctx, pat| {
            let symetric_add = ctx.insert_add(&pat.y, &pat.x);
            ctx.union(&pat.add, symetric_add);
        },
    );
    let report = MyTx::run_ruleset(ruleset, RunConfig::Sat);
    println!("{:#?}", report);
    MyTx::table_view();

    let c: Expr<MyTx, ConstTy> = Const::new(10);
    c.commit();
    if MyTx::canonical_raw(&expr) != MyTx::canonical_raw(&c) {
        panic!("should infer to 10");
    }

    expr.pull();
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
#[derive(Clone)]
struct MyHook;
impl RuleCtxHook for MyHook {
    fn on_insert(&self, table: &str, key: &[egglog::Value]) {
        println!("insert {} {:?}", table, key)
    }
    fn on_union(&self, x: egglog::Value, y: egglog::Value) {
        println!("union {x:?} {y:?}")
    }
    fn on_subsume(&self, table: &str, key: &[egglog::Value]) {}
    fn dyn_clone(&self) -> Box<dyn RuleCtxHook> {
        Box::new(self.clone())
    }
}
