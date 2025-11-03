use egglog::util::IndexSet;
use eggplant::prelude::*;
use eggplant::slotted_tx_rx_vt_pr;
use eggplant::wrap::NodeDropperSgl;
use eggplant::wrap::RuleCtxHook;
use std::sync::Arc;

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
    // let expr: Expr<MyTx, _> = Add::new(&Mul::new(&Var::new(), &Var::new()), &Const::new(4));
    let expr: Expr<MyTx, _> = Add::new(&Var::new(), &Var::new());
    expr.commit();

    let ruleset = MyTx::new_ruleset("constant_prop");
    MyTx::add_rule(
        stringify!("add commutative"),
        ruleset,
        || {
            let x = Var::query_slot("x");
            let y = Var::query_slot("y");
            let add = Add::query(&x, &y);
            #[eggplant::slotted_pat_vars_catch]
            struct AddPat {
                x: Var,
                y: Var,
                add: Add,
            }
        },
        |ctx, pat| {
            println!("{:?}", pat);
            // ctx.remove_add(&pat.y, &pat.x, ctx.devalue(pat.add.0.ver));
            let symetric_add = ctx.insert_add(&pat.y, &pat.x);
            // context should be passed from query to action
            println!("{:#?}", symetric_add.1.tensor());
            ctx.union(&pat.add, symetric_add);
        },
    );
    println!("first");
    // let report = MyTx::run_ruleset(ruleset, RunConfig::Once);
    // println!("second");
    // let report = MyTx::run_ruleset(ruleset, RunConfig::Once);
    // println!("third");
    // let report = MyTx::run_ruleset(ruleset, RunConfig::Once);
    // println!("{:#?}", report);
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

    view();
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
    fn on_remove(&self, table: &str, key: &[egglog::Value]) {}
    fn dyn_clone(&self) -> Box<dyn RuleCtxHook> {
        Box::new(self.clone())
    }
}

fn view() {
    use eggplant_viewer::*;
    let map = MyPatRec::sgl().slotted_ctx.clone();
    #[derive(Clone)]
    struct SlotEventHandler {
        map: Arc<SlottedCtx>,
    }
    impl EventHandle for SlotEventHandler {
        fn dyn_clone(&self) -> Box<dyn EventHandle> {
            Box::new(Self {
                map: self.map.clone(),
            })
        }

        fn on_drag(&self, cano_value: u32) {}

        fn on_hover(&self, cano_value: u32) {
            // println!("{cano_value} hovered")
        }

        fn on_newly_selected(&self, cano_value: u32) {
            match self
                .map
                .cano_value2seclasses
                .get(&egglog::Value::new_const(cano_value))
            {
                Some(seclasses) => {
                    let seclasses = seclasses.value();
                    println!("{seclasses:?} selected");
                }
                None => {
                    println!("seclasses not generated")
                }
            }
        }
    }

    let egraph = MyTx::sgl().egraph.lock().unwrap();
    eggplant::eggplant_viewer::eframe::run_native(
        "eggplant_egui_graphs demo",
        Default::default(),
        Box::new(|cc| {
            Ok(Box::new(EGraphApp::new(
                cc,
                DemoLayout::Hierarchical,
                &egraph,
                SlotEventHandler { map }.dyn_clone(),
            )))
        }),
    )
    .unwrap()
}
impl<T: eggplant::wrap::TxSgl + eggplant::wrap::NonPatRecSgl + eggplant::wrap::WithPatRecSgl>
    self::Expr<T, VarTy>
{
    fn new_slot(name: &'static str) -> Self {
        let expr = Var::new();
        T::replace_meta(
            expr.cur_sym(),
            Box::new(ArcSlotMetaInner {
                inner: Arc::new(SlotMetaInner {
                    sub_metas: vec![],
                    var_id_set: {
                        let mut idx_set = IndexSet::default();
                        idx_set.insert(name);
                        idx_set
                    },
                }),
            }),
        );
        expr
    }
}
