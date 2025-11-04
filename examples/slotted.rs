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
    let expr: Expr<MyTx, _> = Add::new(&Var::new_slot("a"), &Var::new_slot("b"));
    expr.commit();

    let ruleset = MyTx::new_ruleset("constant_prop");
    MyTx::add_rule(
        stringify!("add commutative"),
        ruleset,
        || {
            let x = Var::query_slot("x");
            let y = Var::query_slot("y");
            let add = Add::query(&x, &y);
            #[eggplant::slotted_pat_vars]
            struct AddPat {
                x: Var,
                y: Var,
                add: Add,
            }
            AddPat::new(x, y, add)
        },
        |ctx, pat| {
            println!("{:?}", pat);
            // ctx.remove_add(&pat.y, &pat.x, ctx.devalue(pat.add.0.ver));
            println!("Hello {:?} {:?}", pat.y, pat.x);
            let symetric_add = ctx.insert_add(&pat.y, &pat.x);
            // context should be passed from query to action
            println!("{:#?}", symetric_add.1.tensor());
            ctx.union(&pat.add, symetric_add);
        },
    );
    println!("first");
    let report = MyTx::run_ruleset(ruleset, RunConfig::Once);
    // println!("second");
    // let report = MyTx::run_ruleset(ruleset, RunConfig::Once);
    // println!("third");
    // let report = MyTx::run_ruleset(ruleset, RunConfig::Once);
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
    fn on_subsume(&self, _table: &str, _key: &[egglog::Value]) {}
    fn on_remove(&self, _table: &str, _key: &[egglog::Value]) {}
    fn dyn_clone(&self) -> Box<dyn RuleCtxHook> {
        Box::new(self.clone())
    }
}

fn view() {
    use eggplant_viewer::*;
    use eframe::egui;
    use egglog::NumericId;
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

        fn on_init(&self, ctx: &egui::Context) {
            println!("SlotEventHandler::on_init called!");
            // Create a new SidePanel for Slotted EGraph visualization
            // This allows users to create additional graph panels for seclasses
            egui::SidePanel::left("slotted_seclasses")
                .default_width(400.0)  // Increased width for better visibility
                .min_width(300.0)
                .resizable(true)
                .show(ctx, |ui| {
                    ui.heading("🎯 Slotted SEClasses");
                    ui.separator();

                    // Add some debug info
                    ui.label("This is the Slotted SEClasses SidePanel");
                    ui.label("Created via EventHandler::on_init");
                    ui.separator();

                    // Display information about slotted seclasses
                    if self.map.cano_value2seclasses.is_empty() {
                        ui.label("No SEClasses data available");
                    } else {
                        ui.label(format!(
                            "Total SEClasses entries: {}",
                            self.map.cano_value2seclasses.len()
                        ));

                        // Show basic information about seclasses
                        let mut count = 0;
                        for entry in self.map.cano_value2seclasses.iter().take(10) {
                            let cano_value = entry.key();
                            let seclasses = entry.value();
                            ui.collapsing(format!("Canonical Value: {}", cano_value.rep()), |ui| {
                                ui.label("Slotted EGraph SEClasses data available");
                                ui.label(format!("Canonical Value ID: {}", cano_value.rep()));
                            });
                            count += 1;
                        }

                        if count < self.map.cano_value2seclasses.len() {
                            ui.label("... and more");
                        }
                    }
                });
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
            Box::new(SlotMeta {
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
