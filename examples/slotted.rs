use eggplant::prelude::*;
use eggplant::slotted_tx_rx_vt_pr;
use eggplant::wrap::NodeDropperSgl;
#[cfg(feature = "viewer")]
use eggplant::egglog::NumericId;
use indexmap::IndexSet;
use serde::Deserialize;
use serde::Serialize;
use std::sync::Arc;
#[cfg(feature = "viewer")]
use std::sync::Mutex;

#[eggplant::slotted_dsl(base = SlotMetaBase)]
pub enum Expr {
    Var {},
    Const {
        num: i64,
    },
    Mul {
        l: Expr,
        r: Expr,
    },
    Add {
        l: Expr,
        r: Expr,
    },
}
#[eggplant::base_ty]
#[derive(Serialize, Deserialize, Debug, Clone, Hash, PartialEq, Eq, Default)]
pub enum SlotMetaBase {
    Inner {
        inner: SlotMeta,
    },
    #[default]
    Unknown,
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
            let x = Var::query_slot("x".to_string());
            let y = Var::query_slot("y".to_string());
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

    #[cfg(feature = "viewer")]
    view();
}

impl<T: TxSgl + NodeDropperSgl + SlottedPatRecSgl> QuerySlot for Var<T> {
    fn query_slot(var_id: SlotVarID) -> Self {
        let node = Var::query();
        T::on_new_query_slot(&node, var_id);
        node
    }
}
#[cfg(feature = "viewer")]
fn render_bucket_summary(ui: &mut eggplant::eggplant_viewer::eframe::egui::Ui, bucket: &SlottedBucket) {
    ui.label(format!("Canonical Value ID: {}", bucket.canonical_value().rep()));
    ui.label(format!("Slotted EClasses: {}", bucket.seclass_count()));
    ui.label(format!("Slotted ENodes: {}", bucket.senode_count()));
}

#[cfg(feature = "viewer")]
fn render_bucket_detail(ui: &mut eggplant::eggplant_viewer::eframe::egui::Ui, bucket: &SlottedBucket) {
    render_bucket_summary(ui, bucket);
    ui.separator();

    for eclass in bucket.eclasses() {
        ui.collapsing(format!("SEClass {}", eclass.eclass_id()), |ui| {
            ui.label(format!("Slots: {:?}", eclass.slots()));
            ui.label(format!("SENodes: {:?}", eclass.senode_ids()));
            ui.label(format!(
                "Symmetry generators: {}",
                eclass.group().generators().len()
            ));
            ui.label(format!(
                "Representative shape: {:?}",
                eclass.group().representative()
            ));

            for (shape, entry) in eclass.shapes() {
                ui.collapsing(
                    format!("Shape {} {:?}", shape.ty_name(), shape.de_bruijn()),
                    |ui| {
                        for witness in entry.witnesses() {
                            ui.label(format!(
                                "Witness senode={} renaming={:?}",
                                witness.senode_id(),
                                witness.renaming()
                            ));
                        }
                    },
                );
            }
        });
    }
}

#[cfg(feature = "viewer")]
fn view() {
    use eframe::egui;
    use egglog::NumericId;
    use eggplant_viewer::*;
    let map = MyPatRec::sgl().slotted_ctx.clone();
    let selected_cano_value = Arc::new(Mutex::new(None::<egglog::Value>));
    #[derive(Clone)]
    struct SlotEventHandler {
        map: Arc<SlottedCtx>,
        selected_cano_value: Arc<Mutex<Option<egglog::Value>>>,
    }
    impl EventHandle for SlotEventHandler {
        fn dyn_clone(&self) -> Box<dyn EventHandle> {
            Box::new(Self {
                map: self.map.clone(),
                selected_cano_value: self.selected_cano_value.clone(),
            })
        }

        fn on_drag(&self, _cano_value: u32) {}

        fn on_hover(&self, _cano_value: u32) {}

        fn on_newly_selected(&self, cano_value: u32) {
            let selected = egglog::Value::new_const(cano_value);
            *self.selected_cano_value.lock().unwrap() = Some(selected);
            match self.map.bucket(selected) {
                Some(bucket) => {
                    println!("{bucket:?} selected");
                }
                None => {
                    println!("seclasses not generated")
                }
            }
        }

        fn on_init(&self, ctx: &egui::Context) {
            egui::SidePanel::left("slotted_seclasses")
                .default_width(400.0) // Increased width for better visibility
                .min_width(300.0)
                .resizable(true)
                .show(ctx, |ui| {
                    ui.heading("Slotted Buckets");
                    ui.separator();

                    if self.map.bucket_count() == 0 {
                        ui.label("No SEClasses data available");
                    } else {
                        ui.label(format!(
                            "Total buckets: {}",
                            self.map.bucket_count()
                        ));

                        let mut count = 0;
                        for bucket in self.map.buckets().into_iter().take(10) {
                            ui.collapsing(
                                format!("Canonical Value: {}", bucket.canonical_value().rep()),
                                |ui| {
                                    render_bucket_summary(ui, &bucket);
                                },
                            );
                            count += 1;
                        }

                        if count < self.map.bucket_count() {
                            ui.label("... and more");
                        }
                    }
                });

            egui::SidePanel::left("slotted_selected_detail")
                .default_width(500.0)
                .min_width(400.0)
                .resizable(true)
                .show(ctx, |ui| {
                    ui.heading("Selected Slotted Detail");
                    ui.separator();

                    let selected = *self.selected_cano_value.lock().unwrap();
                    match selected.and_then(|cano| self.map.bucket(cano)) {
                        Some(bucket) => render_bucket_detail(ui, &bucket),
                        None => {
                            ui.label("Select a node in the main graph to inspect its slotted bucket.");
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
                SlotEventHandler {
                    map,
                    selected_cano_value,
                }
                .dyn_clone(),
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
                        idx_set.insert(name.to_string());
                        idx_set
                    },
                }),
            }),
        );
        expr
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn slotted_add_union_merges_slotted_seclasses() {
        MyPatRec::sgl().slotted_ctx.clear();

        let expr_ab: Expr<MyTx, _> = Add::new(&Var::new_slot("a"), &Var::new_slot("b"));
        let expr_aa: Expr<MyTx, _> = Add::new(&Var::new_slot("a"), &Var::new_slot("a"));
        expr_ab.commit();
        expr_aa.commit();

        let cano_ab_before = MyTx::canonical_raw(&expr_ab);
        let cano_aa_before = MyTx::canonical_raw(&expr_aa);
        assert_ne!(cano_ab_before, cano_aa_before);
        assert_eq!(MyPatRec::sgl().slotted_ctx.senode_count(cano_ab_before), 1);
        assert_eq!(MyPatRec::sgl().slotted_ctx.seclass_count(cano_ab_before), 1);
        assert_eq!(MyPatRec::sgl().slotted_ctx.senode_count(cano_aa_before), 1);
        assert_eq!(MyPatRec::sgl().slotted_ctx.seclass_count(cano_aa_before), 1);

        let ruleset = MyTx::new_ruleset("slotted_support_add_union");
        MyTx::add_rule(
            "add_union",
            ruleset,
            || {
                let x = Var::query_slot("x".to_string());
                let y = Var::query_slot("y".to_string());
                let add_xy = Add::query(&x, &y);
                let add_xx = Add::query(&x, &x);
                #[eggplant::slotted_pat_vars]
                struct AddPat {
                    x: Var,
                    y: Var,
                    add_xy: Add,
                    add_xx: Add,
                }
                AddPat::new(x, y, add_xy, add_xx)
            },
            |ctx, pat| {
                ctx.union(&pat.add_xy, &pat.add_xx);
            },
        );

        let report = MyTx::run_ruleset(ruleset, RunConfig::Once);
        assert!(report.updated);

        let cano_ab_after = MyTx::canonical_raw(&expr_ab);
        let cano_aa_after = MyTx::canonical_raw(&expr_aa);
        assert_eq!(cano_ab_after, cano_aa_after);
        assert_eq!(MyPatRec::sgl().slotted_ctx.senode_count(cano_ab_after), 2);
        assert_eq!(MyPatRec::sgl().slotted_ctx.seclass_count(cano_ab_after), 1);
        let eclasses = MyPatRec::sgl().slotted_ctx.eclasses(cano_ab_after);
        assert_eq!(eclasses.len(), 1);
        assert_eq!(eclasses[0].senode_ids().len(), 2);
        assert_eq!(eclasses[0].shapes().len(), 2);
    }

    #[test]
    fn top_level_union_updates_slotted_seclasses() {
        MyPatRec::sgl().slotted_ctx.clear();

        let expr_ab: Expr<MyTx, _> = Add::new(&Var::new_slot("a"), &Var::new_slot("b"));
        let expr_aa: Expr<MyTx, _> = Add::new(&Var::new_slot("a"), &Var::new_slot("a"));
        expr_ab.commit();
        expr_aa.commit();

        let cano_ab_before = MyTx::canonical_raw(&expr_ab);
        let cano_aa_before = MyTx::canonical_raw(&expr_aa);
        assert_ne!(cano_ab_before, cano_aa_before);
        assert_eq!(MyPatRec::sgl().slotted_ctx.seclass_count(cano_ab_before), 1);
        assert_eq!(MyPatRec::sgl().slotted_ctx.seclass_count(cano_aa_before), 1);

        MyTx::on_union(&expr_ab, &expr_aa);

        let cano_ab_after = MyTx::canonical_raw(&expr_ab);
        let cano_aa_after = MyTx::canonical_raw(&expr_aa);
        assert_eq!(cano_ab_after, cano_aa_after);
        assert_eq!(MyPatRec::sgl().slotted_ctx.senode_count(cano_ab_after), 2);
        assert_eq!(MyPatRec::sgl().slotted_ctx.seclass_count(cano_ab_after), 1);
        let eclasses = MyPatRec::sgl().slotted_ctx.eclasses(cano_ab_after);
        assert_eq!(eclasses.len(), 1);
        assert_eq!(eclasses[0].senode_ids().len(), 2);
        assert_eq!(eclasses[0].shapes().len(), 2);
    }

    #[test]
    fn bucket_snapshot_exposes_canonical_bucket_state() {
        MyPatRec::sgl().slotted_ctx.clear();

        let expr_ab: Expr<MyTx, _> = Add::new(&Var::new_slot("a"), &Var::new_slot("b"));
        let expr_aa: Expr<MyTx, _> = Add::new(&Var::new_slot("a"), &Var::new_slot("a"));
        expr_ab.commit();
        expr_aa.commit();
        MyTx::on_union(&expr_ab, &expr_aa);

        let cano = MyTx::canonical_raw(&expr_ab);
        let bucket = MyPatRec::sgl()
            .slotted_ctx
            .bucket(cano)
            .expect("expected slotted bucket for canonical value");

        assert_eq!(bucket.canonical_value(), cano);
        assert_eq!(bucket.senode_count(), 2);
        assert_eq!(bucket.eclasses().len(), 1);
        assert_eq!(bucket.eclasses()[0].senode_ids().len(), 2);
    }

    #[test]
    fn union_collapses_two_buckets_into_one() {
        MyPatRec::sgl().slotted_ctx.clear();

        let expr_ab: Expr<MyTx, _> = Add::new(&Var::new_slot("a"), &Var::new_slot("b"));
        let expr_aa: Expr<MyTx, _> = Add::new(&Var::new_slot("a"), &Var::new_slot("a"));
        expr_ab.commit();
        expr_aa.commit();

        assert_eq!(MyPatRec::sgl().slotted_ctx.bucket_count(), 3);
        let cano_ab_before = MyTx::canonical_raw(&expr_ab);
        let cano_aa_before = MyTx::canonical_raw(&expr_aa);
        assert_ne!(cano_ab_before, cano_aa_before);

        MyTx::on_union(&expr_ab, &expr_aa);

        assert_eq!(MyPatRec::sgl().slotted_ctx.bucket_count(), 2);
        let cano = MyTx::canonical_raw(&expr_ab);
        let buckets = MyPatRec::sgl().slotted_ctx.buckets();
        assert_eq!(buckets.len(), 2);
        assert!(buckets.iter().any(|bucket| bucket.canonical_value() == cano));
    }

}
