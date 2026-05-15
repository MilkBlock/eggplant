#[cfg(feature = "viewer")]
use eggplant::egglog::NumericId;
use eggplant::prelude::*;
use eggplant::slotted_tx_rx_vt_pr;
use eggplant::wrap::NodeDropperSgl;
use indexmap::IndexSet;
use serde::{Deserialize, Serialize};
use std::sync::Arc;
#[cfg(feature = "viewer")]
use std::sync::Mutex;
use std::time::{Duration, Instant};

#[eggplant::slotted_dsl(base = SlotMetaBase)]
pub enum Expr {
    #[eggplant::typst("{name}")]
    Var { name: &'static str },
    #[eggplant::typst("{l} + {r}")]
    #[eggplant::precedence(100)]
    Add { l: Expr, r: Expr },
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

impl<T: eggplant::wrap::TxSgl + eggplant::wrap::NonPatRecSgl + eggplant::wrap::WithPatRecSgl>
    self::Expr<T, VarTy>
{
    fn new_slot(name: &'static str) -> Self {
        let expr = Var::new(name);
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
                    history: Default::default(),
                }),
            }),
        );
        expr
    }
}

#[derive(Debug, Clone)]
struct Stats {
    elapsed: Duration,
    raw_num_tuples: usize,
    bucket_count: usize,
    total_seclasses: usize,
    total_senodes: usize,
}

fn seed_add_chain() -> Expr<MyTx, AddTy> {
    let a = Var::new_slot("a");
    let b = Var::new_slot("b");
    let c = Var::new_slot("c");
    let d = Var::new_slot("d");
    let e = Var::new_slot("e");
    let f = Var::new_slot("f");
    let g = Var::new_slot("g");
    let h = Var::new_slot("h");

    Add::new(
        &Add::new(
            &Add::new(&Add::new(&a, &b), &Add::new(&c, &d)),
            &Add::new(&e, &f),
        ),
        &Add::new(&g, &h),
    )
}

fn install_rules() -> RuleSetId {
    let ruleset = MyTx::new_ruleset("slotted_add_chain_comm_only");
    MyTx::add_rule(
        "add_comm",
        ruleset,
        || {
            let x = Expr::query_slot("x".to_string());
            let y = Expr::query_slot("y".to_string());
            let add = Add::query(&x, &y);
            #[eggplant::slotted_pat_vars]
            struct Pat {
                x: Expr,
                y: Expr,
                add: Add,
            }
            Pat::new(x, y, add)
        },
        |ctx, pat| {
            let rhs = ctx.insert_add(&pat.y, &pat.x);
            ctx.union(&pat.add, rhs);
        },
    );
    ruleset
}

fn compute_stats() -> Stats {
    MyPatRec::sgl().slotted_ctx.clear();
    let root = seed_add_chain();
    root.commit();
    let ruleset = install_rules();

    let started = Instant::now();
    loop {
        let report = MyTx::run_ruleset(ruleset, RunConfig::Once);
        if !report.updated {
            break;
        }
    }
    let elapsed = started.elapsed();

    let raw_num_tuples = MyTx::sgl().egraph.lock().unwrap().num_tuples();
    let buckets = MyPatRec::sgl().slotted_ctx.buckets();
    let bucket_count = buckets.len();
    let total_seclasses = buckets.iter().map(|bucket| bucket.seclass_count()).sum();
    let total_senodes = buckets.iter().map(|bucket| bucket.senode_count()).sum();

    Stats {
        elapsed,
        raw_num_tuples,
        bucket_count,
        total_seclasses,
        total_senodes,
    }
}

fn main() {
    env_logger::init();
    let stats = compute_stats();
    println!("add-chain-comm-only time: {:?}", stats.elapsed);
    println!("[raw] total num_tuples = {}", stats.raw_num_tuples);
    println!("[slotted] bucket_count = {}", stats.bucket_count);
    println!("[slotted] total_seclasses = {}", stats.total_seclasses);
    println!("[slotted] total_senodes = {}", stats.total_senodes);
    #[cfg(feature = "viewer")]
    view(stats);
}

#[cfg(feature = "viewer")]
fn render_bucket_summary(
    ui: &mut eggplant::eggplant_viewer::eframe::egui::Ui,
    bucket: &SlottedBucket,
) {
    ui.label(format!(
        "Canonical Value ID: {}",
        bucket.canonical_value().rep()
    ));
    ui.label(format!("Slotted EClasses: {}", bucket.seclass_count()));
    ui.label(format!("Slotted ENodes: {}", bucket.senode_count()));
}

#[cfg(feature = "viewer")]
fn render_bucket_detail(
    ui: &mut eggplant::eggplant_viewer::eframe::egui::Ui,
    bucket: &SlottedBucket,
) {
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
fn view(stats: Stats) {
    use eframe::egui;
    use eggplant_viewer::*;

    let map = MyPatRec::sgl().slotted_ctx.clone();
    let selected_cano_value = Arc::new(Mutex::new(None::<egglog::Value>));
    #[derive(Clone)]
    struct SlotEventHandler {
        map: Arc<SlottedCtx>,
        selected_cano_value: Arc<Mutex<Option<egglog::Value>>>,
        stats: Stats,
    }
    impl EventHandle for SlotEventHandler {
        fn dyn_clone(&self) -> Box<dyn EventHandle> {
            Box::new(Self {
                map: self.map.clone(),
                selected_cano_value: self.selected_cano_value.clone(),
                stats: self.stats.clone(),
            })
        }

        fn on_drag(&self, _cano_value: u32) {}
        fn on_hover(&self, _cano_value: u32) {}

        fn on_newly_selected(&self, cano_value: u32) {
            *self.selected_cano_value.lock().unwrap() = Some(egglog::Value::new_const(cano_value));
        }

        fn on_init(&self, ctx: &egui::Context) {
            egui::SidePanel::left("slotted_add_chain_summary")
                .default_width(360.0)
                .show(ctx, |ui| {
                    ui.heading("Raw vs Slotted");
                    ui.separator();
                    ui.label(format!("Elapsed: {:?}", self.stats.elapsed));
                    ui.label(format!("Raw num_tuples: {}", self.stats.raw_num_tuples));
                    ui.label(format!("Slotted buckets: {}", self.stats.bucket_count));
                    ui.label(format!("Slotted seclasses: {}", self.stats.total_seclasses));
                    ui.label(format!("Slotted senodes: {}", self.stats.total_senodes));
                    ui.separator();
                    ui.label(format!(
                        "Total buckets visible: {}",
                        self.map.bucket_count()
                    ));
                    for bucket in self.map.buckets().into_iter().take(12) {
                        ui.collapsing(
                            format!("Canonical Value {}", bucket.canonical_value().rep()),
                            |ui| render_bucket_summary(ui, &bucket),
                        );
                    }
                });

            egui::SidePanel::left("slotted_add_chain_selected")
                .default_width(460.0)
                .show(ctx, |ui| {
                    ui.heading("Selected Slotted Detail");
                    ui.separator();
                    let selected = *self.selected_cano_value.lock().unwrap();
                    match selected.and_then(|cano| self.map.bucket(cano)) {
                        Some(bucket) => render_bucket_detail(ui, &bucket),
                        None => {
                            ui.label("Select a node in the main graph.");
                        }
                    }
                });
        }
    }

    let egraph = MyTx::sgl().egraph.lock().unwrap();
    eggplant::eggplant_viewer::eframe::run_native(
        "slotted_add_chain_comm_only",
        Default::default(),
        Box::new(|cc| {
            Ok(Box::new(EGraphApp::new(
                cc,
                DemoLayout::Hierarchical,
                &egraph,
                SlotEventHandler {
                    map,
                    selected_cano_value,
                    stats,
                }
                .dyn_clone(),
            )))
        }),
    )
    .unwrap()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn add_chain_comm_only_runs() {
        let stats = compute_stats();
        assert!(stats.raw_num_tuples > 0);
        assert!(stats.total_senodes > 0);
    }
}
