use eggplant::prelude::*;
use eggplant::slotted_tx_rx_vt_pr;
use eggplant::wrap::NodeDropperSgl;
#[cfg(feature = "viewer")]
use eggplant::egglog::NumericId;
use indexmap::IndexSet;
use serde::{Deserialize, Serialize};
#[cfg(feature = "viewer")]
use std::collections::{HashMap, hash_map::DefaultHasher};
#[cfg(feature = "viewer")]
use std::hash::{Hash, Hasher};
#[cfg(feature = "viewer")]
use std::io::Write;
#[cfg(feature = "viewer")]
use std::path::PathBuf;
#[cfg(feature = "viewer")]
use std::process::{Command, Stdio};
use std::sync::{Arc, OnceLock};
#[cfg(feature = "viewer")]
use std::sync::Mutex;
#[cfg(feature = "viewer")]
use std::thread;
use std::time::{Duration, Instant};

const RUN_ITERS: usize = 4;

#[eggplant::slotted_dsl(base = SlotMetaBase)]
pub enum MathExpr {
    #[eggplant::display("v")]
    #[eggplant::typst("x")]
    Var {},
    #[eggplant::display("{num}")]
    #[eggplant::typst("{num}")]
    Const {
        num: i64,
    },
    #[eggplant::display("{l} + {r}")]
    #[eggplant::typst("{l} + {r}")]
    #[eggplant::precedence(100)]
    Add {
        l: MathExpr,
        r: MathExpr,
    },
    #[eggplant::display("{l} * {r}")]
    #[eggplant::typst("{l} dot {r}")]
    #[eggplant::precedence(200)]
    Mul {
        l: MathExpr,
        r: MathExpr,
    },
}

#[eggplant::base_ty]
#[derive(Serialize, Deserialize, Debug, Clone, Hash, PartialEq, Eq, Default)]
pub enum SlotMetaBase {
    Inner { inner: SlotMeta },
    #[default]
    Unknown,
}

slotted_tx_rx_vt_pr!(MyTx, MyPatRec);

impl<T: TxSgl + NodeDropperSgl + SlottedPatRecSgl> QuerySlot for Var<T> {
    fn query_slot(var_id: SlotVarID) -> Self {
        let node = Var::query();
        T::on_new_query_slot(&node, var_id);
        node
    }
}

impl<T: eggplant::wrap::TxSgl + eggplant::wrap::NonPatRecSgl + eggplant::wrap::WithPatRecSgl>
    self::MathExpr<T, VarTy>
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

#[derive(Debug, Clone)]
struct SlottedMathStats {
    elapsed: Duration,
    egraph_num_tuples: usize,
    bucket_count: usize,
    total_seclasses: usize,
    total_senodes: usize,
}

fn install_rules() -> RuleSetId {
    let ruleset = MyTx::new_ruleset("slotted_math_microbench");

    MyTx::add_rule(
        "add_comm",
        ruleset,
        || {
            let x = Var::query_slot("x".to_string());
            let y = Var::query_slot("y".to_string());
            let add = Add::query(&x, &y);
            #[eggplant::slotted_pat_vars]
            struct AddCommPat {
                x: Var,
                y: Var,
                add: Add,
            }
            AddCommPat::new(x, y, add)
        },
        |ctx, pat| {
            let rhs = ctx.insert_add(&pat.y, &pat.x);
            ctx.union(&pat.add, rhs);
        },
    );

    MyTx::add_rule(
        "mul_comm",
        ruleset,
        || {
            let x = Var::query_slot("x".to_string());
            let y = Var::query_slot("y".to_string());
            let mul = Mul::query(&x, &y);
            #[eggplant::slotted_pat_vars]
            struct MulCommPat {
                x: Var,
                y: Var,
                mul: Mul,
            }
            MulCommPat::new(x, y, mul)
        },
        |ctx, pat| {
            let rhs = ctx.insert_mul(&pat.y, &pat.x);
            ctx.union(&pat.mul, rhs);
        },
    );

    MyTx::add_rule(
        "add_assoc",
        ruleset,
        || {
            let x = Var::query_slot("x".to_string());
            let y = Var::query_slot("y".to_string());
            let z = Var::query_slot("z".to_string());
            let add_xy = Add::query(&x, &y);
            let add = Add::query(&add_xy, &z);
            #[eggplant::slotted_pat_vars]
            struct AddAssocPat {
                x: Var,
                y: Var,
                z: Var,
                add: Add,
            }
            AddAssocPat::new(x, y, z, add)
        },
        |ctx, pat| {
            let yz = ctx.insert_add(&pat.y, &pat.z);
            let rhs = ctx.insert_add(&pat.x, yz);
            ctx.union(&pat.add, rhs);
        },
    );

    MyTx::add_rule(
        "mul_distrib",
        ruleset,
        || {
            let a = Var::query_slot("a".to_string());
            let b = Var::query_slot("b".to_string());
            let c = Var::query_slot("c".to_string());
            let add_bc = Add::query(&b, &c);
            let mul = Mul::query(&a, &add_bc);
            #[eggplant::slotted_pat_vars]
            struct MulDistribPat {
                a: Var,
                b: Var,
                c: Var,
                mul: Mul,
            }
            MulDistribPat::new(a, b, c, mul)
        },
        |ctx, pat| {
            let ab = ctx.insert_mul(&pat.a, &pat.b);
            let ac = ctx.insert_mul(&pat.a, &pat.c);
            let rhs = ctx.insert_add(ab, ac);
            ctx.union(&pat.mul, rhs);
        },
    );

    ruleset
}

fn seed_expressions() {
    let expr: MathExpr<MyTx, _> = Add::new(&Var::new_slot("a"), &Var::new_slot("b"));
    expr.commit();

    let expr: MathExpr<MyTx, _> = Add::new(&Var::new_slot("b"), &Var::new_slot("a"));
    expr.commit();

    let expr: MathExpr<MyTx, _> = Add::new(
        &Add::new(&Var::new_slot("a"), &Var::new_slot("b")),
        &Var::new_slot("c"),
    );
    expr.commit();

    let expr: MathExpr<MyTx, _> = Add::new(
        &Var::new_slot("a"),
        &Add::new(&Var::new_slot("b"), &Var::new_slot("c")),
    );
    expr.commit();

    let expr: MathExpr<MyTx, _> = Add::new(
        &Add::new(&Var::new_slot("b"), &Var::new_slot("a")),
        &Var::new_slot("c"),
    );
    expr.commit();

    let expr: MathExpr<MyTx, _> = Mul::new(
        &Var::new_slot("a"),
        &Add::new(&Var::new_slot("b"), &Var::new_slot("c")),
    );
    expr.commit();

    let expr: MathExpr<MyTx, _> = Add::new(
        &Mul::new(&Var::new_slot("a"), &Var::new_slot("b")),
        &Mul::new(&Var::new_slot("a"), &Var::new_slot("c")),
    );
    expr.commit();

    let expr: MathExpr<MyTx, _> = Mul::new(
        &Add::new(&Var::new_slot("a"), &Var::new_slot("b")),
        &Var::new_slot("c"),
    );
    expr.commit();

    let expr: MathExpr<MyTx, _> = Add::new(
        &Mul::new(&Var::new_slot("a"), &Var::new_slot("c")),
        &Mul::new(&Var::new_slot("b"), &Var::new_slot("c")),
    );
    expr.commit();

    let expr: MathExpr<MyTx, _> = Mul::new(
        &Add::new(&Var::new_slot("b"), &Var::new_slot("a")),
        &Var::new_slot("c"),
    );
    expr.commit();
}

fn compute_slotted_math_microbench() -> SlottedMathStats {
    MyPatRec::sgl().slotted_ctx.clear();
    seed_expressions();
    let ruleset = install_rules();

    let started = Instant::now();
    for _ in 0..RUN_ITERS {
        let _ = MyTx::run_ruleset(ruleset, RunConfig::Once);
    }
    let elapsed = started.elapsed();

    let egraph_num_tuples = MyTx::sgl().egraph.lock().unwrap().num_tuples();
    let buckets = MyPatRec::sgl().slotted_ctx.buckets();
    let bucket_count = buckets.len();
    let total_seclasses = buckets.iter().map(|bucket| bucket.seclass_count()).sum();
    let total_senodes = buckets.iter().map(|bucket| bucket.senode_count()).sum();

    SlottedMathStats {
        elapsed,
        egraph_num_tuples,
        bucket_count,
        total_seclasses,
        total_senodes,
    }
}

fn run_slotted_math_microbench() -> SlottedMathStats {
    static STATS: OnceLock<SlottedMathStats> = OnceLock::new();
    STATS.get_or_init(compute_slotted_math_microbench).clone()
}

fn preferred_extract_bucket() -> SlottedBucket {
    MyPatRec::sgl()
        .slotted_ctx
        .buckets()
        .into_iter()
        .find(|bucket| {
            bucket.seclass_count() > 0
                && bucket
                    .eclasses()
                    .iter()
                    .any(|eclass| eclass.shapes().keys().any(|shape| shape.ty_name() == "Add"))
        })
        .or_else(|| {
            MyPatRec::sgl()
                .slotted_ctx
                .buckets()
                .into_iter()
                .find(|bucket| bucket.senode_count() > 1)
        })
        .or_else(|| MyPatRec::sgl().slotted_ctx.buckets().into_iter().next())
        .expect("expected at least one slotted bucket")
}

fn print_stats(stats: &SlottedMathStats) {
    println!("slotted math time: {:?}", stats.elapsed);
    println!("[raw] total num_tuples = {}", stats.egraph_num_tuples);
    println!("[slotted] bucket_count = {}", stats.bucket_count);
    println!("[slotted] total_seclasses = {}", stats.total_seclasses);
    println!("[slotted] total_senodes = {}", stats.total_senodes);
}

fn extract_best_term_for_value(cano_value: egglog::Value) -> String {
    let egraph = MyTx::sgl().egraph.lock().unwrap();
    let sort = egraph
        .get_sort_by_name("MathExpr")
        .expect("MathExpr sort should exist");
    let canonical = egraph.get_canonical_value(cano_value, sort);
    let (term, cost) = egraph
        .extract_value_to_string(sort, canonical)
        .expect("extract should succeed for canonical value");
    format!("{term}\ncost: {cost}")
}

#[cfg(feature = "viewer")]
fn normalize_typst_math_source(source: &str) -> String {
    let trimmed = source.trim();
    if trimmed.starts_with("$$") && trimmed.ends_with("$$") && trimmed.len() >= 4 {
        return trimmed[2..trimmed.len() - 2].trim().to_owned();
    }
    if trimmed.starts_with('$') && trimmed.ends_with('$') && trimmed.len() >= 2 {
        return trimmed[1..trimmed.len() - 1].trim().to_owned();
    }
    trimmed.to_owned()
}

#[cfg(feature = "viewer")]
fn build_typst_math_document(source: &str) -> String {
    [
        "#set page(width: auto, height: auto, margin: 0pt)",
        "#set par(justify: false)",
        &format!(
            "#box(inset: (x: 1.2pt, y: 1.6pt))[$ {} $]",
            normalize_typst_math_source(source)
        ),
    ]
    .join("\n")
}

#[cfg(feature = "viewer")]
fn extract_best_term_typst_for_value(cano_value: egglog::Value) -> String {
    let egraph = MyTx::sgl().egraph.lock().unwrap();
    let sort = egraph
        .get_sort_by_name("MathExpr")
        .expect("MathExpr sort should exist");
    eggplant::wrap::extract_value_template_string(&egraph, sort, cano_value, true)
        .expect("extract typst should succeed")
}

#[cfg(feature = "viewer")]
fn render_typst_svg_bytes(source: &str) -> Result<Arc<[u8]>, String> {
    let mut child = Command::new("typst")
        .args(["compile", "-", "-", "--format", "svg"])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .map_err(|err| format!("failed to spawn typst: {err}"))?;

    let document = build_typst_math_document(source);
    {
        let mut stdin = child
            .stdin
            .take()
            .ok_or_else(|| "typst stdin unavailable".to_owned())?;
        stdin
            .write_all(document.as_bytes())
            .map_err(|err| format!("failed to write typst input: {err}"))?;
    }

    let output = child
        .wait_with_output()
        .map_err(|err| format!("failed to wait for typst: {err}"))?;
    if !output.status.success() {
        return Err(String::from_utf8_lossy(&output.stderr).into_owned());
    }
    Ok(Arc::<[u8]>::from(output.stdout))
}

#[cfg(feature = "viewer")]
fn typst_uri_for_source(source: &str) -> String {
    let mut hasher = DefaultHasher::new();
    source.hash(&mut hasher);
    format!("bytes://slotted-extract-{:x}.svg", hasher.finish())
}

#[cfg(feature = "viewer")]
#[derive(Clone)]
enum TypstPreviewState {
    Rendering,
    Ready(Arc<[u8]>),
    Error(String),
}

#[cfg(feature = "viewer")]
fn typst_preview_cache() -> &'static Mutex<HashMap<String, TypstPreviewState>> {
    static CACHE: OnceLock<Mutex<HashMap<String, TypstPreviewState>>> = OnceLock::new();
    CACHE.get_or_init(|| Mutex::new(HashMap::new()))
}

#[cfg(feature = "viewer")]
fn ensure_typst_render_started(ctx: &eframe::egui::Context, source: &str) {
    let cache = typst_preview_cache();
    {
        let cache = cache.lock().unwrap();
        if cache.contains_key(source) {
            return;
        }
    }

    cache
        .lock()
        .unwrap()
        .insert(source.to_owned(), TypstPreviewState::Rendering);

    let source_owned = source.to_owned();
    let repaint_ctx = ctx.clone();
    thread::spawn(move || {
        let next_state = match render_typst_svg_bytes(&source_owned) {
            Ok(bytes) => TypstPreviewState::Ready(bytes),
            Err(err) => TypstPreviewState::Error(err),
        };
        typst_preview_cache()
            .lock()
            .unwrap()
            .insert(source_owned, next_state);
        repaint_ctx.request_repaint();
    });
}

#[cfg(feature = "viewer")]
fn typst_preview_state(source: &str) -> Option<TypstPreviewState> {
    typst_preview_cache().lock().unwrap().get(source).cloned()
}

#[cfg(feature = "viewer")]
fn write_typst_svg_for_value(cano_value: egglog::Value) -> Result<PathBuf, String> {
    let source = extract_best_term_typst_for_value(cano_value);
    let svg = render_typst_svg_bytes(&source)?;
    let path = std::env::temp_dir().join(format!(
        "eggplant_slotted_math_extract_{:x}.svg",
        cano_value.rep()
    ));
    std::fs::write(&path, &*svg).map_err(|err| format!("failed to write svg: {err}"))?;
    Ok(path)
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
fn view(stats: SlottedMathStats) {
    use eframe::egui;
    use eggplant_viewer::*;

    let map = MyPatRec::sgl().slotted_ctx.clone();
    let initially_selected = Some(preferred_extract_bucket().canonical_value());
    let selected_cano_value = Arc::new(Mutex::new(initially_selected));

    #[derive(Clone)]
    struct SlotEventHandler {
        map: Arc<SlottedCtx>,
        stats: SlottedMathStats,
        selected_cano_value: Arc<Mutex<Option<egglog::Value>>>,
    }

    impl EventHandle for SlotEventHandler {
        fn dyn_clone(&self) -> Box<dyn EventHandle> {
            Box::new(Self {
                map: self.map.clone(),
                stats: self.stats.clone(),
                selected_cano_value: self.selected_cano_value.clone(),
            })
        }

        fn on_drag(&self, _cano_value: u32) {}

        fn on_hover(&self, _cano_value: u32) {}

        fn on_newly_selected(&self, cano_value: u32) {
            let selected = egglog::Value::new_const(cano_value);
            *self.selected_cano_value.lock().unwrap() = Some(selected);
        }

        fn on_init(&self, ctx: &egui::Context) {
            egui::SidePanel::left("slotted_math_microbench_summary")
                .default_width(360.0)
                .min_width(280.0)
                .resizable(true)
                .show(ctx, |ui| {
                    ui.heading("Slotted Math Summary");
                    ui.separator();
                    ui.label(format!("Elapsed: {:?}", self.stats.elapsed));
                    ui.label(format!("Raw tuples: {}", self.stats.egraph_num_tuples));
                    ui.label(format!("Slotted buckets: {}", self.stats.bucket_count));
                    ui.label(format!("Slotted eclasses: {}", self.stats.total_seclasses));
                    ui.label(format!("Slotted senodes: {}", self.stats.total_senodes));
                    ui.separator();

                    for bucket in self.map.buckets().into_iter().take(12) {
                        ui.collapsing(
                            format!("Canonical Value {}", bucket.canonical_value().rep()),
                            |ui| render_bucket_summary(ui, &bucket),
                        );
                    }
                });

            egui::SidePanel::left("slotted_math_microbench_selected")
                .default_width(480.0)
                .min_width(360.0)
                .resizable(true)
                .show(ctx, |ui| {
                    ui.heading("Selected Slotted Detail");
                    ui.separator();
                    let selected = *self.selected_cano_value.lock().unwrap();
                    match selected.and_then(|cano| self.map.bucket(cano)) {
                        Some(bucket) => {
                            let extracted = extract_best_term_for_value(bucket.canonical_value());
                            let typst_source = extract_best_term_typst_for_value(bucket.canonical_value());
                            render_bucket_detail(ui, &bucket);
                            ui.separator();
                            ui.heading("Best Extract");
                            ui.code(extracted);
                            ui.separator();
                            ui.heading("Typst");
                            ui.code(&typst_source);
                            ui.separator();
                            ui.heading("Typst Preview");
                            ensure_typst_render_started(ctx, &typst_source);
                            match typst_preview_state(&typst_source) {
                                Some(TypstPreviewState::Rendering) => {
                                    ui.label("Rendering Typst preview...");
                                    ui.add(egui::Spinner::new());
                                }
                                Some(TypstPreviewState::Ready(svg)) => {
                                    let uri = typst_uri_for_source(&typst_source);
                                    ui.add(
                                        egui::Image::from_bytes(uri, svg)
                                            .max_width(ui.available_width()),
                                    );
                                }
                                Some(TypstPreviewState::Error(err)) => {
                                    ui.label(format!("Typst render failed: {err}"));
                                }
                                None => {}
                            }
                        }
                        None => {
                            ui.label("Select a node in the main graph to inspect its slotted bucket.");
                        }
                    }
                });
        }
    }

    eggplant::eggplant_viewer::eframe::run_native(
        "slotted math microbench viewer",
        Default::default(),
        Box::new(|cc| {
            let egraph = MyTx::sgl().egraph.lock().unwrap();
            Ok(Box::new(EGraphApp::new(
                cc,
                DemoLayout::Hierarchical,
                &egraph,
                SlotEventHandler {
                    map,
                    stats,
                    selected_cano_value,
                }
                .dyn_clone(),
            )))
        }),
    )
    .unwrap();
}

fn main() {
    env_logger::init();
    let stats = run_slotted_math_microbench();
    print_stats(&stats);
    MyTx::table_view();

    #[cfg(feature = "viewer")]
    view(stats);
}

#[cfg(test)]
mod tests {
    use super::*;
    use eggplant::wrap::{EgglogEnumVariantTy, RenderedTemplateField, render_variant_typst};

    #[test]
    fn slotted_math_microbench_produces_compaction() {
        let stats = run_slotted_math_microbench();
        assert!(stats.egraph_num_tuples > 0);
        assert!(stats.bucket_count > 0);
        assert!(stats.total_seclasses > 0);
        assert!(stats.total_senodes > 0);
        assert!(
            stats.total_senodes < stats.egraph_num_tuples,
            "expected slotted senodes ({}) to be fewer than raw egraph tuples ({})",
            stats.total_senodes,
            stats.egraph_num_tuples
        );
    }

    #[test]
    fn slotted_math_microbench_extracts_non_empty_best_term() {
        let _ = run_slotted_math_microbench();
        let bucket = preferred_extract_bucket();
        let extracted = extract_best_term_for_value(bucket.canonical_value());
        assert!(!extracted.trim().is_empty());
    }

    #[test]
    fn render_precedence_matches_expected() {
        let x = RenderedTemplateField::atom("x");
        let y = RenderedTemplateField::atom("y");
        let z = RenderedTemplateField::atom("z");

        let add_xy =
            render_variant_typst::<AddTy>(&[("l", x.clone()), ("r", y.clone())]).unwrap();
        let mul_yz =
            render_variant_typst::<MulTy>(&[("l", y.clone()), ("r", z.clone())]).unwrap();
        let add_x_mul_yz = render_variant_typst::<AddTy>(&[
            ("l", RenderedTemplateField::new("x", <VarTy as EgglogEnumVariantTy>::PRECEDENCE)),
            (
                "r",
                RenderedTemplateField::new(mul_yz, <MulTy as EgglogEnumVariantTy>::PRECEDENCE),
            ),
        ])
        .unwrap();
        let mul_add_xy_z = render_variant_typst::<MulTy>(&[
            (
                "l",
                RenderedTemplateField::new(add_xy, <AddTy as EgglogEnumVariantTy>::PRECEDENCE),
            ),
            ("r", RenderedTemplateField::new("z", <VarTy as EgglogEnumVariantTy>::PRECEDENCE)),
        ])
        .unwrap();

        assert_eq!(<AddTy as EgglogEnumVariantTy>::PRECEDENCE, 100);
        assert_eq!(<MulTy as EgglogEnumVariantTy>::PRECEDENCE, 200);
        assert_eq!(add_x_mul_yz, "x + y dot z");
        assert_eq!(mul_add_xy_z, "(x + y) dot z");
    }

    #[cfg(feature = "viewer")]
    #[test]
    fn simple_extract_display_matches_expected() {
        let expr: MathExpr<MyTx, _> = Mul::new(&Var::new(), &Add::new(&Var::new(), &Var::new()));
        expr.commit();
        let value = MyTx::canonical_raw(&expr);
        let egraph = MyTx::sgl().egraph.lock().unwrap();
        let sort = egraph
            .get_sort_by_name("MathExpr")
            .expect("MathExpr sort should exist");
        let rendered = eggplant::wrap::extract_value_template_string(&egraph, sort, value, false)
            .expect("display extract should succeed");
        assert_eq!(rendered, "v * (v + v)");
    }

    #[cfg(feature = "viewer")]
    #[test]
    fn simple_extract_typst_matches_experimental_probe_exactly() {
        let expr: MathExpr<MyTx, _> = Mul::new(&Var::new(), &Add::new(&Var::new(), &Var::new()));
        expr.commit();
        let value = MyTx::canonical_raw(&expr);
        let source = extract_best_term_typst_for_value(value);
        assert_eq!(source, "x dot (x + x)");
    }

    #[cfg(feature = "viewer")]
    #[test]
    fn slotted_math_microbench_writes_typst_svg() {
        let _ = run_slotted_math_microbench();
        let bucket = preferred_extract_bucket();
        let source = extract_best_term_typst_for_value(bucket.canonical_value());
        println!("typed typst source: {}", source);
        let path = write_typst_svg_for_value(bucket.canonical_value()).expect("svg write should succeed");
        assert!(path.exists(), "expected svg at {}", path.display());
        let bytes = std::fs::read(&path).expect("svg should be readable");
        assert!(!bytes.is_empty(), "svg file should not be empty");
        println!("typed svg path: {}", path.display());
    }

    #[cfg(feature = "viewer")]
    #[test]
    fn slotted_math_microbench_extract_typst_uses_templates() {
        let _ = run_slotted_math_microbench();
        let bucket = preferred_extract_bucket();
        let source = extract_best_term_typst_for_value(bucket.canonical_value());
        assert!(!source.contains("Add"));
        assert!(!source.contains("Var"));
        assert!(source.contains('x'));
        assert!(source.contains('+') || source.contains("dot"));
    }
}
