use eggplant::egglog::NumericId;
use eggplant::prelude::*;
use eggplant::slotted_tx_rx_vt_pr;
use eggplant::wrap::{EgglogTypeRegistry, RenderedTemplateField, render_template_with_precedence};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet, hash_map::DefaultHasher};
use std::fs;
use std::hash::{Hash, Hasher};
use std::io::Write;
use std::path::PathBuf;
use std::process::{Command, Stdio};
use std::sync::{Mutex, OnceLock};
use std::time::{Duration, Instant};

const DEFAULT_RUN_ITERS: usize = 2;

#[eggplant::slotted_dsl(base = SlotMetaBase)]
pub enum Math {
    #[eggplant::typst("diff({x}, {f})")]
    MDiff { x: Math, f: Math },
    #[eggplant::typst("integral {f} quad d {x}")]
    MIntegral { f: Math, x: Math },
    #[eggplant::typst("{a} + {b}")]
    #[eggplant::precedence(100)]
    MAdd { a: Math, b: Math },
    #[eggplant::typst("{a} - {b}")]
    #[eggplant::precedence(100)]
    MSub { a: Math, b: Math },
    #[eggplant::typst("{a} dot {b}")]
    #[eggplant::precedence(200)]
    MMul { a: Math, b: Math },
    #[eggplant::typst("{a} / {b}")]
    #[eggplant::precedence(200)]
    MDiv { a: Math, b: Math },
    #[eggplant::typst("{a}^({b})")]
    #[eggplant::precedence(300)]
    MPow { a: Math, b: Math },
    #[eggplant::typst("ln({a})")]
    MLn { a: Math },
    #[eggplant::typst("sqrt({a})")]
    MSqrt { a: Math },
    #[eggplant::typst("sin({a})")]
    MSin { a: Math },
    #[eggplant::typst("cos({a})")]
    MCos { a: Math },
    #[eggplant::typst("{num}")]
    MConst { num: i64 },
    #[eggplant::typst("{name}")]
    MVar { name: &'static str },
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
    self::Math<T, MVarTy>
{
    fn new_slot(name: &'static str) -> Self {
        let expr = MVar::new(name);
        T::replace_meta(
            expr.cur_sym(),
            Box::new(SlotMeta {
                inner: std::sync::Arc::new(SlotMetaInner {
                    sub_metas: vec![],
                    var_id_set: {
                        let mut idx_set = indexmap::IndexSet::default();
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
struct SlottedMathPortStats {
    elapsed: Duration,
    egraph_num_tuples: usize,
    bucket_count: usize,
    total_seclasses: usize,
    total_senodes: usize,
}

#[derive(Debug, Clone, Default)]
struct SlottedAnalysis {
    bucket_size_histogram: BTreeMap<usize, usize>,
    seclass_size_histogram: BTreeMap<usize, usize>,
    senodes_by_ty: BTreeMap<&'static str, usize>,
    seclasses_by_ty: BTreeMap<&'static str, usize>,
}

#[derive(Debug, Clone)]
struct SeedRootExtract {
    index: usize,
    canonical_value: egglog::Value,
    typst_source: String,
    svg_path: PathBuf,
}

#[derive(Debug, Clone)]
struct SingletonBucketSample {
    canonical_value: egglog::Value,
    senode_count: usize,
    seclass_count: usize,
    ty_name: &'static str,
    shape: Vec<Vec<usize>>,
    witness_count: usize,
    extract: String,
}

fn reset_seed_only_state() {
    MyPatRec::sgl().slotted_ctx.clear();
}

fn current_stats() -> SlottedMathPortStats {
    let egraph_num_tuples = MyTx::sgl().egraph.lock().unwrap().num_tuples();
    let buckets = MyPatRec::sgl().slotted_ctx.buckets();
    let bucket_count = buckets.len();
    let total_seclasses = buckets.iter().map(|bucket| bucket.seclass_count()).sum();
    let total_senodes = buckets.iter().map(|bucket| bucket.senode_count()).sum();
    SlottedMathPortStats {
        elapsed: Duration::default(),
        egraph_num_tuples,
        bucket_count,
        total_seclasses,
        total_senodes,
    }
}

fn current_analysis() -> SlottedAnalysis {
    let buckets = MyPatRec::sgl().slotted_ctx.buckets();
    let mut analysis = SlottedAnalysis::default();
    for bucket in buckets {
        *analysis
            .bucket_size_histogram
            .entry(bucket.senode_count())
            .or_default() += 1;
        for eclass in bucket.eclasses() {
            *analysis
                .seclass_size_histogram
                .entry(eclass.senode_ids().len())
                .or_default() += 1;
            if let Some((shape_key, _)) = eclass.shapes().first() {
                *analysis
                    .seclasses_by_ty
                    .entry(shape_key.ty_name())
                    .or_default() += 1;
                *analysis
                    .senodes_by_ty
                    .entry(shape_key.ty_name())
                    .or_default() += eclass.senode_ids().len();
            }
        }
    }
    analysis
}

fn print_analysis(label: &str, analysis: &SlottedAnalysis) {
    println!("[{label}] bucket_size_histogram:");
    for (size, count) in &analysis.bucket_size_histogram {
        println!("[{label}]   buckets with {size} senodes = {count}");
    }
    println!("[{label}] seclass_size_histogram:");
    for (size, count) in &analysis.seclass_size_histogram {
        println!("[{label}]   seclasses with {size} senodes = {count}");
    }
    println!("[{label}] senodes_by_ty:");
    for (ty, count) in &analysis.senodes_by_ty {
        println!("[{label}]   {ty} senodes = {count}");
    }
    println!("[{label}] seclasses_by_ty:");
    for (ty, count) in &analysis.seclasses_by_ty {
        println!("[{label}]   {ty} seclasses = {count}");
    }
}

fn collect_singleton_bucket_samples(
    ty_name: &'static str,
    limit: usize,
) -> Vec<SingletonBucketSample> {
    let mut samples = MyPatRec::sgl()
        .slotted_ctx
        .buckets()
        .into_iter()
        .filter_map(|bucket| {
            if bucket.senode_count() != 1 || bucket.eclasses().len() != 1 {
                return None;
            }
            let eclass = &bucket.eclasses()[0];
            let (shape_key, shape_entry) = eclass.shapes().first()?;
            if shape_key.ty_name() != ty_name {
                return None;
            }
            let extract = extract_best_term_typst_for_value(bucket.canonical_value());
            Some(SingletonBucketSample {
                canonical_value: bucket.canonical_value(),
                senode_count: bucket.senode_count(),
                seclass_count: bucket.seclass_count(),
                ty_name,
                shape: shape_key.de_bruijn().to_vec(),
                witness_count: shape_entry.witnesses().len(),
                extract,
            })
        })
        .collect::<Vec<_>>();

    samples.sort_by_key(|sample| std::cmp::Reverse(sample.extract.len()));
    samples.truncate(limit);
    samples
}

fn print_singleton_bucket_samples(label: &str, samples: &[SingletonBucketSample]) {
    println!("[{label}] singleton bucket samples = {}", samples.len());
    for (idx, sample) in samples.iter().enumerate() {
        println!(
            "[{label}] sample#{idx} cano={} ty={} senodes={} seclasses={} witnesses={} shape={:?} extract={}",
            sample.canonical_value.rep(),
            sample.ty_name,
            sample.senode_count,
            sample.seclass_count,
            sample.witness_count,
            sample.shape,
            sample.extract
        );
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct TrailStep {
    rule: &'static str,
    output_func: &'static str,
    input_label: &'static str,
    input_shape_hash: u64,
    input_history_hash: u64,
    projection_hash: u64,
    output_shape_hash: u64,
    event_hash: u64,
}

impl TrailStep {
    fn label(&self) -> String {
        format!("{}:{}<-{}", self.rule, self.output_func, self.input_label)
    }

    fn compose_hash(&self) -> u64 {
        hash_parts([
            hash_label(self.rule),
            hash_label(self.output_func),
            hash_label(self.input_label),
            self.input_shape_hash,
            self.output_shape_hash,
            self.projection_hash,
        ])
    }

    #[cfg(test)]
    fn strong_hash(&self) -> u64 {
        hash_parts([
            self.event_hash,
            hash_label(self.input_label),
            self.input_shape_hash,
            self.input_history_hash,
            self.projection_hash,
            self.output_shape_hash,
        ])
    }
}

#[derive(Clone, Debug, Default)]
struct ValueHistory {
    sketch: HistorySketch,
    strong_hash: u64,
    last_rule: Option<u32>,
    truncated: bool,
    trails: Vec<Vec<TrailStep>>,
}

#[derive(Clone, Debug)]
struct HistoryInput {
    label: &'static str,
    meta: SlotMeta,
    history: ValueHistory,
}

#[derive(Clone, Debug)]
struct HistoryEvent {
    id: usize,
    rule: &'static str,
    output_func: &'static str,
    output_value: u64,
    local_hash: u64,
    strong_hash: u64,
    truncated: bool,
    sketch: HistorySketch,
    trails: Vec<Vec<TrailStep>>,
}

#[derive(Clone, Debug)]
struct HistoryGroup {
    suffix_len: usize,
    suffix_hash: u64,
    support_events: usize,
    support_outputs: usize,
    truncated_events: usize,
    rules: Vec<&'static str>,
    trail_labels: Vec<String>,
    samples: Vec<String>,
}

#[derive(Clone, Debug, Default)]
struct HistorySummary {
    event_count: usize,
    value_count: usize,
    repeated_groups: Vec<HistoryGroup>,
}

impl HistorySummary {
    fn has_significant_partial_flow(&self, min_len: usize, min_support: usize) -> bool {
        self.repeated_groups
            .iter()
            .any(|group| group.suffix_len >= min_len && group.support_events >= min_support)
    }
}

#[derive(Default)]
struct HistoryRecorder {
    values: BTreeMap<u64, ValueHistory>,
    events: Vec<HistoryEvent>,
}

impl HistoryRecorder {
    fn lookup(&self, value: egglog::Value) -> ValueHistory {
        self.values
            .get(&value_rep(value))
            .cloned()
            .unwrap_or_default()
    }

    fn record_insert<T>(
        &mut self,
        rule: &'static str,
        output_func: &'static str,
        inputs: &[(&'static str, (egglog::Value, SlotMeta))],
        output: &(Value<T>, SlotMeta),
    ) {
        let input_snapshots = if inputs.is_empty() {
            vec![HistoryInput {
                label: "_const",
                meta: output.1.clone(),
                history: ValueHistory::default(),
            }]
        } else {
            inputs
                .iter()
                .map(|(label, (value, meta))| HistoryInput {
                    label,
                    meta: meta.clone(),
                    history: self.lookup(*value),
                })
                .collect()
        };

        let rule_id = rule_id(rule);
        let output_shape = meta_shape_hash(output_func, &output.1);
        let consumer_shape = consumer_pattern_hash(rule, output_func, &input_snapshots);
        let input_shape_atom_hashes = input_snapshots
            .iter()
            .enumerate()
            .map(|(idx, input)| {
                hash_parts([
                    hash_label(input.label),
                    meta_shape_hash(input.label, &input.meta),
                    input_projection_hash(idx, input),
                ])
            })
            .collect::<Vec<_>>();
        let local_event_hash = hash_parts([
            rule_id as u64,
            hash_label(output_func),
            output_shape,
            consumer_shape,
            hash_unordered_u64s(input_shape_atom_hashes),
        ]);
        let input_anchor_hashes = input_snapshots
            .iter()
            .enumerate()
            .map(|(idx, input)| {
                hash_parts([
                    hash_label(input.label),
                    meta_shape_hash(input.label, &input.meta),
                    input_projection_hash(idx, input),
                    input.history.strong_hash,
                ])
            })
            .collect::<Vec<_>>();
        let event_hash = hash_parts([
            hash_label("anchored-event"),
            local_event_hash,
            hash_unordered_u64s(input_anchor_hashes),
        ]);
        let sketch = HistorySketch::from_parent_edges(input_snapshots.iter().enumerate().map(
            |(idx, input)| {
                (
                    input.history.sketch.clone(),
                    DependencyEdgeSig {
                        producer_rule: input.history.last_rule.unwrap_or(0),
                        producer_output_shape: meta_shape_hash(input.label, &input.meta),
                        use_projection: input_projection_hash(idx, input),
                        consumer_rule: rule_id,
                        consumer_pattern_shape: consumer_shape ^ output_shape,
                    },
                )
            },
        ));
        let (trails, trail_truncated) = extend_debug_trails(
            rule,
            output_func,
            output_shape,
            event_hash,
            &input_snapshots,
        );
        let truncated = trail_truncated
            || sketch.is_truncated()
            || input_snapshots.iter().any(|input| input.history.truncated);
        let history = ValueHistory {
            sketch: sketch.clone(),
            strong_hash: event_hash,
            last_rule: Some(rule_id),
            truncated,
            trails: trails.clone(),
        };
        let output_value = value_rep(output.0.val);
        self.values.insert(output_value, history);
        self.events.push(HistoryEvent {
            id: self.events.len(),
            rule,
            output_func,
            output_value,
            local_hash: local_event_hash,
            strong_hash: event_hash,
            truncated,
            sketch,
            trails,
        });
    }

    fn record_union(&mut self, lhs: &(egglog::Value, SlotMeta), rhs: &(egglog::Value, SlotMeta)) {
        let lhs_value = value_rep(lhs.0);
        let rhs_value = value_rep(rhs.0);
        let lhs_history = self.lookup(lhs.0);
        let rhs_history = self.lookup(rhs.0);
        let strong_hash = hash_parts([
            hash_label("union"),
            hash_unordered_u64s([lhs_history.strong_hash, rhs_history.strong_hash]),
        ]);
        let last_rule = rhs_history.last_rule.or(lhs_history.last_rule);
        let inherited_truncated = lhs_history.truncated || rhs_history.truncated;
        let sketch = HistorySketch::merge_children([&lhs_history.sketch, &rhs_history.sketch]);
        let (trails, trail_truncated) = merge_debug_trails(lhs_history.trails, rhs_history.trails);
        let truncated = trail_truncated || sketch.is_truncated() || inherited_truncated;
        let merged = ValueHistory {
            sketch,
            strong_hash,
            last_rule,
            truncated,
            trails,
        };
        self.values.insert(lhs_value, merged.clone());
        self.values.insert(rhs_value, merged);
    }

    fn summary(&self) -> HistorySummary {
        #[derive(Default)]
        struct Acc {
            event_ids: BTreeSet<usize>,
            output_values: BTreeSet<u64>,
            truncated_event_ids: BTreeSet<usize>,
            rules: BTreeSet<&'static str>,
            trail_labels: BTreeSet<String>,
            samples: BTreeSet<String>,
        }

        let mut groups: BTreeMap<(usize, u64), Acc> = BTreeMap::new();
        for event in &self.events {
            for suffix_len in 1..=HISTORY_TRAJECTORY_WINDOW {
                for (hash, label) in
                    trail_suffix_compose_hash_label_pairs(&event.trails, suffix_len)
                {
                    let group = groups.entry((suffix_len, hash)).or_default();
                    group.event_ids.insert(event.id);
                    group.output_values.insert(event.output_value);
                    if event.truncated {
                        group.truncated_event_ids.insert(event.id);
                    }
                    group.rules.insert(event.rule);
                    group.trail_labels.insert(label.clone());
                    group.samples.insert(format!(
                        "{} value={} local_hash={:#x} event_hash={:#x} spines={} truncated={} {}",
                        event.output_func,
                        event.output_value,
                        event.local_hash,
                        event.strong_hash,
                        event.sketch.spine_count(),
                        event.truncated,
                        label
                    ));
                }
            }
        }

        let mut repeated_groups = groups
            .into_iter()
            .filter_map(|((suffix_len, suffix_hash), acc)| {
                if acc.event_ids.len() < 2 {
                    return None;
                }
                Some(HistoryGroup {
                    suffix_len,
                    suffix_hash,
                    support_events: acc.event_ids.len(),
                    support_outputs: acc.output_values.len(),
                    truncated_events: acc.truncated_event_ids.len(),
                    rules: acc.rules.into_iter().collect(),
                    trail_labels: acc.trail_labels.into_iter().take(4).collect(),
                    samples: acc.samples.into_iter().take(4).collect(),
                })
            })
            .collect::<Vec<_>>();
        repeated_groups.sort_by(|a, b| {
            b.support_events
                .cmp(&a.support_events)
                .then_with(|| b.suffix_len.cmp(&a.suffix_len))
                .then_with(|| a.suffix_hash.cmp(&b.suffix_hash))
        });

        HistorySummary {
            event_count: self.events.len(),
            value_count: self.values.len(),
            repeated_groups,
        }
    }
}

fn history_recorder() -> &'static Mutex<HistoryRecorder> {
    static RECORDER: OnceLock<Mutex<HistoryRecorder>> = OnceLock::new();
    RECORDER.get_or_init(|| Mutex::new(HistoryRecorder::default()))
}

fn reset_history_recorder() {
    *history_recorder().lock().unwrap() = HistoryRecorder::default();
}

fn record_history_insert<T>(
    rule: &'static str,
    output_func: &'static str,
    inputs: &[(&'static str, (egglog::Value, SlotMeta))],
    output: &(Value<T>, SlotMeta),
) {
    history_recorder()
        .lock()
        .unwrap()
        .record_insert(rule, output_func, inputs, output);
}

fn record_history_union(lhs: &(egglog::Value, SlotMeta), rhs: &(egglog::Value, SlotMeta)) {
    history_recorder().lock().unwrap().record_union(lhs, rhs);
}

fn value_meta<T, I>(
    ctx: &eggplant::wrap::RuleCtx<'_, '_, '_>,
    node: &I,
) -> (egglog::Value, SlotMeta)
where
    I: Insertable<T, MetaTy = SlotMeta>,
{
    (node.to_value(ctx).val, node.meta())
}

fn value_rep(value: egglog::Value) -> u64 {
    value.rep() as u64
}

fn rule_id(rule: &'static str) -> u32 {
    let hash = hash_parts([hash_label(rule)]);
    let id = (hash & 0xffff_ffff) as u32;
    if id == 0 { 1 } else { id }
}

fn hash_label(label: &str) -> u64 {
    let mut hasher = DefaultHasher::new();
    label.hash(&mut hasher);
    hasher.finish()
}

fn hash_parts(parts: impl IntoIterator<Item = u64>) -> u64 {
    let mut hasher = DefaultHasher::new();
    for part in parts {
        part.hash(&mut hasher);
    }
    hasher.finish()
}

fn hash_unordered_u64s(parts: impl IntoIterator<Item = u64>) -> u64 {
    let mut parts = parts.into_iter().collect::<Vec<_>>();
    parts.sort_unstable();
    hash_parts(parts)
}

fn meta_shape_hash(func: &'static str, meta: &SlotMeta) -> u64 {
    let mut hasher = DefaultHasher::new();
    func.hash(&mut hasher);
    meta.get_current_layer_de_bruijn().hash(&mut hasher);
    hasher.finish()
}

fn consumer_pattern_hash(
    rule: &'static str,
    output_func: &'static str,
    inputs: &[HistoryInput],
) -> u64 {
    let mut hasher = DefaultHasher::new();
    rule.hash(&mut hasher);
    output_func.hash(&mut hasher);
    for input in inputs {
        input.label.hash(&mut hasher);
        input.meta.get_current_layer_de_bruijn().hash(&mut hasher);
    }
    hasher.finish()
}

fn input_projection_hash(index: usize, input: &HistoryInput) -> u64 {
    let mut hasher = DefaultHasher::new();
    index.hash(&mut hasher);
    input.label.hash(&mut hasher);
    input.meta.get_current_layer_de_bruijn().hash(&mut hasher);
    hasher.finish()
}

fn extend_debug_trails(
    rule: &'static str,
    output_func: &'static str,
    output_shape_hash: u64,
    event_hash: u64,
    inputs: &[HistoryInput],
) -> (Vec<Vec<TrailStep>>, bool) {
    let mut trails = Vec::new();
    let mut truncated = false;
    for (idx, input) in inputs.iter().enumerate() {
        let input_shape_hash = meta_shape_hash(input.label, &input.meta);
        let step = TrailStep {
            rule,
            output_func,
            input_label: input.label,
            input_shape_hash,
            input_history_hash: input.history.strong_hash,
            projection_hash: input_projection_hash(idx, input),
            output_shape_hash,
            event_hash,
        };
        if input.history.trails.is_empty() {
            trails.push(vec![step]);
        } else {
            for trail in &input.history.trails {
                let mut next = trail.clone();
                next.push(step.clone());
                if next.len() > HISTORY_TRAJECTORY_WINDOW {
                    next.remove(0);
                    truncated = true;
                }
                trails.push(next);
            }
        }
    }
    let (trails, canonical_truncated) = canonicalize_debug_trails(trails);
    (trails, truncated || canonical_truncated)
}

fn merge_debug_trails(
    lhs: Vec<Vec<TrailStep>>,
    rhs: Vec<Vec<TrailStep>>,
) -> (Vec<Vec<TrailStep>>, bool) {
    canonicalize_debug_trails(lhs.into_iter().chain(rhs).collect())
}

fn trail_suffix_compose_hash_label_pairs(
    trails: &[Vec<TrailStep>],
    suffix_len: usize,
) -> Vec<(u64, String)> {
    let mut pairs = trails
        .iter()
        .filter(|trail| trail.len() >= suffix_len)
        .map(|trail| {
            let suffix = &trail[trail.len() - suffix_len..];
            let hash = hash_parts(suffix.iter().map(TrailStep::compose_hash));
            let label = suffix
                .iter()
                .map(TrailStep::label)
                .collect::<Vec<_>>()
                .join(" -> ");
            (hash, label)
        })
        .collect::<Vec<_>>();
    pairs.sort();
    pairs.dedup();
    pairs
}

#[derive(Clone, Copy)]
struct RulePatternDoc {
    matched: &'static str,
    inserted: &'static [&'static str],
    flows: &'static [RuleFlowDoc],
}

#[derive(Clone, Copy)]
struct RuleFlowDoc {
    trail: &'static str,
    path: &'static str,
}

fn rule_pattern_doc(rule: &str) -> Option<RulePatternDoc> {
    match rule {
        "add_comm" => Some(RulePatternDoc {
            matched: "MAdd(a, b)",
            inserted: &["rhs = MAdd(b, a)"],
            flows: &[
                RuleFlowDoc {
                    trail: "add_comm:MAdd<-a",
                    path: "a -> rhs = MAdd(b, a)",
                },
                RuleFlowDoc {
                    trail: "add_comm:MAdd<-b",
                    path: "b -> rhs = MAdd(b, a)",
                },
            ],
        }),
        "mul_comm" => Some(RulePatternDoc {
            matched: "MMul(a, b)",
            inserted: &["rhs = MMul(b, a)"],
            flows: &[
                RuleFlowDoc {
                    trail: "mul_comm:MMul<-a",
                    path: "a -> rhs = MMul(b, a)",
                },
                RuleFlowDoc {
                    trail: "mul_comm:MMul<-b",
                    path: "b -> rhs = MMul(b, a)",
                },
            ],
        }),
        "mul_assoc" => Some(RulePatternDoc {
            matched: "MMul(a, MMul(b, c))",
            inserted: &["ab = MMul(a, b)", "rhs = MMul(ab, c)"],
            flows: &[
                RuleFlowDoc {
                    trail: "mul_assoc:MMul<-b -> mul_assoc:MMul<-ab",
                    path: "b -> ab = MMul(a, b) -> rhs = MMul(ab, c)",
                },
                RuleFlowDoc {
                    trail: "mul_assoc:MMul<-a -> mul_assoc:MMul<-ab",
                    path: "a -> ab = MMul(a, b) -> rhs = MMul(ab, c)",
                },
            ],
        }),
        "sub_to_add_neg" => Some(RulePatternDoc {
            matched: "MSub(a, b)",
            inserted: &[
                "neg1 = MConst(-1)",
                "neg_b = MMul(neg1, b)",
                "rhs = MAdd(a, neg_b)",
            ],
            flows: &[
                RuleFlowDoc {
                    trail: "sub_to_add_neg:MConst<-_const -> sub_to_add_neg:MMul<-neg1 -> sub_to_add_neg:MAdd<-neg_b",
                    path: "-1 -> neg1 = MConst(-1) -> neg_b = MMul(neg1, b) -> rhs = MAdd(a, neg_b)",
                },
                RuleFlowDoc {
                    trail: "sub_to_add_neg:MMul<-b -> sub_to_add_neg:MAdd<-neg_b",
                    path: "b -> neg_b = MMul(neg1, b) -> rhs = MAdd(a, neg_b)",
                },
            ],
        }),
        "int_mul" => Some(RulePatternDoc {
            matched: "MIntegral(MMul(a, b), x)",
            inserted: &[
                "i_b = MIntegral(b, x)",
                "a_i_b = MMul(a, i_b)",
                "dxa = MDiff(x, a)",
                "mul = MMul(dxa, i_b)",
                "i2 = MIntegral(mul, x)",
                "rhs = MSub(a_i_b, i2)",
            ],
            flows: &[
                RuleFlowDoc {
                    trail: "int_mul:MIntegral<-b -> int_mul:MMul<-i_b -> int_mul:MSub<-a_i_b",
                    path: "b -> i_b = MIntegral(b, x) -> a_i_b = MMul(a, i_b) -> rhs = MSub(a_i_b, i2)",
                },
                RuleFlowDoc {
                    trail: "int_mul:MIntegral<-b -> int_mul:MMul<-i_b -> int_mul:MIntegral<-mul",
                    path: "b -> i_b = MIntegral(b, x) -> mul = MMul(dxa, i_b) -> i2 = MIntegral(mul, x)",
                },
                RuleFlowDoc {
                    trail: "int_mul:MDiff<-a -> int_mul:MMul<-dxa -> int_mul:MIntegral<-mul",
                    path: "a -> dxa = MDiff(x, a) -> mul = MMul(dxa, i_b) -> i2 = MIntegral(mul, x)",
                },
            ],
        }),
        "add_assoc" => Some(RulePatternDoc {
            matched: "MAdd(a, MAdd(b, c))",
            inserted: &["ab = MAdd(a, b)", "rhs = MAdd(ab, c)"],
            flows: &[
                RuleFlowDoc {
                    trail: "add_assoc:MAdd<-b -> add_assoc:MAdd<-ab",
                    path: "b -> ab = MAdd(a, b) -> rhs = MAdd(ab, c)",
                },
                RuleFlowDoc {
                    trail: "add_assoc:MAdd<-a -> add_assoc:MAdd<-ab",
                    path: "a -> ab = MAdd(a, b) -> rhs = MAdd(ab, c)",
                },
            ],
        }),
        "mul_distrib" => Some(RulePatternDoc {
            matched: "MMul(a, MAdd(b, c))",
            inserted: &["ab = MMul(a, b)", "ac = MMul(a, c)", "rhs = MAdd(ab, ac)"],
            flows: &[RuleFlowDoc {
                trail: "mul_distrib:MMul<-a -> mul_distrib:MAdd<-ab",
                path: "a -> ab = MMul(a, b) -> rhs = MAdd(ab, ac)",
            }],
        }),
        "add_factor" => Some(RulePatternDoc {
            matched: "MAdd(MMul(a, b), MMul(a, c))",
            inserted: &["bc = MAdd(b, c)", "rhs = MMul(a, bc)"],
            flows: &[RuleFlowDoc {
                trail: "add_factor:MMul<-b -> add_factor:MAdd<-bc",
                path: "b -> bc = MAdd(b, c) -> rhs = MMul(a, bc)",
            }],
        }),
        "diff_mul" => Some(RulePatternDoc {
            matched: "MDiff(x, MMul(a, b))",
            inserted: &[
                "da = MDiff(x, a)",
                "db = MDiff(x, b)",
                "a_db = MMul(a, db)",
                "b_da = MMul(b, da)",
                "rhs = MAdd(a_db, b_da)",
            ],
            flows: &[RuleFlowDoc {
                trail: "diff_mul:MDiff<-a -> diff_mul:MMul<-da -> diff_mul:MAdd<-a_db",
                path: "a -> da = MDiff(x, a) -> a_db = MMul(a, db) -> rhs = MAdd(a_db, b_da)",
            }],
        }),
        _ => None,
    }
}

fn rule_pattern_inserted_bindings(
    inserted: &[&'static str],
) -> Option<BTreeMap<String, ReportMathExpr>> {
    let mut bindings = BTreeMap::new();
    for line in inserted {
        let (lhs, rhs) = split_report_assignment(line)?;
        let rhs = parse_report_math_expr(rhs.trim())?;
        bindings.insert(lhs.trim().to_owned(), rhs);
    }
    Some(bindings)
}

fn rule_pattern_final_rhs(inserted: &[&'static str]) -> Option<ReportMathExpr> {
    let mut last_rhs = None;
    for line in inserted {
        let (_, rhs) = split_report_assignment(line)?;
        last_rhs = Some(parse_report_math_expr(rhs.trim())?);
    }
    last_rhs
}

fn expand_rule_pattern_expr(
    expr: &ReportMathExpr,
    bindings: &BTreeMap<String, ReportMathExpr>,
    stack: &mut BTreeSet<String>,
) -> ReportMathExpr {
    match expr {
        ReportMathExpr::Atom(atom) => {
            if stack.contains(atom) {
                return expr.clone();
            }
            if let Some(bound) = bindings.get(atom) {
                stack.insert(atom.clone());
                let expanded = expand_rule_pattern_expr(bound, bindings, stack);
                stack.remove(atom);
                expanded
            } else {
                expr.clone()
            }
        }
        ReportMathExpr::Call { head, args } => ReportMathExpr::Call {
            head: head.clone(),
            args: args
                .iter()
                .map(|arg| expand_rule_pattern_expr(arg, bindings, stack))
                .collect(),
        },
    }
}

fn rule_pattern_overall_formula(doc: RulePatternDoc) -> Option<String> {
    let bindings = rule_pattern_inserted_bindings(doc.inserted)?;
    let rhs = rule_pattern_final_rhs(doc.inserted)?;
    let expanded_rhs = expand_rule_pattern_expr(&rhs, &bindings, &mut BTreeSet::new());
    Some(format!(
        "{} = {}",
        typst_math_formula(doc.matched),
        render_report_math_expr_value(&expanded_rhs)
    ))
}

fn render_report_math_expr_value(expr: &ReportMathExpr) -> String {
    render_report_math_expr_node(expr, typst_report_registry())
        .text
        .into_owned()
}

fn render_report_math_assignment(lhs: &str, rhs: &ReportMathExpr) -> String {
    format!(
        "{} = {}",
        render_report_math_atom(lhs.trim()),
        render_report_math_expr_value(rhs)
    )
}

fn push_rule_pattern_doc(report: &mut String, rule: &'static str) {
    let Some(doc) = rule_pattern_doc(rule) else {
        return;
    };
    report.push_str("#### Rule Pattern Context\n\n");
    report.push_str(&format!("- matched: `{}`\n", doc.matched));
    report.push_str("- inserted DAG:\n");
    for line in doc.inserted {
        report.push_str(&format!("  - `{line}`\n"));
    }
    if let Some(formula) = rule_pattern_overall_formula(doc) {
        report.push_str(&format!("- overall formula: `{formula}`\n"));
    }
    report.push_str("- partial-flow paths:\n");
    for flow in doc.flows {
        report.push_str(&format!(
            "  - observed trail: `{}`; DAG path: `{}`\n",
            flow.trail, flow.path
        ));
    }
    report.push('\n');
}

fn push_rule_pattern_legend(report: &mut String, rules: &[&'static str]) {
    if rules.is_empty() {
        return;
    }
    report.push_str("## Rule Pattern Legend\n\n");
    for rule in rules {
        if rule_pattern_doc(rule).is_none() {
            continue;
        }
        report.push_str(&format!("### `{rule}`\n\n"));
        push_rule_pattern_doc(report, rule);
    }
}

fn is_macro_compose_group(group: &HistoryGroup) -> bool {
    group.rules.len() > 1
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct ParsedTrailStep {
    rule: String,
    output_func: String,
    input_label: String,
    raw: String,
}

impl ParsedTrailStep {
    fn signature(&self) -> String {
        format!("{}:{}<-{}", self.rule, self.output_func, self.input_label)
    }
}

fn parse_trail_step_label(label: &str) -> Option<ParsedTrailStep> {
    let (rule, rest) = label.split_once(':')?;
    let (output_func, input_label) = rest.split_once("<-")?;
    Some(ParsedTrailStep {
        rule: rule.trim().to_owned(),
        output_func: output_func.trim().to_owned(),
        input_label: input_label.trim().to_owned(),
        raw: label.trim().to_owned(),
    })
}

fn parse_trail_label_chain(label: &str) -> Vec<ParsedTrailStep> {
    label
        .split(" -> ")
        .filter_map(parse_trail_step_label)
        .collect()
}

fn macro_chain_signature(steps: &[ParsedTrailStep]) -> String {
    steps
        .iter()
        .map(|step| step.rule.as_str())
        .collect::<Vec<_>>()
        .join(" -> ")
}

fn inserted_assignment_lhs(line: &str) -> Option<&str> {
    let (lhs, _) = line.split_once('=')?;
    let lhs = lhs.trim();
    if lhs.is_empty() { None } else { Some(lhs) }
}

fn shared_bridge_inserted_line(rule: &str, next_input_label: &str) -> Option<&'static str> {
    let doc = rule_pattern_doc(rule)?;
    doc.inserted
        .iter()
        .copied()
        .find(|line| inserted_assignment_lhs(line).is_some_and(|lhs| lhs == next_input_label))
}

fn inserted_assignment_parts(line: &str) -> Option<(&str, ReportMathExpr)> {
    let (lhs, rhs) = split_report_assignment(line)?;
    Some((lhs.trim(), parse_report_math_expr(rhs.trim())?))
}

fn report_expr_head(expr: &ReportMathExpr) -> Option<&str> {
    match expr {
        ReportMathExpr::Atom(_) => None,
        ReportMathExpr::Call { head, .. } => Some(head.as_str()),
    }
}

fn report_expr_contains_atom(expr: &ReportMathExpr, target: &str) -> bool {
    match expr {
        ReportMathExpr::Atom(atom) => atom == target,
        ReportMathExpr::Call { args, .. } => args
            .iter()
            .any(|arg| report_expr_contains_atom(arg, target)),
    }
}

fn inserted_line_matches_step(line: &str, step: &ParsedTrailStep) -> bool {
    let Some((_, rhs)) = inserted_assignment_parts(line) else {
        return false;
    };
    report_expr_head(&rhs).is_some_and(|head| head == step.output_func)
        && report_expr_contains_atom(&rhs, step.input_label.as_str())
}

fn inserted_line_has_output_func(line: &str, output_func: &str) -> bool {
    let Some((_, rhs)) = inserted_assignment_parts(line) else {
        return false;
    };
    report_expr_head(&rhs).is_some_and(|head| head == output_func)
}

fn macro_step_inserted_line(steps: &[ParsedTrailStep], step_idx: usize) -> Option<&'static str> {
    let step = steps.get(step_idx)?;
    let doc = rule_pattern_doc(&step.rule)?;
    if let Some(next_input_label) = steps
        .get(step_idx + 1)
        .map(|next| next.input_label.as_str())
        && let Some(line) = doc.inserted.iter().copied().find(|line| {
            inserted_assignment_lhs(line).is_some_and(|lhs| lhs == next_input_label)
                && inserted_line_has_output_func(line, step.output_func.as_str())
        })
    {
        return Some(line);
    }
    doc.inserted
        .iter()
        .copied()
        .find(|line| inserted_line_matches_step(line, step))
}

fn macro_chain_prefix_bindings(
    steps: &[ParsedTrailStep],
    step_idx: usize,
) -> BTreeMap<String, ReportMathExpr> {
    let mut bindings = BTreeMap::new();
    for step in steps.iter().take(step_idx + 1) {
        if let Some(doc) = rule_pattern_doc(&step.rule)
            && let Some(rule_bindings) = rule_pattern_inserted_bindings(doc.inserted)
        {
            bindings.extend(rule_bindings);
        }
    }
    bindings
}

fn macro_step_composed_formula(steps: &[ParsedTrailStep], step_idx: usize) -> Option<String> {
    let line = macro_step_inserted_line(steps, step_idx)?;
    let (lhs, rhs) = inserted_assignment_parts(line)?;
    let bindings = macro_chain_prefix_bindings(steps, step_idx);
    let rhs = expand_rule_pattern_expr(&rhs, &bindings, &mut BTreeSet::new());
    Some(render_report_math_assignment(lhs, &rhs))
}

fn push_rule_pattern_card(report: &mut String, step_idx: usize, steps: &[ParsedTrailStep]) {
    let step = &steps[step_idx];
    report.push_str(&format!("##### Step {}: `{}`\n\n", step_idx + 1, step.rule));
    report.push_str(&format!("- observed trail: `{}`\n", step.raw));
    report.push_str(&format!("- slot path: `{}`\n", step.signature()));
    if let Some(prev_step) = step_idx.checked_sub(1).and_then(|idx| steps.get(idx))
        && shared_bridge_inserted_line(prev_step.rule.as_str(), step.input_label.as_str()).is_some()
    {
        report.push_str(&format!(
            "- input inherited from Step {}: `{}`\n",
            step_idx,
            step.input_label.as_str()
        ));
    }
    if let Some(doc) = rule_pattern_doc(&step.rule) {
        report.push_str(&format!("- matched: `{}`\n", doc.matched));
        report.push_str("- inserted DAG:\n");
        let next_input_label = steps
            .get(step_idx + 1)
            .map(|next| next.input_label.as_str());
        for line in doc.inserted {
            if let Some(next_input_label) = next_input_label
                && inserted_assignment_lhs(line).is_some_and(|lhs| lhs == next_input_label)
            {
                report.push_str(&format!(
                    "  - output used by Step {}: `{line}`\n",
                    step_idx + 2
                ));
                continue;
            }
            report.push_str(&format!("  - `{line}`\n"));
        }
        if let Some(formula) = macro_step_composed_formula(steps, step_idx) {
            report.push_str(&format!("- composed overall formula: `{formula}`\n"));
        }
        report.push_str("- partial-flow paths:\n");
        let matching_flows = doc
            .flows
            .iter()
            .copied()
            .filter(|flow| flow.trail.contains(&step.raw))
            .collect::<Vec<_>>();
        let flows = if matching_flows.is_empty() {
            doc.flows.to_vec()
        } else {
            matching_flows
        };
        for flow in flows {
            report.push_str(&format!(
                "  - observed trail: `{}`; DAG path: `{}`\n",
                flow.trail, flow.path
            ));
        }
    } else {
        report.push_str("- matched: `unavailable`\n");
    }
    report.push('\n');
}

fn push_macro_group_card(report: &mut String, idx: usize, group: &HistoryGroup) {
    let Some(trail_label) = group.trail_labels.first() else {
        return;
    };
    let steps = parse_trail_label_chain(trail_label);
    if steps.is_empty() {
        return;
    }

    report.push_str(&format!("### Group {}\n\n", idx));
    report.push_str(&format!(
        "- suffix_len: `{}`\n- suffix_hash: `{:#x}`\n- support_events: `{}`\n- support_outputs: `{}`\n- truncated_events: `{}`\n",
        group.suffix_len,
        group.suffix_hash,
        group.support_events,
        group.support_outputs,
        group.truncated_events
    ));
    report.push_str(&format!(
        "- macro chain: `{}`\n",
        macro_chain_signature(&steps)
    ));
    if let Some(first_rule) = steps.first().map(|step| step.rule.as_str()) {
        if let Some(doc) = rule_pattern_doc(first_rule) {
            report.push_str(&format!("- Proposition: `{}`\n", doc.matched));
        } else {
            report.push_str("- Proposition: `observed chain`\n");
        }
    }
    report.push('\n');
    report.push_str("#### proof-like chain\n\n");
    for step_idx in 0..steps.len() {
        push_rule_pattern_card(report, step_idx, &steps);
    }
    report.push('\n');
}

fn push_raw_evidence_appendix(report: &mut String, groups: &[&HistoryGroup]) {
    report.push_str("## Raw Evidence Appendix\n\n");
    for (idx, group) in groups.iter().take(20).enumerate() {
        report.push_str(&format!("### Group {}\n\n", idx));
        if !group.trail_labels.is_empty() {
            report.push_str("- trail labels:\n");
            for label in &group.trail_labels {
                report.push_str(&format!("  - `{label}`\n"));
            }
        }
        if !group.samples.is_empty() {
            report.push_str("- samples:\n");
            for sample in &group.samples {
                report.push_str(&format!("  - `{sample}`\n"));
            }
        }
        report.push('\n');
    }
}

fn canonicalize_debug_trails(mut trails: Vec<Vec<TrailStep>>) -> (Vec<Vec<TrailStep>>, bool) {
    trails.sort();
    trails.dedup();
    let truncated = trails.len() > HISTORY_SPINE_LIMIT;
    trails.truncate(HISTORY_SPINE_LIMIT);
    (trails, truncated)
}

fn history_summary_snapshot() -> HistorySummary {
    history_recorder().lock().unwrap().summary()
}

fn format_history_report(summary: &HistorySummary, min_len: usize, min_support: usize) -> String {
    let significant_count = summary
        .repeated_groups
        .iter()
        .filter(|group| group.suffix_len >= min_len && group.support_events >= min_support)
        .count();
    let truncated_group_count = summary
        .repeated_groups
        .iter()
        .filter(|group| group.truncated_events > 0)
        .count();
    let macro_compose_groups = summary
        .repeated_groups
        .iter()
        .filter(|group| is_macro_compose_group(group))
        .collect::<Vec<_>>();
    let omitted_single_rule_groups = summary
        .repeated_groups
        .len()
        .saturating_sub(macro_compose_groups.len());
    let mut report = String::from("# Slotted Math Microbenchmark History Report\n\n");
    report.push_str("## Summary\n\n");
    report.push_str(&format!(
        "- Rule-insert events: `{}`\n",
        summary.event_count
    ));
    report.push_str(&format!(
        "- Distinct history-bearing values: `{}`\n",
        summary.value_count
    ));
    report.push_str(
        "- Color legend: yellow = output used by the next step; green = input inherited from the previous step\n",
    );
    report.push_str(
        "- Group suffix hash basis: `rule + output func + input label + input/output shape + projection`\n",
    );
    report.push_str("- Event hash basis: `group basis + parent history`\n");
    report.push_str(&format!(
        "- Repeated trajectory groups: `{}`\n",
        summary.repeated_groups.len()
    ));
    report.push_str(&format!(
        "- Significant groups with suffix_len >= `{min_len}` and support >= `{min_support}`: `{significant_count}`\n\n",
    ));
    report.push_str(&format!(
        "- Groups with truncated provenance: `{}`\n\n",
        truncated_group_count
    ));
    report.push_str(&format!(
        "- Macro-compose groups shown below: `{}`\n",
        macro_compose_groups.len()
    ));
    report.push_str(&format!(
        "- Same-rule-only groups omitted: `{}`\n\n",
        omitted_single_rule_groups
    ));
    let mut legend_rules = BTreeSet::new();
    for group in &summary.repeated_groups {
        if let Some(rule) = group.rules.first() {
            legend_rules.insert(*rule);
        }
    }
    let legend_rules = legend_rules.into_iter().collect::<Vec<_>>();
    push_rule_pattern_legend(&mut report, &legend_rules);
    report.push_str("## Macro Rule Chains\n\n");
    report.push_str(
        "This section turns each repeated suffix into a proof-like macro view: chain signature, proposition, per-step inputs/outputs, and a small evidence appendix.\n\n",
    );
    for (idx, group) in macro_compose_groups.iter().take(20).enumerate() {
        push_macro_group_card(&mut report, idx, group);
    }
    push_raw_evidence_appendix(&mut report, &macro_compose_groups);
    report
}

fn format_history_report_typst(
    summary: &HistorySummary,
    min_len: usize,
    min_support: usize,
) -> String {
    let significant_count = summary
        .repeated_groups
        .iter()
        .filter(|group| group.suffix_len >= min_len && group.support_events >= min_support)
        .count();
    let truncated_group_count = summary
        .repeated_groups
        .iter()
        .filter(|group| group.truncated_events > 0)
        .count();
    let macro_compose_groups = summary
        .repeated_groups
        .iter()
        .filter(|group| is_macro_compose_group(group))
        .collect::<Vec<_>>();
    let omitted_single_rule_groups = summary
        .repeated_groups
        .len()
        .saturating_sub(macro_compose_groups.len());
    let mut report = String::new();
    report.push_str("#set page(margin: 0.7in)\n");
    report.push_str("#set par(justify: false)\n");
    report.push_str("#set text(size: 9pt)\n\n");
    report.push_str("= Slotted Math Microbenchmark History Report\n\n");
    report.push_str("== Summary\n\n");
    report.push_str(&format!(
        "- Rule-insert events: `{}`\n",
        summary.event_count
    ));
    report.push_str(&format!(
        "- Distinct history-bearing values: `{}`\n",
        summary.value_count
    ));
    report.push_str(
        "- Color legend: yellow = output used by the next step; green = input inherited from the previous step\n",
    );
    report.push_str(
        "- Group suffix hash basis: `rule + output func + input label + input/output shape + projection`\n",
    );
    report.push_str("- Event hash basis: `group basis + parent history`\n");
    report.push_str(&format!(
        "- Repeated trajectory groups: `{}`\n",
        summary.repeated_groups.len()
    ));
    report.push_str(&format!(
        "- Significant groups with suffix_len >= `{min_len}` and support >= `{min_support}`: `{significant_count}`\n",
    ));
    report.push_str(&format!(
        "- Groups with truncated provenance: `{}`\n\n",
        truncated_group_count
    ));
    report.push_str(&format!(
        "- Macro-compose groups shown below: `{}`\n",
        macro_compose_groups.len()
    ));
    report.push_str(&format!(
        "- Same-rule-only groups omitted: `{}`\n\n",
        omitted_single_rule_groups
    ));

    let mut legend_rules = BTreeSet::new();
    for group in &summary.repeated_groups {
        if let Some(rule) = group.rules.first() {
            legend_rules.insert(*rule);
        }
    }
    let legend_rules = legend_rules.into_iter().collect::<Vec<_>>();
    push_rule_pattern_legend_typst(&mut report, &legend_rules);
    report.push_str("== Macro Rule Chains\n\n");
    report.push_str(
        "This section turns each repeated suffix into a proof-like macro view: chain signature, proposition, per-step inputs and outputs, and a small evidence appendix.\n\n",
    );
    for (idx, group) in macro_compose_groups.iter().take(20).enumerate() {
        push_macro_group_card_typst(&mut report, idx, group);
    }
    push_raw_evidence_appendix_typst(&mut report, &macro_compose_groups);
    report
}

fn print_history_summary(summary: &HistorySummary, min_len: usize, min_support: usize) {
    let significant_count = summary
        .repeated_groups
        .iter()
        .filter(|group| group.suffix_len >= min_len && group.support_events >= min_support)
        .count();
    println!("[history] rule_insert_events = {}", summary.event_count);
    println!("[history] values = {}", summary.value_count);
    println!(
        "[history] repeated_groups = {} significant_groups = {}",
        summary.repeated_groups.len(),
        significant_count
    );
    for (idx, group) in summary.repeated_groups.iter().take(8).enumerate() {
        println!(
            "[history] group#{idx} len={} hash={:#x} events={} outputs={} truncated={} rules={:?}",
            group.suffix_len,
            group.suffix_hash,
            group.support_events,
            group.support_outputs,
            group.truncated_events,
            group.rules
        );
        for sample in group.samples.iter().take(2) {
            println!("[history]   sample {sample}");
        }
    }
}

fn push_rule_pattern_legend_typst(report: &mut String, rules: &[&'static str]) {
    if rules.is_empty() {
        return;
    }
    report.push_str("== Rule Pattern Legend\n\n");
    for rule in rules {
        if rule_pattern_doc(rule).is_none() {
            continue;
        }
        report.push_str(&format!("=== `{rule}`\n\n"));
        push_rule_pattern_doc_typst(report, rule);
    }
}

fn push_rule_pattern_doc_typst(report: &mut String, rule: &'static str) {
    let Some(doc) = rule_pattern_doc(rule) else {
        return;
    };
    report.push_str("==== Rule Pattern Context\n\n");
    report.push_str(&format!(
        "- matched: $ {} $\n",
        typst_math_formula(doc.matched)
    ));
    report.push_str("- inserted DAG:\n");
    for line in doc.inserted {
        report.push_str(&format!("  - $ {} $\n", typst_math_formula(line)));
    }
    if let Some(formula) = rule_pattern_overall_formula(doc) {
        report.push_str(&format!("- overall formula: $ {} $\n", formula));
    }
    report.push_str("- partial-flow paths:\n");
    for flow in doc.flows {
        report.push_str(&format!(
            "  - trail `{}`; path `{}`\n",
            flow.trail, flow.path
        ));
    }
    report.push('\n');
    if let Some((formula_line, bindings_line)) = rule_pattern_inference_typst(rule, doc) {
        report.push_str("==== inference rule\n\n");
        report.push_str("#grid(\n");
        report.push_str("  columns: (1fr, auto),\n");
        report.push_str("  gutter: 12pt,\n");
        report.push_str("  align: top,\n");
        report.push_str(")[\n");
        report.push_str(&format!("  {formula_line}\n"));
        report.push_str(&format!("  {bindings_line}\n"));
        report.push_str("]\n\n");
    }
}

fn rule_pattern_inference_typst(
    rule: &'static str,
    doc: RulePatternDoc,
) -> Option<(String, String)> {
    let rhs = doc
        .inserted
        .iter()
        .rev()
        .find_map(|line| line.split_once(" = ").map(|(_, rhs)| rhs.trim()))?;
    let premise = typst_math_formula(doc.matched);
    let conclusion = typst_math_formula(rhs);
    let formula_line = format!(
        r##"#text(size: 8pt, fill: rgb("#52606d"))[$frac({premise}, {premise} arrow.r.double {conclusion}) quad upright("if") quad upright("None")$]"##
    );
    let bindings = rule_pattern_binding_lines(rule)?;
    let bindings_line = format!(
        r##"#text(size: 8pt, fill: rgb("#52606d"))[${}$]"##,
        bindings.join(", ")
    );
    Some((formula_line, bindings_line))
}

fn rule_pattern_binding_lines(rule: &'static str) -> Option<Vec<String>> {
    let lines = match rule {
        "add_comm" => vec![
            r#"a = "a""#.to_owned(),
            r#"b = "b""#.to_owned(),
            typst_math_formula("rhs = MAdd(b, a)"),
        ],
        "mul_comm" => vec![
            r#"a = "a""#.to_owned(),
            r#"b = "b""#.to_owned(),
            typst_math_formula("rhs = MMul(b, a)"),
        ],
        "add_assoc" => vec![
            r#"a = "a""#.to_owned(),
            r#"b = "b""#.to_owned(),
            r#"c = "c""#.to_owned(),
            typst_math_formula("ab = MAdd(a, b)"),
            typst_math_formula("rhs = MAdd(ab, c)"),
        ],
        "mul_assoc" => vec![
            r#"a = "a""#.to_owned(),
            r#"b = "b""#.to_owned(),
            r#"c = "c""#.to_owned(),
            typst_math_formula("ab = MMul(a, b)"),
            typst_math_formula("rhs = MMul(ab, c)"),
        ],
        "sub_to_add_neg" => vec![
            r#"a = "a""#.to_owned(),
            r#"b = "b""#.to_owned(),
            typst_math_formula("neg1 = MConst(-1)"),
            typst_math_formula("neg_b = MMul(neg1, b)"),
            typst_math_formula("rhs = MAdd(a, neg_b)"),
        ],
        "mul_distrib" => vec![
            r#"a = "a""#.to_owned(),
            r#"b = "b""#.to_owned(),
            r#"c = "c""#.to_owned(),
            typst_math_formula("ab = MMul(a, b)"),
            typst_math_formula("ac = MMul(a, c)"),
            typst_math_formula("rhs = MAdd(ab, ac)"),
        ],
        "add_factor" => vec![
            r#"a = "a""#.to_owned(),
            r#"b = "b""#.to_owned(),
            r#"c = "c""#.to_owned(),
            typst_math_formula("bc = MAdd(b, c)"),
            typst_math_formula("rhs = MMul(a, bc)"),
        ],
        "diff_mul" => vec![
            r#"x = "x""#.to_owned(),
            r#"a = "a""#.to_owned(),
            r#"b = "b""#.to_owned(),
            typst_math_formula("da = MDiff(x, a)"),
            typst_math_formula("db = MDiff(x, b)"),
            typst_math_formula("a_db = MMul(a, db)"),
            typst_math_formula("b_da = MMul(b, da)"),
            typst_math_formula("rhs = MAdd(a_db, b_da)"),
        ],
        "int_mul" => vec![
            r#"a = "a""#.to_owned(),
            r#"b = "b""#.to_owned(),
            r#"x = "x""#.to_owned(),
            typst_math_formula("i_b = MIntegral(b, x)"),
            typst_math_formula("a_i_b = MMul(a, i_b)"),
            typst_math_formula("dxa = MDiff(x, a)"),
            typst_math_formula("mul = MMul(dxa, i_b)"),
            typst_math_formula("i2 = MIntegral(mul, x)"),
            typst_math_formula("rhs = MSub(a_i_b, i2)"),
        ],
        _ => return None,
    };
    Some(lines)
}

const TYPST_SHARED_OUTPUT_FILL: &str = "yellow";
const TYPST_SHARED_INPUT_FILL: &str = "green";

fn typst_colored_math(source: &str, fill: &str) -> String {
    format!("#text(fill: {fill})[$ {} $]", typst_math_formula(source))
}

fn push_step_flow_notes_typst(report: &mut String, step_idx: usize, steps: &[ParsedTrailStep]) {
    let step = &steps[step_idx];
    if let Some(prev_step) = step_idx.checked_sub(1).and_then(|idx| steps.get(idx))
        && shared_bridge_inserted_line(prev_step.rule.as_str(), step.input_label.as_str()).is_some()
    {
        report.push_str(&format!(
            "- input inherited from Step {}: {}\n",
            step_idx,
            typst_colored_math(step.input_label.as_str(), TYPST_SHARED_INPUT_FILL)
        ));
    }
    if let Some(next_step) = steps.get(step_idx + 1)
        && let Some(line) =
            shared_bridge_inserted_line(step.rule.as_str(), next_step.input_label.as_str())
    {
        report.push_str(&format!(
            "- output used by Step {}: {}\n",
            step_idx + 2,
            typst_colored_math(line, TYPST_SHARED_OUTPUT_FILL)
        ));
    }
}

fn push_macro_group_card_typst(report: &mut String, idx: usize, group: &HistoryGroup) {
    let Some(trail_label) = group.trail_labels.first() else {
        return;
    };
    let steps = parse_trail_label_chain(trail_label);
    if steps.is_empty() {
        return;
    }

    report.push_str(&format!("=== Group {}\n\n", idx));
    report.push_str(&format!(
        "- suffix_len: `{}`\n- suffix_hash: `{:#x}`\n- support_events: `{}`\n- support_outputs: `{}`\n- truncated_events: `{}`\n",
        group.suffix_len,
        group.suffix_hash,
        group.support_events,
        group.support_outputs,
        group.truncated_events
    ));
    report.push_str(&format!(
        "- macro chain: `{}`\n",
        macro_chain_signature(&steps)
    ));
    if let Some(first_rule) = steps.first().map(|step| step.rule.as_str()) {
        if let Some(doc) = rule_pattern_doc(first_rule) {
            report.push_str(&format!(
                "- Proposition: $ {} $\n",
                typst_math_formula(doc.matched)
            ));
        } else {
            report.push_str("- Proposition: `observed chain`\n");
        }
    }
    report.push('\n');
    report.push_str("==== proof-like chain\n\n");
    for step_idx in 0..steps.len() {
        push_rule_pattern_card_typst(report, step_idx, &steps);
    }
    report.push('\n');
}

fn push_rule_pattern_card_typst(report: &mut String, step_idx: usize, steps: &[ParsedTrailStep]) {
    let step = &steps[step_idx];
    report.push_str(&format!("===== Step {}: `{}`\n\n", step_idx + 1, step.rule));
    report.push_str(&format!("- observed trail: `{}`\n", step.raw));
    report.push_str(&format!("- slot path: `{}`\n", step.signature()));
    push_step_flow_notes_typst(report, step_idx, steps);
    if let Some(doc) = rule_pattern_doc(&step.rule) {
        report.push_str(&format!(
            "- matched: $ {} $\n",
            typst_math_formula(doc.matched)
        ));
        report.push_str("- inserted DAG:\n");
        let next_input_label = steps
            .get(step_idx + 1)
            .map(|next| next.input_label.as_str());
        for line in doc.inserted {
            if let Some(next_input_label) = next_input_label
                && inserted_assignment_lhs(line).is_some_and(|lhs| lhs == next_input_label)
            {
                report.push_str(&format!(
                    "  - output used by Step {}: {}\n",
                    step_idx + 2,
                    typst_colored_math(line, TYPST_SHARED_OUTPUT_FILL)
                ));
                continue;
            }
            report.push_str(&format!("  - $ {} $\n", typst_math_formula(line)));
        }
        if let Some(formula) = macro_step_composed_formula(steps, step_idx) {
            report.push_str(&format!("- composed overall formula: $ {} $\n", formula));
        }
        report.push_str("- partial-flow paths:\n");
        let matching_flows = doc
            .flows
            .iter()
            .copied()
            .filter(|flow| flow.trail.contains(&step.raw))
            .collect::<Vec<_>>();
        let flows = if matching_flows.is_empty() {
            doc.flows.to_vec()
        } else {
            matching_flows
        };
        for flow in flows {
            report.push_str(&format!(
                "  - observed trail: `{}`; DAG path: `{}`\n",
                flow.trail, flow.path
            ));
        }
    } else {
        report.push_str("- matched: `unavailable`\n");
    }
    report.push('\n');
}

fn push_raw_evidence_appendix_typst(report: &mut String, groups: &[&HistoryGroup]) {
    report.push_str("== Raw Evidence Appendix\n\n");
    for (idx, group) in groups.iter().take(20).enumerate() {
        report.push_str(&format!("=== Group {}\n\n", idx));
        if !group.trail_labels.is_empty() {
            report.push_str("- trail labels:\n");
            for label in &group.trail_labels {
                report.push_str(&format!("  - `{label}`\n"));
            }
        }
        if !group.samples.is_empty() {
            report.push_str("- samples:\n");
            for sample in &group.samples {
                report.push_str(&format!("  - `{sample}`\n"));
            }
        }
        report.push('\n');
    }
}

fn write_report_file(path: &PathBuf, contents: String) {
    if let Some(parent) = path.parent()
        && !parent.as_os_str().is_empty()
    {
        fs::create_dir_all(parent).unwrap();
    }
    fs::write(path, contents).unwrap();
    println!("wrote {}", path.display());
}

fn typst_math_formula(source: &str) -> String {
    let registry = typst_report_registry();
    if let Some((lhs, rhs)) = split_report_assignment(source) {
        let lhs = render_report_math_atom(lhs.trim());
        let rhs = render_report_math_expr(rhs.trim(), registry);
        return format!("{lhs} = {rhs}");
    }
    render_report_math_expr(source.trim(), registry)
}

fn typst_report_registry() -> &'static EgglogTypeRegistry {
    static REGISTRY: OnceLock<EgglogTypeRegistry> = OnceLock::new();
    REGISTRY.get_or_init(EgglogTypeRegistry::new_with_inventory)
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum ReportMathExpr {
    Atom(String),
    Call {
        head: String,
        args: Vec<ReportMathExpr>,
    },
}

struct ReportMathParser<'a> {
    chars: Vec<char>,
    idx: usize,
    _text: &'a str,
}

impl<'a> ReportMathParser<'a> {
    fn new(text: &'a str) -> Self {
        Self {
            chars: text.chars().collect(),
            idx: 0,
            _text: text,
        }
    }

    fn eof(&self) -> bool {
        self.idx >= self.chars.len()
    }

    fn skip_ws(&mut self) {
        while let Some(ch) = self.chars.get(self.idx) {
            if ch.is_whitespace() {
                self.idx += 1;
            } else {
                break;
            }
        }
    }

    fn peek(&self) -> Option<char> {
        self.chars.get(self.idx).copied()
    }

    fn parse_expr(&mut self) -> Option<ReportMathExpr> {
        self.skip_ws();
        let token = self.parse_atom_token();
        if token.is_empty() {
            return None;
        }
        self.skip_ws();
        if self.peek() == Some('(') {
            self.idx += 1;
            let args = self.parse_args()?;
            Some(ReportMathExpr::Call { head: token, args })
        } else {
            Some(ReportMathExpr::Atom(token))
        }
    }

    fn parse_args(&mut self) -> Option<Vec<ReportMathExpr>> {
        let mut args = Vec::new();
        loop {
            self.skip_ws();
            match self.peek() {
                Some(')') => {
                    self.idx += 1;
                    return Some(args);
                }
                Some(_) => {
                    let expr = self.parse_expr()?;
                    args.push(expr);
                    self.skip_ws();
                    match self.peek() {
                        Some(',') => {
                            self.idx += 1;
                        }
                        Some(')') => {
                            self.idx += 1;
                            return Some(args);
                        }
                        _ => return None,
                    }
                }
                None => return None,
            }
        }
    }

    fn parse_atom_token(&mut self) -> String {
        let start = self.idx;
        while let Some(ch) = self.chars.get(self.idx).copied() {
            if ch.is_whitespace() || ch == '(' || ch == ')' || ch == ',' {
                break;
            }
            self.idx += 1;
        }
        self.chars[start..self.idx].iter().collect()
    }
}

fn parse_report_math_expr(source: &str) -> Option<ReportMathExpr> {
    let mut parser = ReportMathParser::new(source);
    let expr = parser.parse_expr()?;
    parser.skip_ws();
    if parser.eof() { Some(expr) } else { None }
}

fn split_report_assignment(source: &str) -> Option<(&str, &str)> {
    let mut depth = 0isize;
    for (idx, ch) in source.char_indices() {
        match ch {
            '(' => depth += 1,
            ')' => depth -= 1,
            '=' if depth == 0 => {
                let lhs = source[..idx].trim_end();
                let rhs = source[idx + 1..].trim_start();
                if lhs.is_empty() || rhs.is_empty() {
                    return None;
                }
                return Some((lhs, rhs));
            }
            _ => {}
        }
    }
    None
}

fn render_report_math_expr(source: &str, registry: &EgglogTypeRegistry) -> String {
    parse_report_math_expr(source)
        .map(|expr| {
            render_report_math_expr_node(&expr, registry)
                .text
                .into_owned()
        })
        .unwrap_or_else(|| render_report_math_atom(source))
}

fn render_report_math_expr_node(
    expr: &ReportMathExpr,
    registry: &EgglogTypeRegistry,
) -> RenderedTemplateField<'static> {
    match expr {
        ReportMathExpr::Atom(atom) => render_report_math_atom_field(atom),
        ReportMathExpr::Call { head, args } => {
            let rendered_args = args
                .iter()
                .map(|arg| render_report_math_expr_node(arg, registry))
                .collect::<Vec<_>>();

            if let Some(variant) = registry.get_dsl_variant_for_constructor(head) {
                let field_names = variant
                    .fields
                    .iter()
                    .map(|field| field.name)
                    .filter(|name| !is_hidden_display_field_name(name))
                    .collect::<Vec<_>>();
                let template = variant.typst_template.or(variant.display_template);
                if let Some(template) = template
                    && field_names.len() == rendered_args.len()
                {
                    let field_refs = field_names
                        .iter()
                        .zip(rendered_args.iter())
                        .map(|(field_name, value)| {
                            (
                                *field_name,
                                RenderedTemplateField::new(value.text.as_ref(), value.precedence),
                            )
                        })
                        .collect::<Vec<_>>();
                    return RenderedTemplateField::new(
                        render_template_with_precedence(template, variant.precedence, &field_refs),
                        variant.precedence,
                    );
                }
            }

            let args = rendered_args
                .iter()
                .map(|arg| arg.text.as_ref())
                .collect::<Vec<_>>()
                .join(", ");
            RenderedTemplateField::new(
                if rendered_args.is_empty() {
                    head.clone()
                } else {
                    format!("upright({})({args})", typst_string_literal(head))
                },
                u16::MAX,
            )
        }
    }
}

fn render_report_math_atom(source: &str) -> String {
    render_report_math_atom_field(source).text.into_owned()
}

fn render_report_math_atom_field(source: &str) -> RenderedTemplateField<'static> {
    if source.parse::<i64>().is_ok() || source.parse::<f64>().is_ok() {
        RenderedTemplateField::atom(source.to_owned())
    } else if is_single_math_identifier(source) {
        RenderedTemplateField::atom(source.to_owned())
    } else {
        RenderedTemplateField::atom(format!("upright({})", typst_string_literal(source)))
    }
}

fn is_single_math_identifier(text: &str) -> bool {
    let mut chars = text.chars();
    matches!(chars.next(), Some(first) if first.is_ascii_alphabetic()) && chars.next().is_none()
}

fn typst_string_literal(text: &str) -> String {
    let mut out = String::with_capacity(text.len());
    out.push('"');
    for ch in text.chars() {
        match ch {
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            _ => out.push(ch),
        }
    }
    out.push('"');
    out
}

fn is_hidden_display_field_name(name: &str) -> bool {
    matches!(name, "_meta" | "__meta")
}

macro_rules! tracked_insert {
    ($ctx:expr, $rule:expr, $func:expr, [$($label:expr => $input:expr),* $(,)?], $output:expr) => {{
        let __inputs = vec![$(($label, value_meta(&($ctx).ctx, &$input))),*];
        let __output = $output;
        record_history_insert($rule, $func, &__inputs, &__output);
        __output
    }};
}

macro_rules! tracked_union {
    ($ctx:expr, $lhs:expr, $rhs:expr) => {{
        let __lhs = value_meta(&($ctx).ctx, &$lhs);
        let __rhs = value_meta(&($ctx).ctx, &$rhs);
        record_history_union(&__lhs, &__rhs);
        $ctx.union(&$lhs, &$rhs);
    }};
}

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

fn extract_best_term_typst_for_value(cano_value: egglog::Value) -> String {
    let egraph = MyTx::sgl().egraph.lock().unwrap();
    let sort = egraph
        .get_sort_by_name("Math")
        .expect("Math sort should exist");
    eggplant::wrap::extract_value_template_string(&egraph, sort, cano_value, true)
        .expect("extract typst should succeed")
}

fn render_typst_svg_bytes(source: &str) -> Result<Vec<u8>, String> {
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
    Ok(output.stdout)
}

fn write_seed_only_svgs(roots: &[egglog::Value]) -> Result<Vec<SeedRootExtract>, String> {
    roots
        .iter()
        .enumerate()
        .map(|(index, value)| {
            let source = extract_best_term_typst_for_value(*value);
            let svg = render_typst_svg_bytes(&source)?;
            let path = std::env::temp_dir().join(format!(
                "eggplant_slotted_math_microbenchmark_seed_root{}_{}.svg",
                index,
                value.rep()
            ));
            std::fs::write(&path, svg).map_err(|err| format!("failed to write svg: {err}"))?;
            Ok(SeedRootExtract {
                index,
                canonical_value: *value,
                typst_source: source,
                svg_path: path,
            })
        })
        .collect()
}

fn install_rules() -> RuleSetId {
    let ruleset = MyTx::new_ruleset("slotted_math_microbenchmark_port");

    MyTx::add_rule(
        "add_comm",
        ruleset,
        || {
            let a = Math::query_slot("a".to_string());
            let b = Math::query_slot("b".to_string());
            let add = MAdd::query(&a, &b);
            #[eggplant::slotted_pat_vars]
            struct Pat {
                a: Math,
                b: Math,
                add: MAdd,
            }
            Pat::new(a, b, add)
        },
        |ctx, pat| {
            let rhs = tracked_insert!(ctx,
                "add_comm",
                "MAdd",
                ["b" => pat.b, "a" => pat.a],
                ctx.insert_m_add(&pat.b, &pat.a)
            );
            tracked_union!(ctx, pat.add, rhs);
        },
    );
    MyTx::add_rule(
        "mul_comm",
        ruleset,
        || {
            let a = Math::query_slot("a".to_string());
            let b = Math::query_slot("b".to_string());
            let mul = MMul::query(&a, &b);
            #[eggplant::slotted_pat_vars]
            struct Pat {
                a: Math,
                b: Math,
                mul: MMul,
            }
            Pat::new(a, b, mul)
        },
        |ctx, pat| {
            let rhs = tracked_insert!(ctx,
                "mul_comm",
                "MMul",
                ["b" => pat.b, "a" => pat.a],
                ctx.insert_m_mul(&pat.b, &pat.a)
            );
            tracked_union!(ctx, pat.mul, rhs);
        },
    );
    MyTx::add_rule(
        "add_assoc",
        ruleset,
        || {
            let a = Math::query_slot("a".to_string());
            let b = Math::query_slot("b".to_string());
            let c = Math::query_slot("c".to_string());
            let add_inner = MAdd::query(&b, &c);
            let add_outer = MAdd::query(&a, &add_inner);
            #[eggplant::slotted_pat_vars]
            struct Pat {
                a: Math,
                b: Math,
                c: Math,
                add_outer: MAdd,
            }
            Pat::new(a, b, c, add_outer)
        },
        |ctx, pat| {
            let ab = tracked_insert!(ctx,
                "add_assoc",
                "MAdd",
                ["a" => pat.a, "b" => pat.b],
                ctx.insert_m_add(&pat.a, &pat.b)
            );
            let rhs = tracked_insert!(ctx,
                "add_assoc",
                "MAdd",
                ["ab" => ab, "c" => pat.c],
                ctx.insert_m_add(ab, &pat.c)
            );
            tracked_union!(ctx, pat.add_outer, rhs);
        },
    );
    MyTx::add_rule(
        "mul_assoc",
        ruleset,
        || {
            let a = Math::query_slot("a".to_string());
            let b = Math::query_slot("b".to_string());
            let c = Math::query_slot("c".to_string());
            let mul_inner = MMul::query(&b, &c);
            let mul_outer = MMul::query(&a, &mul_inner);
            #[eggplant::slotted_pat_vars]
            struct Pat {
                a: Math,
                b: Math,
                c: Math,
                mul_outer: MMul,
            }
            Pat::new(a, b, c, mul_outer)
        },
        |ctx, pat| {
            let ab = tracked_insert!(ctx,
                "mul_assoc",
                "MMul",
                ["a" => pat.a, "b" => pat.b],
                ctx.insert_m_mul(&pat.a, &pat.b)
            );
            let rhs = tracked_insert!(ctx,
                "mul_assoc",
                "MMul",
                ["ab" => ab, "c" => pat.c],
                ctx.insert_m_mul(ab, &pat.c)
            );
            tracked_union!(ctx, pat.mul_outer, rhs);
        },
    );
    MyTx::add_rule(
        "sub_to_add_neg",
        ruleset,
        || {
            let a = Math::query_slot("a".to_string());
            let b = Math::query_slot("b".to_string());
            let sub = MSub::query(&a, &b);
            #[eggplant::slotted_pat_vars]
            struct Pat {
                a: Math,
                b: Math,
                sub: MSub,
            }
            Pat::new(a, b, sub)
        },
        |ctx, pat| {
            let neg1 = tracked_insert!(ctx, "sub_to_add_neg", "MConst", [], ctx.insert_m_const(-1));
            let neg_b = tracked_insert!(ctx,
                "sub_to_add_neg",
                "MMul",
                ["neg1" => neg1, "b" => pat.b],
                ctx.insert_m_mul(neg1, &pat.b)
            );
            let rhs = tracked_insert!(ctx,
                "sub_to_add_neg",
                "MAdd",
                ["a" => pat.a, "neg_b" => neg_b],
                ctx.insert_m_add(&pat.a, neg_b)
            );
            tracked_union!(ctx, pat.sub, rhs);
        },
    );
    MyTx::add_rule(
        "add_zero",
        ruleset,
        || {
            let a = Math::query_slot("a".to_string());
            let z = MConst::query();
            let add = MAdd::query(&a, &z);
            #[eggplant::slotted_pat_vars]
            struct Pat {
                z: MConst,
                a: Math,
                add: MAdd,
            }
            Pat::new(z.clone(), a, add).assert(z.handle_num().eq(&0))
        },
        |ctx, pat| {
            tracked_union!(ctx, pat.add, pat.a);
        },
    );
    MyTx::add_rule(
        "mul_zero",
        ruleset,
        || {
            let a = Math::query_slot("a".to_string());
            let z = MConst::query();
            let mul = MMul::query(&a, &z);
            #[eggplant::slotted_pat_vars]
            struct Pat {
                z: MConst,
                mul: MMul,
            }
            Pat::new(z.clone(), mul).assert(z.handle_num().eq(&0))
        },
        |ctx, pat| {
            let z = tracked_insert!(ctx, "mul_zero", "MConst", [], ctx.insert_m_const(0));
            tracked_union!(ctx, pat.mul, z);
        },
    );
    MyTx::add_rule(
        "mul_one",
        ruleset,
        || {
            let a = Math::query_slot("a".to_string());
            let one = MConst::query();
            let mul = MMul::query(&a, &one);
            #[eggplant::slotted_pat_vars]
            struct Pat {
                one: MConst,
                a: Math,
                mul: MMul,
            }
            Pat::new(one.clone(), a, mul).assert(one.handle_num().eq(&1))
        },
        |ctx, pat| {
            tracked_union!(ctx, pat.mul, pat.a);
        },
    );
    MyTx::add_rule(
        "sub_self_zero",
        ruleset,
        || {
            let a = Math::query_slot("a".to_string());
            let sub = MSub::query(&a, &a);
            #[eggplant::slotted_pat_vars]
            struct Pat {
                sub: MSub,
            }
            Pat::new(sub)
        },
        |ctx, pat| {
            let z = tracked_insert!(ctx, "sub_self_zero", "MConst", [], ctx.insert_m_const(0));
            tracked_union!(ctx, pat.sub, z);
        },
    );
    MyTx::add_rule(
        "mul_distrib",
        ruleset,
        || {
            let a = Math::query_slot("a".to_string());
            let b = Math::query_slot("b".to_string());
            let c = Math::query_slot("c".to_string());
            let add = MAdd::query(&b, &c);
            let mul = MMul::query(&a, &add);
            #[eggplant::slotted_pat_vars]
            struct Pat {
                a: Math,
                b: Math,
                c: Math,
                mul: MMul,
            }
            Pat::new(a, b, c, mul)
        },
        |ctx, pat| {
            let ab = tracked_insert!(ctx,
                "mul_distrib",
                "MMul",
                ["a" => pat.a, "b" => pat.b],
                ctx.insert_m_mul(&pat.a, &pat.b)
            );
            let ac = tracked_insert!(ctx,
                "mul_distrib",
                "MMul",
                ["a" => pat.a, "c" => pat.c],
                ctx.insert_m_mul(&pat.a, &pat.c)
            );
            let rhs = tracked_insert!(ctx,
                "mul_distrib",
                "MAdd",
                ["ab" => ab, "ac" => ac],
                ctx.insert_m_add(ab, ac)
            );
            tracked_union!(ctx, pat.mul, rhs);
        },
    );
    MyTx::add_rule(
        "add_factor",
        ruleset,
        || {
            let a = Math::query_slot("a".to_string());
            let b = Math::query_slot("b".to_string());
            let c = Math::query_slot("c".to_string());
            let ab = MMul::query(&a, &b);
            let ac = MMul::query(&a, &c);
            let add = MAdd::query(&ab, &ac);
            #[eggplant::slotted_pat_vars]
            struct Pat {
                a: Math,
                b: Math,
                c: Math,
                add: MAdd,
            }
            Pat::new(a, b, c, add)
        },
        |ctx, pat| {
            let bc = tracked_insert!(ctx,
                "add_factor",
                "MAdd",
                ["b" => pat.b, "c" => pat.c],
                ctx.insert_m_add(&pat.b, &pat.c)
            );
            let rhs = tracked_insert!(ctx,
                "add_factor",
                "MMul",
                ["a" => pat.a, "bc" => bc],
                ctx.insert_m_mul(&pat.a, bc)
            );
            tracked_union!(ctx, pat.add, rhs);
        },
    );
    MyTx::add_rule(
        "mul_pow_combine",
        ruleset,
        || {
            let a = Math::query_slot("a".to_string());
            let b = Math::query_slot("b".to_string());
            let c = Math::query_slot("c".to_string());
            let pow_ab = MPow::query(&a, &b);
            let pow_ac = MPow::query(&a, &c);
            let mul = MMul::query(&pow_ab, &pow_ac);
            #[eggplant::slotted_pat_vars]
            struct Pat {
                a: Math,
                b: Math,
                c: Math,
                mul: MMul,
            }
            Pat::new(a, b, c, mul)
        },
        |ctx, pat| {
            let bc = tracked_insert!(ctx,
                "mul_pow_combine",
                "MAdd",
                ["b" => pat.b, "c" => pat.c],
                ctx.insert_m_add(&pat.b, &pat.c)
            );
            let rhs = tracked_insert!(ctx,
                "mul_pow_combine",
                "MPow",
                ["a" => pat.a, "bc" => bc],
                ctx.insert_m_pow(&pat.a, bc)
            );
            tracked_union!(ctx, pat.mul, rhs);
        },
    );
    MyTx::add_rule(
        "pow_one",
        ruleset,
        || {
            let x = Math::query_slot("x".to_string());
            let one = MConst::query();
            let pow = MPow::query(&x, &one);
            #[eggplant::slotted_pat_vars]
            struct Pat {
                one: MConst,
                x: Math,
                pow: MPow,
            }
            Pat::new(one.clone(), x, pow).assert(one.handle_num().eq(&1))
        },
        |ctx, pat| {
            tracked_union!(ctx, pat.pow, pat.x);
        },
    );
    MyTx::add_rule(
        "pow_two",
        ruleset,
        || {
            let x = Math::query_slot("x".to_string());
            let two = MConst::query();
            let pow = MPow::query(&x, &two);
            #[eggplant::slotted_pat_vars]
            struct Pat {
                two: MConst,
                x: Math,
                pow: MPow,
            }
            Pat::new(two.clone(), x, pow).assert(two.handle_num().eq(&2))
        },
        |ctx, pat| {
            let rhs = tracked_insert!(ctx,
                "pow_two",
                "MMul",
                ["x0" => pat.x, "x1" => pat.x],
                ctx.insert_m_mul(&pat.x, &pat.x)
            );
            tracked_union!(ctx, pat.pow, rhs);
        },
    );
    MyTx::add_rule(
        "diff_add",
        ruleset,
        || {
            let x = Math::query_slot("x".to_string());
            let a = Math::query_slot("a".to_string());
            let b = Math::query_slot("b".to_string());
            let add = MAdd::query(&a, &b);
            let diff = MDiff::query(&x, &add);
            #[eggplant::slotted_pat_vars]
            struct Pat {
                x: Math,
                a: Math,
                b: Math,
                diff: MDiff,
            }
            Pat::new(x, a, b, diff)
        },
        |ctx, pat| {
            let da = tracked_insert!(ctx,
                "diff_add",
                "MDiff",
                ["x" => pat.x, "a" => pat.a],
                ctx.insert_m_diff(&pat.x, &pat.a)
            );
            let db = tracked_insert!(ctx,
                "diff_add",
                "MDiff",
                ["x" => pat.x, "b" => pat.b],
                ctx.insert_m_diff(&pat.x, &pat.b)
            );
            let rhs = tracked_insert!(ctx,
                "diff_add",
                "MAdd",
                ["da" => da, "db" => db],
                ctx.insert_m_add(da, db)
            );
            tracked_union!(ctx, pat.diff, rhs);
        },
    );
    MyTx::add_rule(
        "diff_mul",
        ruleset,
        || {
            let x = Math::query_slot("x".to_string());
            let a = Math::query_slot("a".to_string());
            let b = Math::query_slot("b".to_string());
            let mul = MMul::query(&a, &b);
            let diff = MDiff::query(&x, &mul);
            #[eggplant::slotted_pat_vars]
            struct Pat {
                x: Math,
                a: Math,
                b: Math,
                diff: MDiff,
            }
            Pat::new(x, a, b, diff)
        },
        |ctx, pat| {
            let db = tracked_insert!(ctx,
                "diff_mul",
                "MDiff",
                ["x" => pat.x, "b" => pat.b],
                ctx.insert_m_diff(&pat.x, &pat.b)
            );
            let da = tracked_insert!(ctx,
                "diff_mul",
                "MDiff",
                ["x" => pat.x, "a" => pat.a],
                ctx.insert_m_diff(&pat.x, &pat.a)
            );
            let a_db = tracked_insert!(ctx,
                "diff_mul",
                "MMul",
                ["a" => pat.a, "db" => db],
                ctx.insert_m_mul(&pat.a, db)
            );
            let b_da = tracked_insert!(ctx,
                "diff_mul",
                "MMul",
                ["b" => pat.b, "da" => da],
                ctx.insert_m_mul(&pat.b, da)
            );
            let rhs = tracked_insert!(ctx,
                "diff_mul",
                "MAdd",
                ["a_db" => a_db, "b_da" => b_da],
                ctx.insert_m_add(a_db, b_da)
            );
            tracked_union!(ctx, pat.diff, rhs);
        },
    );
    MyTx::add_rule(
        "diff_sin",
        ruleset,
        || {
            let x = Math::query_slot("x".to_string());
            let sin = MSin::query(&x);
            let diff = MDiff::query(&x, &sin);
            #[eggplant::slotted_pat_vars]
            struct Pat {
                x: Math,
                diff: MDiff,
            }
            Pat::new(x, diff)
        },
        |ctx, pat| {
            let rhs = tracked_insert!(ctx,
                "diff_sin",
                "MCos",
                ["x" => pat.x],
                ctx.insert_m_cos(&pat.x)
            );
            tracked_union!(ctx, pat.diff, rhs);
        },
    );
    MyTx::add_rule(
        "diff_cos",
        ruleset,
        || {
            let x = Math::query_slot("x".to_string());
            let cos = MCos::query(&x);
            let diff = MDiff::query(&x, &cos);
            #[eggplant::slotted_pat_vars]
            struct Pat {
                x: Math,
                diff: MDiff,
            }
            Pat::new(x, diff)
        },
        |ctx, pat| {
            let neg1 = tracked_insert!(ctx, "diff_cos", "MConst", [], ctx.insert_m_const(-1));
            let sin = tracked_insert!(ctx,
                "diff_cos",
                "MSin",
                ["x" => pat.x],
                ctx.insert_m_sin(&pat.x)
            );
            let rhs = tracked_insert!(ctx,
                "diff_cos",
                "MMul",
                ["neg1" => neg1, "sin" => sin],
                ctx.insert_m_mul(neg1, sin)
            );
            tracked_union!(ctx, pat.diff, rhs);
        },
    );
    MyTx::add_rule(
        "int_one",
        ruleset,
        || {
            let x = Math::query_slot("x".to_string());
            let one = MConst::query();
            let integ = MIntegral::query(&one, &x);
            #[eggplant::slotted_pat_vars]
            struct Pat {
                one: MConst,
                x: Math,
                integ: MIntegral,
            }
            Pat::new(one.clone(), x, integ).assert(one.handle_num().eq(&1))
        },
        |ctx, pat| {
            tracked_union!(ctx, pat.integ, pat.x);
        },
    );
    MyTx::add_rule(
        "int_cos",
        ruleset,
        || {
            let x = Math::query_slot("x".to_string());
            let cos = MCos::query(&x);
            let integ = MIntegral::query(&cos, &x);
            #[eggplant::slotted_pat_vars]
            struct Pat {
                x: Math,
                integ: MIntegral,
            }
            Pat::new(x, integ)
        },
        |ctx, pat| {
            let rhs = tracked_insert!(ctx,
                "int_cos",
                "MSin",
                ["x" => pat.x],
                ctx.insert_m_sin(&pat.x)
            );
            tracked_union!(ctx, pat.integ, rhs);
        },
    );
    MyTx::add_rule(
        "int_sin",
        ruleset,
        || {
            let x = Math::query_slot("x".to_string());
            let sin = MSin::query(&x);
            let integ = MIntegral::query(&sin, &x);
            #[eggplant::slotted_pat_vars]
            struct Pat {
                x: Math,
                integ: MIntegral,
            }
            Pat::new(x, integ)
        },
        |ctx, pat| {
            let neg1 = tracked_insert!(ctx, "int_sin", "MConst", [], ctx.insert_m_const(-1));
            let cos = tracked_insert!(ctx,
                "int_sin",
                "MCos",
                ["x" => pat.x],
                ctx.insert_m_cos(&pat.x)
            );
            let rhs = tracked_insert!(ctx,
                "int_sin",
                "MMul",
                ["neg1" => neg1, "cos" => cos],
                ctx.insert_m_mul(neg1, cos)
            );
            tracked_union!(ctx, pat.integ, rhs);
        },
    );
    MyTx::add_rule(
        "int_add",
        ruleset,
        || {
            let f = Math::query_slot("f".to_string());
            let g = Math::query_slot("g".to_string());
            let x = Math::query_slot("x".to_string());
            let add = MAdd::query(&f, &g);
            let integ = MIntegral::query(&add, &x);
            #[eggplant::slotted_pat_vars]
            struct Pat {
                f: Math,
                g: Math,
                x: Math,
                integ: MIntegral,
            }
            Pat::new(f, g, x, integ)
        },
        |ctx, pat| {
            let i_f = tracked_insert!(ctx,
                "int_add",
                "MIntegral",
                ["f" => pat.f, "x" => pat.x],
                ctx.insert_m_integral(&pat.f, &pat.x)
            );
            let i_g = tracked_insert!(ctx,
                "int_add",
                "MIntegral",
                ["g" => pat.g, "x" => pat.x],
                ctx.insert_m_integral(&pat.g, &pat.x)
            );
            let rhs = tracked_insert!(ctx,
                "int_add",
                "MAdd",
                ["i_f" => i_f, "i_g" => i_g],
                ctx.insert_m_add(i_f, i_g)
            );
            tracked_union!(ctx, pat.integ, rhs);
        },
    );
    MyTx::add_rule(
        "int_sub",
        ruleset,
        || {
            let f = Math::query_slot("f".to_string());
            let g = Math::query_slot("g".to_string());
            let x = Math::query_slot("x".to_string());
            let sub = MSub::query(&f, &g);
            let integ = MIntegral::query(&sub, &x);
            #[eggplant::slotted_pat_vars]
            struct Pat {
                f: Math,
                g: Math,
                x: Math,
                integ: MIntegral,
            }
            Pat::new(f, g, x, integ)
        },
        |ctx, pat| {
            let i_f = tracked_insert!(ctx,
                "int_sub",
                "MIntegral",
                ["f" => pat.f, "x" => pat.x],
                ctx.insert_m_integral(&pat.f, &pat.x)
            );
            let i_g = tracked_insert!(ctx,
                "int_sub",
                "MIntegral",
                ["g" => pat.g, "x" => pat.x],
                ctx.insert_m_integral(&pat.g, &pat.x)
            );
            let rhs = tracked_insert!(ctx,
                "int_sub",
                "MSub",
                ["i_f" => i_f, "i_g" => i_g],
                ctx.insert_m_sub(i_f, i_g)
            );
            tracked_union!(ctx, pat.integ, rhs);
        },
    );
    MyTx::add_rule(
        "int_mul",
        ruleset,
        || {
            let a = Math::query_slot("a".to_string());
            let b = Math::query_slot("b".to_string());
            let x = Math::query_slot("x".to_string());
            let mul = MMul::query(&a, &b);
            let integ = MIntegral::query(&mul, &x);
            #[eggplant::slotted_pat_vars]
            struct Pat {
                a: Math,
                b: Math,
                x: Math,
                integ: MIntegral,
            }
            Pat::new(a, b, x, integ)
        },
        |ctx, pat| {
            let i_b = tracked_insert!(ctx,
                "int_mul",
                "MIntegral",
                ["b" => pat.b, "x" => pat.x],
                ctx.insert_m_integral(&pat.b, &pat.x)
            );
            let a_i_b = tracked_insert!(ctx,
                "int_mul",
                "MMul",
                ["a" => pat.a, "i_b" => i_b],
                ctx.insert_m_mul(&pat.a, i_b.clone())
            );
            let dxa = tracked_insert!(ctx,
                "int_mul",
                "MDiff",
                ["x" => pat.x, "a" => pat.a],
                ctx.insert_m_diff(&pat.x, &pat.a)
            );
            let mul = tracked_insert!(ctx,
                "int_mul",
                "MMul",
                ["dxa" => dxa, "i_b" => i_b],
                ctx.insert_m_mul(dxa, i_b)
            );
            let i2 = tracked_insert!(ctx,
                "int_mul",
                "MIntegral",
                ["mul" => mul, "x" => pat.x],
                ctx.insert_m_integral(mul, &pat.x)
            );
            let rhs = tracked_insert!(ctx,
                "int_mul",
                "MSub",
                ["a_i_b" => a_i_b, "i2" => i2],
                ctx.insert_m_sub(a_i_b, i2)
            );
            tracked_union!(ctx, pat.integ, rhs);
        },
    );

    ruleset
}

fn seed_expressions() -> Vec<egglog::Value> {
    let mut roots = Vec::new();
    let x = MVar::new_slot("x");
    let y = MVar::new_slot("y");
    let five = MVar::new_slot("five");

    let expr: Math<MyTx, _> = MIntegral::new(&MLn::new(&x), &x);
    expr.commit();
    roots.push(MyTx::canonical_raw(&expr));

    let expr: Math<MyTx, _> = MIntegral::new(&MAdd::new(&x, &MCos::new(&x)), &x);
    expr.commit();
    roots.push(MyTx::canonical_raw(&expr));

    let expr: Math<MyTx, _> = MIntegral::new(&MMul::new(&MCos::new(&x), &x), &x);
    expr.commit();
    roots.push(MyTx::canonical_raw(&expr));

    let expr: Math<MyTx, _> = MDiff::new(
        &x,
        &MAdd::new(&MConst::new(1), &MMul::new(&MConst::new(2), &x)),
    );
    expr.commit();
    roots.push(MyTx::canonical_raw(&expr));

    let expr: Math<MyTx, _> = MDiff::new(
        &x,
        &MSub::new(
            &MPow::new(&x, &MConst::new(3)),
            &MMul::new(&MConst::new(7), &MPow::new(&x, &MConst::new(2))),
        ),
    );
    expr.commit();
    roots.push(MyTx::canonical_raw(&expr));

    let expr: Math<MyTx, _> = MAdd::new(
        &MMul::new(&y, &MAdd::new(&x, &y)),
        &MSub::new(&MAdd::new(&x, &MConst::new(2)), &MAdd::new(&x, &x)),
    );
    expr.commit();
    roots.push(MyTx::canonical_raw(&expr));

    let expr: Math<MyTx, _> = MMul::new(&x, &MConst::new(1));
    expr.commit();
    roots.push(MyTx::canonical_raw(&expr));

    let expr: Math<MyTx, _> = MDiv::new(
        &MConst::new(1),
        &MSub::new(
            &MDiv::new(
                &MAdd::new(&MConst::new(1), &MSqrt::new(&five)),
                &MConst::new(2),
            ),
            &MDiv::new(
                &MSub::new(&MConst::new(1), &MSqrt::new(&five)),
                &MConst::new(2),
            ),
        ),
    );
    expr.commit();
    roots.push(MyTx::canonical_raw(&expr));
    roots
}

fn compute_stats_with_iters(iters: usize) -> SlottedMathPortStats {
    MyPatRec::sgl().slotted_ctx.clear();
    reset_history_recorder();
    let _ = seed_expressions();
    let ruleset = install_rules();

    let started = Instant::now();
    for _ in 0..iters {
        let _ = MyTx::run_ruleset(ruleset, RunConfig::Once);
    }
    let elapsed = started.elapsed();

    let egraph_num_tuples = MyTx::sgl().egraph.lock().unwrap().num_tuples();
    let buckets = MyPatRec::sgl().slotted_ctx.buckets();
    let bucket_count = buckets.len();
    let total_seclasses = buckets.iter().map(|bucket| bucket.seclass_count()).sum();
    let total_senodes = buckets.iter().map(|bucket| bucket.senode_count()).sum();

    SlottedMathPortStats {
        elapsed,
        egraph_num_tuples,
        bucket_count,
        total_seclasses,
        total_senodes,
    }
}

fn compute_stats() -> SlottedMathPortStats {
    compute_stats_with_iters(DEFAULT_RUN_ITERS)
}

fn run_port() -> SlottedMathPortStats {
    static STATS: OnceLock<SlottedMathPortStats> = OnceLock::new();
    STATS.get_or_init(compute_stats).clone()
}

fn main() {
    env_logger::init();
    let args = std::env::args().collect::<Vec<_>>();
    let iters = args
        .windows(2)
        .find(|window| window[0] == "--iters")
        .and_then(|window| window[1].parse::<usize>().ok())
        .unwrap_or(DEFAULT_RUN_ITERS);
    let dump_mmul_singletons = args
        .windows(2)
        .find(|window| window[0] == "--dump-mmul-singletons")
        .and_then(|window| window[1].parse::<usize>().ok());
    let history_report = args
        .windows(2)
        .find(|window| window[0] == "--history-report")
        .map(|window| PathBuf::from(&window[1]));
    let history_typst_report = args
        .windows(2)
        .find(|window| window[0] == "--history-typst-report")
        .map(|window| PathBuf::from(&window[1]));
    let history_min_len = args
        .windows(2)
        .find(|window| window[0] == "--history-min-len")
        .and_then(|window| window[1].parse::<usize>().ok())
        .unwrap_or(2);
    let history_min_support = args
        .windows(2)
        .find(|window| window[0] == "--history-min-support")
        .and_then(|window| window[1].parse::<usize>().ok())
        .unwrap_or(2);
    let require_history = args.iter().any(|arg| arg == "--require-history");
    let collect_history =
        history_report.is_some() || history_typst_report.is_some() || require_history;
    if std::env::args().any(|arg| arg == "--seed-only") {
        reset_seed_only_state();
        let roots = seed_expressions();
        let stats = current_stats();
        println!("slotted full math seed-only time: {:?}", stats.elapsed);
        println!("[raw] total num_tuples = {}", stats.egraph_num_tuples);
        println!("[slotted] bucket_count = {}", stats.bucket_count);
        println!("[slotted] total_seclasses = {}", stats.total_seclasses);
        println!("[slotted] total_senodes = {}", stats.total_senodes);
        match write_seed_only_svgs(&roots) {
            Ok(extracts) => {
                for extract in extracts {
                    println!(
                        "seed root {} canonical={} typst={}",
                        extract.index,
                        extract.canonical_value.rep(),
                        extract.typst_source
                    );
                    println!(
                        "seed root {} svg path: {}",
                        extract.index,
                        extract.svg_path.display()
                    );
                }
            }
            Err(err) => {
                eprintln!("failed to write seed-only svg: {err}");
                std::process::exit(1);
            }
        }
        return;
    }
    let analyze = std::env::args().any(|arg| arg == "--analyze");
    let stats = if collect_history || iters != DEFAULT_RUN_ITERS {
        compute_stats_with_iters(iters)
    } else {
        run_port()
    };
    println!("slotted full math time: {:?}", stats.elapsed);
    println!("[raw] total num_tuples = {}", stats.egraph_num_tuples);
    println!("[slotted] bucket_count = {}", stats.bucket_count);
    println!("[slotted] total_seclasses = {}", stats.total_seclasses);
    println!("[slotted] total_senodes = {}", stats.total_senodes);
    if analyze {
        let analysis = current_analysis();
        print_analysis("slotted", &analysis);
    }
    if let Some(limit) = dump_mmul_singletons {
        let samples = collect_singleton_bucket_samples("MMul", limit);
        print_singleton_bucket_samples("slotted/mmul", &samples);
    }
    if collect_history {
        let summary = history_summary_snapshot();
        print_history_summary(&summary, history_min_len, history_min_support);
        if let Some(path) = history_report {
            write_report_file(
                &path,
                format_history_report(&summary, history_min_len, history_min_support),
            );
        }
        if let Some(path) = history_typst_report {
            write_report_file(
                &path,
                format_history_report_typst(&summary, history_min_len, history_min_support),
            );
        }
        if require_history
            && !summary.has_significant_partial_flow(history_min_len, history_min_support)
        {
            eprintln!(
                "history smoke test failed: no group with suffix_len >= {} and support >= {}",
                history_min_len, history_min_support
            );
            std::process::exit(2);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use eggplant::wrap::EgglogTy;

    #[test]
    fn slotted_full_math_microbenchmark_compiles_shape() {
        let _seed: fn() -> Vec<egglog::Value> = seed_expressions;
        let _rules: fn() -> RuleSetId = install_rules;
        assert_eq!(<Math<(), ()> as EgglogTy>::TY_NAME, "Math");
    }

    #[test]
    fn strong_trail_hash_changes_with_projection_and_parent_history() {
        let base = TrailStep {
            rule: "r",
            output_func: "F",
            input_label: "x",
            input_shape_hash: 0x10,
            input_history_hash: 0x20,
            projection_hash: 0x30,
            output_shape_hash: 0x40,
            event_hash: 0x50,
        };
        let changed_projection = TrailStep {
            projection_hash: 0x31,
            ..base
        };
        let changed_parent = TrailStep {
            input_history_hash: 0x21,
            event_hash: 0x51,
            ..base
        };
        assert_ne!(base.compose_hash(), changed_projection.compose_hash());
        assert_eq!(base.compose_hash(), changed_parent.compose_hash());
        assert_ne!(base.strong_hash(), changed_projection.strong_hash());
        assert_ne!(base.strong_hash(), changed_parent.strong_hash());
    }

    #[test]
    fn compose_hash_ignores_unrelated_sibling_context() {
        let base = TrailStep {
            rule: "mul_assoc",
            output_func: "MMul",
            input_label: "b",
            input_shape_hash: 0x10,
            input_history_hash: 0x20,
            projection_hash: 0x30,
            output_shape_hash: 0x40,
            event_hash: 0x60,
        };
        let changed_sibling_context = TrailStep {
            event_hash: 0x80,
            ..base
        };
        assert_eq!(base.compose_hash(), changed_sibling_context.compose_hash());
    }

    #[test]
    fn history_report_explains_rule_dag_for_repeated_group() {
        let summary = HistorySummary {
            event_count: 2,
            value_count: 2,
            repeated_groups: vec![HistoryGroup {
                suffix_len: 2,
                suffix_hash: 0x1b6e5f3034caed04,
                support_events: 2,
                support_outputs: 2,
                truncated_events: 1,
                rules: vec!["mul_assoc"],
                trail_labels: vec!["mul_assoc:MMul<-b -> mul_assoc:MMul<-ab".to_owned()],
                samples: vec![
                    "MMul value=105 local_hash=0x1 event_hash=0x2 spines=4 mul_assoc:MMul<-b -> mul_assoc:MMul<-ab".to_owned(),
                ],
            }],
        };

        let report = format_history_report(&summary, 2, 2);

        assert!(report.contains("#### Rule Pattern Context"));
        assert!(report.contains("matched: `MMul(a, MMul(b, c))`"));
        assert!(report.contains("`ab = MMul(a, b)`"));
        assert!(report.contains("`rhs = MMul(ab, c)`"));
        assert!(report.contains("DAG path: `b -> ab = MMul(a, b) -> rhs = MMul(ab, c)`"));
        assert!(report.contains("truncated"));
    }

    #[test]
    fn history_report_renders_macro_chain_cards() {
        let summary = HistorySummary {
            event_count: 3,
            value_count: 2,
            repeated_groups: vec![HistoryGroup {
                suffix_len: 2,
                suffix_hash: 0x2a,
                support_events: 3,
                support_outputs: 2,
                truncated_events: 0,
                rules: vec!["add_assoc", "mul_comm"],
                trail_labels: vec!["add_assoc:MAdd<-b -> mul_comm:MMul<-ab".to_owned()],
                samples: vec![
                    "MAdd value=7 local_hash=0x1 event_hash=0x2 spines=3 add_assoc:MAdd<-b -> mul_comm:MMul<-ab".to_owned(),
                ],
            }],
        };

        let report = format_history_report(&summary, 2, 2);

        assert!(report.contains("## Macro Rule Chains"));
        assert!(report.contains("macro chain: `add_assoc -> mul_comm`"));
        assert!(report.contains("Proposition"));
        assert!(report.contains("Step 1"));
        assert!(report.contains("Step 2"));
        assert!(report.contains("proof-like"));
        assert!(report.contains("input inherited from Step 1"));
        assert!(report.contains("output used by Step 2"));
    }

    #[test]
    fn history_report_omits_same_rule_internal_groups() {
        let summary = HistorySummary {
            event_count: 2,
            value_count: 1,
            repeated_groups: vec![HistoryGroup {
                suffix_len: 2,
                suffix_hash: 0x15,
                support_events: 2,
                support_outputs: 1,
                truncated_events: 0,
                rules: vec!["int_mul"],
                trail_labels: vec!["int_mul:MDiff<-a -> int_mul:MMul<-dxa".to_owned()],
                samples: vec![
                    "MMul value=7 local_hash=0x1 event_hash=0x2 spines=3 int_mul:MDiff<-a -> int_mul:MMul<-dxa".to_owned(),
                ],
            }],
        };

        let report = format_history_report_typst(&summary, 2, 2);

        assert!(report.contains("Macro-compose groups shown below: `0`"));
        assert!(report.contains("Same-rule-only groups omitted: `1`"));
        assert!(!report.contains("- macro chain: `int_mul -> int_mul`"));
        assert!(!report.contains("observed trail: `int_mul:MDiff<-a`"));
    }

    #[test]
    fn history_report_renders_composed_step_formulas() {
        let summary = HistorySummary {
            event_count: 2,
            value_count: 1,
            repeated_groups: vec![HistoryGroup {
                suffix_len: 2,
                suffix_hash: 0x2b,
                support_events: 2,
                support_outputs: 1,
                truncated_events: 0,
                rules: vec!["add_assoc", "add_assoc"],
                trail_labels: vec!["add_assoc:MAdd<-b -> add_assoc:MAdd<-ab".to_owned()],
                samples: vec![
                    "MAdd value=7 local_hash=0x1 event_hash=0x2 spines=3 add_assoc:MAdd<-b -> add_assoc:MAdd<-ab".to_owned(),
                ],
            }],
        };

        let report = format_history_report(&summary, 2, 2);

        assert!(report.contains("composed overall formula"));
        assert!(report.contains("`upright(\"ab\") = a + b`"));
        assert!(report.contains("`upright(\"rhs\") = a + b + c`"));
    }

    #[test]
    fn typst_history_report_has_typst_front_matter_and_no_meta() {
        let summary = HistorySummary {
            event_count: 1,
            value_count: 1,
            repeated_groups: vec![HistoryGroup {
                suffix_len: 2,
                suffix_hash: 0x2a,
                support_events: 2,
                support_outputs: 2,
                truncated_events: 0,
                rules: vec!["add_assoc"],
                trail_labels: vec!["add_assoc:MAdd<-b -> add_assoc:MAdd<-ab".to_owned()],
                samples: vec!["MAdd value=7 local_hash=0x1 event_hash=0x2 spines=3 add_assoc:MAdd<-b -> add_assoc:MAdd<-ab".to_owned()],
            }],
        };

        let report = format_history_report_typst(&summary, 2, 2);

        assert!(report.contains("#set page"));
        assert!(report.contains("= Slotted Math Microbenchmark History Report"));
        assert!(report.contains("== Macro Rule Chains"));
        assert!(report.contains("a + b + c"));
        assert!(report.contains("==== inference rule"));
        assert!(report.contains(r##"#text(size: 8pt, fill: rgb("#52606d"))[$frac("##));
        assert!(report.contains(r#"quad upright("if") quad upright("None")"#));
        assert!(report.contains(r##"#text(size: 8pt, fill: rgb("#52606d"))[$a = "a", b = "b", c = "c", upright("ab") = a + b, upright("rhs") = upright("ab") + c$]"##));
        assert!(report.contains(r#"upright("ab") = a + b"#));
        assert!(report.contains(r#"upright("rhs") = upright("ab") + c"#));
        assert!(!report.contains(r#"upright("MAdd")("#));
        assert_eq!(
            typst_math_formula("MIntegral(MMul(a, b), x)"),
            "integral (a dot b) quad d x"
        );
        assert!(!report.contains("__meta"));
        assert!(!report.contains("_meta"));
    }

    #[test]
    fn typst_history_report_renders_composed_step_formulas() {
        let summary = HistorySummary {
            event_count: 2,
            value_count: 1,
            repeated_groups: vec![HistoryGroup {
                suffix_len: 2,
                suffix_hash: 0x2b,
                support_events: 2,
                support_outputs: 1,
                truncated_events: 0,
                rules: vec!["add_assoc", "add_assoc"],
                trail_labels: vec!["add_assoc:MAdd<-b -> add_assoc:MAdd<-ab".to_owned()],
                samples: vec![
                    "MAdd value=7 local_hash=0x1 event_hash=0x2 spines=3 add_assoc:MAdd<-b -> add_assoc:MAdd<-ab".to_owned(),
                ],
            }],
        };

        let report = format_history_report_typst(&summary, 2, 2);

        assert!(report.contains("composed overall formula"));
        assert!(report.contains(r#"upright("ab") = a + b"#));
        assert!(report.contains(r#"upright("rhs") = a + b + c"#));
    }

    #[test]
    fn rule_pattern_overall_formula_expands_int_mul_intermediates() {
        let formula = rule_pattern_overall_formula(rule_pattern_doc("int_mul").unwrap())
            .expect("int_mul formula should render");

        assert!(formula.contains("integral"));
        assert!(formula.contains("diff"));
        assert!(!formula.contains("i_b"));
        assert!(!formula.contains("a_i_b"));
        assert!(!formula.contains("dxa"));
        assert!(!formula.contains("mul"));
        assert!(!formula.contains("i2"));
    }

    #[test]
    fn typst_history_report_marks_step_flow_direction() {
        let summary = HistorySummary {
            event_count: 2,
            value_count: 2,
            repeated_groups: vec![HistoryGroup {
                suffix_len: 2,
                suffix_hash: 0x2a,
                support_events: 2,
                support_outputs: 2,
                truncated_events: 0,
                rules: vec!["add_assoc", "add_assoc"],
                trail_labels: vec!["add_assoc:MAdd<-b -> add_assoc:MAdd<-ab".to_owned()],
                samples: vec![
                    "MAdd value=7 local_hash=0x1 event_hash=0x2 spines=3 add_assoc:MAdd<-b -> add_assoc:MAdd<-ab".to_owned(),
                ],
            }],
        };

        let report = format_history_report_typst(&summary, 2, 2);

        assert!(report.contains("Color legend: yellow = output used by the next step"));
        assert!(report.contains("input inherited from Step 1"));
        assert!(report.contains("output used by Step 2"));
        assert!(!report.contains("shared bridges"));
        assert!(report.contains(r#"upright("ab") = a + b"#));
        assert!(report.contains(r#"#text(fill: yellow)[$ upright("ab") = a + b $]"#));
        assert!(report.contains(r#"#text(fill: green)[$ upright("ab") $]"#));
    }
}
