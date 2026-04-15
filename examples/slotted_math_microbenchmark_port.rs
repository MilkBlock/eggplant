use eggplant::prelude::*;
use eggplant::egglog::NumericId;
use eggplant::slotted_tx_rx_vt_pr;
use eggplant::wrap::NodeDropperSgl;
use serde::{Deserialize, Serialize};
use std::io::Write;
use std::path::PathBuf;
use std::process::{Command, Stdio};
use std::sync::OnceLock;
use std::time::{Duration, Instant};
use std::collections::BTreeMap;

const DEFAULT_RUN_ITERS: usize = 2;

#[eggplant::slotted_dsl(base = SlotMetaBase)]
pub enum Math {
    #[eggplant::typst("diff({x}, {f})")]
    MDiff { x: Math, f: Math },
    #[eggplant::typst("integral({f}, {x})")]
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
    Inner { inner: SlotMeta },
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
                *analysis.seclasses_by_ty.entry(shape_key.ty_name()).or_default() += 1;
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

fn collect_singleton_bucket_samples(ty_name: &'static str, limit: usize) -> Vec<SingletonBucketSample> {
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
    roots.iter()
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
            let rhs = ctx.insert_m_add(&pat.b, &pat.a);
            ctx.union(&pat.add, rhs);
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
            let rhs = ctx.insert_m_mul(&pat.b, &pat.a);
            ctx.union(&pat.mul, rhs);
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
            let ab = ctx.insert_m_add(&pat.a, &pat.b);
            let rhs = ctx.insert_m_add(ab, &pat.c);
            ctx.union(&pat.add_outer, rhs);
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
            let ab = ctx.insert_m_mul(&pat.a, &pat.b);
            let rhs = ctx.insert_m_mul(ab, &pat.c);
            ctx.union(&pat.mul_outer, rhs);
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
            let neg1 = ctx.insert_m_const(-1);
            let neg_b = ctx.insert_m_mul(neg1, &pat.b);
            let rhs = ctx.insert_m_add(&pat.a, neg_b);
            ctx.union(&pat.sub, rhs);
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
            ctx.union(&pat.add, &pat.a);
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
            let z = ctx.insert_m_const(0);
            ctx.union(&pat.mul, z);
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
            ctx.union(&pat.mul, &pat.a);
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
            let z = ctx.insert_m_const(0);
            ctx.union(&pat.sub, z);
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
            let ab = ctx.insert_m_mul(&pat.a, &pat.b);
            let ac = ctx.insert_m_mul(&pat.a, &pat.c);
            let rhs = ctx.insert_m_add(ab, ac);
            ctx.union(&pat.mul, rhs);
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
            let bc = ctx.insert_m_add(&pat.b, &pat.c);
            let rhs = ctx.insert_m_mul(&pat.a, bc);
            ctx.union(&pat.add, rhs);
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
            let bc = ctx.insert_m_add(&pat.b, &pat.c);
            let rhs = ctx.insert_m_pow(&pat.a, bc);
            ctx.union(&pat.mul, rhs);
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
            ctx.union(&pat.pow, &pat.x);
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
            let rhs = ctx.insert_m_mul(&pat.x, &pat.x);
            ctx.union(&pat.pow, rhs);
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
            let da = ctx.insert_m_diff(&pat.x, &pat.a);
            let db = ctx.insert_m_diff(&pat.x, &pat.b);
            let rhs = ctx.insert_m_add(da, db);
            ctx.union(&pat.diff, rhs);
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
            let db = ctx.insert_m_diff(&pat.x, &pat.b);
            let da = ctx.insert_m_diff(&pat.x, &pat.a);
            let a_db = ctx.insert_m_mul(&pat.a, db);
            let b_da = ctx.insert_m_mul(&pat.b, da);
            let rhs = ctx.insert_m_add(a_db, b_da);
            ctx.union(&pat.diff, rhs);
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
            let rhs = ctx.insert_m_cos(&pat.x);
            ctx.union(&pat.diff, rhs);
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
            let neg1 = ctx.insert_m_const(-1);
            let sin = ctx.insert_m_sin(&pat.x);
            let rhs = ctx.insert_m_mul(neg1, sin);
            ctx.union(&pat.diff, rhs);
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
            ctx.union(&pat.integ, &pat.x);
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
            let rhs = ctx.insert_m_sin(&pat.x);
            ctx.union(&pat.integ, rhs);
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
            let neg1 = ctx.insert_m_const(-1);
            let cos = ctx.insert_m_cos(&pat.x);
            let rhs = ctx.insert_m_mul(neg1, cos);
            ctx.union(&pat.integ, rhs);
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
            let i_f = ctx.insert_m_integral(&pat.f, &pat.x);
            let i_g = ctx.insert_m_integral(&pat.g, &pat.x);
            let rhs = ctx.insert_m_add(i_f, i_g);
            ctx.union(&pat.integ, rhs);
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
            let i_f = ctx.insert_m_integral(&pat.f, &pat.x);
            let i_g = ctx.insert_m_integral(&pat.g, &pat.x);
            let rhs = ctx.insert_m_sub(i_f, i_g);
            ctx.union(&pat.integ, rhs);
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
            let i_b = ctx.insert_m_integral(&pat.b, &pat.x);
            let a_i_b = ctx.insert_m_mul(&pat.a, i_b.clone());
            let dxa = ctx.insert_m_diff(&pat.x, &pat.a);
            let mul = ctx.insert_m_mul(dxa, i_b);
            let i2 = ctx.insert_m_integral(mul, &pat.x);
            let rhs = ctx.insert_m_sub(a_i_b, i2);
            ctx.union(&pat.integ, rhs);
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
        &MSub::new(
            &MAdd::new(&x, &MConst::new(2)),
            &MAdd::new(&x, &x),
        ),
    );
    expr.commit();
    roots.push(MyTx::canonical_raw(&expr));

    let expr: Math<MyTx, _> = MMul::new(&x, &MConst::new(1));
    expr.commit();
    roots.push(MyTx::canonical_raw(&expr));

    let expr: Math<MyTx, _> = MDiv::new(
        &MConst::new(1),
        &MSub::new(
            &MDiv::new(&MAdd::new(&MConst::new(1), &MSqrt::new(&five)), &MConst::new(2)),
            &MDiv::new(&MSub::new(&MConst::new(1), &MSqrt::new(&five)), &MConst::new(2)),
        ),
    );
    expr.commit();
    roots.push(MyTx::canonical_raw(&expr));
    roots
}

fn compute_stats_with_iters(iters: usize) -> SlottedMathPortStats {
    MyPatRec::sgl().slotted_ctx.clear();
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
                    println!("seed root {} svg path: {}", extract.index, extract.svg_path.display());
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
    let stats = if iters == DEFAULT_RUN_ITERS {
        run_port()
    } else {
        compute_stats_with_iters(iters)
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
}
