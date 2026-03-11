use egglog::EGraph;
use egglog::SerializeConfig;
use egglog::Value;
use egglog::prelude::*;
use std::path::Path;
use std::path::PathBuf;
use std::sync::Once;
use std::sync::Arc;

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

static CONFIGURE_RAYON: Once = Once::new();

pub fn configure_rayon_once() {
    CONFIGURE_RAYON.call_once(|| {
        rayon::ThreadPoolBuilder::new()
            .num_threads(1)
            .build_global()
            .unwrap();
    });
}

#[derive(Clone)]
pub struct EgglogBenchCase {
    pub name: String,
    pub filename: String,
    pub program: String,
}

impl ToString for EgglogBenchCase {
    fn to_string(&self) -> String {
        self.name.clone()
    }
}

pub fn egglog_bench_cases(glob_pat: &str) -> Vec<EgglogBenchCase> {
    configure_rayon_once();

    glob::glob(glob_pat)
        .unwrap()
        .filter_map(Result::ok)
        .filter(|path| !path.to_string_lossy().contains("fail-typecheck"))
        // The egglog test harness runs `tests/proofs/*.egg` under proofs mode; running them in
        // normal mode will panic (e.g. on `(prove ...)`).
        .filter(|path| !path.parent().is_some_and(|p| p.ends_with("proofs")))
        .map(|path: PathBuf| {
            let filename = path.to_string_lossy().to_string();
            let program = std::fs::read_to_string(&filename).unwrap();
            let name = path.file_stem().unwrap().to_string_lossy().to_string();
            EgglogBenchCase {
                name,
                filename,
                program,
            }
        })
        .collect()
}

pub fn bench_egglog_case(case: &EgglogBenchCase) {
    configure_rayon_once();

    let egglog_root = egglog_repo_root();
    let mut egraph = EGraph::default();
    egraph.fact_directory = Some(egglog_root.clone());

    let program = rewrite_relative_file_paths(&case.program, &egglog_root);
    egraph
        .parse_and_run_program(Some(case.filename.clone()), &program)
        .unwrap();
    // Match egglog's benchmark behavior: include serialization cost.
    egraph.serialize(SerializeConfig::default());
}

fn egglog_repo_root() -> PathBuf {
    // eggplant_backup/benches -> eggplant_backup -> stable/egglog_sync_serialize_raw
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../stable/egglog_sync_serialize_raw")
}

fn rewrite_relative_file_paths(program: &str, root: &Path) -> String {
    // Bench harness runs from eggplant's workspace; some egglog tests use paths relative
    // to the egglog repo root (e.g. `(include "tests/web-demo/path.egg")`). Rewrite those
    // to absolute paths so the benchmark can run from anywhere.
    let mut out = String::with_capacity(program.len());
    for line in program.lines() {
        if let Some(rewritten) = rewrite_one_line(line, root) {
            out.push_str(&rewritten);
        } else {
            out.push_str(line);
        }
        out.push('\n');
    }
    out
}

fn rewrite_one_line(line: &str, root: &Path) -> Option<String> {
    let line_trim = line.trim_start();
    if !(line_trim.starts_with("(include") || line_trim.starts_with("(input")) {
        return None;
    };

    // Find first quoted string on this line.
    let idx_first = line_trim.find('"')?;
    let idx_second = line_trim[idx_first + 1..].find('"')? + idx_first + 1;
    let path_str = &line_trim[idx_first + 1..idx_second];

    // Absolute paths: keep as-is.
    if path_str.starts_with('/') {
        return None;
    }

    // For `(input ...)` we can leave relative paths alone if `fact_directory` is set,
    // but rewriting is harmless and makes behavior consistent across versions.
    let joined = root.join(path_str);
    let joined_str = joined.to_string_lossy();

    // Rebuild the original line by replacing the quoted path.
    let prefix_len = line.len() - line_trim.len();
    let prefix = &line[..prefix_len];

    let mut rebuilt = String::with_capacity(line.len() + joined_str.len());
    rebuilt.push_str(prefix);

    rebuilt.push_str(&line_trim[..idx_first + 1]);
    rebuilt.push_str(&joined_str);
    rebuilt.push_str(&line_trim[idx_second..]);
    Some(rebuilt)
}

fn define_union_chain_world(egraph: &mut EGraph) {
    egraph
        .parse_and_run_program(
            Some("union_chain.egg".to_owned()),
            r#"
(sort Expr)
(constructor Node (i64) Expr)
(relation Edge (i64 i64))
        "#,
        )
        .unwrap();
}

fn insert_edges_program(n_edges: usize) -> String {
    // Edges: (0 -> 1), (1 -> 2), ..., (n_edges-1 -> n_edges)
    let mut program = String::with_capacity(n_edges * 18);
    for i in 0..n_edges {
        let a = i as i64;
        let b = (i + 1) as i64;
        program.push_str("(Edge ");
        program.push_str(&a.to_string());
        program.push(' ');
        program.push_str(&b.to_string());
        program.push_str(")\n");
    }
    program
}

pub fn bench_union_chain(n_edges: usize, proofs: bool, typed_union: bool, do_prove: bool) {
    configure_rayon_once();

    let mut egraph = if proofs {
        EGraph::new_with_proofs()
    } else {
        EGraph::new_with_term_encoding()
    };
    define_union_chain_world(&mut egraph);

    add_ruleset(&mut egraph, "rs").unwrap();

    // Populate the Edge relation.
    let edges_program = insert_edges_program(n_edges);
    egraph
        .parse_and_run_program(Some("edges.egg".to_owned()), &edges_program)
        .unwrap();

    let edge_output_sort = egraph
        .get_function("Edge")
        .expect("Edge function should exist")
        .schema()
        .output
        .clone();

    let edge_view_proof: Option<Arc<str>> = if proofs && typed_union {
        Some(Arc::<str>::from(
            egraph
                .proof_view_proof_name("Edge")
                .expect("Edge should have a view-proof function in proofs mode")
                .to_owned(),
        ))
    } else {
        None
    };

    // One rust rule: for each (Edge a b), union Node(a) ~ Node(b).
    //
    // Use an `=` fact to bind the (instrumented) output value so we can also look up
    // the row's view-proof value in proofs mode.
    rust_rule(
        &mut egraph,
        "union_chain",
        "rs",
        vars![a: i64, b: i64, u: { edge_output_sort.clone() }],
        facts![(= u (Edge a b))],
        move |ctx: &mut RustRuleContext, values: &[Value]| {
            let [a, b, u] = values else { unreachable!() };
            let node_a = ctx.lookup("Node", &[*a]).unwrap();
            let node_b = ctx.lookup("Node", &[*b]).unwrap();
            if typed_union {
                let premise_proofs: Option<[Value; 1]> = edge_view_proof.as_ref().map(|vp| {
                    let prf = ctx.lookup(vp.as_ref(), &[*a, *b, *u]).unwrap();
                    [prf]
                });
                if let Some(premises) = premise_proofs.as_ref() {
                    ctx.union_typed("Expr", node_a, node_b, premises.as_slice());
                } else {
                    ctx.union_typed("Expr", node_a, node_b, &[]);
                }
            } else {
                ctx.union(node_a, node_b);
            }
            Some(())
        },
    )
    .unwrap();

    // One iteration is enough: Edge is a base relation (no semi-naive dependencies).
    run_ruleset(&mut egraph, "rs").unwrap();

    if do_prove {
        // Avoid `eval_expr` here: in term-encoding/proofs mode it can create fresh values
        // that aren't the same ones the rust rule inserted/unioned.
        let k0 = egraph.base_to_value::<i64>(0);
        let kn = egraph.base_to_value::<i64>(n_edges as i64);
        let v0 = egraph
            .lookup_function("Node", &[k0])
            .expect("expected Node(0) row to exist");
        let vn = egraph
            .lookup_function("Node", &[kn])
            .expect("expected Node(n) row to exist");
        let proof = egraph.prove_values_equal_pretty("Expr", v0, vn).unwrap();
        divan::black_box(proof);
    }
}
