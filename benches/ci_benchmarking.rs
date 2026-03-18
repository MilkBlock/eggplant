mod common;
mod runners;

const HEAVY_CASES: &[&str] = &[
    "stresstest_large_expr",
    "extract-vec-bench",
    "typeinfer",
    "herbie",
    "herbie-tutorial",
    "repro-665-set-union",
    "cykjson",
    "taylor51",
    #[cfg(feature = "eggcc_extraction")]
    "eggcc-extraction",
    "python_array_optimize",
    "math-microbenchmark",
];

const PERF_SPECS: &[common::EgglogBenchSpec<'_>] = &[
    common::EgglogBenchSpec {
        name: "stresstest_large_expr",
        file_stem: "stresstest_large_expr",
        mode: common::EgglogRunMode::Normal,
    },
    common::EgglogBenchSpec {
        name: "extract-vec-bench",
        file_stem: "extract-vec-bench",
        mode: common::EgglogRunMode::Normal,
    },
    common::EgglogBenchSpec {
        name: "typeinfer",
        file_stem: "typeinfer",
        mode: common::EgglogRunMode::Normal,
    },
    common::EgglogBenchSpec {
        name: "herbie",
        file_stem: "herbie",
        mode: common::EgglogRunMode::Normal,
    },
    common::EgglogBenchSpec {
        name: "herbie-tutorial",
        file_stem: "herbie-tutorial",
        mode: common::EgglogRunMode::Normal,
    },
    common::EgglogBenchSpec {
        name: "repro-665-set-union",
        file_stem: "repro-665-set-union",
        mode: common::EgglogRunMode::Normal,
    },
    common::EgglogBenchSpec {
        name: "cykjson",
        file_stem: "cykjson",
        mode: common::EgglogRunMode::Normal,
    },
    common::EgglogBenchSpec {
        name: "taylor51",
        file_stem: "taylor51",
        mode: common::EgglogRunMode::Normal,
    },
    #[cfg(feature = "eggcc_extraction")]
    common::EgglogBenchSpec {
        name: "eggcc-extraction",
        file_stem: "eggcc-extraction",
        mode: common::EgglogRunMode::Normal,
    },
    common::EgglogBenchSpec {
        name: "python_array_optimize",
        file_stem: "python_array_optimize",
        mode: common::EgglogRunMode::Normal,
    },
    common::EgglogBenchSpec {
        name: "math-microbenchmark",
        file_stem: "math-microbenchmark",
        mode: common::EgglogRunMode::Normal,
    },
    common::EgglogBenchSpec {
        name: "merge-during-rebuild",
        file_stem: "merge-during-rebuild",
        mode: common::EgglogRunMode::Normal,
    },
    common::EgglogBenchSpec {
        name: "vec",
        file_stem: "vec",
        mode: common::EgglogRunMode::Normal,
    },
    common::EgglogBenchSpec {
        name: "web-demo/unify",
        file_stem: "tests/web-demo/unify.egg",
        mode: common::EgglogRunMode::Normal,
    },
    common::EgglogBenchSpec {
        name: "web-demo/set",
        file_stem: "tests/web-demo/set.egg",
        mode: common::EgglogRunMode::Normal,
    },
    common::EgglogBenchSpec {
        name: "proof_testing_unify",
        file_stem: "unify",
        mode: common::EgglogRunMode::ProofTesting,
    },
    common::EgglogBenchSpec {
        name: "proof_testing_typecheck",
        file_stem: "typecheck",
        mode: common::EgglogRunMode::ProofTesting,
    },
    common::EgglogBenchSpec {
        name: "proof_testing_eqsat-basic",
        file_stem: "eqsat-basic",
        mode: common::EgglogRunMode::ProofTesting,
    },
];

#[divan::bench(
    args = common::egglog_bench_cases("../stable/egglog_sync_serialize_raw/tests/**/*.egg"),
    sample_count = 10
)]
fn egglog_ci_suite(case: &common::EgglogBenchCase) {
    common::bench_egglog_case(case);
}

#[divan::bench(
    args = common::egglog_bench_cases_specs(
        "../upstream_egglog/tests/**/*.egg",
        PERF_SPECS,
        common::upstream_egglog_repo_root()
    ),
    sample_count = 10
)]
fn tests(case: &common::EgglogBenchCase) {
    common::bench_egglog_case(case);
}

#[divan::bench(
    args = common::egglog_bench_cases_specs(
        "../upstream_egglog/tests/**/*.egg",
        PERF_SPECS,
        common::upstream_egglog_repo_root()
    ),
    sample_count = 10
)]
fn tests_upstream(case: &common::EgglogBenchCase) {
    runners::upstream::bench_egglog_case_upstream(case);
}

#[divan::bench(
    args = common::egglog_bench_cases_selected(
        "../upstream_egglog/tests/**/*.egg",
        HEAVY_CASES,
        common::upstream_egglog_repo_root()
    ),
    sample_count = 10
)]
fn tests_heavy_patched(case: &common::EgglogBenchCase) {
    common::bench_egglog_case(case);
}

#[divan::bench(
    args = common::egglog_bench_cases_selected(
        "../upstream_egglog/tests/**/*.egg",
        HEAVY_CASES,
        common::upstream_egglog_repo_root()
    ),
    sample_count = 10
)]
fn tests_heavy_upstream(case: &common::EgglogBenchCase) {
    runners::upstream::bench_egglog_case_upstream(case);
}

#[divan::bench(sample_count = 10)]
fn eggplant_rewrite_extract_vec_bench() {
    runners::eggplant_rewrite::bench_extract_vec_bench_rewrite();
}

#[divan::bench(sample_count = 10)]
fn eggplant_rewrite_merge_during_rebuild() {
    runners::eggplant_rewrite::bench_merge_during_rebuild_rewrite();
}

#[divan::bench(sample_count = 10)]
fn eggplant_rewrite_vec() {
    runners::eggplant_rewrite::bench_vec_builtins_rewrite();
}

#[divan::bench(sample_count = 10)]
fn eggplant_rewrite_repro_665_set_union() {
    runners::eggplant_rewrite::bench_repro_665_set_union_rewrite();
}

#[divan::bench(sample_count = 10)]
fn eggplant_rewrite_web_demo_unify() {
    runners::eggplant_rewrite::bench_web_demo_unify_rewrite();
}

#[divan::bench(sample_count = 10)]
fn eggplant_rewrite_web_demo_set() {
    runners::eggplant_rewrite::bench_web_demo_set_rewrite();
}

#[divan::bench(sample_count = 10)]
fn eggplant_rewrite_taylor51() {
    runners::eggplant_rewrite::bench_taylor51_rewrite();
}

#[divan::bench(sample_count = 10)]
fn eggplant_rewrite_python_array_optimize() {
    runners::eggplant_rewrite::bench_python_array_optimize_rewrite();
}

#[cfg(feature = "eggcc_extraction")]
#[divan::bench(sample_count = 10)]
fn eggplant_rewrite_eggcc_extraction() {
    runners::eggplant_rewrite::bench_eggcc_extraction_rewrite();
}

#[divan::bench(sample_count = 10)]
fn eggplant_rewrite_math_microbenchmark() {
    runners::eggplant_rewrite::bench_math_microbenchmark_rewrite();
}

#[divan::bench(args = [128, 1_024, 8_192], sample_count = 10)]
fn union_chain_proofs_typed_no_prove(n_edges: usize) {
    // Proofs enabled + `union_typed` on each edge.
    common::bench_union_chain(n_edges, true, true, false);
}

#[divan::bench(args = [128, 1_024, 8_192], sample_count = 10)]
fn union_chain_proofs_typed_with_prove(n_edges: usize) {
    // Same workload + export a proof at the end (Node(0) = Node(n)).
    common::bench_union_chain(n_edges, true, true, true);
}

#[divan::bench(args = [128, 1_024, 8_192], sample_count = 10)]
fn union_chain_term_encoding_untyped_no_prove(n_edges: usize) {
    // Term-encoding enabled but proofs disabled; use plain `union` (baseline for union cost).
    common::bench_union_chain(n_edges, false, false, false);
}

fn main() {
    divan::main();
}
