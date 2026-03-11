mod common;

#[divan::bench(
    args = common::egglog_bench_cases("../stable/egglog_sync_serialize_raw/tests/**/*.egg"),
    sample_count = 10
)]
fn egglog_ci_suite(case: &common::EgglogBenchCase) {
    common::bench_egglog_case(case);
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
