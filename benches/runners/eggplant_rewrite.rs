use crate::common::configure_rayon_once;

pub fn bench_web_demo_unify_rewrite() {
    configure_rayon_once();
    web_demo_unify::bench();
}

pub fn bench_web_demo_set_rewrite() {
    configure_rayon_once();
    web_demo_set::bench();
}

pub fn bench_repro_665_set_union_rewrite() {
    configure_rayon_once();
    repro_665_set_union::bench();
}

pub fn bench_extract_vec_bench_rewrite() {
    configure_rayon_once();
    extract_vec_bench::bench();
}

pub fn bench_merge_during_rebuild_rewrite() {
    configure_rayon_once();
    merge_during_rebuild::bench();
}

pub fn bench_vec_builtins_rewrite() {
    configure_rayon_once();
    vec_builtins::bench();
}

pub fn bench_taylor51_rewrite() {
    configure_rayon_once();
    taylor51::bench();
}

pub fn bench_python_array_optimize_rewrite() {
    configure_rayon_once();
    python_array_optimize::bench();
}

#[cfg(feature = "eggcc_extraction")]
pub fn bench_eggcc_extraction_rewrite() {
    configure_rayon_once();
    eggcc_extraction::bench();
}

pub fn bench_math_microbenchmark_rewrite() {
    configure_rayon_once();
    math_microbenchmark::bench();
}

#[cfg(feature = "eggcc_extraction")]
#[path = "eggplant_rewrite/eggcc_extraction.rs"]
mod eggcc_extraction;
#[path = "eggplant_rewrite/extract_vec_bench.rs"]
mod extract_vec_bench;
#[path = "eggplant_rewrite/math_microbenchmark.rs"]
mod math_microbenchmark;
#[path = "eggplant_rewrite/merge_during_rebuild.rs"]
mod merge_during_rebuild;
#[path = "eggplant_rewrite/python_array_optimize.rs"]
mod python_array_optimize;
#[path = "eggplant_rewrite/repro_665_set_union.rs"]
mod repro_665_set_union;
#[path = "eggplant_rewrite/taylor51.rs"]
mod taylor51;
#[path = "eggplant_rewrite/vec_builtins.rs"]
mod vec_builtins;
#[path = "eggplant_rewrite/web_demo_set.rs"]
mod web_demo_set;
#[path = "eggplant_rewrite/web_demo_unify.rs"]
mod web_demo_unify;
