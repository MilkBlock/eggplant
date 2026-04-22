use egglog_upstream::EGraph;
use egglog_upstream::SerializeConfig;

use crate::common::{
    EgglogBenchCase, EgglogRunMode, configure_rayon_once, rewrite_relative_file_paths,
};

pub fn bench_egglog_case_upstream(case: &EgglogBenchCase) {
    configure_rayon_once();

    let mut egraph = match case.mode {
        EgglogRunMode::Normal => EGraph::default(),
        EgglogRunMode::ProofTesting => EGraph::new_with_proofs().with_proof_testing(),
    };
    egraph.fact_directory = Some(case.root.clone());

    let program = rewrite_relative_file_paths(&case.program, &case.root);
    egraph
        .parse_and_run_program(Some(case.filename.clone()), &program)
        .unwrap();
    egraph.serialize(SerializeConfig::default());
    std::mem::forget(egraph);
}
