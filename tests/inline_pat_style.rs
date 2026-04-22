#[test]
fn hand_written_benches_and_examples_use_inline_pattern_closures() {
    let manifest_dir = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let files = [
        manifest_dir.join("examples/action_sample_recorder.rs"),
        manifest_dir.join("examples/sample_trace_json.rs"),
        manifest_dir.join("examples/pat_ref.rs"),
        manifest_dir.join("examples/dynamic_action_trace_demo.rs"),
        manifest_dir.join("benches/runners/eggplant_rewrite/web_demo_unify.rs"),
        manifest_dir.join("benches/runners/eggplant_rewrite/repro_665_set_union.rs"),
        manifest_dir.join("benches/runners/eggplant_rewrite/web_demo_set.rs"),
        manifest_dir.join("benches/runners/eggplant_rewrite/vec_builtins.rs"),
    ];

    for path in files {
        let source = std::fs::read_to_string(&path).expect("source file should be readable");
        assert!(
            !source.contains("_pat<"),
            "legacy `_pat` helper found in {}",
            path.display()
        );
        assert!(
            !source.contains("Pat<PR: PatRecSgl>"),
            "legacy top-level Pat struct found in {}",
            path.display()
        );
    }
}
