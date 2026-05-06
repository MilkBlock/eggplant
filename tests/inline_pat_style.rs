#[test]
fn hand_written_benches_and_examples_use_inline_pattern_closures() {
    let files = [
        "/Users/mineralsteins/Repos/egg_related/eggplant_backup/examples/action_sample_recorder.rs",
        "/Users/mineralsteins/Repos/egg_related/eggplant_backup/examples/sample_trace_json.rs",
        "/Users/mineralsteins/Repos/egg_related/eggplant_backup/examples/pat_ref.rs",
        "/Users/mineralsteins/Repos/egg_related/eggplant_backup/examples/dynamic_action_trace_demo.rs",
        "/Users/mineralsteins/Repos/egg_related/eggplant_backup/benches/runners/eggplant_rewrite/web_demo_unify.rs",
        "/Users/mineralsteins/Repos/egg_related/eggplant_backup/benches/runners/eggplant_rewrite/repro_665_set_union.rs",
        "/Users/mineralsteins/Repos/egg_related/eggplant_backup/benches/runners/eggplant_rewrite/web_demo_set.rs",
        "/Users/mineralsteins/Repos/egg_related/eggplant_backup/benches/runners/eggplant_rewrite/vec_builtins.rs",
    ];

    for path in files {
        let source = std::fs::read_to_string(path).expect("source file should be readable");
        assert!(
            !source.contains("_pat<"),
            "legacy `_pat` helper found in {path}"
        );
        assert!(
            !source.contains("Pat<PR: PatRecSgl>"),
            "legacy top-level Pat struct found in {path}"
        );
    }
}
