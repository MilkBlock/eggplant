#[path = "../examples/math_microbenchmark_support.rs"]
mod rust_rule_math_microbenchmark;
#[path = "../benches/runners/eggplant_rewrite/math_microbenchmark.rs"]
mod typed_math_microbenchmark;

#[test]
fn typed_math_microbenchmark_rules_use_inline_pattern_closures() {
    let source = std::fs::read_to_string(
        "/Users/mineralsteins/Repos/egg_related/eggplant_backup/benches/runners/eggplant_rewrite/math_microbenchmark.rs",
    )
    .expect("benchmark source should be readable");

    for legacy_name in [
        "struct AddCommPat",
        "struct MulCommPat",
        "struct AddAssocPat",
        "struct MulAssocPat",
        "struct SubToAddNegPat",
        "struct AddZeroPat",
        "struct MulZeroPat",
        "struct MulOnePat",
        "struct SubSelfZeroPat",
        "struct MulDistribPat",
        "struct AddFactorPat",
        "struct MulPowCombinePat",
        "struct DivAddPat",
        "struct DivSubPat",
        "struct PowOnePat",
        "struct PowTwoPat",
        "struct DiffAddPat",
        "struct DiffMulPat",
        "struct DiffSinPat",
        "struct DiffCosPat",
        "struct IntOnePat",
        "struct IntCosPat",
        "struct IntSinPat",
        "struct IntAddPat",
        "struct IntSubPat",
        "struct IntMulPat",
        "fn add_comm_pat",
        "fn mul_comm_pat",
        "fn add_assoc_pat",
        "fn mul_assoc_pat",
        "fn sub_to_add_neg_pat",
        "fn add_zero_pat",
        "fn mul_zero_pat",
        "fn mul_one_pat",
        "fn sub_self_zero_pat",
        "fn mul_distrib_pat",
        "fn add_factor_pat",
        "fn mul_pow_combine_pat",
        "fn div_add_pat",
        "fn div_sub_pat",
        "fn pow_one_pat",
        "fn pow_two_pat",
        "fn diff_add_pat",
        "fn diff_mul_pat",
        "fn diff_sin_pat",
        "fn diff_cos_pat",
        "fn int_one_pat",
        "fn int_cos_pat",
        "fn int_sin_pat",
        "fn int_add_pat",
        "fn int_sub_pat",
        "fn int_mul_pat",
    ] {
        assert!(
            !source.contains(legacy_name),
            "legacy helper `{legacy_name}` should be inlined into add_rule pattern closures"
        );
    }
}

#[test]
fn typed_math_microbenchmark_stats_are_non_empty() {
    let stats = typed_math_microbenchmark::run_and_collect_stats(false);
    assert!(stats.total_num_tuples > 0);
    assert!(
        stats
            .table_sizes
            .iter()
            .any(|(name, size)| *name == "MAdd" && *size > 0)
    );
}

#[test]
fn rust_rule_math_microbenchmark_stats_are_non_empty() {
    let stats = rust_rule_math_microbenchmark::run_and_collect_stats();
    assert!(stats.total_num_tuples > 0);
    assert!(
        stats
            .table_sizes
            .iter()
            .any(|(name, size)| *name == "MAdd" && *size > 0)
    );
}
