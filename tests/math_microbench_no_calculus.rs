#![cfg(feature = "rustsat-extract")]

#[path = "../benches/runners/eggplant_rewrite/math_microbenchmark_no_calculus.rs"]
mod typed_math_microbenchmark_no_calculus;

use std::sync::{Mutex, OnceLock};

fn test_guard() -> &'static Mutex<()> {
    static GUARD: OnceLock<Mutex<()>> = OnceLock::new();
    GUARD.get_or_init(|| Mutex::new(()))
}

#[test]
fn no_calculus_rules_use_inline_pattern_closures() {
    let source = std::fs::read_to_string(
        "/Users/mineralsteins/Repos/egg_related/eggplant_backup/benches/runners/eggplant_rewrite/math_microbenchmark_no_calculus.rs",
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
        "struct AddFracPat",
        "struct SubFracPat",
        "struct PowOnePat",
        "struct PowTwoPat",
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
        "fn add_frac_pat",
        "fn sub_frac_pat",
        "fn pow_one_pat",
        "fn pow_two_pat",
    ] {
        assert!(
            !source.contains(legacy_name),
            "legacy helper `{legacy_name}` should be inlined into add_rule pattern closures"
        );
    }
}

#[cfg(feature = "rustsat-extract")]
#[test]
fn typed_math_microbenchmark_no_calculus_stats_are_non_empty() {
    let _guard = test_guard().lock().unwrap();
    let stats = typed_math_microbenchmark_no_calculus::run_and_collect_stats(false);
    assert!(stats.total_num_tuples > 0);
    assert!(
        stats
            .table_sizes
            .iter()
            .any(|(name, size)| *name == "MAdd" && *size > 0)
    );
    assert!(
        stats
            .table_sizes
            .iter()
            .all(|(name, _)| *name != "MDiff" && *name != "MIntegral"),
        "no-calculus benchmark should not expose MDiff/MIntegral tables"
    );
}

#[cfg(feature = "rustsat-extract")]
#[test]
fn no_calculus_extract_report_omits_diff_and_integral_forms() {
    let _guard = test_guard().lock().unwrap();
    let rows =
        typed_math_microbenchmark_no_calculus::run_extract_comparison_with_iters_and_mem_cap(1, 20);
    assert_eq!(rows.len(), 4);
    for row in rows {
        assert!(
            !row.rendered.contains("MDiff") && !row.rendered.contains("MIntegral"),
            "no-calculus extraction should not contain MDiff/MIntegral: {}",
            row.rendered
        );
    }
}

#[cfg(feature = "rustsat-extract")]
#[test]
fn no_calculus_timeline_contains_one_point_per_iteration() {
    let _guard = test_guard().lock().unwrap();
    let report =
        typed_math_microbenchmark_no_calculus::run_extract_timeline_with_iters_and_mem_cap(2, 20);
    assert_eq!(report.requested_rewrite_iters, 2);
    assert_eq!(report.points.len(), 2);
    for point in report.points {
        assert!(point.iteration >= 1);
        assert_eq!(point.extracts.len(), 4);
        assert!(!point.rule_matches.is_empty());
        for metric in point.extracts {
            assert!(metric.cost.is_some());
        }
    }
}

#[cfg(feature = "rustsat-extract")]
#[test]
fn no_calculus_timeline_honors_selected_extractors() {
    let _guard = test_guard().lock().unwrap();
    let report = typed_math_microbenchmark_no_calculus::run_extract_timeline_with_options(
        1,
        20,
        &["default".to_string(), "layered".to_string()],
        Some(1),
    );
    assert_eq!(report.selected_extractors, vec!["default", "layered"]);
    assert_eq!(report.max_extract_time_secs, Some(1));
    assert_eq!(report.points.len(), 1);
    assert_eq!(report.points[0].extracts.len(), 2);
    assert_eq!(report.points[0].extracts[0].method, "default");
    assert_eq!(report.points[0].extracts[1].method, "layered");
}

#[cfg(feature = "rustsat-extract")]
#[test]
fn no_calculus_progress_callback_receives_rewrite_and_extract_events() {
    let _guard = test_guard().lock().unwrap();
    let mut saw_rewrite = false;
    let mut saw_extract = false;
    let _rows = typed_math_microbenchmark_no_calculus::run_extract_comparison_with_iters_and_mem_cap_and_progress(
        1,
        20,
        |event| match event {
            typed_math_microbenchmark_no_calculus::ProgressEvent::RewriteIterationComplete { .. } => {
                saw_rewrite = true;
            }
            typed_math_microbenchmark_no_calculus::ProgressEvent::ExtractPhaseStart { .. } => {
                saw_extract = true;
            }
            _ => {}
        },
    );
    assert!(saw_rewrite);
    assert!(saw_extract);
}

#[test]
fn no_calculus_cancel_neg_add_rewrites_to_zero() {
    let _guard = test_guard().lock().unwrap();
    let rendered = typed_math_microbenchmark_no_calculus::run_cancel_neg_add_rewrite_smoke(
        1,
        eggplant::egglog::extract::TreeAdditiveCostModel::default(),
    );
    assert_eq!(rendered, "(MConst 0)");
}
