use eggplant::egglog::prelude::{BaseSort, Fact, Facts, I64Sort, exprs, query};
use eggplant::egglog::{expr, fact, facts, sort, vars};
use eggplant::prelude::*;
use eggplant::tx_rx_vt_pr;

#[eggplant::dsl]
enum TsExpr {
    Leaf { n: i64 },
}

#[eggplant::relation]
struct RecentLeaf {
    n: i64,
}

tx_rx_vt_pr!(TsTx, TsPatRec);

#[test]
fn add_rule_timestamp_constraint_filters_old_matches() {
    let _ = env_logger::builder().is_test(true).try_init();
    TsTx::sgl().reset_for_bench();

    let old_leaf = Leaf::<TsTx>::new(1);
    old_leaf.commit();

    let cutoff = {
        let egraph = TsTx::egraph();
        egraph.lock().unwrap().current_timestamp()
    };

    let new_leaf = Leaf::<TsTx>::new(2);
    new_leaf.commit();

    let ruleset = TsTx::new_ruleset("timestamp_filtered_recent_leaf");
    TsTx::add_rule(
        "timestamp_filtered_recent_leaf",
        ruleset,
        move || {
            let leaf = Leaf::query();
            #[eggplant::pat_vars]
            struct Pat {
                leaf: Leaf,
            }
            Pat::new(leaf).timestamp(|x| x.ge(cutoff).lt(cutoff.saturating_add(10)))
        },
        |ctx, pat| {
            let n = ctx.devalue(pat.leaf.n);
            ctx.insert_recent_leaf(n);
        },
    );
    let report = TsTx::run_ruleset(ruleset, RunConfig::Once);
    assert_eq!(
        report
            .num_matches_per_rule
            .get("@timestamp_filtered_recent_leaf")
            .copied()
            .unwrap_or(0),
        1
    );

    let egraph = TsTx::egraph();
    let mut egraph = egraph.lock().unwrap();
    let results = query(&mut egraph, vars![n: i64], facts![(RecentLeaf n)]).unwrap();
    let matched = results
        .iter()
        .map(|row| egraph.value_to_base::<i64>(row[0]))
        .collect::<Vec<_>>();
    assert_eq!(matched, vec![2]);
}

#[eggplant::dsl]
enum TsWindowExpr {
    WindowNode { n: i64 },
}

#[eggplant::relation]
struct WindowLeaf {
    n: i64,
}

tx_rx_vt_pr!(TsWindowTx, TsWindowPatRec);

#[test]
fn timestamp_constraint_uses_inclusive_lower_and_exclusive_upper_bounds() {
    let _ = env_logger::builder().is_test(true).try_init();
    TsWindowTx::sgl().reset_for_bench();

    let oldest_leaf = WindowNode::<TsWindowTx>::new(10);
    oldest_leaf.commit();

    let lower = {
        let egraph = TsWindowTx::egraph();
        egraph.lock().unwrap().current_timestamp()
    };

    let middle_leaf = WindowNode::<TsWindowTx>::new(20);
    middle_leaf.commit();

    let upper = {
        let egraph = TsWindowTx::egraph();
        egraph.lock().unwrap().current_timestamp()
    };

    let newest_leaf = WindowNode::<TsWindowTx>::new(30);
    newest_leaf.commit();

    let ruleset = TsWindowTx::new_ruleset("timestamp_window_only_middle_leaf");
    TsWindowTx::add_rule(
        "timestamp_window_only_middle_leaf",
        ruleset,
        move || {
            let leaf = WindowNode::query();
            #[eggplant::pat_vars]
            struct Pat {
                leaf: WindowNode,
            }
            Pat::new(leaf).timestamp(|x| x.ge(lower).lt(upper))
        },
        |ctx, pat| {
            let n = ctx.devalue(pat.leaf.n);
            ctx.insert_window_leaf(n);
        },
    );

    let report = TsWindowTx::run_ruleset(ruleset, RunConfig::Once);
    assert_eq!(
        report
            .num_matches_per_rule
            .get("@timestamp_window_only_middle_leaf")
            .copied()
            .unwrap_or(0),
        1
    );

    let egraph = TsWindowTx::egraph();
    let mut egraph = egraph.lock().unwrap();
    let results = query(&mut egraph, vars![n: i64], facts![(WindowLeaf n)]).unwrap();
    let matched = results
        .iter()
        .map(|row| egraph.value_to_base::<i64>(row[0]))
        .collect::<Vec<_>>();

    assert!(
        matched.contains(&20),
        "middle leaf should satisfy [lower, upper)"
    );
    assert!(
        !matched.contains(&10),
        "oldest leaf should be filtered out by lower bound"
    );
    assert!(
        !matched.contains(&30),
        "newest leaf should be filtered out by exclusive upper bound"
    );
}
