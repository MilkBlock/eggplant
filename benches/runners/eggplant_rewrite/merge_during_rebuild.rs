use eggplant::prelude::*;
use eggplant::tx_rx_vt_pr;

#[eggplant::dsl]
enum N {
    #[eggplant::typst("Node({i})")]
    Node { i: i64 },
}

#[allow(non_camel_case_types)]
#[eggplant::func(output = i64, merge = "(min old new)")]
struct distance {
    a: N,
    b: N,
}

tx_rx_vt_pr!(MyTxDist, MyPatRecDist);

pub fn bench() {
    MyTxDist::sgl().reset_for_bench();

    // Seed facts (ports `tests/merge-during-rebuild.egg`).
    let seed = MyTxDist::new_ruleset("merge_during_rebuild_seed");
    MyTxDist::add_rule(
        "merge_during_rebuild_seed",
        seed,
        || {
            #[eggplant::pat_vars_catch]
            struct Unit {}
        },
        |ctx, _pat| {
            let a = ctx.insert_node(0);
            let b = ctx.insert_node(1);
            let x = ctx.insert_node(2);
            let y = ctx.insert_node(3);

            ctx.set_distance(x.clone(), y.clone(), 1);
            ctx.set_distance(a.clone(), b.clone(), 2);

            ctx.union(a, x);
            ctx.union(b, y);
        },
    );
    MyTxDist::run_ruleset(seed, RunConfig::Once);

    // Force a rebuild/merge pass (matches `run 1` behavior for merge-on-rebuild).
    let rebuild = MyTxDist::new_ruleset("merge_during_rebuild_rebuild");
    MyTxDist::run_ruleset(rebuild, RunConfig::Once);

    // Check: after unions, min-merge should prevent the distance from increasing.
    let check = MyTxDist::new_ruleset("merge_during_rebuild_check");
    MyTxDist::add_rule(
        "merge_during_rebuild_check",
        check,
        || {
            #[eggplant::pat_vars_catch]
            struct Unit {}
        },
        |ctx, _pat| {
            let x = ctx.insert_node(2);
            let y = ctx.insert_node(3);
            let got = ctx
                .try_read_distance(x, y)
                .expect("distance(x,y) should be set");
            assert_eq!(got, 1);
        },
    );
    MyTxDist::run_ruleset(check, RunConfig::Once);

    let egraph = MyTxDist::egraph();
    let egraph = egraph.lock().unwrap();
    egraph.serialize(egglog::SerializeConfig::default());
}
