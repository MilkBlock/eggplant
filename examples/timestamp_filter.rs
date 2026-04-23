#[cfg(not(feature = "fork-egglog"))]
fn main() {
    eprintln!(
        "timestamp_filter requires fork-egglog timestamp-constraint support and is disabled on stable-big-pr"
    );
}

#[cfg(feature = "fork-egglog")]
mod enabled {
    use eggplant::egglog::prelude::{I64Sort, exprs, query};
    use eggplant::egglog::{facts, vars};
    use eggplant::prelude::*;
    use eggplant::tx_rx_vt_pr;
    use eggplant::wrap::NonPatRecSgl;

    #[eggplant::dsl]
    enum TsExpr {
        Leaf { n: i64 },
    }

    #[eggplant::relation]
    struct RecentLeaf {
        n: i64,
    }

    tx_rx_vt_pr!(TsTx, TsPatRec);

    pub fn main() {
        let _ = env_logger::try_init();

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

        println!(
            "matches: {}",
            report
                .num_matches_per_rule
                .get("@timestamp_filtered_recent_leaf")
                .copied()
                .unwrap_or(0)
        );
        let egraph = TsTx::egraph();
        let mut egraph = egraph.lock().unwrap();
        let results =
            query(&mut egraph, vars![n: i64], facts![(RecentLeaf n)]).expect("query should succeed");
        let matched = results
            .iter()
            .map(|row| egraph.value_to_base::<i64>(row[0]))
            .collect::<Vec<_>>();
        println!("recent leaf matches: {:?}", matched);
    }
}

#[cfg(feature = "fork-egglog")]
fn main() {
    enabled::main();
}
