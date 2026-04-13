use std::sync::{Arc, Mutex, OnceLock};

use eggplant::{prelude::*, tx_rx_vt_pr};

#[eggplant::dsl]
enum FastExpr {
    FastConst { n: i64 },
    FastAdd { lhs: FastExpr, rhs: FastExpr },
}

#[eggplant::dsl]
enum FastRoot {
    FastRootNode { node: FastExpr },
}

tx_rx_vt_pr!(FastTx, FastPatRec);

#[eggplant::pat_vars]
struct FastAddPat<PR: PatRecSgl> {
    lhs: FastExpr<PR>,
    rhs: FastExpr<PR>,
    add: FastAdd<PR>,
}

fn fast_add_pat<PR: PatRecSgl>() -> FastAddPat<PR> {
    let lhs = FastExpr::query_leaf();
    let rhs = FastExpr::query_leaf();
    let add = FastAdd::query(&lhs, &rhs);
    FastAddPat::new(lhs, rhs, add)
}

#[test]
fn cached_lookup_matches_named_lookup() {
    static FAST_ADD_ID: OnceLock<eggplant::egglog::FunctionId> = OnceLock::new();

    let root =
        FastRootNode::<FastTx>::new(&FastAdd::new(&FastConst::new(2), &FastConst::new(3)));
    root.commit();

    let ruleset = FastTx::new_ruleset("cached_lookup_matches_named_lookup");
    let seen = Arc::new(Mutex::new(None));
    let seen_in_rule = Arc::clone(&seen);
    FastTx::add_rule(
        "check_cached_lookup",
        ruleset,
        fast_add_pat,
        move |ctx, pat| {
            let key = [pat.lhs.erase(), pat.rhs.erase()];
            let named = ctx.lookup_expect("FastAdd", &key);
            let cached = ctx.lookup_expect_cached("FastAdd", &FAST_ADD_ID, &key);
            *seen_in_rule.lock().unwrap() = Some((named, cached));
        },
    );

    let _report = FastTx::run_ruleset(ruleset, RunConfig::Once);

    let (named, cached) = seen
        .lock()
        .unwrap()
        .take()
        .expect("rule should have observed the FastAdd node");
    assert_eq!(named, cached);
}
