use egglog_reports::RunReport;
use eggplant::prelude::*;
use eggplant::tx_rx_vt_pr;

#[eggplant::container]
struct IVec {
    inner: VecContainer<i64>,
}

#[eggplant::dsl]
enum X {
    #[eggplant::typst("a")]
    a {},
    #[eggplant::typst("b")]
    b {},
}

#[eggplant::container]
struct VX {
    inner: VecContainer<X>,
}

#[eggplant::func(output = VX, no_merge)]
struct P {}

#[eggplant::func(output = VX, no_merge)]
struct Q {}

tx_rx_vt_pr!(MyTxVec, MyPatRecVec);

#[eggplant::pat_vars]
struct CheckIVecPat<PR: PatRecSgl> {
    v: BaseVar<IVec, PR>,
}

#[eggplant::pat_vars]
struct CheckUnitPat<PR: PatRecSgl> {}

#[eggplant::pat_vars]
struct CheckI64Pat<PR: PatRecSgl> {
    v: BaseVar<i64, PR>,
}

fn expect_rule_matches(report: &RunReport, rule: &str) {
    let key = format!("@{rule}");
    assert!(
        report
            .num_matches_per_rule
            .get(key.as_str())
            .copied()
            .unwrap_or(0)
            > 0,
        "expected rule to match: @{rule}"
    );
}

fn pat_vec_check_vec_of<PR: PatRecSgl>() -> CheckIVecPat<PR> {
    let v = BaseVar::<IVec, PR>::query_named("v");
    let vh = v.handle();
    let e1 = vec_of::<IVec, _, _>([&1_i64, &2_i64]);
    let e2 = vec_empty::<IVec>().vec_push(&1_i64).vec_push(&2_i64);
    CheckIVecPat::new(v).assert(vh.eq(&e1)).assert(vh.eq(&e2))
}

fn pat_vec_check_vec_append<PR: PatRecSgl>() -> CheckIVecPat<PR> {
    let v = BaseVar::<IVec, PR>::query_named("v");
    let vh = v.handle();
    let lhs =
        vec_of::<IVec, _, _>([&1_i64, &2_i64]).vec_append(vec_of::<IVec, _, _>([&3_i64, &4_i64]));
    let rhs = vec_of::<IVec, _, _>([&1_i64, &2_i64, &3_i64, &4_i64]);
    CheckIVecPat::new(v).assert(vh.eq(&lhs)).assert(vh.eq(&rhs))
}

fn pat_vec_check_vec_pop<PR: PatRecSgl>() -> CheckIVecPat<PR> {
    let v = BaseVar::<IVec, PR>::query_named("v");
    let vh = v.handle();
    let lhs = vec_of::<IVec, _, _>([&1_i64, &2_i64, &3_i64]).vec_pop();
    let rhs = vec_of::<IVec, _, _>([&1_i64, &2_i64]);
    CheckIVecPat::new(v).assert(vh.eq(&lhs)).assert(vh.eq(&rhs))
}

fn pat_vec_check_vec_not_contains<PR: PatRecSgl>() -> CheckUnitPat<PR> {
    let e = vec_of::<IVec, _, _>([&1_i64, &2_i64, &3_i64]).vec_not_contains(&4_i64);
    CheckUnitPat::new().assert(e)
}

fn pat_vec_check_vec_contains<PR: PatRecSgl>() -> CheckUnitPat<PR> {
    let e = vec_of::<IVec, _, _>([&1_i64, &2_i64, &3_i64]).vec_contains(&2_i64);
    CheckUnitPat::new().assert(e)
}

fn pat_vec_check_vec_length<PR: PatRecSgl>() -> CheckI64Pat<PR> {
    let v = BaseVar::<i64, PR>::query_named("n");
    let vh = v.handle();
    let e = vec_of::<IVec, _, _>([&1_i64, &2_i64, &3_i64]).vec_len();
    CheckI64Pat::new(v).assert(vh.eq(&e)).assert(vh.eq(&3_i64))
}

fn pat_vec_check_vec_get<PR: PatRecSgl>() -> CheckI64Pat<PR> {
    let v = BaseVar::<i64, PR>::query_named("n");
    let vh = v.handle();
    let e = vec_of::<IVec, _, _>([&1_i64, &2_i64, &3_i64]).vec_get(&1_i64);
    CheckI64Pat::new(v).assert(vh.eq(&e)).assert(vh.eq(&2_i64))
}

fn pat_vec_check_vec_set<PR: PatRecSgl>() -> CheckIVecPat<PR> {
    let v = BaseVar::<IVec, PR>::query_named("v");
    let vh = v.handle();
    let lhs = vec_of::<IVec, _, _>([&1_i64, &2_i64, &3_i64]).vec_set(&1_i64, &4_i64);
    let rhs = vec_of::<IVec, _, _>([&1_i64, &4_i64, &3_i64]);
    CheckIVecPat::new(v).assert(vh.eq(&lhs)).assert(vh.eq(&rhs))
}

pub fn bench() {
    MyTxVec::sgl().reset_for_bench();

    let rs = MyTxVec::new_ruleset("vec_checks");

    // (check (= (vec-of 1 2) (vec-push (vec-push (vec-empty) 1) 2)))
    MyTxVec::add_rule(
        "vec_check_vec_of",
        rs,
        pat_vec_check_vec_of,
        |_ctx, _pat| {},
    );

    // (check (= (vec-append (vec-of 1 2) (vec-of 3 4)) (vec-of 1 2 3 4)))
    MyTxVec::add_rule(
        "vec_check_vec_append",
        rs,
        pat_vec_check_vec_append,
        |_ctx, _pat| {},
    );

    // (check (= (vec-pop (vec-of 1 2 3)) (vec-of 1 2)))
    MyTxVec::add_rule(
        "vec_check_vec_pop",
        rs,
        pat_vec_check_vec_pop,
        |_ctx, _pat| {},
    );

    // (check (vec-not-contains (vec-of 1 2 3) 4))
    MyTxVec::add_rule(
        "vec_check_vec_not_contains",
        rs,
        pat_vec_check_vec_not_contains,
        |_ctx, _pat| {},
    );

    // (check (vec-contains (vec-of 1 2 3) 2))
    MyTxVec::add_rule(
        "vec_check_vec_contains",
        rs,
        pat_vec_check_vec_contains,
        |_ctx, _pat| {},
    );

    // (check (= (vec-length (vec-of 1 2 3)) 3))
    MyTxVec::add_rule(
        "vec_check_vec_length",
        rs,
        pat_vec_check_vec_length,
        |_ctx, _pat| {},
    );

    // (check (= (vec-get (vec-of 1 2 3) 1) 2))
    MyTxVec::add_rule(
        "vec_check_vec_get",
        rs,
        pat_vec_check_vec_get,
        |_ctx, _pat| {},
    );

    // (check (= (vec-set (vec-of 1 2 3) 1 4) (vec-of 1 4 3)))
    MyTxVec::add_rule(
        "vec_check_vec_set",
        rs,
        pat_vec_check_vec_set,
        |_ctx, _pat| {},
    );

    let report = MyTxVec::run_ruleset(rs, RunConfig::Once);
    expect_rule_matches(&report, "vec_check_vec_of");
    expect_rule_matches(&report, "vec_check_vec_append");
    expect_rule_matches(&report, "vec_check_vec_pop");
    expect_rule_matches(&report, "vec_check_vec_not_contains");
    expect_rule_matches(&report, "vec_check_vec_contains");
    expect_rule_matches(&report, "vec_check_vec_length");
    expect_rule_matches(&report, "vec_check_vec_get");
    expect_rule_matches(&report, "vec_check_vec_set");

    // Rebuilding portion (ports the last part of `tests/vec.egg`).
    let seed = MyTxVec::new_ruleset("vec_rebuild_seed");
    MyTxVec::add_rule(
        "vec_rebuild_seed",
        seed,
        || {
            #[eggplant::pat_vars_catch]
            struct Unit {}
        },
        |ctx, _pat| {
            let a = ctx.insert_a();
            let b = ctx.insert_b();

            let mut p = VecContainer::<X>::new();
            p.push(a);
            ctx.set_p(p);

            let mut q = VecContainer::<X>::new();
            q.push(b);
            ctx.set_q(q);
        },
    );
    MyTxVec::run_ruleset(seed, RunConfig::Once);

    {
        let egraph = MyTxVec::egraph();
        let egraph = egraph.lock().unwrap();
        let sort = egraph.get_sort_by_name("VX").unwrap();
        let p0 = egraph.lookup_function("P", &[]).expect("P() should be set");
        let q0 = egraph.lookup_function("Q", &[]).expect("Q() should be set");
        assert_ne!(
            egraph.get_canonical_value(p0, sort),
            egraph.get_canonical_value(q0, sort)
        );
    }

    let u = MyTxVec::new_ruleset("vec_rebuild_union");
    MyTxVec::add_rule(
        "vec_rebuild_union",
        u,
        || {
            #[eggplant::pat_vars_catch]
            struct Unit {}
        },
        |ctx, _pat| {
            let a = ctx.insert_a();
            let b = ctx.insert_b();
            ctx.union(a, b);
        },
    );
    MyTxVec::run_ruleset(u, RunConfig::Once);
    let rebuild = MyTxVec::new_ruleset("vec_rebuild_rebuild");
    MyTxVec::run_ruleset(rebuild, RunConfig::Once);

    {
        let egraph = MyTxVec::egraph();
        let egraph = egraph.lock().unwrap();
        let sort = egraph.get_sort_by_name("VX").unwrap();
        let p1 = egraph.lookup_function("P", &[]).expect("P() should be set");
        let q1 = egraph.lookup_function("Q", &[]).expect("Q() should be set");
        assert_eq!(
            egraph.get_canonical_value(p1, sort),
            egraph.get_canonical_value(q1, sort)
        );
    }

    let egraph = MyTxVec::egraph();
    let mut egraph = egraph.lock().unwrap();
    egraph.serialize(egglog::SerializeConfig::default());
}
