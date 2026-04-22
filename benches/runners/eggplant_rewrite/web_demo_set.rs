use egglog_reports::RunReport;
use eggplant::prelude::*;
use eggplant::tx_rx_vt_pr;
use std::time::Instant;

#[eggplant::container]
struct ISetBase {
    inner: SetContainer<i64>,
}

#[eggplant::dsl]
enum SeenIdx {
    #[eggplant::typst("Seen({j})")]
    #[eggplant::precedence(100)]
    Seen { j: i64 },
}

// Equivalent to egglog's:
// `(function ISet-get (ISet i64) i64 :no-merge)`
//
// We declare it directly in the type inventory because base-container sorts (e.g. `(Set i64)`)
// are currently value-only wrappers in eggplant and can't be used as `#[eggplant::func]`
// inputs (macro requires `EgglogFuncInputs` on the input tuple).
inventory::submit! {
    eggplant::wrap::Decl::EgglogFuncTy {
        name: "ISet-get",
        input: &["ISetBase", "i64"],
        output: "i64",
        merge: None,
        hidden: false,
        let_binding: false,
        typst_template: None,
        precedence: u16::MAX,
    }
}

#[eggplant::func(output = ISetBase, no_merge)]
struct Myset {}

// Constants for the "builtin checks" section.
//
// We seed these via a ruleset action (using `SetContainer<i64>`), then reference them from
// check rules via function-table facts. This avoids `set-of` polymorphism inference issues
// when the global type inventory contains multiple `(Set i64)`-like sorts.
#[eggplant::func(output = ISetBase, no_merge)]
struct SEmpty {}
#[eggplant::func(output = ISetBase, no_merge)]
struct S12 {}
#[eggplant::func(output = ISetBase, no_merge)]
struct S34 {}
#[eggplant::func(output = ISetBase, no_merge)]
struct S1234 {}
#[eggplant::func(output = ISetBase, no_merge)]
struct S111 {}
#[eggplant::func(output = ISetBase, no_merge)]
struct S1m111 {}
#[eggplant::func(output = ISetBase, no_merge)]
struct S1m1241 {}
#[eggplant::func(output = ISetBase, no_merge)]
struct S123 {}

tx_rx_vt_pr!(MyTxSet, MyPatRecSet);

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

fn bind_const_set<PR: PatRecSgl>(
    func: &'static str,
    var: BaseVar<ISetBase, PR>,
) -> BaseVar<ISetBase, PR> {
    MyPatRecSet::on_new_table_fact(func.to_string(), vec![(var.name(), "ISetBase".to_string())]);
    var
}

#[eggplant::pat_vars]
struct CheckSetInsertPat<PR: PatRecSgl> {
    expected: BaseVar<ISetBase, PR>,
    empty: BaseVar<ISetBase, PR>,
}

fn pat_set_check_set_of_12_push_12<PR: PatRecSgl>() -> CheckSetInsertPat<PR> {
    let expected = bind_const_set::<PR>("S12", BaseVar::<ISetBase, PR>::query_named("s12"));
    let empty = bind_const_set::<PR>("SEmpty", BaseVar::<ISetBase, PR>::query_named("empty"));
    let expected_h = expected.handle();
    let empty_h = empty.handle();

    let rhs = empty_h.clone().set_insert(&1_i64).set_insert(&2_i64);

    CheckSetInsertPat::new(expected, empty).assert(expected_h.eq(&rhs))
}

fn pat_set_check_set_of_12_push_21<PR: PatRecSgl>() -> CheckSetInsertPat<PR> {
    let expected = bind_const_set::<PR>("S12", BaseVar::<ISetBase, PR>::query_named("s12"));
    let empty = bind_const_set::<PR>("SEmpty", BaseVar::<ISetBase, PR>::query_named("empty"));
    let expected_h = expected.handle();
    let empty_h = empty.handle();

    let rhs = empty_h.clone().set_insert(&2_i64).set_insert(&1_i64);

    CheckSetInsertPat::new(expected, empty).assert(expected_h.eq(&rhs))
}

#[eggplant::pat_vars]
struct CheckSetUnionPat<PR: PatRecSgl> {
    s12: BaseVar<ISetBase, PR>,
    s34: BaseVar<ISetBase, PR>,
    expected: BaseVar<ISetBase, PR>,
}

fn pat_set_check_set_union_1234<PR: PatRecSgl>() -> CheckSetUnionPat<PR> {
    let s12 = bind_const_set::<PR>("S12", BaseVar::<ISetBase, PR>::query_named("s12"));
    let s34 = bind_const_set::<PR>("S34", BaseVar::<ISetBase, PR>::query_named("s34"));
    let expected = bind_const_set::<PR>("S1234", BaseVar::<ISetBase, PR>::query_named("s1234"));
    let expected_h = expected.handle();

    let lhs = s12.handle().set_union(s34.handle());

    CheckSetUnionPat::new(s12, s34, expected).assert(expected_h.eq(&lhs))
}

#[eggplant::pat_vars]
struct CheckSetLenPat<PR: PatRecSgl> {
    set: BaseVar<ISetBase, PR>,
}

fn pat_set_check_set_length_empty_0<PR: PatRecSgl>() -> CheckSetLenPat<PR> {
    let set = bind_const_set::<PR>("SEmpty", BaseVar::<ISetBase, PR>::query_named("s"));
    let len = set.handle().set_len();
    CheckSetLenPat::new(set).assert(len.eq(&0_i64))
}

fn pat_set_check_set_length_of_111_1<PR: PatRecSgl>() -> CheckSetLenPat<PR> {
    let set = bind_const_set::<PR>("S111", BaseVar::<ISetBase, PR>::query_named("s"));
    let len = set.handle().set_len();
    CheckSetLenPat::new(set).assert(len.eq(&1_i64))
}

fn pat_set_check_set_length_of_1m111_2<PR: PatRecSgl>() -> CheckSetLenPat<PR> {
    let set = bind_const_set::<PR>("S1m111", BaseVar::<ISetBase, PR>::query_named("s"));
    let len = set.handle().set_len();
    CheckSetLenPat::new(set).assert(len.eq(&2_i64))
}

#[eggplant::pat_vars]
struct CheckSetGetPat<PR: PatRecSgl> {
    set: BaseVar<ISetBase, PR>,
}

fn pat_set_check_set_get_1m1241_0_is_1<PR: PatRecSgl>() -> CheckSetGetPat<PR> {
    let set = bind_const_set::<PR>("S1m1241", BaseVar::<ISetBase, PR>::query_named("s"));
    let got = set.handle().set_get(&0_i64);
    CheckSetGetPat::new(set).assert(got.eq(&1_i64))
}

fn pat_set_check_set_get_1m1241_1_is_2<PR: PatRecSgl>() -> CheckSetGetPat<PR> {
    let set = bind_const_set::<PR>("S1m1241", BaseVar::<ISetBase, PR>::query_named("s"));
    let got = set.handle().set_get(&1_i64);
    CheckSetGetPat::new(set).assert(got.eq(&2_i64))
}

fn pat_set_check_set_get_1m1241_2_is_4<PR: PatRecSgl>() -> CheckSetGetPat<PR> {
    let set = bind_const_set::<PR>("S1m1241", BaseVar::<ISetBase, PR>::query_named("s"));
    let got = set.handle().set_get(&2_i64);
    CheckSetGetPat::new(set).assert(got.eq(&4_i64))
}

fn pat_set_check_set_get_1m1241_3_is_m1<PR: PatRecSgl>() -> CheckSetGetPat<PR> {
    let set = bind_const_set::<PR>("S1m1241", BaseVar::<ISetBase, PR>::query_named("s"));
    let got = set.handle().set_get(&3_i64);
    CheckSetGetPat::new(set).assert(got.eq(&-1_i64))
}

// Reify rules.
#[eggplant::pat_vars]
struct Reify0Pat<PR: PatRecSgl> {
    x: BaseVar<ISetBase, PR>,
    y: BaseVar<i64, PR>,
}

fn reify0_pat<PR: PatRecSgl>() -> Reify0Pat<PR> {
    let x = BaseVar::<ISetBase, PR>::query_named("x");
    let y = BaseVar::<i64, PR>::query_named("y");
    let yh = y.handle();

    // Bind `x` by reading the 0-arg "global" function `Myset()` (replaces egglog's `$myset`).
    //
    // Without a table-fact assignment like this, `x` would be an unassigned variable and the
    // rule would fail egglog's rule typechecking.
    MyPatRecSet::on_new_table_fact(
        "Myset".to_string(),
        vec![(x.name(), "ISetBase".to_string())],
    );

    let len = x.handle().set_len();
    let y_expr = x.handle().set_get(&0_i64);

    Reify0Pat::new(x, y)
        .assert(len.gt(&0_i64))
        .assert(yh.eq(&y_expr))
}

#[eggplant::pat_vars]
struct ReifyStepPat<PR: PatRecSgl> {
    seen: Seen,
    x: BaseVar<ISetBase, PR>,
    i: BaseVar<i64, PR>,
    y: BaseVar<i64, PR>,
}

fn reify_step_pat<PR: PatRecSgl>() -> ReifyStepPat<PR> {
    let seen = Seen::query();
    let x = BaseVar::<ISetBase, PR>::query_named("x");
    let i = BaseVar::<i64, PR>::query_named("i");
    let y = BaseVar::<i64, PR>::query_named("y");
    let ih = i.handle();
    let yh = y.handle();

    // Same `Myset()` binding as `reify0_pat`.
    MyPatRecSet::on_new_table_fact(
        "Myset".to_string(),
        vec![(x.name(), "ISetBase".to_string())],
    );

    let i_expr = seen.handle_j() + (&1_i64).as_handle();
    let len = x.handle().set_len();
    let y_expr = x.handle().set_get(&ih);

    ReifyStepPat::new(seen, x, i, y)
        .assert(ih.eq(&i_expr))
        .assert(ih.lt(&len))
        .assert(yh.eq(&y_expr))
}

pub fn bench() {
    let breakdown = std::env::var_os("EGGPLANT_BENCH_BREAKDOWN").is_some();
    let t_total = Instant::now();

    let t = Instant::now();
    MyTxSet::sgl().reset_for_bench();
    if breakdown {
        eprintln!(
            "[bench-breakdown] web-demo/set reset_for_bench (checks): {:?}",
            t.elapsed()
        );
    }

    // Seed constants used by the builtin checks.
    let t_seed_setup = Instant::now();
    let seed_consts = MyTxSet::new_ruleset("web_demo_set_checks_seed_consts");
    MyTxSet::add_rule(
        "web_demo_set_checks_seed_consts",
        seed_consts,
        || {
            #[eggplant::pat_vars_catch]
            struct Unit {}
        },
        |ctx, _pat| {
            let mk_set = |vals: &[i64]| {
                let mut out = SetContainer::<i64>::new();
                for &v in vals {
                    out.insert(ctx.intern_base::<i64, _>(v));
                }
                out
            };

            ctx.set_s_empty(mk_set(&[]));
            ctx.set_s12(mk_set(&[1, 2]));
            ctx.set_s34(mk_set(&[3, 4]));
            ctx.set_s1234(mk_set(&[1, 2, 3, 4]));
            ctx.set_s111(mk_set(&[1, 1, 1]));
            ctx.set_s1m111(mk_set(&[1, -1, 1, 1]));
            ctx.set_s1m1241(mk_set(&[1, -1, 2, 4, 1]));
            ctx.set_s123(mk_set(&[1, 2, 3]));
        },
    );
    if breakdown {
        eprintln!(
            "[bench-breakdown] web-demo/set seed consts add_rule: {:?}",
            t_seed_setup.elapsed()
        );
    }

    let t_seed_run = Instant::now();
    MyTxSet::run_ruleset(seed_consts, RunConfig::Once);
    if breakdown {
        eprintln!(
            "[bench-breakdown] web-demo/set seed consts run_ruleset: {:?}",
            t_seed_run.elapsed()
        );
    }

    // Sanity: ensure the seeded constants are structurally consistent.
    //
    // This avoids spending time debugging a rule-matching issue when the underlying container
    // values themselves are wrong.
    let t_sanity = Instant::now();
    {
        let egraph = MyTxSet::egraph();
        let mut egraph = egraph.lock().unwrap();
        let s123_v = egraph
            .lookup_function("S123", &[])
            .expect("S123() should be set");
        let s12_v = egraph
            .lookup_function("S12", &[])
            .expect("S12() should be set");
        let x3 = egraph.base_to_value(3_i64);
        let s12 = egraph
            .value_to_container::<egglog::sort::SetContainer>(s12_v)
            .expect("S12 should be a SetContainer")
            .clone();
        let mut got = egraph
            .value_to_container::<egglog::sort::SetContainer>(s123_v)
            .expect("S123 should be a SetContainer")
            .clone();
        got.data.remove(&x3);
        assert_eq!(
            got, s12,
            "web_demo_set seeded constants inconsistent: set-remove(S123,3) != S12"
        );
    }
    if breakdown {
        eprintln!(
            "[bench-breakdown] web-demo/set seeded-const sanity check: {:?}",
            t_sanity.elapsed()
        );
    }

    // Builtin checks (ports the top part of `tests/web-demo/set.egg`).
    let t_checks_setup = Instant::now();
    let checks = MyTxSet::new_ruleset("web_demo_set_checks");
    MyTxSet::add_rule(
        "set_check_set_of_12_push_12",
        checks,
        pat_set_check_set_of_12_push_12,
        |_ctx, _pat| {},
    );
    MyTxSet::add_rule(
        "set_check_set_of_12_push_21",
        checks,
        pat_set_check_set_of_12_push_21,
        |_ctx, _pat| {},
    );
    MyTxSet::add_rule(
        "set_check_set_union_1234",
        checks,
        pat_set_check_set_union_1234,
        |_ctx, _pat| {},
    );
    MyTxSet::add_rule(
        "set_check_set_length_empty_0",
        checks,
        pat_set_check_set_length_empty_0,
        |_ctx, _pat| {},
    );
    MyTxSet::add_rule(
        "set_check_set_length_of_111_1",
        checks,
        pat_set_check_set_length_of_111_1,
        |_ctx, _pat| {},
    );
    MyTxSet::add_rule(
        "set_check_set_length_of_1m111_2",
        checks,
        pat_set_check_set_length_of_1m111_2,
        |_ctx, _pat| {},
    );
    MyTxSet::add_rule(
        "set_check_set_get_1m1241_0_is_1",
        checks,
        pat_set_check_set_get_1m1241_0_is_1,
        |_ctx, _pat| {},
    );
    MyTxSet::add_rule(
        "set_check_set_get_1m1241_1_is_2",
        checks,
        pat_set_check_set_get_1m1241_1_is_2,
        |_ctx, _pat| {},
    );
    MyTxSet::add_rule(
        "set_check_set_get_1m1241_2_is_4",
        checks,
        pat_set_check_set_get_1m1241_2_is_4,
        |_ctx, _pat| {},
    );
    MyTxSet::add_rule(
        "set_check_set_get_1m1241_3_is_m1",
        checks,
        pat_set_check_set_get_1m1241_3_is_m1,
        |_ctx, _pat| {},
    );
    if breakdown {
        eprintln!(
            "[bench-breakdown] web-demo/set builtin checks add_rule: {:?}",
            t_checks_setup.elapsed()
        );
    }

    let t_checks_run = Instant::now();
    let report = MyTxSet::run_ruleset(checks, RunConfig::Once);
    expect_rule_matches(&report, "set_check_set_of_12_push_12");
    expect_rule_matches(&report, "set_check_set_of_12_push_21");
    expect_rule_matches(&report, "set_check_set_union_1234");
    expect_rule_matches(&report, "set_check_set_length_empty_0");
    expect_rule_matches(&report, "set_check_set_length_of_111_1");
    expect_rule_matches(&report, "set_check_set_length_of_1m111_2");
    expect_rule_matches(&report, "set_check_set_get_1m1241_0_is_1");
    expect_rule_matches(&report, "set_check_set_get_1m1241_1_is_2");
    expect_rule_matches(&report, "set_check_set_get_1m1241_2_is_4");
    expect_rule_matches(&report, "set_check_set_get_1m1241_3_is_m1");
    if breakdown {
        eprintln!(
            "[bench-breakdown] web-demo/set builtin checks run+assert: {:?}",
            t_checks_run.elapsed()
        );
    }

    // Isolate the reify portion from the builtin checks to match the egglog source structure
    // (the `let/run/check` block is independent of the earlier `check`s).
    let t = Instant::now();
    MyTxSet::sgl().reset_for_bench();
    if breakdown {
        eprintln!(
            "[bench-breakdown] web-demo/set reset_for_bench (reify): {:?}",
            t.elapsed()
        );
    }

    // Reify set portion (ports the bottom half of `tests/web-demo/set.egg`).
    // Seed `(let $myset (IS (set-of 2 4 1 4 -1)))`.
    let t_reify_seed_setup = Instant::now();
    let seed = MyTxSet::new_ruleset("web_demo_set_reify_seed");
    MyTxSet::add_rule(
        "web_demo_set_reify_seed",
        seed,
        || {
            #[eggplant::pat_vars_catch]
            struct Unit {}
        },
        |ctx, _pat| {
            let mut set = SetContainer::<i64>::new();
            let v2 = ctx.intern_base::<i64, _>(2_i64);
            let v4 = ctx.intern_base::<i64, _>(4_i64);
            let v1 = ctx.intern_base::<i64, _>(1_i64);
            let vm1 = ctx.intern_base::<i64, _>(-1_i64);
            set.insert(v2);
            set.insert(v4);
            set.insert(v1);
            set.insert(v4);
            set.insert(vm1);

            ctx.set_myset(set);
        },
    );
    if breakdown {
        eprintln!(
            "[bench-breakdown] web-demo/set reify seed add_rule: {:?}",
            t_reify_seed_setup.elapsed()
        );
    }

    let t_reify_seed_run = Instant::now();
    MyTxSet::run_ruleset(seed, RunConfig::Once);
    if breakdown {
        eprintln!(
            "[bench-breakdown] web-demo/set reify seed run_ruleset: {:?}",
            t_reify_seed_run.elapsed()
        );
    }

    let t_reify_setup = Instant::now();
    let reify = MyTxSet::new_ruleset("web_demo_set_reify");
    MyTxSet::add_rule("web_demo_set_reify0", reify, reify0_pat, |ctx, pat| {
        // Manual `ISet-get` access (declared via inventory above).
        //
        // Generated `ctx.set_i_set_get` helpers are unavailable because base-container sorts
        // can't currently be used as `#[eggplant::func]` inputs.
        let key = [pat.x.erase(), ctx.intern_base::<i64, _>(0_i64).erase()];
        if ctx.lookup("ISet-get", &key).is_none() {
            let row = [
                pat.x.erase(),
                ctx.intern_base::<i64, _>(0_i64).erase(),
                pat.y.erase(),
            ];
            ctx.insert_func_tbl("ISet-get", &row);
            ctx.insert_seen(0);
        }
    });
    MyTxSet::add_rule(
        "web_demo_set_reify_step",
        reify,
        reify_step_pat,
        |ctx, pat| {
            let i = ctx._devalue_base::<i64>(pat.i.erase());
            let key = [pat.x.erase(), pat.i.erase()];
            if ctx.lookup("ISet-get", &key).is_none() {
                let row = [pat.x.erase(), pat.i.erase(), pat.y.erase()];
                ctx.insert_func_tbl("ISet-get", &row);
                ctx.insert_seen(i);
            }
        },
    );
    if breakdown {
        eprintln!(
            "[bench-breakdown] web-demo/set reify add_rule: {:?}",
            t_reify_setup.elapsed()
        );
    }

    let t_reify_run = Instant::now();
    MyTxSet::run_ruleset(reify, RunConfig::Times(100));
    if breakdown {
        eprintln!(
            "[bench-breakdown] web-demo/set reify run_ruleset: {:?}",
            t_reify_run.elapsed()
        );
    }

    // Checks (via raw function-table lookups, avoids AST/pull).
    let t_final = Instant::now();
    let egraph = MyTxSet::egraph();
    let mut egraph = egraph.lock().unwrap();
    let myset_v = egraph
        .lookup_function("Myset", &[])
        .expect("Myset() should be set");
    let i0 = egraph.base_to_value(0_i64);
    let i1 = egraph.base_to_value(1_i64);
    let i2 = egraph.base_to_value(2_i64);
    let i3 = egraph.base_to_value(3_i64);
    let g0 = egraph.lookup_function("ISet-get", &[myset_v, i0]).unwrap();
    let g1 = egraph.lookup_function("ISet-get", &[myset_v, i1]).unwrap();
    let g2 = egraph.lookup_function("ISet-get", &[myset_v, i2]).unwrap();
    let g3 = egraph.lookup_function("ISet-get", &[myset_v, i3]).unwrap();
    assert_eq!(egraph.value_to_base::<i64>(g0), 1);
    assert_eq!(egraph.value_to_base::<i64>(g1), 2);
    assert_eq!(egraph.value_to_base::<i64>(g2), 4);
    assert_eq!(egraph.value_to_base::<i64>(g3), -1);
    egraph.serialize(egglog::SerializeConfig::default());
    if breakdown {
        eprintln!(
            "[bench-breakdown] web-demo/set final checks+serialize: {:?}",
            t_final.elapsed()
        );
        eprintln!(
            "[bench-breakdown] web-demo/set total: {:?}",
            t_total.elapsed()
        );
    }
}
