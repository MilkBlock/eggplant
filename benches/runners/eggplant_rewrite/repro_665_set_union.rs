use eggplant::prelude::*;
use eggplant::tx_rx_vt_pr;
use eggplant::wrap::SetContainer;

#[eggplant::container]
struct IntSet {
    pub inner: SetContainer<i64>,
}

#[eggplant::dsl]
enum RRel {
    #[eggplant::typst("R({i})")]
    R { i: i64 },
}

#[allow(non_camel_case_types)]
#[eggplant::func(output = IntSet, merge = "(set-union old new)")]
struct f {}

tx_rx_vt_pr!(MyTxRepro, MyPatRecRepro);

#[eggplant::pat_vars]
struct StepPat<PR: PatRecSgl> {
    r: R,
}

fn step_pat<PR: PatRecSgl>() -> StepPat<PR> {
    let r = R::query();
    let constraint = r.handle_i().lt(&2000);
    StepPat::new(r).assert(constraint)
}

pub fn bench() {
    MyTxRepro::sgl().reset_for_bench();

    let seed = MyTxRepro::new_ruleset("repro_665_seed");
    MyTxRepro::add_rule(
        "repro_665_seed",
        seed,
        || {
            #[eggplant::pat_vars_catch]
            struct Unit {}
        },
        |ctx, _pat| {
            ctx.insert_r(0);
        },
    );

    let step = MyTxRepro::new_ruleset("repro_665_step");
    MyTxRepro::add_rule("repro_665_step", step, step_pat, |ctx, pat| {
        let i = ctx.devalue(pat.r.i);
        ctx.insert_r(i + 1);

        let mut set = SetContainer::<i64>::new();
        set.insert(pat.r.i);
        ctx.set_f(set);
    });

    MyTxRepro::run_ruleset(seed, RunConfig::Once);
    MyTxRepro::run_ruleset(step, RunConfig::Sat);

    // Correctness check inside a rule (avoids AST/eval_expr).
    let check = MyTxRepro::new_ruleset("repro_665_check");
    MyTxRepro::add_rule(
        "repro_665_check",
        check,
        || {
            #[eggplant::pat_vars_catch]
            struct Unit {}
        },
        |ctx, _pat| {
            let out = ctx.try_read_f().expect("f() should be set");
            let set = ctx
                ._devalue_container::<egglog::sort::SetContainer>(out.val)
                .expect("f() output should be a set container");
            assert_eq!(set.data.len(), 2000);
        },
    );
    MyTxRepro::run_ruleset(check, RunConfig::Once);

    let egraph = MyTxRepro::egraph();
    let mut egraph = egraph.lock().unwrap();
    egraph.serialize(egglog::SerializeConfig::default());
}
