#![allow(non_camel_case_types)]

use eggplant::{prelude::*, tx_rx_vt_pr};

#[eggplant::func(output = i64, no_merge)]
struct sched_fib {
    x: i64,
}

#[eggplant::pat_vars]
struct FibStep<PR: PatRecSgl> {
    x: i64,
    x1: i64,
    x2: i64,
    f0: i64,
    f1: i64,
}

tx_rx_vt_pr!(ScheduleBuilderTx, ScheduleBuilderPatRec);

#[test]
fn schedule_builder_runs_nested_repeat_schedule() {
    let seed = ScheduleBuilderTx::new_ruleset("schedule_builder_seed");
    ScheduleBuilderTx::add_rule(
        "schedule_builder_seed",
        seed,
        || {
            #[eggplant::pat_vars_catch]
            struct Unit {}
        },
        |ctx, _pat| {
            ctx.set_sched_fib(0, 0);
            ctx.set_sched_fib(1, 1);
        },
    );

    let step = ScheduleBuilderTx::new_ruleset("schedule_builder_step");
    ScheduleBuilderTx::add_rule(
        "schedule_builder_step",
        step,
        || {
            let (x, x1, x2) = (
                sched_fib::x(),
                sched_fib::x().named("x1"),
                sched_fib::x().named("x2"),
            );
            let x1_constraint = x1.handle().eq(&(x.handle() + (&1_i64).as_handle()));
            let x2_constraint = x2.handle().eq(&(x.handle() + (&2_i64).as_handle()));
            let f0 = sched_fib::query(&x);
            let f1 = sched_fib::query(&x1);
            FibStep::new(x, x1, x2, f0, f1)
                .assert(x1_constraint)
                .assert(x2_constraint)
        },
        |ctx, pat| {
            let x2 = ctx.devalue(pat.x2);
            let f0 = ctx.devalue(pat.f0);
            let f1 = ctx.devalue(pat.f1);
            ctx.set_sched_fib(x2, f0 + f1);
        },
    );

    let schedule = RunSchedule::builder()
        .run(seed)
        .repeat(7, |schedule| schedule.run(step))
        .build();

    ScheduleBuilderTx::run_schedule(schedule);

    assert_eq!(sched_fib::<ScheduleBuilderTx>::get(&7), 13);
}
