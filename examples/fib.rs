#![allow(non_camel_case_types)]

use eggplant::egglog;
use eggplant::prelude::*;
use eggplant::tx_rx_vt_pr;

tx_rx_vt_pr!(MyTx, MyPatRec);

// Egglog 原始 demo（`tests/web-demo/fibonacci.egg`）：
//
// (function fib (i64) i64 :no-merge)
// (set (fib 0) 0)
// (set (fib 1) 1)
// (rule ((= f0 (fib x))
//        (= f1 (fib (+ x 1))))
//       ((set (fib (+ x 2)) (+ f0 f1))))
// (run 7)
// (check (= (fib 7) 13))
//
// 关键点：LHS 里有 “函数表查询” `(= f0 (fib x))` / `(= f1 (fib (+ x 1)))`。
// 这种 fact 不对应任何 AST 节点，不能只靠 PatRecorder.map 里的节点来生成；
// 必须通过 `PatRec::on_new_table_fact(...)` 把 `fib(x, f0)` 这类表事实注入 query。

#[eggplant::func(output = i64, no_merge)]
struct fib {
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
fn main() {
    let seed_ruleset = MyTx::new_ruleset("fib_seed");
    MyTx::add_rule(
        "fib_seed",
        seed_ruleset,
        || {
            #[eggplant::pat_vars_catch]
            struct Unit {}
        },
        |ctx, _pat| {
            ctx.set_fib(0, 0);
            ctx.set_fib(1, 1);
        },
    );

    let step_ruleset = MyTx::new_ruleset("fib_step");
    MyTx::add_rule(
        "fib_step",
        step_ruleset,
        || {
            // x, x+1, x+2
            let (x, x1, x2) = (fib::x(), fib::x().named("x1"), fib::x().named("x2"));

            let x1_constraint = x1.handle().eq(&(x.handle() + (&1_i64).as_handle()));
            let x2_constraint = x2.handle().eq(&(x.handle() + (&2_i64).as_handle()));

            let f0 = fib::query(&x);
            let f1 = fib::query(&x1);
            FibStep::new(x, x1, x2, f0, f1)
                .assert(x1_constraint)
                .assert(x2_constraint)
        },
        |ctx, pat| {
            let x2 = ctx.devalue(pat.x2);
            let f0 = ctx.devalue(pat.f0);
            let f1 = ctx.devalue(pat.f1);

            ctx.set_fib(x2, f0 + f1);
        },
    );

    MyTx::run_ruleset(seed_ruleset, RunConfig::Once);
    MyTx::run_ruleset(step_ruleset, RunConfig::Times(7));

    let got = fib::<MyTx>::get(&7);
    assert_eq!(got, 13);
    println!("fib(7) = {got}");

    if std::env::var_os("EGGPLANT_DOT").is_some() {
        MyTx::egraph_to_dot("egraph.dot");
    }
}
