use egglog::prelude::*;
use egglog::{
    facts,
    prelude::{add_ruleset, query, run_ruleset, rust_rule},
    vars,
};
use eggplant::{SingletonGetter, basic_tx_rx_vt, eggplant_func};

#[eggplant_func(output=i64)]
struct Fib {
    x: i64,
}

basic_tx_rx_vt!(MyRx);

fn main() {
    Fib::<MyRx>::set(&0, &1);
    Fib::<MyRx>::set(&1, &1);
    {
        let big_number = 20;
        // check that `(Fib 20)` is kknot in the e-graph
        // let results = query(
        //     &mut egraph,
        //     vars![f: i64],
        //     facts![(= (Fib (unquote exprs::int(big_number))) f)],
        // )?;

        // assert!(results.iter().next().is_none());

        let egraph = &mut MyRx::sgl().egraph.lock().unwrap();
        let ruleset = "custom_ruleset";
        add_ruleset(egraph, ruleset).unwrap();

        // add the rule from `build_test_database` to the egraph
        rust_rule(
            egraph,
            ruleset,
            // variable decl
            // variable binding
            //        decl and binding can be merged together
            // calculate new info
            //        omit some value to base ...
            vars![x: i64, f0: i64, f1: i64],
            facts![
                (= f0 (Fib x))
                (= f1 (Fib (+ x 1)))
                // (= f2 (Fib (+ x 1)))
            ],
            move |ctx, values: &[egglog::Value]| {
                let [x, f0, f1] = values else { unreachable!() };
                let x = ctx.value_to_base::<i64>(*x);
                let f0 = ctx.value_to_base::<i64>(*f0);
                let f1 = ctx.value_to_base::<i64>(*f1);
                let y = ctx.base_to_value::<i64>(x + 2);
                let f2 = ctx.base_to_value::<i64>(f0 + f1);
                ctx.insert("Fib", [y, f2].into_iter());
                Some(())
            },
        )
        .unwrap();

        // run that rule 10 times
        for _ in 0..10 {
            run_ruleset(egraph, ruleset).unwrap();
        }

        // check that `(Fib 0)` is now in the e-graph
        let results = query(
            egraph,
            vars![f: i64],
            facts![(= (Fib (unquote exprs::int(big_number))) f)],
        )
        .unwrap();
        let _ = egraph.base_to_value::<i64>(6765);
        let _: Vec<_> = results.iter().collect();
    }

    MyRx::sgl().egraph_to_dot("egraph.dot".into());
    println!("fib of 2 is {}", Fib::<MyRx>::get(&3))
}
