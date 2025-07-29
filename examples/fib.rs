use egglog::prelude::*;
use egglog::{
    facts,
    prelude::{query, run_ruleset, rust_rule},
    vars,
};
use eggplant::{SingletonGetter, basic_tx_rx_vt};

#[eggplant::func(output=i64)]
struct Fib {
    x: i64,
}

basic_tx_rx_vt!(MyRx);

fn main() {
    let big_number = 20;
    // check that `(fib 20)` is kknot in the e-graph
    // let results = query(
    //     &mut egraph,
    //     vars![f: i64],
    //     facts![(= (fib (unquote exprs::int(big_number))) f)],
    // )?;

    // assert!(results.iter().next().is_none());

    let egraph = &mut MyRx::sgl().egraph.lock().unwrap();
    let ruleset = "custom_ruleset";
    // add_ruleset(&mut MyRx::sgl().egraph.lock().unwrap(), ruleset).unwrap();

    // let id = egraph.get_function(name);

    // egglog::BackendRule::new(egraph, functions, type_info);

    // let rsb = RuleSetBuilder::new(&mut egraph.backend.db);
    // let mut add_query = rsb.new_rule();
    // let x = add_query.new_var();
    // let y = add_query.new_var();
    // let z = add_query.new_var();
    // let t1 = add_query.new_var();
    // let t2 = add_query.new_var();
    // let t3 = add_query.new_var();
    // let a = add_query.new_var();
    // let b = add_query.new_var();
    // let mut sym2var: HashMap<Sym, Variable> = HashMap::new();
    // // insert 的应该 self.cur_sym  然后 输入 new_var
    // sym2var.insert("a".into(), x);
    // sym2var.insert("b".into(), y);

    // e

    // add_ruleset(egraph, ruleset);
    // let func_id = egraph.get_function(name).unwrap().backend_id;
    // let table_id = egraph.get_table_id(name);

    // add_query.add_atom(table_id, &[], std::iter::empty());

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
        ],
        move |ctx, values| {
            let [x, f0, f1] = values else { unreachable!() };
            let x = ctx.value_to_base::<i64>(*x);
            let f0 = ctx.value_to_base::<i64>(*f0);
            let f1 = ctx.value_to_base::<i64>(*f1);

            let y = ctx.base_to_value(x + 2);
            let f2 = ctx.base_to_value(f0 + f1);
            ctx.insert("Fib", [y, f2].into_iter());
            Some(())
        },
    )
    .unwrap();

    // run that rule 10 times
    for _ in 0..10 {
        run_ruleset(egraph, ruleset).unwrap();
    }

    // check that `(fib 0)` is now in the e-graph
    let results = query(
        egraph,
        vars![f: i64],
        facts![(= (fib (unquote exprs::int(big_number))) f)],
    )
    .unwrap();
    let _ = egraph.base_to_value::<i64>(6765);
    let _: Vec<_> = results.iter().collect();
}
