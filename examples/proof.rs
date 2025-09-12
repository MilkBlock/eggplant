use eggplant::{
    prelude::*,
    wrap::{Rx, Value},
};
#[eggplant::ty]
enum Math {
    MNum { num: i64 },
    MAdd { l: Math, r: Math },
}
eggplant::tx_rx_vt_pr_pf!(MyTx, MyPatRec);
macro_rules! prop {
    ($ty:ident,$op:tt,$pat_name:ident,$ruleset:ident) => {
        #[eggplant::pat_vars]
        struct $pat_name {
            l: MNum,
            r: MNum,
            p: $ty,
        }
        MyTx::add_rule(
            stringify!($pat_name),
            $ruleset,
            || {
                let l = MNum::query();
                let r = MNum::query();
                let p = $ty::query(&l, &r);
                $pat_name::new(l, r, p)
            },
            |ctx, values| {
                let cal = ctx.devalue(values.l.num) $op ctx.devalue(values.r.num);
                let op_value = ctx.insert_m_num(cal);
                ctx.union(values.p, op_value);
            },
        );
    };
}

fn main() {
    env_logger::init();
    let sum: Math<MyTx, _> = MAdd::new(&MNum::new(1), &MNum::new(2));
    sum.commit();
    let ruleset = MyTx::new_ruleset("constant_prop_ruleset");
    prop!(MAdd,+,AddPat,ruleset);
    MyTx::run_ruleset(ruleset, RunConfig::Sat);
    let ruleset = MyTx::new_ruleset("commun_ruleset");
    MyTx::add_rule(
        "add_commun",
        ruleset,
        || {
            let (l, r) = (MNum::query(), MNum::query());
            let p = MAdd::query(&l, &r);
            AddPat::new(l, r, p)
        },
        |ctx, values| {
            let val = ctx.insert_m_add(values.r, values.l);
            println!("value of commun_add is {:?}", &val);
            ctx.union(values.p, val);
        },
    );
    let num_3: Math<MyTx, _> = MNum::new(3);
    num_3.commit();
    MyTx::run_ruleset(ruleset, RunConfig::Sat);
    println!("value of sum is {:?}", MyTx::value(&sum));
    MyTx::sgl().on_pull_value(MyTx::value(&sum));
    println!("value of num3 is {:?}", MyTx::value(&num_3));
    MyTx::sgl().on_pull_value(MyTx::value(&num_3));
    MyTx::egraph_to_dot("egraph.dot".into());
    MyTx::wag_to_dot("wag.dot".into());
    MyTx::proof_to_dot("proof.dot".into());

    MyTx::explain(Value::<i64>::new(egglog::Value::new_const(4)));
    // let store = MyTx::sgl().proof_store.lock().unwrap();
    // store.print_term_proof(id, &mut std::io::stdout()).unwrap();

    let egraph = MyTx::sgl().egraph.lock().unwrap();
    egraph.backend.dump_debug_info();
}
