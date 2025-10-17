use eggplant::prelude::*;
use eggplant::tx_rx_vt_pr_fp;
use eggplant::wrap::VecContainer;
#[eggplant::dsl(container=Array, container = VarSet)]
pub enum Expr {
    Const { num: i64 },
    VecSum { exprs: Array },
    VarSet { exprs: SetStruct },
}
#[eggplant::container]
struct Array {
    inner: VecContainer<Expr>,
}
#[eggplant::container]
struct SetStruct {
    inner: SetContainer<Expr>,
}
#[eggplant::pat_vars]
struct VecPat {
    vec_expr: VecSum,
}

tx_rx_vt_pr_fp!(MyTx, MyPatRec);
fn main() {
    env_logger::init();
    let expr: Expr<MyTx, _> = VecSum::new(&Array::new(vec![
        &Const::new(3),
        &Const::new(2),
        &Const::new(1),
    ]));
    expr.commit();

    let ruleset = MyTx::new_ruleset("constant_prop");
    MyTx::add_rule(
        "sum_vec",
        ruleset,
        || {
            let vec_expr = Array::query_leaf();
            VecPat::new(VecSum::query(&vec_expr))
        },
        |ctx, pat| {
            println!("{:?}", pat);
            ctx.insert_vec_sum(ctx.insert_array({
                let mut v = VecContainer::new();
                v.push(ctx.insert_const(3));
                v
            }));
            ctx.insert_var_set(ctx.insert_set_struct({
                let mut set = SetContainer::new();
                set.insert(ctx.insert_const(4));
                set
            }));
            let v = ctx.devalue(pat.vec_expr.exprs);
            for expr in v.iter() {
                println!("got expr {:?}", expr)
            }
        },
    );
    let report = MyTx::run_ruleset(ruleset, RunConfig::Sat);
    println!("{:#?}", report);
    MyTx::table_view();

    expr.pull();
    MyTx::egraph_to_dot("egraph.dot");
    MyTx::wag_to_dot("wag.dot");
    // paterns to dot
    MyPatRec::sgl().pats_to_dot("pats.dot");
}
