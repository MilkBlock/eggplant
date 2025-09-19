use eggplant::prelude::*;
use eggplant::tx_rx_vt_pr_fp;
#[eggplant::dsl(container =Array)]
pub enum Expr {
    Const { num: i64 },
    VecSum { exprs: Array },
}
#[eggplant::container]
struct Array {
    inner: Vec<Expr>,
}
#[eggplant::pat_vars]
struct SumVec {
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
            SumVec::new(VecSum::query(&vec_expr))
        },
        |ctx, values| {
            println!("{:?}", values);
            let v = ctx.devalue(values.vec_expr.exprs);
            for expr in v.iter() {
                println!("got expr {:?}", expr)
            }
        },
    );
    let report = MyTx::run_ruleset(ruleset, RunConfig::Sat);
    println!("{:#?}", report);
    MyTx::table_view();

    expr.pull();
    MyTx::egraph_to_dot("egraph.dot".into());
    MyTx::wag_to_dot("wag.dot".into());
    // paterns to dot
    MyPatRec::sgl().pats_to_dot("pats.dot".into());
}
