use eggplant::prelude::*;
use eggplant::tx_rx_vt_pr;
use eggplant::wrap::Rx;
// use eggplant::tx_rx_vt_pr_pf;
#[eggplant::ty]
pub enum Math {
    MNum { num: i64 },
    MVar { s: String },
    MAdd { l: Math, r: Math },
    MSub { l: Math, r: Math },
    MMul { l: Math, r: Math },
    MDiv { l: Math, r: Math },
    MMod { l: Math, r: Math },
    MMin { l: Math, r: Math },
    MMax { l: Math, r: Math },
    MAnd { l: Math, r: Math },
    MOr { l: Math, r: Math },
    MGte { l: Math, r: Math },
    MLt { l: Math, r: Math },
    MFloorTo { l: Math, r: Math },
    MReplace { l: Math, r: Math, s: Math },
    MAccum {},
}
#[eggplant::ty]
pub enum LoopType {
    Loop { s: String, math: Math },
}
#[eggplant::ty]
pub enum Expr {
    Tensor {
        name: String,
    },
    LoopIn {
        expr: Expr,
        loop_type: LoopType,
        math: Math,
    },
    Sub {
        l: Expr,
        r: Expr,
    },
    Add {
        l: Expr,
        r: Expr,
    },
    Div {
        l: Expr,
        r: Expr,
    },
    Exp {
        base: Expr,
    },
    Recip {
        recip: Expr,
    },
    Max {
        l: Expr,
        r: Expr,
    },
    Binary {
        op: String,
        l: Expr,
        r: Expr,
    },
    NewAcc {
        acc: i64,
    },
    AccOut {
        expr: Expr,
        loop_ty: LoopType,
    },
    SwapLoops {
        expr: Expr,
        a: String,
        b: String,
    },
}
tx_rx_vt_pr!(MyTx, MyPatRec);
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
    let expr: Math<MyTx, _> = MAdd::new(&MMul::new(&MNum::new(3), &MNum::new(2)), &MNum::new(4));
    expr.commit();

    let ruleset = MyTx::new_ruleset("constant_prop");
    prop!(MAdd,+,AddPat,ruleset);
    prop!(MSub,-,SubPat,ruleset);
    prop!(MMul,*,MulPat,ruleset);
    prop!(MDiv,/,DivPat,ruleset);
    let report = MyTx::run_ruleset(ruleset, RunConfig::Sat);
    println!("{:#?}", report);
    MyTx::egraph_to_dot("egraph.dot".into());
    let val = MyTx::value(&expr);
    MyTx::sgl().on_pull_value(val);
    println!("{:?}", val);
    // let explaination = MyTx::explain(val);
    // println!("{:#?}", explaination);
}
