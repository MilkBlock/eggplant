use eggplant::prelude::*;
// use eggplant::tx_rx_vt_pr;
use eggplant::tx_rx_vt_pr_fp;
use eggplant::wrap::G;
use eggplant::wrap::Rx;
#[eggplant::dsl]
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
#[eggplant::dsl]
pub enum LoopType {
    Loop { layer_name: String, z_range: Math },
}
#[eggplant::dsl]
pub enum Expr {
    Tensor {
        name: String,
    },
    LIn {
        expr: Expr,
        arr_and_range: LoopType,
        stride: Math,
    },
    LOut {
        expr: Expr,
        arr_and_range: LoopType,
        stride: Math,
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
tx_rx_vt_pr_fp!(MyTx, MyPatRec);
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
            |ctx, pat| {
                let cal = ctx.devalue(pat.l.num) $op ctx.devalue(pat.r.num);
                let op_value = ctx.insert_m_num(cal);
                ctx.union(pat.p, op_value);
            },
        );
    };
}
fn main() {
    env_logger::init();

    let ruleset = MyTx::new_ruleset("constant_prop");
    prop!(MAdd,+,AddPat,ruleset);
    prop!(MSub,-,SubPat,ruleset);
    prop!(MMul,*,MulPat,ruleset);
    prop!(MDiv,/,DivPat,ruleset);

    // compute max in arr
    let tensor_arr: Expr<MyTx, TensorTy> = Tensor::new("Arr [1,5,2,9]".to_string());
    let acc_init: Expr<MyTx, NewAccTy> = NewAcc::new(0);
    let arr_loop = LoopType::new("i".to_string(), &MNum::new(4));
    let col1 = MVar::new("z".to_string());
    let arr_loop_in = LIn::new(&tensor_arr, &arr_loop, &col1);
    let acc_loop_in = LIn::new(&acc_init, &arr_loop, &MAccum::new());
    let max = Max::new(&arr_loop_in, &acc_loop_in);
    let max_arr: Expr<MyTx, LOutTy> = LOut::new(
        &max,
        &Loop::new("fold".to_string(), &MNum::new(4)),
        &MAccum::new(),
    );
    max_arr.commit();

    let report = MyTx::run_ruleset(ruleset, RunConfig::Sat);
    println!("{:#?}", report);
    MyTx::egraph_to_dot("egraph.dot");

    let val = MyTx::value(&max);
    MyTx::sgl().on_pull_value(val);
    MyTx::wag_to_dot("wag.dot");
    a::<MyTx>();
}

fn a<T: G>() {
    MNum::<T>::new(3);
}
