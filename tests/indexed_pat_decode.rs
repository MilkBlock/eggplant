use eggplant::prelude::*;
use eggplant::tx_rx_vt_pr;
use eggplant::wrap::NonPatRecSgl;

#[eggplant::dsl]
enum IndexedDecodeExpr {
    IxConst {
        n: i64,
    },
    IxAdd {
        a: IndexedDecodeExpr,
        b: IndexedDecodeExpr,
    },
    IxNeg {
        a: IndexedDecodeExpr,
    },
}

tx_rx_vt_pr!(MyTxIndexedDecode, MyPatRecIndexedDecode);

fn serialized_user_node_count() -> usize {
    let egraph = MyTxIndexedDecode::egraph();
    let egraph = egraph.lock().unwrap();
    let serialized = egraph
        .serialize(eggplant::egglog::SerializeConfig::default())
        .egraph;
    serialized
        .nodes
        .values()
        .filter(|node| matches!(node.op.as_str(), "IxConst" | "IxAdd" | "IxNeg"))
        .count()
}

#[test]
fn indexed_pat_decode_preserves_node_growth_and_canonical_result() {
    let _ = env_logger::builder().is_test(true).try_init();
    MyTxIndexedDecode::sgl().reset_for_bench();

    let lhs = IxConst::<MyTxIndexedDecode>::new(1);
    let rhs = IxConst::<MyTxIndexedDecode>::new(2);
    let add = IxAdd::<MyTxIndexedDecode>::new(&lhs, &rhs);
    let root = IxNeg::<MyTxIndexedDecode>::new(&add);
    root.commit();

    let before_nodes = serialized_user_node_count();

    let ruleset = MyTxIndexedDecode::new_ruleset("indexed_pat_decode_fold");
    MyTxIndexedDecode::add_rule(
        "indexed_pat_decode_fold",
        ruleset,
        || {
            let lhs = IxConst::query();
            let rhs = IxConst::query();
            let add = IxAdd::query(&lhs, &rhs);
            let root = IxNeg::query(&add);
            #[eggplant::pat_vars_catch]
            struct Pat {
                lhs: IxConst,
                rhs: IxConst,
                add: IxAdd,
                root: IxNeg,
            }
        },
        |ctx, pat| {
            let folded = ctx.insert_ix_const(ctx.devalue(pat.lhs.n) * 10 + ctx.devalue(pat.rhs.n));
            let folded_neg = ctx.insert_ix_neg(folded);
            ctx.union(pat.root, folded_neg);
        },
    );

    let report = MyTxIndexedDecode::run_ruleset(ruleset, RunConfig::Once);
    assert_eq!(
        report
            .num_matches_per_rule
            .get("@indexed_pat_decode_fold")
            .copied()
            .unwrap_or(0),
        1
    );

    let after_nodes = serialized_user_node_count();
    assert_eq!(after_nodes - before_nodes, 2);

    let expected_const = IxConst::<MyTxIndexedDecode>::new(12);
    expected_const.commit();
    let expected_root = IxNeg::<MyTxIndexedDecode>::new(&expected_const);
    expected_root.commit();

    assert_eq!(
        MyTxIndexedDecode::canonical_raw(&root),
        MyTxIndexedDecode::canonical_raw(&expected_root)
    );
}
