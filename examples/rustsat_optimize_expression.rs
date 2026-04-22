use eggplant::{prelude::*, tx_rx_vt_pr};

#[eggplant::dsl]
enum RustsatExpr {
    Leaf {
        n: i64,
    },
    #[cost(0)]
    CheapWrap {
        inner: RustsatExpr,
    },
    #[cost(5)]
    ExpensiveWrap {
        inner: RustsatExpr,
    },
}

tx_rx_vt_pr!(RustsatDemoTx, RustsatDemoPatRec);

fn main() {
    let _ = env_logger::try_init();
    RustsatDemoTx::sgl().reset_for_bench();

    let leaf = Leaf::<RustsatDemoTx>::new(7);
    let cheap = CheapWrap::<RustsatDemoTx>::new(&leaf);
    cheap.commit();
    let expensive = ExpensiveWrap::<RustsatDemoTx>::new(&leaf);
    expensive.commit();

    let ruleset = RustsatDemoTx::new_ruleset("union_wrapper_variants_for_rustsat_demo");
    RustsatDemoTx::add_rule(
        "union_wrapper_variants_for_rustsat_demo",
        ruleset,
        || {
            let leaf = Leaf::query();
            let cheap = CheapWrap::query(&leaf);
            let expensive = ExpensiveWrap::query(&leaf);
            #[eggplant::pat_vars_catch]
            struct Pat {
                cheap: CheapWrap,
                expensive: ExpensiveWrap,
            }
        },
        |ctx, pat| {
            ctx.union(pat.cheap, pat.expensive);
        },
    );
    RustsatDemoTx::run_ruleset(ruleset, RunConfig::Once);

    let (legacy_rendered, legacy_cost) =
        RustsatDemoTx::extract_node_to_string(&cheap).expect("legacy extraction should succeed");
    let (rustsat_rendered, rustsat_cost) = RustsatDemoTx::extract_node_to_string_with_backend(
        &cheap,
        ExtractBackend::<eggplant::egglog::extract::TreeAdditiveCostModel>::rustsat(
            RustsatExtractConfig::default(),
        ),
    )
    .expect("rustsat extraction should succeed on the acyclic wrapper demo");

    println!("legacy extracted term: {legacy_rendered}");
    println!("legacy extracted cost: {legacy_cost}");
    println!("rustsat extracted term: {rustsat_rendered}");
    println!("rustsat extracted cost: {rustsat_cost}");

    assert!(
        rustsat_rendered.contains("CheapWrap"),
        "rustsat backend should pick the lower-cost wrapper"
    );
    assert_eq!(rustsat_cost, 1);
}
