use eggplant::{prelude::*, tx_rx_vt_pr};

#[eggplant::relation]
struct Edge {
    src: i64,
    dst: i64,
}

#[eggplant::relation]
struct Path {
    src: i64,
    dst: i64,
}

#[allow(non_camel_case_types)]
#[eggplant::func(output = bool, no_merge)]
struct path_mark {
    src: i64,
    dst: i64,
}

tx_rx_vt_pr!(RelTx, RelPatRec);

fn main() {
    let _ = env_logger::try_init();

    Edge::<RelTx>::insert(1, 2);
    Edge::<RelTx>::insert(2, 3);

    let ruleset = RelTx::new_ruleset("relation_path");
    RelTx::add_rule(
        "seed_path",
        ruleset,
        || {
            let edge = Edge::query();
            #[eggplant::pat_vars_catch]
            struct Pat {
                edge: Edge,
            }
        },
        |ctx, pat| {
            let src = ctx.devalue(pat.edge.src);
            let dst = ctx.devalue(pat.edge.dst);
            ctx.insert_path(src, dst);
            ctx.set_path_mark(src, dst, true);
        },
    );
    RelTx::add_rule(
        "extend_path",
        ruleset,
        || {
            let path = Path::query();
            let edge = Edge::query();
            let join = path.handle_dst().eq(&edge.handle_src());
            #[eggplant::pat_vars]
            struct Pat {
                path: Path,
                edge: Edge,
            }
            Pat::new(path, edge).assert(join)
        },
        |ctx, pat| {
            let src = ctx.devalue(pat.path.src);
            let dst = ctx.devalue(pat.edge.dst);
            ctx.insert_path(src, dst);
            ctx.set_path_mark(src, dst, true);
        },
    );

    let report = RelTx::run_ruleset(ruleset, RunConfig::Sat);
    println!("matches: {:?}", report.num_matches_per_rule);
    println!("path(1,2): {}", path_mark::<RelTx>::get((&1, &2)));
    println!("path(1,3): {}", path_mark::<RelTx>::get((&1, &3)));
}
