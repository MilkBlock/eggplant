use eggplant::{self, slotted_tx_rx_vt_pr};
use eggplant::prelude::*;

#[eggplant::relation(typst = "edge({src}, {dst})", precedence = 40)]
struct RelEdge {
    src: i64,
    dst: i64,
}

#[eggplant::relation]
struct RelPath {
    src: i64,
    dst: i64,
}

#[allow(non_camel_case_types)]
#[eggplant::func(output = bool, no_merge)]
struct rel_path_mark {
    src: i64,
    dst: i64,
}

#[test]
fn typed_relation_api_supports_seed_query_and_action_insert() {
    let _ = env_logger::builder().is_test(true).try_init();

    let type_defs = eggplant::wrap::EgglogTypeRegistry::collect_type_defs();
    assert!(type_defs.iter().any(|cmd| {
        matches!(
            cmd,
            eggplant::egglog::ast::Command::Relation { name, .. } if name == "RelPath"
        )
    }));

    slotted_tx_rx_vt_pr!(RelTx, RelPatRec);

    RelEdge::<RelTx>::insert(1, 2);
    RelEdge::<RelTx>::insert(2, 3);
    RelTx::receive(eggplant::wrap::TxCommand::StringCommand {
        command: "(check (RelEdge 1 2))".to_string(),
    });

    let ruleset = RelTx::new_ruleset("typed_relation_path");
    RelTx::add_rule(
        "seed_path",
        ruleset,
        || {
            let edge = RelEdge::query();
            #[eggplant::pat_vars_catch]
            struct Pat {
                edge: RelEdge,
            }
        },
        |ctx, pat| {
            let src = ctx.devalue(pat.edge.src);
            let dst = ctx.devalue(pat.edge.dst);
            ctx.insert_rel_path(src, dst);
            ctx.set_rel_path_mark(src, dst, true);
        },
    );
    RelTx::add_rule(
        "extend_path",
        ruleset,
        || {
            let path = RelPath::query();
            let edge = RelEdge::query();
            let join = path.handle_dst().eq(&edge.handle_src());
            #[eggplant::pat_vars]
            struct Pat {
                path: RelPath,
                edge: RelEdge,
            }
            Pat::new(path, edge).assert(join)
        },
        |ctx, pat| {
            let src = ctx.devalue(pat.path.src);
            let dst = ctx.devalue(pat.edge.dst);
            ctx.insert_rel_path(src, dst);
            ctx.set_rel_path_mark(src, dst, true);
        },
    );

    let report = RelTx::run_ruleset(ruleset, RunConfig::Sat);

    let seed_matches: usize = report
        .num_matches_per_rule
        .iter()
        .filter(|(name, _)| name.contains("seed_path"))
        .map(|(_, count)| *count)
        .sum();
    let extend_matches: usize = report
        .num_matches_per_rule
        .iter()
        .filter(|(name, _)| name.contains("extend_path"))
        .map(|(_, count)| *count)
        .sum();

    assert!(seed_matches > 0);
    assert!(extend_matches > 0);
    assert_eq!(rel_path_mark::<RelTx>::get((&1, &2)), true);
    assert_eq!(rel_path_mark::<RelTx>::get((&1, &3)), true);
}
