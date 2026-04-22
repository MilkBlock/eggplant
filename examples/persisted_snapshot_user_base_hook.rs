use eggplant::artifact::{build_persisted_snapshot_v1, persisted_snapshot_capability_summary};
use eggplant::prelude::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
#[eggplant::base_ty]
struct DemoUserBase {
    n: i64,
}

static DEMO_USER_BASE_RESTORE_HOOK: eggplant::wrap::SerdeJsonUserBaseSortHook<DemoUserBase> =
    eggplant::wrap::SerdeJsonUserBaseSortHook::new("serde-json-boxed");

inventory::submit! {
    eggplant::wrap::PersistedSnapshotUserBaseSortHookRegistration::new(
        "DemoUserBase",
        &DEMO_USER_BASE_RESTORE_HOOK,
    )
}

fn main() {
    let mut egraph = eggplant::egglog::EGraph::default();
    for sort in inventory::iter::<eggplant::wrap::UserBaseSort> {
        if sort.name == "DemoUserBase" {
            (sort.sort_insert_fn)(&mut egraph);
        }
    }

    egraph
        .parse_and_run_program(
            None,
            r#"
(datatype DemoExpr (DemoLeaf DemoUserBase))
"#,
        )
        .unwrap();

    eggplant::egglog::prelude::run_ephemeral_rust_rule(
        &mut egraph,
        "seed_demo_user_base_snapshot",
        &[],
        eggplant::egglog::ast::Facts(Vec::new()),
        |ctx, _| {
            let base =
                ctx.base_to_value(eggplant::egglog::sort::Boxed::new(DemoUserBase { n: 42 }));
            let _ = ctx.lookup("DemoLeaf", &[base]);
            Some(())
        },
    )
    .unwrap();

    let snapshot =
        build_persisted_snapshot_v1(&egraph, eggplant::egglog::SerializeConfig::default());
    let summary = persisted_snapshot_capability_summary(&snapshot);

    println!(
        "guaranteed restorable: {:?}",
        summary
            .guaranteed_restorable
            .iter()
            .map(|entry| entry.key.as_str())
            .collect::<Vec<_>>()
    );
}
