use std::fs;
use std::path::PathBuf;
use std::sync::{Mutex, MutexGuard, OnceLock};

use eggplant::artifact::{
    PersistedSnapshot, build_persisted_snapshot_v1, build_persisted_snapshot_v2_eqclass,
    restore_persisted_snapshot_v1,
};
use eggplant::prelude::*;
use eggplant::tx_rx_vt_pr;

#[eggplant::dsl]
enum FixtureExpr {
    Const { num: i64 },
}

#[eggplant::dsl]
enum FixtureRoot {
    Root { node: FixtureExpr },
}

#[eggplant::relation]
struct FixtureEdge {
    src: i64,
    dst: i64,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
#[eggplant::base_ty]
struct HookedFixtureBase {
    n: i64,
}

struct HookedFixtureBaseRestoreHook;

impl eggplant::wrap::PersistedSnapshotUserBaseSortHook for HookedFixtureBaseRestoreHook {
    fn capability_label(&self) -> &'static str {
        "fixture-hooked-json-object"
    }

    fn export_machine_value(
        &self,
        egraph: &eggplant::egglog::EGraph,
        value: eggplant::egglog::Value,
    ) -> Option<serde_json::Value> {
        let value = egraph.value_to_base::<eggplant::egglog::sort::Boxed<HookedFixtureBase>>(value);
        Some(serde_json::json!({ "n": value.0.n }))
    }

    fn restore_machine_value(
        &self,
        ctx: &mut eggplant::egglog::prelude::RustRuleContext<'_, '_, '_>,
        machine_value: &serde_json::Value,
    ) -> Result<eggplant::egglog::Value, String> {
        let n = machine_value
            .get("n")
            .and_then(|value| value.as_i64())
            .ok_or_else(|| "missing integer field `n`".to_string())?;
        Ok(ctx.base_to_value(eggplant::egglog::sort::Boxed::new(HookedFixtureBase { n })))
    }
}

static HOOKED_FIXTURE_BASE_RESTORE_HOOK: HookedFixtureBaseRestoreHook =
    HookedFixtureBaseRestoreHook;

inventory::submit! {
    eggplant::wrap::PersistedSnapshotUserBaseSortHookRegistration {
        name: "HookedFixtureBase",
        hook: &HOOKED_FIXTURE_BASE_RESTORE_HOOK,
    }
}

tx_rx_vt_pr!(FixtureTx, FixturePatRec);

fn fixture_path(name: &str) -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("fixtures")
        .join("persisted_snapshot_v1")
        .join(format!("{name}.json"))
}

fn binary_fixture_path(name: &str) -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("fixtures")
        .join("persisted_snapshot_v1")
        .join(format!("{name}.egbin"))
}

fn fixture_test_guard() -> MutexGuard<'static, ()> {
    static LOCK: OnceLock<Mutex<()>> = OnceLock::new();
    LOCK.get_or_init(|| Mutex::new(()))
        .lock()
        .unwrap_or_else(|err| err.into_inner())
}

fn normalize_snapshot_for_fixture(snapshot: &mut PersistedSnapshot) {
    if let Some(producer) = snapshot.producer.as_mut() {
        producer.crate_name = "<crate>".to_string();
        producer.version = "<version>".to_string();
    }
    if let Some(source) = snapshot.source_schema.as_mut() {
        source.engine_fingerprint = "<engine_fingerprint>".to_string();
        source.dsl_runtime_fingerprint = "<dsl_runtime_fingerprint>".to_string();
        source.dsl_metadata_fingerprint = "<dsl_metadata_fingerprint>".to_string();
        source.macro_rev = "<macro_rev>".to_string();
    }
    for (idx, ruleset) in snapshot.dictionary.rulesets.iter_mut().enumerate() {
        *ruleset = format!("ruleset_{idx}");
    }
    for (idx, decl) in snapshot.schema.ruleset_decls.iter_mut().enumerate() {
        decl.ruleset_id = idx;
        decl.name = format!("ruleset_{idx}");
    }
}

fn build_common_path_snapshot() -> PersistedSnapshot {
    FixtureTx::sgl().reset_for_bench();
    let root_a = Root::<FixtureTx>::new(&Const::new(7));
    let root_b = Root::<FixtureTx>::new(&Const::new(9));
    root_a.commit();
    root_b.commit();
    FixtureEdge::<FixtureTx>::insert(1, 2);
    let egraph = FixtureTx::sgl().egraph.lock().unwrap();
    build_persisted_snapshot_v1(&egraph, eggplant::egglog::SerializeConfig::default())
}

fn build_common_path_fixture_snapshot() -> PersistedSnapshot {
    let mut snapshot = build_common_path_snapshot();
    normalize_snapshot_for_fixture(&mut snapshot);
    snapshot
}

fn build_common_path_v2_eqclass_snapshot() -> PersistedSnapshot {
    FixtureTx::sgl().reset_for_bench();
    let root_a = Root::<FixtureTx>::new(&Const::new(7));
    let root_b = Root::<FixtureTx>::new(&Const::new(9));
    root_a.commit();
    root_b.commit();
    FixtureEdge::<FixtureTx>::insert(1, 2);
    let egraph = FixtureTx::sgl().egraph.lock().unwrap();
    build_persisted_snapshot_v2_eqclass(&egraph, eggplant::egglog::SerializeConfig::default())
}

fn build_common_path_v2_eqclass_fixture_snapshot() -> PersistedSnapshot {
    let mut snapshot = build_common_path_v2_eqclass_snapshot();
    normalize_snapshot_for_fixture(&mut snapshot);
    snapshot
}

fn build_plain_source_snapshot() -> PersistedSnapshot {
    let mut egraph = eggplant::egglog::EGraph::default();
    egraph
        .parse_and_run_program(
            None,
            r#"
(relation edge (i64 i64))
(edge 1 2)
"#,
        )
        .unwrap();
    build_persisted_snapshot_v1(&egraph, eggplant::egglog::SerializeConfig::default())
}

fn build_plain_source_fixture_snapshot() -> PersistedSnapshot {
    let mut snapshot = build_plain_source_snapshot();
    normalize_snapshot_for_fixture(&mut snapshot);
    snapshot
}

fn build_eqclass_union_v2_fixture_snapshot() -> PersistedSnapshot {
    let mut egraph = eggplant::egglog::EGraph::default();
    egraph
        .parse_and_run_program(
            None,
            r#"
(datatype Expr (Const i64) (Alias Expr))
(union (Const 7) (Alias (Const 7)))
"#,
        )
        .unwrap();
    let mut snapshot =
        build_persisted_snapshot_v2_eqclass(&egraph, eggplant::egglog::SerializeConfig::default());
    normalize_snapshot_for_fixture(&mut snapshot);
    snapshot
}

fn build_hooked_user_base_snapshot() -> PersistedSnapshot {
    let mut egraph = eggplant::egglog::EGraph::default();
    for sort in inventory::iter::<eggplant::wrap::UserBaseSort> {
        if sort.name == "HookedFixtureBase" {
            (sort.sort_insert_fn)(&mut egraph);
        }
    }
    egraph
        .parse_and_run_program(
            None,
            r#"
(datatype HookedFixtureExpr (HookedLeaf HookedFixtureBase))
"#,
        )
        .unwrap();
    eggplant::egglog::prelude::run_ephemeral_rust_rule(
        &mut egraph,
        "seed_hooked_fixture_snapshot",
        &[],
        eggplant::egglog::ast::Facts(Vec::new()),
        |ctx, _| {
            let base = ctx.base_to_value(eggplant::egglog::sort::Boxed::new(HookedFixtureBase {
                n: 17,
            }));
            let _ = ctx.lookup("HookedLeaf", &[base]);
            Some(())
        },
    )
    .unwrap();
    build_persisted_snapshot_v1(&egraph, eggplant::egglog::SerializeConfig::default())
}

fn build_hooked_user_base_fixture_snapshot() -> PersistedSnapshot {
    let mut snapshot = build_hooked_user_base_snapshot();
    normalize_snapshot_for_fixture(&mut snapshot);
    snapshot
}

fn write_fixture(name: &str, snapshot: &PersistedSnapshot) {
    let path = fixture_path(name);
    fs::create_dir_all(path.parent().unwrap()).unwrap();
    fs::write(path, serde_json::to_string_pretty(snapshot).unwrap()).unwrap();
}

fn write_binary_fixture(name: &str, snapshot: &PersistedSnapshot) {
    let path = binary_fixture_path(name);
    fs::create_dir_all(path.parent().unwrap()).unwrap();
    snapshot.write_binary_file(path).unwrap();
}

fn read_fixture(name: &str) -> PersistedSnapshot {
    let path = fixture_path(name);
    let json = fs::read_to_string(path).unwrap();
    serde_json::from_str(&json).unwrap()
}

fn read_binary_fixture(name: &str) -> PersistedSnapshot {
    PersistedSnapshot::read_binary_file(binary_fixture_path(name)).unwrap()
}

fn row_count_by_op(snapshot: &PersistedSnapshot) -> std::collections::BTreeMap<usize, usize> {
    let mut counts = std::collections::BTreeMap::<usize, usize>::new();
    for row in &snapshot.state.function_rows {
        *counts.entry(row.op_id).or_default() += 1;
    }
    counts
}

fn fact_payloads(snapshot: &PersistedSnapshot) -> Vec<(usize, Vec<String>)> {
    let mut rows = snapshot
        .state
        .facts
        .iter()
        .map(|fact| {
            let payload = fact
                .inputs
                .iter()
                .map(|value| match value {
                    eggplant::artifact::PersistedSnapshotValue::Lit { value, .. } => {
                        value.value.clone()
                    }
                    eggplant::artifact::PersistedSnapshotValue::Ref { logical_id, .. } => {
                        logical_id.clone()
                    }
                })
                .collect::<Vec<_>>();
            (fact.op_id, payload)
        })
        .collect::<Vec<_>>();
    rows.sort();
    rows
}

#[test]
#[ignore = "helper for refreshing committed golden fixtures"]
fn regen_persisted_snapshot_golden_fixtures() {
    write_fixture("common_path", &build_common_path_fixture_snapshot());
    write_binary_fixture("common_path", &build_common_path_fixture_snapshot());
    write_fixture(
        "v2_eqclass_common_path",
        &build_common_path_v2_eqclass_fixture_snapshot(),
    );
    write_binary_fixture(
        "v2_eqclass_common_path",
        &build_common_path_v2_eqclass_fixture_snapshot(),
    );
    write_fixture(
        "v2_eqclass_union",
        &build_eqclass_union_v2_fixture_snapshot(),
    );
    write_binary_fixture(
        "v2_eqclass_union",
        &build_eqclass_union_v2_fixture_snapshot(),
    );
    write_fixture(
        "plain_source_non_goal",
        &build_plain_source_fixture_snapshot(),
    );
    write_binary_fixture(
        "plain_source_non_goal",
        &build_plain_source_fixture_snapshot(),
    );
    write_fixture(
        "hooked_user_base",
        &build_hooked_user_base_fixture_snapshot(),
    );
    write_binary_fixture(
        "hooked_user_base",
        &build_hooked_user_base_fixture_snapshot(),
    );
}

#[test]
fn persisted_snapshot_common_path_fixture_matches_current_export() {
    let _guard = fixture_test_guard();
    let current = build_common_path_fixture_snapshot();
    let fixture = read_fixture("common_path");
    assert_eq!(current, fixture);
}

#[test]
fn persisted_snapshot_common_path_binary_fixture_matches_current_export() {
    let _guard = fixture_test_guard();
    let current = build_common_path_fixture_snapshot();
    let fixture = read_binary_fixture("common_path");
    assert_eq!(current, fixture);
}

#[test]
fn persisted_snapshot_plain_source_fixture_matches_current_export() {
    let _guard = fixture_test_guard();
    let current = build_plain_source_fixture_snapshot();
    let fixture = read_fixture("plain_source_non_goal");
    assert_eq!(current, fixture);
}

#[test]
fn persisted_snapshot_plain_source_binary_fixture_matches_current_export() {
    let _guard = fixture_test_guard();
    let current = build_plain_source_fixture_snapshot();
    let fixture = read_binary_fixture("plain_source_non_goal");
    assert_eq!(current, fixture);
}

#[test]
fn persisted_snapshot_hooked_user_base_fixture_matches_current_export() {
    let _guard = fixture_test_guard();
    let current = build_hooked_user_base_fixture_snapshot();
    let fixture = read_fixture("hooked_user_base");
    assert_eq!(current, fixture);
}

#[test]
fn persisted_snapshot_hooked_user_base_binary_fixture_matches_current_export() {
    let _guard = fixture_test_guard();
    let current = build_hooked_user_base_fixture_snapshot();
    let fixture = read_binary_fixture("hooked_user_base");
    assert_eq!(current, fixture);
}

#[test]
fn persisted_snapshot_v2_common_path_fixture_matches_current_export() {
    let _guard = fixture_test_guard();
    let current = build_common_path_v2_eqclass_fixture_snapshot();
    let fixture = read_fixture("v2_eqclass_common_path");
    assert_eq!(current, fixture);
}

#[test]
fn persisted_snapshot_v2_common_path_binary_fixture_matches_current_export() {
    let _guard = fixture_test_guard();
    let current = build_common_path_v2_eqclass_fixture_snapshot();
    let fixture = read_binary_fixture("v2_eqclass_common_path");
    assert_eq!(current, fixture);
}

#[test]
fn persisted_snapshot_v2_eqclass_union_fixture_matches_current_export() {
    let _guard = fixture_test_guard();
    let current = build_eqclass_union_v2_fixture_snapshot();
    let fixture = read_fixture("v2_eqclass_union");
    assert_eq!(current, fixture);
}

#[test]
fn persisted_snapshot_v2_eqclass_union_binary_fixture_matches_current_export() {
    let _guard = fixture_test_guard();
    let current = build_eqclass_union_v2_fixture_snapshot();
    let fixture = read_binary_fixture("v2_eqclass_union");
    assert_eq!(current, fixture);
}

#[test]
fn persisted_snapshot_binary_fixtures_are_smaller_than_json_fixtures() {
    let _guard = fixture_test_guard();
    for name in ["common_path", "v2_eqclass_common_path", "hooked_user_base"] {
        let json_len = fs::read(fixture_path(name)).unwrap().len();
        let binary_len = fs::read(binary_fixture_path(name)).unwrap().len();
        assert!(
            binary_len < json_len,
            "{name}: expected binary fixture to be smaller than json fixture, got json={json_len} bytes binary={binary_len} bytes"
        );
    }
}

#[test]
fn persisted_snapshot_common_path_fixture_restore_is_compatible() {
    let _guard = fixture_test_guard();
    let current = build_common_path_snapshot();
    let mut fixture = read_fixture("common_path");
    fixture.source_schema = current.source_schema.clone();
    fixture.producer = current.producer.clone();

    FixtureTx::sgl().reset_for_bench();
    let report = {
        let mut egraph = FixtureTx::sgl().egraph.lock().unwrap();
        restore_persisted_snapshot_v1(&mut egraph, &fixture).unwrap()
    };
    let restored = {
        let egraph = FixtureTx::sgl().egraph.lock().unwrap();
        build_persisted_snapshot_v1(&egraph, eggplant::egglog::SerializeConfig::default())
    };

    assert_eq!(row_count_by_op(&restored), row_count_by_op(&fixture));
    assert_eq!(fact_payloads(&restored), fact_payloads(&fixture));
    assert_eq!(report.restored_facts, fixture.state.facts.len());
    assert_eq!(
        report.restored_function_rows,
        fixture.state.function_rows.len()
    );
}

#[test]
fn persisted_snapshot_hooked_user_base_fixture_restore_is_compatible() {
    let _guard = fixture_test_guard();
    let current = build_hooked_user_base_snapshot();
    let mut fixture = read_fixture("hooked_user_base");
    fixture.source_schema = current.source_schema.clone();
    fixture.producer = current.producer.clone();

    let mut egraph = eggplant::egglog::EGraph::default();
    for sort in inventory::iter::<eggplant::wrap::UserBaseSort> {
        if sort.name == "HookedFixtureBase" {
            (sort.sort_insert_fn)(&mut egraph);
        }
    }
    egraph
        .parse_and_run_program(
            None,
            r#"
(datatype HookedFixtureExpr (HookedLeaf HookedFixtureBase))
"#,
        )
        .unwrap();

    let report = restore_persisted_snapshot_v1(&mut egraph, &fixture).unwrap();
    let restored =
        build_persisted_snapshot_v1(&egraph, eggplant::egglog::SerializeConfig::default());

    assert_eq!(row_count_by_op(&restored), row_count_by_op(&fixture));
    assert_eq!(fact_payloads(&restored), fact_payloads(&fixture));
    assert_eq!(report.restored_facts, fixture.state.facts.len());
    assert_eq!(
        report.restored_function_rows,
        fixture.state.function_rows.len()
    );
}

#[test]
fn persisted_snapshot_v2_common_path_fixture_restore_is_compatible() {
    let _guard = fixture_test_guard();
    let current = build_common_path_v2_eqclass_snapshot();
    let mut fixture = read_fixture("v2_eqclass_common_path");
    fixture.source_schema = current.source_schema.clone();
    fixture.producer = current.producer.clone();

    FixtureTx::sgl().reset_for_bench();
    let report = {
        let mut egraph = FixtureTx::sgl().egraph.lock().unwrap();
        restore_persisted_snapshot_v1(&mut egraph, &fixture).unwrap()
    };
    let restored = {
        let egraph = FixtureTx::sgl().egraph.lock().unwrap();
        build_persisted_snapshot_v1(&egraph, eggplant::egglog::SerializeConfig::default())
    };

    assert_eq!(row_count_by_op(&restored), row_count_by_op(&fixture));
    assert_eq!(fact_payloads(&restored), fact_payloads(&fixture));
    assert_eq!(report.restored_facts, fixture.state.facts.len());
    assert_eq!(
        report.restored_function_rows,
        fixture.state.function_rows.len()
    );
}
