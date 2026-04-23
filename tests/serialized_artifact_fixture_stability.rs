#![cfg(feature = "fork-egglog")]

use std::fs;
use std::path::PathBuf;
use std::sync::{Mutex, MutexGuard, OnceLock};

use eggplant::artifact::{
    SerializedEggplantArtifact, build_serialized_eggplant_artifact, compare_artifact_to_current,
};
use eggplant::prelude::*;
use eggplant::tx_rx_vt_pr;

#[eggplant::dsl]
enum ArtifactFixtureExpr {
    Const { num: i64 },
}

#[eggplant::dsl]
enum ArtifactFixtureRoot {
    Root { node: ArtifactFixtureExpr },
}

#[eggplant::relation]
struct ArtifactFixtureEdge {
    src: i64,
    dst: i64,
}

tx_rx_vt_pr!(ArtifactFixtureTx, ArtifactFixturePatRec);

fn fixture_test_guard() -> MutexGuard<'static, ()> {
    static LOCK: OnceLock<Mutex<()>> = OnceLock::new();
    LOCK.get_or_init(|| Mutex::new(()))
        .lock()
        .unwrap_or_else(|err| err.into_inner())
}

fn json_fixture_path(name: &str) -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("fixtures")
        .join("serialized_artifact_v1")
        .join(format!("{name}.json"))
}

fn binary_fixture_path(name: &str) -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("fixtures")
        .join("serialized_artifact_v1")
        .join(format!("{name}.egbin"))
}

fn build_common_path_artifact() -> SerializedEggplantArtifact {
    ArtifactFixtureTx::sgl().reset_for_bench();
    let root_a = Root::<ArtifactFixtureTx>::new(&Const::new(7));
    let root_b = Root::<ArtifactFixtureTx>::new(&Const::new(9));
    root_a.commit();
    root_b.commit();
    ArtifactFixtureEdge::<ArtifactFixtureTx>::insert(1, 2);
    let egraph = ArtifactFixtureTx::sgl().egraph.lock().unwrap();
    build_serialized_eggplant_artifact(&egraph, eggplant::egglog::SerializeConfig::default())
        .unwrap()
}

fn write_json_fixture(name: &str, artifact: &SerializedEggplantArtifact) {
    let path = json_fixture_path(name);
    fs::create_dir_all(path.parent().unwrap()).unwrap();
    fs::write(path, serde_json::to_string_pretty(artifact).unwrap()).unwrap();
}

fn write_binary_fixture(name: &str, artifact: &SerializedEggplantArtifact) {
    let path = binary_fixture_path(name);
    fs::create_dir_all(path.parent().unwrap()).unwrap();
    artifact.write_binary_file(path).unwrap();
}

fn read_json_fixture(name: &str) -> SerializedEggplantArtifact {
    serde_json::from_str(&fs::read_to_string(json_fixture_path(name)).unwrap()).unwrap()
}

fn read_binary_fixture(name: &str) -> SerializedEggplantArtifact {
    SerializedEggplantArtifact::read_binary_file(binary_fixture_path(name)).unwrap()
}

#[test]
#[ignore = "helper for refreshing committed golden fixtures"]
fn regen_serialized_artifact_golden_fixtures() {
    let artifact = build_common_path_artifact();
    write_json_fixture("common_path", &artifact);
    write_binary_fixture("common_path", &artifact);
}

#[test]
fn serialized_artifact_common_path_json_fixture_matches_current_export() {
    let _guard = fixture_test_guard();
    let current = build_common_path_artifact();
    let fixture = read_json_fixture("common_path");
    assert_eq!(current, fixture);
}

#[test]
fn serialized_artifact_common_path_binary_fixture_matches_current_export() {
    let _guard = fixture_test_guard();
    let current = build_common_path_artifact();
    let fixture = read_binary_fixture("common_path");
    assert_eq!(current, fixture);
}

#[test]
fn serialized_artifact_common_path_binary_fixture_is_continuation_compatible() {
    let _guard = fixture_test_guard();
    let fixture = read_binary_fixture("common_path");

    let report = {
        let egraph = ArtifactFixtureTx::sgl().egraph.lock().unwrap();
        compare_artifact_to_current(&fixture, &egraph)
    };

    assert!(report.typed_continuation_allowed);
    assert!(report.engine_fingerprint_matches);
    assert!(report.dsl_runtime_fingerprint_matches);
    assert!(report.dsl_metadata_fingerprint_matches);
}

#[test]
fn serialized_artifact_binary_fixture_is_smaller_than_json_fixture() {
    let _guard = fixture_test_guard();
    let json_len = fs::read(json_fixture_path("common_path")).unwrap().len();
    let binary_len = fs::read(binary_fixture_path("common_path")).unwrap().len();
    assert!(
        binary_len < json_len,
        "common_path artifact: expected binary fixture to be smaller than json fixture, got json={json_len} bytes binary={binary_len} bytes"
    );
}
