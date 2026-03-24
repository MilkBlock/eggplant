use std::collections::{BTreeMap, HashMap};

use egglog::{
    EGraph, EngineSchemaManifest as EgglogEngineSchemaManifest, SchemaFunctionKind, SchemaSortKind,
    SerializeConfig,
};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};

use crate::wrap::{DslFieldKind, DslVariantDecl, UserBaseSort};

pub const EGGPLANT_ARTIFACT_FORMAT_VERSION: u32 = 1;
pub const EGGPLANT_DSL_MACRO_REV: &str = "eggplant-dsl-schema-v1";
pub const EGGPLANT_PERSISTED_SNAPSHOT_FORMAT: &str = "eggplant.persisted-snapshot";
pub const EGGPLANT_PERSISTED_SNAPSHOT_PROFILE: &str = "eggplant-common-path-v1";
pub const EGGPLANT_PERSISTED_SNAPSHOT_VERSION: u32 = 1;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ArtifactChangeKind {
    Added,
    Removed,
    Changed,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ArtifactSchemaLayer {
    Artifact,
    EngineSort,
    EngineFunction,
    DslRuntime,
    DslMetadata,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ArtifactCompatibilityIssue {
    pub layer: ArtifactSchemaLayer,
    pub change: ArtifactChangeKind,
    pub key: String,
    pub blocking: bool,
    pub detail: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ArtifactCompatibilityReport {
    pub typed_continuation_allowed: bool,
    pub viewer_only_fallback_allowed: bool,
    pub engine_fingerprint_matches: bool,
    pub dsl_runtime_fingerprint_matches: bool,
    pub dsl_metadata_fingerprint_matches: bool,
    pub issues: Vec<ArtifactCompatibilityIssue>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ArtifactSortKind {
    Eq,
    Container,
    Base,
}

impl From<SchemaSortKind> for ArtifactSortKind {
    fn from(value: SchemaSortKind) -> Self {
        match value {
            SchemaSortKind::Eq => Self::Eq,
            SchemaSortKind::Container => Self::Container,
            SchemaSortKind::Base => Self::Base,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ArtifactFunctionKind {
    Constructor,
    Function,
}

impl From<SchemaFunctionKind> for ArtifactFunctionKind {
    fn from(value: SchemaFunctionKind) -> Self {
        match value {
            SchemaFunctionKind::Constructor => Self::Constructor,
            SchemaFunctionKind::Function => Self::Function,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct EngineSortSchema {
    pub name: String,
    pub kind: ArtifactSortKind,
    pub inner_sorts: Vec<String>,
    pub unionable: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct EngineFunctionSchema {
    pub key: String,
    pub name: String,
    pub kind: ArtifactFunctionKind,
    pub input: Vec<String>,
    pub output: String,
    pub merge: Option<String>,
    pub cost: Option<u64>,
    pub unextractable: bool,
    pub hidden: bool,
    pub let_binding: bool,
    pub term_constructor: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct EngineSchemaManifest {
    pub sorts: Vec<EngineSortSchema>,
    pub functions: Vec<EngineFunctionSchema>,
}

impl From<EgglogEngineSchemaManifest> for EngineSchemaManifest {
    fn from(value: EgglogEngineSchemaManifest) -> Self {
        Self {
            sorts: value
                .sorts
                .into_iter()
                .map(|sort| EngineSortSchema {
                    name: sort.name,
                    kind: sort.kind.into(),
                    inner_sorts: sort.inner_sorts,
                    unionable: sort.unionable,
                })
                .collect(),
            functions: value
                .functions
                .into_iter()
                .map(|function| EngineFunctionSchema {
                    key: function.key,
                    name: function.name,
                    kind: function.kind.into(),
                    input: function.input,
                    output: function.output,
                    merge: function.merge,
                    cost: function.cost,
                    unextractable: function.unextractable,
                    hidden: function.hidden,
                    let_binding: function.let_binding,
                    term_constructor: function.term_constructor,
                })
                .collect(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ArtifactDslFieldKind {
    Base,
    UserBase,
    Complex,
    Container,
}

impl From<DslFieldKind> for ArtifactDslFieldKind {
    fn from(value: DslFieldKind) -> Self {
        match value {
            DslFieldKind::Base => Self::Base,
            DslFieldKind::UserBase => Self::UserBase,
            DslFieldKind::Complex => Self::Complex,
            DslFieldKind::Container => Self::Container,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct DslFieldSchema {
    pub name: String,
    pub ty: String,
    pub kind: ArtifactDslFieldKind,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct DslVariantSchema {
    pub key: String,
    pub owner_ty: String,
    pub variant_name: String,
    pub fields: Vec<DslFieldSchema>,
    pub display_template: Option<String>,
    pub typst_template: Option<String>,
    pub precedence: u16,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct DslSchemaManifest {
    pub macro_rev: String,
    pub user_base_sorts: Vec<String>,
    pub variants: Vec<DslVariantSchema>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
struct DslRuntimeVariantSchema {
    pub key: String,
    pub field_tys: Vec<String>,
    pub field_kinds: Vec<ArtifactDslFieldKind>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
struct DslRuntimeManifest {
    pub macro_rev: String,
    pub variants: Vec<DslRuntimeVariantSchema>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct SerializedGraphPayload {
    pub egraph: serde_json::Value,
    pub truncated_functions: Vec<String>,
    pub discarded_functions: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct SerializedEggplantArtifact {
    pub format_version: u32,
    pub engine_schema: EngineSchemaManifest,
    pub dsl_schema: DslSchemaManifest,
    pub engine_fingerprint: String,
    pub dsl_runtime_fingerprint: String,
    pub dsl_metadata_fingerprint: String,
    pub typed_continuation_fingerprint: String,
    pub payload: SerializedGraphPayload,
}

pub fn current_engine_schema_manifest(egraph: &EGraph) -> EngineSchemaManifest {
    egraph.schema_manifest().into()
}

pub fn current_dsl_schema_manifest() -> DslSchemaManifest {
    let mut user_base_sorts = inventory::iter::<UserBaseSort>
        .into_iter()
        .map(|sort| sort.name.to_string())
        .collect::<Vec<_>>();
    user_base_sorts.sort();
    user_base_sorts.dedup();

    let mut variants = inventory::iter::<DslVariantDecl>
        .into_iter()
        .map(|variant| {
            let fields = variant
                .fields
                .iter()
                .map(|field| DslFieldSchema {
                    name: field.name.to_string(),
                    ty: field.ty.to_string(),
                    kind: field.kind.into(),
                })
                .collect::<Vec<_>>();
            let key = dsl_variant_key(variant.owner_ty, variant.variant_name, &fields);
            DslVariantSchema {
                key,
                owner_ty: variant.owner_ty.to_string(),
                variant_name: variant.variant_name.to_string(),
                fields,
                display_template: variant.display_template.map(str::to_string),
                typst_template: variant.typst_template.map(str::to_string),
                precedence: variant.precedence,
            }
        })
        .collect::<Vec<_>>();
    variants.sort_by(|a, b| a.key.cmp(&b.key));
    variants.dedup_by(|a, b| a.key == b.key);

    DslSchemaManifest {
        macro_rev: EGGPLANT_DSL_MACRO_REV.to_string(),
        user_base_sorts,
        variants,
    }
}

pub fn engine_schema_fingerprint(manifest: &EngineSchemaManifest) -> serde_json::Result<String> {
    canonical_hash(manifest)
}

pub fn dsl_runtime_fingerprint(manifest: &DslSchemaManifest) -> serde_json::Result<String> {
    canonical_hash(&dsl_runtime_manifest(manifest))
}

pub fn dsl_metadata_fingerprint(manifest: &DslSchemaManifest) -> serde_json::Result<String> {
    canonical_hash(manifest)
}

pub fn build_serialized_eggplant_artifact(
    egraph: &EGraph,
    config: SerializeConfig,
) -> serde_json::Result<SerializedEggplantArtifact> {
    let engine_schema = current_engine_schema_manifest(egraph);
    let dsl_schema = current_dsl_schema_manifest();
    let engine_fingerprint = engine_schema_fingerprint(&engine_schema)?;
    let dsl_runtime_fingerprint = dsl_runtime_fingerprint(&dsl_schema)?;
    let dsl_metadata_fingerprint = dsl_metadata_fingerprint(&dsl_schema)?;
    let typed_continuation_fingerprint =
        combined_fingerprint(&engine_fingerprint, &dsl_runtime_fingerprint)?;

    let serialized = egraph.serialize(config);
    let payload = SerializedGraphPayload {
        egraph: serde_json::to_value(&serialized.egraph)?,
        truncated_functions: serialized.truncated_functions,
        discarded_functions: serialized.discarded_functions,
    };

    Ok(SerializedEggplantArtifact {
        format_version: EGGPLANT_ARTIFACT_FORMAT_VERSION,
        engine_schema,
        dsl_schema,
        engine_fingerprint,
        dsl_runtime_fingerprint,
        dsl_metadata_fingerprint,
        typed_continuation_fingerprint,
        payload,
    })
}

pub fn compare_artifact_to_current(
    artifact: &SerializedEggplantArtifact,
    egraph: &EGraph,
) -> ArtifactCompatibilityReport {
    let current_engine = current_engine_schema_manifest(egraph);
    let current_dsl = current_dsl_schema_manifest();
    let current_runtime = dsl_runtime_manifest(&current_dsl);
    let artifact_runtime = dsl_runtime_manifest(&artifact.dsl_schema);

    let mut issues = Vec::new();
    if artifact.format_version != EGGPLANT_ARTIFACT_FORMAT_VERSION {
        issues.push(ArtifactCompatibilityIssue {
            layer: ArtifactSchemaLayer::Artifact,
            change: ArtifactChangeKind::Changed,
            key: "format_version".to_string(),
            blocking: true,
            detail: format!(
                "artifact format {} does not match current {}",
                artifact.format_version, EGGPLANT_ARTIFACT_FORMAT_VERSION
            ),
        });
    }

    compare_named_items(
        &mut issues,
        ArtifactSchemaLayer::EngineSort,
        true,
        map_by_key(
            current_engine
                .sorts
                .iter()
                .map(|sort| (sort.name.clone(), sort)),
        ),
        map_by_key(
            artifact
                .engine_schema
                .sorts
                .iter()
                .map(|sort| (sort.name.clone(), sort)),
        ),
    );
    compare_named_items(
        &mut issues,
        ArtifactSchemaLayer::EngineFunction,
        true,
        map_by_key(
            current_engine
                .functions
                .iter()
                .map(|function| (function.key.clone(), function)),
        ),
        map_by_key(
            artifact
                .engine_schema
                .functions
                .iter()
                .map(|function| (function.key.clone(), function)),
        ),
    );
    compare_named_items(
        &mut issues,
        ArtifactSchemaLayer::DslRuntime,
        true,
        map_by_key(
            current_runtime
                .variants
                .iter()
                .map(|variant| (variant.key.clone(), variant)),
        ),
        map_by_key(
            artifact_runtime
                .variants
                .iter()
                .map(|variant| (variant.key.clone(), variant)),
        ),
    );
    compare_named_items(
        &mut issues,
        ArtifactSchemaLayer::DslMetadata,
        false,
        map_by_key(
            current_dsl
                .variants
                .iter()
                .map(|variant| (variant.key.clone(), variant)),
        ),
        map_by_key(
            artifact
                .dsl_schema
                .variants
                .iter()
                .map(|variant| (variant.key.clone(), variant)),
        ),
    );

    let engine_fingerprint_matches = engine_schema_fingerprint(&current_engine).ok()
        == Some(artifact.engine_fingerprint.clone());
    let dsl_runtime_fingerprint_matches = dsl_runtime_fingerprint(&current_dsl).ok()
        == Some(artifact.dsl_runtime_fingerprint.clone());
    let dsl_metadata_fingerprint_matches = dsl_metadata_fingerprint(&current_dsl).ok()
        == Some(artifact.dsl_metadata_fingerprint.clone());
    let typed_continuation_allowed = issues.iter().all(|issue| !issue.blocking);

    ArtifactCompatibilityReport {
        typed_continuation_allowed,
        viewer_only_fallback_allowed: artifact.format_version == EGGPLANT_ARTIFACT_FORMAT_VERSION,
        engine_fingerprint_matches,
        dsl_runtime_fingerprint_matches,
        dsl_metadata_fingerprint_matches,
        issues,
    }
}

fn dsl_variant_key(owner_ty: &str, variant_name: &str, fields: &[DslFieldSchema]) -> String {
    let ordered_tys = fields
        .iter()
        .map(|field| field.ty.clone())
        .collect::<Vec<_>>()
        .join(",");
    format!("{owner_ty}::{variant_name}({ordered_tys})->{owner_ty}")
}

fn dsl_runtime_manifest(manifest: &DslSchemaManifest) -> DslRuntimeManifest {
    DslRuntimeManifest {
        macro_rev: manifest.macro_rev.clone(),
        variants: manifest
            .variants
            .iter()
            .map(|variant| DslRuntimeVariantSchema {
                key: variant.key.clone(),
                field_tys: variant
                    .fields
                    .iter()
                    .map(|field| field.ty.clone())
                    .collect(),
                field_kinds: variant.fields.iter().map(|field| field.kind).collect(),
            })
            .collect(),
    }
}

fn combined_fingerprint(lhs: &str, rhs: &str) -> serde_json::Result<String> {
    canonical_hash(&(lhs, rhs))
}

fn canonical_hash<T: Serialize>(value: &T) -> serde_json::Result<String> {
    let bytes = serde_json::to_vec(value)?;
    let digest = Sha256::digest(bytes);
    Ok(digest.iter().map(|byte| format!("{byte:02x}")).collect())
}

fn map_by_key<'a, T>(iter: impl Iterator<Item = (String, &'a T)>) -> BTreeMap<String, &'a T> {
    iter.collect()
}

fn compare_named_items<T: Serialize + PartialEq>(
    issues: &mut Vec<ArtifactCompatibilityIssue>,
    layer: ArtifactSchemaLayer,
    blocking: bool,
    current: BTreeMap<String, &T>,
    artifact: BTreeMap<String, &T>,
) {
    for (key, current_value) in &current {
        match artifact.get(key) {
            None => issues.push(ArtifactCompatibilityIssue {
                layer,
                change: ArtifactChangeKind::Added,
                key: key.clone(),
                blocking,
                detail: "present in current schema but missing from artifact".to_string(),
            }),
            Some(artifact_value) if *artifact_value != *current_value => {
                issues.push(ArtifactCompatibilityIssue {
                    layer,
                    change: ArtifactChangeKind::Changed,
                    key: key.clone(),
                    blocking,
                    detail: format!(
                        "artifact={} current={}",
                        serde_json::to_string(artifact_value).expect("schema must serialize"),
                        serde_json::to_string(current_value).expect("schema must serialize"),
                    ),
                });
            }
            Some(_) => {}
        }
    }

    for key in artifact.keys() {
        if !current.contains_key(key) {
            issues.push(ArtifactCompatibilityIssue {
                layer,
                change: ArtifactChangeKind::Removed,
                key: key.clone(),
                blocking,
                detail: "present in artifact schema but missing from current schema".to_string(),
            });
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct PersistedSnapshotProducer {
    #[serde(rename = "crate")]
    pub crate_name: String,
    pub version: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct PersistedSnapshotDictionary {
    pub strings: Vec<String>,
    pub symbols: Vec<String>,
    pub sorts: Vec<String>,
    pub ops: Vec<String>,
    pub rulesets: Vec<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum PersistedSnapshotSortKind {
    Eqsort,
    Container,
    Primitive,
}

impl From<ArtifactSortKind> for PersistedSnapshotSortKind {
    fn from(value: ArtifactSortKind) -> Self {
        match value {
            ArtifactSortKind::Eq => Self::Eqsort,
            ArtifactSortKind::Container => Self::Container,
            ArtifactSortKind::Base => Self::Primitive,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct PersistedSnapshotSortDecl {
    pub sort_id: usize,
    pub name: String,
    pub kind: PersistedSnapshotSortKind,
    pub metadata: Option<serde_json::Value>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct PersistedSnapshotFunctionDecl {
    pub op_id: usize,
    pub name: String,
    pub input_sort_ids: Vec<usize>,
    pub output_sort_id: usize,
    pub is_relation: bool,
    pub merge: Option<String>,
    pub cost: Option<u64>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct PersistedSnapshotRulesetDecl {
    pub ruleset_id: usize,
    pub name: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct PersistedSnapshotSchema {
    pub sort_decls: Vec<PersistedSnapshotSortDecl>,
    pub function_decls: Vec<PersistedSnapshotFunctionDecl>,
    pub constructor_decls: Vec<PersistedSnapshotFunctionDecl>,
    pub ruleset_decls: Vec<PersistedSnapshotRulesetDecl>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct PersistedSnapshotLiteralValue {
    pub tag: String,
    pub value: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum PersistedSnapshotValue {
    Lit {
        sort_id: usize,
        value: PersistedSnapshotLiteralValue,
    },
    Ref {
        sort_id: usize,
        logical_id: String,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct PersistedSnapshotFact {
    pub op_id: usize,
    pub inputs: Vec<PersistedSnapshotValue>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct PersistedSnapshotFunctionRow {
    pub op_id: usize,
    pub inputs: Vec<PersistedSnapshotValue>,
    pub output: PersistedSnapshotValue,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct PersistedSnapshotUnion {
    pub sort_id: usize,
    pub lhs: PersistedSnapshotValue,
    pub rhs: PersistedSnapshotValue,
    pub reason: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct PersistedSnapshotRun {
    pub ruleset_id: usize,
    pub sequence_no: usize,
    pub until: Option<String>,
    pub node_limit: Option<usize>,
    pub time_limit_ms: Option<u64>,
}

#[derive(Debug, Clone, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct PersistedSnapshotState {
    pub facts: Vec<PersistedSnapshotFact>,
    pub function_rows: Vec<PersistedSnapshotFunctionRow>,
    pub unions: Vec<PersistedSnapshotUnion>,
    pub runs: Vec<PersistedSnapshotRun>,
    pub fresh_id_cursor: Option<u64>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct PersistedSnapshotValueId {
    pub logical_id: String,
    pub sort_id: usize,
    pub debug_value: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct PersistedSnapshotRestoreMapping {
    pub value_ids: Vec<PersistedSnapshotValueId>,
    pub notes: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct PersistedSnapshotDiagnostic {
    pub code: String,
    pub message: String,
    pub path: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct PersistedSnapshot {
    pub snapshot_version: u32,
    pub format: String,
    pub profile: String,
    pub producer: Option<PersistedSnapshotProducer>,
    pub dictionary: PersistedSnapshotDictionary,
    pub schema: PersistedSnapshotSchema,
    pub state: PersistedSnapshotState,
    pub restore_mapping: PersistedSnapshotRestoreMapping,
    pub diagnostics: Vec<PersistedSnapshotDiagnostic>,
}

pub fn build_persisted_snapshot_v1(egraph: &EGraph, config: SerializeConfig) -> PersistedSnapshot {
    let engine_schema = current_engine_schema_manifest(egraph);
    let raw_rows = egraph.serialize_raw(config);
    let mut dictionary = PersistedSnapshotDictionary::default();
    let mut sort_ids = HashMap::new();
    let mut op_ids = HashMap::new();
    let mut diagnostics = Vec::new();

    let sort_decls = engine_schema
        .sorts
        .iter()
        .enumerate()
        .map(|(sort_id, sort)| {
            sort_ids.insert(sort.name.clone(), sort_id);
            dictionary.sorts.push(sort.name.clone());
            PersistedSnapshotSortDecl {
                sort_id,
                name: sort.name.clone(),
                kind: sort.kind.into(),
                metadata: None,
            }
        })
        .collect::<Vec<_>>();

    let mut function_decls = Vec::new();
    let mut constructor_decls = Vec::new();
    let mut value_ids = Vec::new();
    let mut logical_value_ids = HashMap::<String, String>::new();
    let mut facts = Vec::new();
    let mut function_rows = Vec::new();

    for function in &engine_schema.functions {
        let op_id = dictionary.ops.len();
        dictionary.ops.push(function.name.clone());
        op_ids.insert(function.key.clone(), op_id);

        let input_sort_ids = function
            .input
            .iter()
            .map(|sort_name| sort_ids[sort_name])
            .collect::<Vec<_>>();
        let output_sort_id = sort_ids[&function.output];
        let decl = PersistedSnapshotFunctionDecl {
            op_id,
            name: function.name.clone(),
            input_sort_ids: input_sort_ids.clone(),
            output_sort_id,
            is_relation: is_relation_decl(function),
            merge: function.merge.clone(),
            cost: function.cost,
        };

        match function.kind {
            ArtifactFunctionKind::Constructor => constructor_decls.push(decl.clone()),
            ArtifactFunctionKind::Function => function_decls.push(decl.clone()),
        }

        let Some(rows) = raw_rows.get(&function.name) else {
            continue;
        };

        for row in rows.iter().filter(|row| !row.subsumed) {
            let inputs = function
                .input
                .iter()
                .zip(&row.inputs_complex)
                .map(|(sort_name, value)| {
                    persisted_snapshot_value(
                        egraph,
                        &sort_ids,
                        sort_name,
                        *value,
                        &mut logical_value_ids,
                        &mut value_ids,
                    )
                })
                .collect::<Vec<_>>();

            if decl.is_relation {
                facts.push(PersistedSnapshotFact { op_id, inputs });
                continue;
            }

            let output = persisted_snapshot_value(
                egraph,
                &sort_ids,
                &function.output,
                row.output,
                &mut logical_value_ids,
                &mut value_ids,
            );
            function_rows.push(PersistedSnapshotFunctionRow {
                op_id,
                inputs,
                output,
            });
        }
    }

    diagnostics.push(PersistedSnapshotDiagnostic {
        code: "unsupported-feature".to_string(),
        message: "v1 exporter does not yet persist union history; state.unions is empty"
            .to_string(),
        path: Some("state.unions".to_string()),
    });
    diagnostics.push(PersistedSnapshotDiagnostic {
        code: "unsupported-feature".to_string(),
        message: "v1 exporter does not yet persist run scheduling history; state.runs is empty"
            .to_string(),
        path: Some("state.runs".to_string()),
    });
    diagnostics.push(PersistedSnapshotDiagnostic {
        code: "unsupported-feature".to_string(),
        message: "v1 exporter does not yet persist named ruleset declarations; schema.ruleset_decls is empty"
            .to_string(),
        path: Some("schema.ruleset_decls".to_string()),
    });

    PersistedSnapshot {
        snapshot_version: EGGPLANT_PERSISTED_SNAPSHOT_VERSION,
        format: EGGPLANT_PERSISTED_SNAPSHOT_FORMAT.to_string(),
        profile: EGGPLANT_PERSISTED_SNAPSHOT_PROFILE.to_string(),
        producer: Some(PersistedSnapshotProducer {
            crate_name: env!("CARGO_PKG_NAME").to_string(),
            version: env!("CARGO_PKG_VERSION").to_string(),
        }),
        dictionary,
        schema: PersistedSnapshotSchema {
            sort_decls,
            function_decls,
            constructor_decls,
            ruleset_decls: Vec::new(),
        },
        state: PersistedSnapshotState {
            facts,
            function_rows,
            unions: Vec::new(),
            runs: Vec::new(),
            fresh_id_cursor: None,
        },
        restore_mapping: PersistedSnapshotRestoreMapping {
            value_ids,
            notes: vec![
                "logical_id values are snapshot-local and must be remapped during import".to_string(),
                "runtime ids, hashes, and class ids are intentionally excluded from persisted semantic state"
                    .to_string(),
            ],
        },
        diagnostics,
    }
}

fn is_relation_decl(function: &EngineFunctionSchema) -> bool {
    function.output == "Unit" || function.output == "()"
}

fn persisted_snapshot_value(
    egraph: &EGraph,
    sort_ids: &HashMap<String, usize>,
    sort_name: &str,
    value: egglog::Value,
    logical_value_ids: &mut HashMap<String, String>,
    value_ids: &mut Vec<PersistedSnapshotValueId>,
) -> PersistedSnapshotValue {
    let sort = egraph
        .get_sort_by_name(sort_name)
        .unwrap_or_else(|| panic!("missing sort {sort_name} while exporting snapshot"));
    let sort_id = sort_ids[sort_name];

    if !sort.is_eq_sort() {
        return PersistedSnapshotValue::Lit {
            sort_id,
            value: PersistedSnapshotLiteralValue {
                tag: sort_name.to_string(),
                value: egraph.base_value_print(value, sort),
            },
        };
    }

    let canonical = egraph.get_canonical_value(value, sort);
    let logical_key = format!("{sort_name}:{canonical:?}");
    let logical_id = logical_value_ids
        .entry(logical_key.clone())
        .or_insert_with(|| {
            let logical_id = format!("v{}", value_ids.len());
            let debug_value = egraph
                .extract_value_to_string(sort, canonical)
                .ok()
                .map(|(rendered, _)| rendered);
            value_ids.push(PersistedSnapshotValueId {
                logical_id: logical_id.clone(),
                sort_id,
                debug_value,
            });
            logical_id
        })
        .clone();

    PersistedSnapshotValue::Ref {
        sort_id,
        logical_id,
    }
}
