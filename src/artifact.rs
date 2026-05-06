use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::fmt::{Display, Formatter};
use std::path::Path;
use std::sync::{Arc, Mutex};

use egglog::{EGraph, SerializeConfig};
use serde::{Deserialize, Serialize, de::DeserializeOwned};
use serde_json::{Value as JsonValue, json};
use sha2::{Digest, Sha256};

use crate::wrap::{
    Decl, DslFieldKind, DslVariantDecl, EgglogCompatExt,
    EngineSchemaManifest as EgglogEngineSchemaManifest, PersistedSnapshotUserBaseSortSupport,
    SchemaFunctionKind, SchemaSortKind, UserBaseSort, run_ephemeral_rust_rule,
    user_base_sort_restore_hook, user_base_sort_restore_support,
};

pub const EGGPLANT_ARTIFACT_FORMAT_VERSION: u32 = 1;
pub const EGGPLANT_SERIALIZED_ARTIFACT_FORMAT: &str = "eggplant.serialized-artifact";
pub const EGGPLANT_DSL_MACRO_REV: &str = "eggplant-dsl-schema-v1";
pub const EGGPLANT_PERSISTED_SNAPSHOT_FORMAT: &str = "eggplant.persisted-snapshot";
pub const EGGPLANT_PERSISTED_SNAPSHOT_PROFILE: &str = "eggplant-common-path-v1";
pub const EGGPLANT_PERSISTED_SNAPSHOT_VERSION: u32 = 1;
pub const EGGPLANT_PERSISTED_SNAPSHOT_V2_EQCLASS_PROFILE: &str = "eggplant-eqclass-aware-v2";
pub const EGGPLANT_PERSISTED_SNAPSHOT_V2_EQCLASS_VERSION: u32 = 2;
pub const EGGPLANT_BINARY_CODEC_MAGIC: [u8; 8] = *b"EGBIN001";
pub const EGGPLANT_BINARY_CODEC_VERSION: u32 = 1;

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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum BinaryArtifactPayloadKind {
    SerializedEggplantArtifact,
    PersistedSnapshot,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum BinaryArtifactPayloadCodec {
    MessagePack,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum BinaryArtifactCompatibilityPolicy {
    ArtifactTypedContinuationOrViewerFallback,
    PersistedSnapshotSourceSchemaAwareRestore,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct BinaryArtifactHeader {
    pub magic: [u8; 8],
    pub envelope_version: u32,
    pub payload_kind: BinaryArtifactPayloadKind,
    pub payload_codec: BinaryArtifactPayloadCodec,
    pub payload_format: String,
    pub payload_version: u32,
    pub payload_profile: Option<String>,
    pub compatibility_policy: BinaryArtifactCompatibilityPolicy,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinaryArtifactCodecError {
    Encode(String),
    Decode(String),
    MagicMismatch {
        expected: [u8; 8],
        actual: [u8; 8],
    },
    VersionMismatch {
        expected: u32,
        actual: u32,
    },
    PayloadKindMismatch {
        expected: BinaryArtifactPayloadKind,
        actual: BinaryArtifactPayloadKind,
    },
    HeaderPayloadMismatch(String),
}

impl Display for BinaryArtifactCodecError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryArtifactCodecError::Encode(err) => {
                write!(f, "failed to encode binary artifact payload: {err}")
            }
            BinaryArtifactCodecError::Decode(err) => {
                write!(f, "failed to decode binary artifact payload: {err}")
            }
            BinaryArtifactCodecError::MagicMismatch { expected, actual } => write!(
                f,
                "binary artifact magic mismatch: expected {:?}, got {:?}",
                std::str::from_utf8(expected).unwrap_or("<non-utf8>"),
                std::str::from_utf8(actual).unwrap_or("<non-utf8>")
            ),
            BinaryArtifactCodecError::VersionMismatch { expected, actual } => write!(
                f,
                "binary artifact codec version mismatch: expected {expected}, got {actual}"
            ),
            BinaryArtifactCodecError::PayloadKindMismatch { expected, actual } => write!(
                f,
                "binary artifact payload kind mismatch: expected {expected:?}, got {actual:?}"
            ),
            BinaryArtifactCodecError::HeaderPayloadMismatch(detail) => {
                write!(f, "binary artifact header/payload mismatch: {detail}")
            }
        }
    }
}

impl std::error::Error for BinaryArtifactCodecError {}

#[derive(Debug)]
pub enum BinaryArtifactIoError {
    Io(std::io::Error),
    Codec(BinaryArtifactCodecError),
}

impl Display for BinaryArtifactIoError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryArtifactIoError::Io(err) => write!(f, "binary artifact file io failed: {err}"),
            BinaryArtifactIoError::Codec(err) => Display::fmt(err, f),
        }
    }
}

impl std::error::Error for BinaryArtifactIoError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            BinaryArtifactIoError::Io(err) => Some(err),
            BinaryArtifactIoError::Codec(err) => Some(err),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
struct BinaryArtifactEnvelope {
    header: BinaryArtifactHeader,
    payload_bytes: Vec<u8>,
}

impl SerializedEggplantArtifact {
    pub fn binary_header(&self) -> BinaryArtifactHeader {
        BinaryArtifactHeader {
            magic: EGGPLANT_BINARY_CODEC_MAGIC,
            envelope_version: EGGPLANT_BINARY_CODEC_VERSION,
            payload_kind: BinaryArtifactPayloadKind::SerializedEggplantArtifact,
            payload_codec: BinaryArtifactPayloadCodec::MessagePack,
            payload_format: EGGPLANT_SERIALIZED_ARTIFACT_FORMAT.to_string(),
            payload_version: self.format_version,
            payload_profile: None,
            compatibility_policy:
                BinaryArtifactCompatibilityPolicy::ArtifactTypedContinuationOrViewerFallback,
        }
    }

    pub fn to_binary_vec(&self) -> Result<Vec<u8>, BinaryArtifactCodecError> {
        encode_binary_payload(self)
    }

    pub fn from_binary_slice(bytes: &[u8]) -> Result<Self, BinaryArtifactCodecError> {
        decode_binary_payload(bytes, BinaryArtifactPayloadKind::SerializedEggplantArtifact)
    }

    pub fn write_binary_file(&self, path: impl AsRef<Path>) -> Result<(), BinaryArtifactIoError> {
        write_binary_file(path, self)
    }

    pub fn read_binary_file(path: impl AsRef<Path>) -> Result<Self, BinaryArtifactIoError> {
        read_binary_file(path)
    }
}

impl PersistedSnapshot {
    pub fn binary_header(&self) -> BinaryArtifactHeader {
        BinaryArtifactHeader {
            magic: EGGPLANT_BINARY_CODEC_MAGIC,
            envelope_version: EGGPLANT_BINARY_CODEC_VERSION,
            payload_kind: BinaryArtifactPayloadKind::PersistedSnapshot,
            payload_codec: BinaryArtifactPayloadCodec::MessagePack,
            payload_format: self.format.clone(),
            payload_version: self.snapshot_version,
            payload_profile: Some(self.profile.clone()),
            compatibility_policy:
                BinaryArtifactCompatibilityPolicy::PersistedSnapshotSourceSchemaAwareRestore,
        }
    }

    pub fn to_binary_vec(&self) -> Result<Vec<u8>, BinaryArtifactCodecError> {
        encode_binary_payload(self)
    }

    pub fn from_binary_slice(bytes: &[u8]) -> Result<Self, BinaryArtifactCodecError> {
        decode_binary_payload(bytes, BinaryArtifactPayloadKind::PersistedSnapshot)
    }

    pub fn write_binary_file(&self, path: impl AsRef<Path>) -> Result<(), BinaryArtifactIoError> {
        write_binary_file(path, self)
    }

    pub fn read_binary_file(path: impl AsRef<Path>) -> Result<Self, BinaryArtifactIoError> {
        read_binary_file(path)
    }
}

pub fn read_binary_artifact_header(
    bytes: &[u8],
) -> Result<BinaryArtifactHeader, BinaryArtifactCodecError> {
    let envelope = rmp_serde::from_slice::<BinaryArtifactEnvelope>(bytes)
        .map_err(|err| BinaryArtifactCodecError::Decode(err.to_string()))?;
    Ok(envelope.header)
}

pub fn read_binary_artifact_header_from_file(
    path: impl AsRef<Path>,
) -> Result<BinaryArtifactHeader, BinaryArtifactIoError> {
    let bytes = std::fs::read(path).map_err(BinaryArtifactIoError::Io)?;
    read_binary_artifact_header(&bytes).map_err(BinaryArtifactIoError::Codec)
}

trait BinaryArtifactPayloadSpec {
    fn binary_header(&self) -> BinaryArtifactHeader;
    fn payload_kind() -> BinaryArtifactPayloadKind
    where
        Self: Sized;
}

impl BinaryArtifactPayloadSpec for SerializedEggplantArtifact {
    fn binary_header(&self) -> BinaryArtifactHeader {
        SerializedEggplantArtifact::binary_header(self)
    }

    fn payload_kind() -> BinaryArtifactPayloadKind {
        BinaryArtifactPayloadKind::SerializedEggplantArtifact
    }
}

impl BinaryArtifactPayloadSpec for PersistedSnapshot {
    fn binary_header(&self) -> BinaryArtifactHeader {
        PersistedSnapshot::binary_header(self)
    }

    fn payload_kind() -> BinaryArtifactPayloadKind {
        BinaryArtifactPayloadKind::PersistedSnapshot
    }
}

fn encode_binary_payload<T: Serialize + BinaryArtifactPayloadSpec>(
    payload: &T,
) -> Result<Vec<u8>, BinaryArtifactCodecError> {
    let payload_bytes = rmp_serde::to_vec_named(payload)
        .map_err(|err| BinaryArtifactCodecError::Encode(err.to_string()))?;
    rmp_serde::to_vec_named(&BinaryArtifactEnvelope {
        header: payload.binary_header(),
        payload_bytes,
    })
    .map_err(|err| BinaryArtifactCodecError::Encode(err.to_string()))
}

fn decode_binary_payload<T: DeserializeOwned + BinaryArtifactPayloadSpec>(
    bytes: &[u8],
    expected_kind: BinaryArtifactPayloadKind,
) -> Result<T, BinaryArtifactCodecError> {
    let envelope = rmp_serde::from_slice::<BinaryArtifactEnvelope>(bytes)
        .map_err(|err| BinaryArtifactCodecError::Decode(err.to_string()))?;
    if envelope.header.magic != EGGPLANT_BINARY_CODEC_MAGIC {
        return Err(BinaryArtifactCodecError::MagicMismatch {
            expected: EGGPLANT_BINARY_CODEC_MAGIC,
            actual: envelope.header.magic,
        });
    }
    if envelope.header.envelope_version != EGGPLANT_BINARY_CODEC_VERSION {
        return Err(BinaryArtifactCodecError::VersionMismatch {
            expected: EGGPLANT_BINARY_CODEC_VERSION,
            actual: envelope.header.envelope_version,
        });
    }
    if envelope.header.payload_kind != expected_kind {
        return Err(BinaryArtifactCodecError::PayloadKindMismatch {
            expected: expected_kind,
            actual: envelope.header.payload_kind,
        });
    }
    let payload: T = rmp_serde::from_slice(&envelope.payload_bytes)
        .map_err(|err| BinaryArtifactCodecError::Decode(err.to_string()))?;
    let expected_header = payload.binary_header();
    if envelope.header != expected_header {
        return Err(BinaryArtifactCodecError::HeaderPayloadMismatch(format!(
            "expected {:?}, got {:?}",
            expected_header, envelope.header
        )));
    }
    Ok(payload)
}

fn write_binary_file<T: Serialize + BinaryArtifactPayloadSpec>(
    path: impl AsRef<Path>,
    payload: &T,
) -> Result<(), BinaryArtifactIoError> {
    let bytes = encode_binary_payload(payload).map_err(BinaryArtifactIoError::Codec)?;
    std::fs::write(path, bytes).map_err(BinaryArtifactIoError::Io)
}

fn read_binary_file<T: DeserializeOwned + BinaryArtifactPayloadSpec>(
    path: impl AsRef<Path>,
) -> Result<T, BinaryArtifactIoError> {
    let bytes = std::fs::read(path).map_err(BinaryArtifactIoError::Io)?;
    decode_binary_payload(&bytes, T::payload_kind()).map_err(BinaryArtifactIoError::Codec)
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

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct PersistedSnapshotSourceSchema {
    pub engine_fingerprint: String,
    pub dsl_runtime_fingerprint: String,
    pub dsl_metadata_fingerprint: String,
    pub macro_rev: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum PersistedSnapshotAlignmentLayer {
    Header,
    Engine,
    DslRuntime,
    DslMetadata,
    MacroRev,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct PersistedSnapshotAlignmentIssue {
    pub layer: PersistedSnapshotAlignmentLayer,
    pub blocking: bool,
    pub detail: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PersistedSnapshotAlignmentReport {
    pub has_alignment_header: bool,
    pub exact_source_match: bool,
    pub restore_schema_compatible: bool,
    pub engine_fingerprint_matches: bool,
    pub dsl_runtime_fingerprint_matches: bool,
    pub dsl_metadata_fingerprint_matches: bool,
    pub macro_rev_matches: bool,
    pub issues: Vec<PersistedSnapshotAlignmentIssue>,
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
    pub metadata: Option<serde_json::Value>,
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
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub machine_value: Option<JsonValue>,
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PersistedSnapshotRestoreReport {
    pub restored_facts: usize,
    pub restored_function_rows: usize,
    pub resolved_logical_values: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PersistedSnapshotRestoreError {
    UnsupportedFormat {
        expected: String,
        actual: String,
    },
    UnsupportedProfile {
        expected: String,
        actual: String,
    },
    UnsupportedVersion {
        expected: u32,
        actual: u32,
    },
    MissingSort(String),
    MissingFunction(String),
    SchemaMismatch(String),
    UnsupportedSnapshotFeature(String),
    TargetNotFresh(String),
    UnsupportedLiteral {
        sort: String,
        value: String,
    },
    UnresolvedRows {
        pending_facts: usize,
        pending_function_rows: usize,
    },
    Runtime(String),
}

impl Display for PersistedSnapshotRestoreError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            PersistedSnapshotRestoreError::UnsupportedFormat { expected, actual } => {
                write!(
                    f,
                    "unsupported snapshot format: expected `{expected}`, got `{actual}`"
                )
            }
            PersistedSnapshotRestoreError::UnsupportedProfile { expected, actual } => {
                write!(
                    f,
                    "unsupported snapshot profile: expected `{expected}`, got `{actual}`"
                )
            }
            PersistedSnapshotRestoreError::UnsupportedVersion { expected, actual } => {
                write!(
                    f,
                    "unsupported snapshot version: expected `{expected}`, got `{actual}`"
                )
            }
            PersistedSnapshotRestoreError::MissingSort(sort) => {
                write!(f, "snapshot references missing sort `{sort}`")
            }
            PersistedSnapshotRestoreError::MissingFunction(func) => {
                write!(f, "snapshot references missing function `{func}`")
            }
            PersistedSnapshotRestoreError::SchemaMismatch(msg) => f.write_str(msg),
            PersistedSnapshotRestoreError::UnsupportedSnapshotFeature(msg) => f.write_str(msg),
            PersistedSnapshotRestoreError::TargetNotFresh(msg) => f.write_str(msg),
            PersistedSnapshotRestoreError::UnsupportedLiteral { sort, value } => write!(
                f,
                "unsupported persisted literal for sort `{sort}`: `{value}`"
            ),
            PersistedSnapshotRestoreError::UnresolvedRows {
                pending_facts,
                pending_function_rows,
            } => write!(
                f,
                "snapshot restore stalled with {pending_facts} pending facts and {pending_function_rows} pending function rows"
            ),
            PersistedSnapshotRestoreError::Runtime(msg) => f.write_str(msg),
        }
    }
}

impl std::error::Error for PersistedSnapshotRestoreError {}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct PersistedSnapshotDiagnostic {
    pub code: String,
    pub message: String,
    pub path: Option<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum PersistedSnapshotEqClassSemantics {
    InspectOnly,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct PersistedSnapshotEqClassMemberRow {
    pub op_id: usize,
    pub inputs: Vec<PersistedSnapshotValue>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct PersistedSnapshotEqClass {
    pub sort_id: usize,
    pub logical_id: String,
    pub debug_value: Option<String>,
    pub members: Vec<PersistedSnapshotEqClassMemberRow>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct PersistedSnapshotEqClassPayload {
    pub semantics: PersistedSnapshotEqClassSemantics,
    pub classes: Vec<PersistedSnapshotEqClass>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Default)]
pub struct PersistedSnapshotCapabilityEntry {
    pub key: String,
    pub detail: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Default)]
pub struct PersistedSnapshotCapabilitySummary {
    pub required_preconditions: Vec<String>,
    pub guaranteed_restorable: Vec<PersistedSnapshotCapabilityEntry>,
    pub non_goals: Vec<PersistedSnapshotCapabilityEntry>,
    pub missing_hooks: Vec<PersistedSnapshotCapabilityEntry>,
    pub other_limitations: Vec<PersistedSnapshotCapabilityEntry>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct PersistedSnapshot {
    pub snapshot_version: u32,
    pub format: String,
    pub profile: String,
    pub producer: Option<PersistedSnapshotProducer>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub source_schema: Option<PersistedSnapshotSourceSchema>,
    pub dictionary: PersistedSnapshotDictionary,
    pub schema: PersistedSnapshotSchema,
    pub state: PersistedSnapshotState,
    pub restore_mapping: PersistedSnapshotRestoreMapping,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub capability_summary: Option<PersistedSnapshotCapabilitySummary>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub eq_class_payload: Option<PersistedSnapshotEqClassPayload>,
    pub diagnostics: Vec<PersistedSnapshotDiagnostic>,
}

pub fn persisted_snapshot_capability_summary(
    snapshot: &PersistedSnapshot,
) -> PersistedSnapshotCapabilitySummary {
    if let Some(summary) = &snapshot.capability_summary {
        return summary.clone();
    }
    derive_persisted_snapshot_capability_summary(snapshot)
}

pub fn compare_persisted_snapshot_to_current(
    snapshot: &PersistedSnapshot,
    egraph: &EGraph,
) -> PersistedSnapshotAlignmentReport {
    let current_engine = current_engine_schema_manifest(egraph);
    let current_dsl = current_dsl_schema_manifest();
    let current_engine_fingerprint = engine_schema_fingerprint(&current_engine).ok();
    let current_dsl_runtime_fingerprint = dsl_runtime_fingerprint(&current_dsl).ok();
    let current_dsl_metadata_fingerprint = dsl_metadata_fingerprint(&current_dsl).ok();

    let Some(source_schema) = snapshot.source_schema.as_ref() else {
        return PersistedSnapshotAlignmentReport {
            has_alignment_header: false,
            exact_source_match: false,
            restore_schema_compatible: false,
            engine_fingerprint_matches: false,
            dsl_runtime_fingerprint_matches: false,
            dsl_metadata_fingerprint_matches: false,
            macro_rev_matches: false,
            issues: vec![PersistedSnapshotAlignmentIssue {
                layer: PersistedSnapshotAlignmentLayer::Header,
                blocking: true,
                detail: "snapshot was produced without source_schema alignment header; v1 restore now requires producer/source alignment proof"
                    .to_string(),
            }],
        };
    };

    let engine_fingerprint_matches =
        current_engine_fingerprint == Some(source_schema.engine_fingerprint.clone());
    let dsl_runtime_fingerprint_matches =
        current_dsl_runtime_fingerprint == Some(source_schema.dsl_runtime_fingerprint.clone());
    let dsl_metadata_fingerprint_matches =
        current_dsl_metadata_fingerprint == Some(source_schema.dsl_metadata_fingerprint.clone());
    let macro_rev_matches = current_dsl.macro_rev == source_schema.macro_rev;

    let mut issues = Vec::new();
    if !engine_fingerprint_matches {
        issues.push(PersistedSnapshotAlignmentIssue {
            layer: PersistedSnapshotAlignmentLayer::Engine,
            blocking: true,
            detail:
                "engine schema fingerprint mismatch between snapshot producer and current runtime"
                    .to_string(),
        });
    }
    if !dsl_runtime_fingerprint_matches {
        issues.push(PersistedSnapshotAlignmentIssue {
            layer: PersistedSnapshotAlignmentLayer::DslRuntime,
            blocking: true,
            detail:
                "dsl runtime fingerprint mismatch between snapshot producer and current codebase"
                    .to_string(),
        });
    }
    if !dsl_metadata_fingerprint_matches {
        issues.push(PersistedSnapshotAlignmentIssue {
            layer: PersistedSnapshotAlignmentLayer::DslMetadata,
            blocking: false,
            detail:
                "dsl metadata fingerprint mismatch between snapshot producer and current codebase"
                    .to_string(),
        });
    }
    if !macro_rev_matches {
        issues.push(PersistedSnapshotAlignmentIssue {
            layer: PersistedSnapshotAlignmentLayer::MacroRev,
            blocking: false,
            detail: "dsl macro revision mismatch between snapshot producer and current codebase"
                .to_string(),
        });
    }

    PersistedSnapshotAlignmentReport {
        has_alignment_header: true,
        exact_source_match: engine_fingerprint_matches
            && dsl_runtime_fingerprint_matches
            && dsl_metadata_fingerprint_matches
            && macro_rev_matches,
        restore_schema_compatible: engine_fingerprint_matches && dsl_runtime_fingerprint_matches,
        engine_fingerprint_matches,
        dsl_runtime_fingerprint_matches,
        dsl_metadata_fingerprint_matches,
        macro_rev_matches,
        issues,
    }
}

pub fn build_persisted_snapshot_v1(egraph: &EGraph, config: SerializeConfig) -> PersistedSnapshot {
    let engine_schema = current_engine_schema_manifest(egraph);
    let dsl_schema = current_dsl_schema_manifest();
    let raw_rows = egraph.serialize_raw(config);
    let mut dictionary = PersistedSnapshotDictionary::default();
    let mut sort_ids = HashMap::new();
    let mut op_ids = HashMap::new();
    let mut diagnostics = Vec::new();
    let source_schema = PersistedSnapshotSourceSchema {
        engine_fingerprint: engine_schema_fingerprint(&engine_schema)
            .expect("engine schema manifest must serialize"),
        dsl_runtime_fingerprint: dsl_runtime_fingerprint(&dsl_schema)
            .expect("dsl runtime manifest must serialize"),
        dsl_metadata_fingerprint: dsl_metadata_fingerprint(&dsl_schema)
            .expect("dsl metadata manifest must serialize"),
        macro_rev: dsl_schema.macro_rev.clone(),
    };

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
                metadata: persisted_snapshot_user_base_sort_metadata(&sort.name),
            }
        })
        .collect::<Vec<_>>();

    let mut function_decls = Vec::new();
    let mut constructor_decls = Vec::new();
    let ruleset_decls = egraph
        .get_all_rulesets()
        .into_iter()
        .filter(|(name, _)| !name.is_empty())
        .enumerate()
        .map(|(ruleset_id, (name, _))| {
            dictionary.rulesets.push(name.clone());
            PersistedSnapshotRulesetDecl { ruleset_id, name }
        })
        .collect::<Vec<_>>();
    let mut value_ids = Vec::new();
    let mut logical_value_ids = HashMap::<String, String>::new();
    let mut facts = Vec::new();
    let mut function_rows = Vec::new();
    let mut unsupported_restore_literal_sorts = BTreeMap::<String, usize>::new();
    let sort_unionable = engine_schema
        .sorts
        .iter()
        .map(|sort| (sort.name.clone(), sort.unionable))
        .collect::<HashMap<_, _>>();
    let mut plain_source_relation_like_rows = BTreeMap::<String, usize>::new();

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
            metadata: persisted_snapshot_function_metadata(function, &dsl_schema),
        };

        match function.kind {
            ArtifactFunctionKind::Constructor => constructor_decls.push(decl.clone()),
            ArtifactFunctionKind::Function => function_decls.push(decl.clone()),
        }

        let Some(rows) = raw_rows.get(&function.name) else {
            continue;
        };
        let live_rows = rows.iter().filter(|row| !row.subsumed).count();
        if live_rows > 0 && is_plain_source_relation_like_decl(function, &sort_unionable) {
            plain_source_relation_like_rows.insert(function.name.clone(), live_rows);
        }

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
                        &mut unsupported_restore_literal_sorts,
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
                &mut unsupported_restore_literal_sorts,
            );
            function_rows.push(PersistedSnapshotFunctionRow {
                op_id,
                inputs,
                output,
            });
        }
    }

    let existing_function_names = function_decls
        .iter()
        .map(|decl| decl.name.clone())
        .collect::<BTreeSet<_>>();
    let relation_output_sort_id = sort_ids.get("Unit").or_else(|| sort_ids.get("()")).copied();
    if let Some(output_sort_id) = relation_output_sort_id {
        for decl in inventory::iter::<Decl> {
            let Decl::EgglogRelationTy {
                name,
                input,
                typst_template,
                precedence,
            } = decl
            else {
                continue;
            };
            if existing_function_names.contains(*name) {
                continue;
            }
            let Some(rows) = raw_rows.get(*name) else {
                continue;
            };
            let op_id = dictionary.ops.len();
            dictionary.ops.push((*name).to_string());
            let input_sort_ids = input
                .iter()
                .map(|sort_name| {
                    let normalized = normalize_snapshot_sort_name(sort_name);
                    sort_ids[&normalized]
                })
                .collect::<Vec<_>>();
            function_decls.push(PersistedSnapshotFunctionDecl {
                op_id,
                name: (*name).to_string(),
                input_sort_ids: input_sort_ids.clone(),
                output_sort_id,
                is_relation: true,
                merge: None,
                cost: None,
                metadata: Some(json!({
                    "typst_template": typst_template,
                    "precedence": precedence,
                })),
            });
            for row in rows.iter().filter(|row| !row.subsumed) {
                let inputs = input
                    .iter()
                    .zip(&row.inputs_complex)
                    .map(|(sort_name, value)| {
                        let normalized = normalize_snapshot_sort_name(sort_name);
                        persisted_snapshot_value(
                            egraph,
                            &sort_ids,
                            &normalized,
                            *value,
                            &mut logical_value_ids,
                            &mut value_ids,
                            &mut unsupported_restore_literal_sorts,
                        )
                    })
                    .collect::<Vec<_>>();
                facts.push(PersistedSnapshotFact { op_id, inputs });
            }
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
        message: "persisted ruleset declarations are provenance-only metadata in v1; restore ignores them and they do not imply rule body/run replay semantics"
            .to_string(),
        path: Some("schema.ruleset_decls".to_string()),
    });
    for (sort_name, count) in unsupported_restore_literal_sorts {
        match user_base_sort_restore_support(&sort_name) {
            Some(PersistedSnapshotUserBaseSortSupport::RegisteredWithoutHook) => {
                diagnostics.push(PersistedSnapshotDiagnostic {
                    code: "user-base-restore-missing-hook".to_string(),
                    message: format!(
                        "user-defined base sort `{sort_name}` exported {count} literal value(s) but does not provide a persisted snapshot restore hook; restore support is not promised"
                    ),
                    path: Some(format!("state.literal_sorts.{sort_name}")),
                });
            }
            _ => {
                diagnostics.push(PersistedSnapshotDiagnostic {
                    code: "restore-coverage-gap".to_string(),
                    message: format!(
                        "v1 restore does not yet support all literals for sort `{sort_name}`; {count} literal value(s) were exported without machine-readable restore payload"
                    ),
                    path: Some(format!("state.literal_sorts.{sort_name}")),
                });
            }
        }
    }
    for (function_name, row_count) in plain_source_relation_like_rows {
        diagnostics.push(PersistedSnapshotDiagnostic {
            code: "plain-source-non-goal".to_string(),
            message: format!(
                "v1 does not treat plain/non-eggplant relation-like source `{function_name}` as supported fact-classification; {row_count} row(s) remain outside the `state.facts` contract and are exported without relation-fact guarantees"
            ),
            path: Some(format!("state.plain_source_relation_like.{function_name}")),
        });
    }

    let mut snapshot = PersistedSnapshot {
        snapshot_version: EGGPLANT_PERSISTED_SNAPSHOT_VERSION,
        format: EGGPLANT_PERSISTED_SNAPSHOT_FORMAT.to_string(),
        profile: EGGPLANT_PERSISTED_SNAPSHOT_PROFILE.to_string(),
        producer: Some(PersistedSnapshotProducer {
            crate_name: env!("CARGO_PKG_NAME").to_string(),
            version: env!("CARGO_PKG_VERSION").to_string(),
        }),
        source_schema: Some(source_schema),
        dictionary,
        schema: PersistedSnapshotSchema {
            sort_decls,
            function_decls,
            constructor_decls,
            ruleset_decls,
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
        capability_summary: None,
        eq_class_payload: None,
        diagnostics,
    };
    snapshot.capability_summary = Some(derive_persisted_snapshot_capability_summary(&snapshot));
    snapshot
}

pub fn build_persisted_snapshot_v2_eqclass(
    egraph: &EGraph,
    config: SerializeConfig,
) -> PersistedSnapshot {
    let mut snapshot = build_persisted_snapshot_v1(egraph, config);
    snapshot.snapshot_version = EGGPLANT_PERSISTED_SNAPSHOT_V2_EQCLASS_VERSION;
    snapshot.profile = EGGPLANT_PERSISTED_SNAPSHOT_V2_EQCLASS_PROFILE.to_string();
    snapshot.eq_class_payload = Some(derive_persisted_snapshot_eq_class_payload(&snapshot));
    snapshot.diagnostics.push(PersistedSnapshotDiagnostic {
        code: "inspect-only-metadata".to_string(),
        message: "v2 eq_class_payload is inspect-only metadata; restore continues to use the v1 semantic-state substrate"
            .to_string(),
        path: Some("eq_class_payload".to_string()),
    });
    snapshot.capability_summary = Some(derive_persisted_snapshot_capability_summary(&snapshot));
    snapshot
}

#[derive(Clone)]
enum SnapshotOpKind {
    Constructor,
    Function,
    Relation,
}

#[derive(Clone)]
struct SnapshotOpDeclResolved {
    name: String,
    input_sort_names: Vec<String>,
    output_sort_name: String,
    kind: SnapshotOpKind,
}

pub fn restore_persisted_snapshot_v1(
    egraph: &mut EGraph,
    snapshot: &PersistedSnapshot,
) -> Result<PersistedSnapshotRestoreReport, PersistedSnapshotRestoreError> {
    if snapshot.format != EGGPLANT_PERSISTED_SNAPSHOT_FORMAT {
        return Err(PersistedSnapshotRestoreError::UnsupportedFormat {
            expected: EGGPLANT_PERSISTED_SNAPSHOT_FORMAT.to_string(),
            actual: snapshot.format.clone(),
        });
    }
    let is_v1_profile = snapshot.profile == EGGPLANT_PERSISTED_SNAPSHOT_PROFILE
        && snapshot.snapshot_version == EGGPLANT_PERSISTED_SNAPSHOT_VERSION;
    let is_v2_eqclass_profile = snapshot.profile == EGGPLANT_PERSISTED_SNAPSHOT_V2_EQCLASS_PROFILE
        && snapshot.snapshot_version == EGGPLANT_PERSISTED_SNAPSHOT_V2_EQCLASS_VERSION;

    if !is_v1_profile && !is_v2_eqclass_profile {
        return Err(PersistedSnapshotRestoreError::UnsupportedProfile {
            expected: format!(
                "{} or {}",
                EGGPLANT_PERSISTED_SNAPSHOT_PROFILE, EGGPLANT_PERSISTED_SNAPSHOT_V2_EQCLASS_PROFILE
            ),
            actual: format!("{}@v{}", snapshot.profile, snapshot.snapshot_version),
        });
    }
    if is_v2_eqclass_profile {
        let Some(payload) = snapshot.eq_class_payload.as_ref() else {
            return Err(PersistedSnapshotRestoreError::UnsupportedSnapshotFeature(
                "v2 eq-class-aware snapshots require eq_class_payload".to_string(),
            ));
        };
        match payload.semantics {
            PersistedSnapshotEqClassSemantics::InspectOnly => {}
        }
    }
    if egraph.num_tuples() != 0 {
        return Err(PersistedSnapshotRestoreError::TargetNotFresh(
            "restore_persisted_snapshot_v1 requires a fresh/empty runtime; target egraph already contains tuples"
                .to_string(),
        ));
    }
    let alignment = compare_persisted_snapshot_to_current(snapshot, egraph);
    if !alignment.restore_schema_compatible {
        let detail = alignment
            .issues
            .iter()
            .map(|issue| issue.detail.as_str())
            .collect::<Vec<_>>()
            .join("; ");
        return Err(PersistedSnapshotRestoreError::SchemaMismatch(format!(
            "snapshot source/schema alignment is not restore-compatible with the current runtime: {detail}"
        )));
    }
    if !snapshot.state.unions.is_empty() {
        return Err(PersistedSnapshotRestoreError::UnsupportedSnapshotFeature(
            "v1 restore does not yet support union history replay".to_string(),
        ));
    }
    if !snapshot.state.runs.is_empty() {
        return Err(PersistedSnapshotRestoreError::UnsupportedSnapshotFeature(
            "v1 restore does not yet support run scheduling replay".to_string(),
        ));
    }

    let sort_names_by_id = snapshot
        .schema
        .sort_decls
        .iter()
        .map(|decl| (decl.sort_id, decl.name.clone()))
        .collect::<HashMap<_, _>>();
    for sort_decl in &snapshot.schema.sort_decls {
        if egraph.get_sort_by_name(&sort_decl.name).is_none() {
            return Err(PersistedSnapshotRestoreError::MissingSort(
                sort_decl.name.clone(),
            ));
        }
    }

    let mut op_decls = HashMap::<usize, SnapshotOpDeclResolved>::new();
    for decl in &snapshot.schema.constructor_decls {
        op_decls.insert(
            decl.op_id,
            SnapshotOpDeclResolved {
                name: decl.name.clone(),
                input_sort_names: decl
                    .input_sort_ids
                    .iter()
                    .map(|id| sort_names_by_id[id].clone())
                    .collect(),
                output_sort_name: sort_names_by_id[&decl.output_sort_id].clone(),
                kind: SnapshotOpKind::Constructor,
            },
        );
    }
    for decl in &snapshot.schema.function_decls {
        op_decls.insert(
            decl.op_id,
            SnapshotOpDeclResolved {
                name: decl.name.clone(),
                input_sort_names: decl
                    .input_sort_ids
                    .iter()
                    .map(|id| sort_names_by_id[id].clone())
                    .collect(),
                output_sort_name: sort_names_by_id[&decl.output_sort_id].clone(),
                kind: if decl.is_relation {
                    SnapshotOpKind::Relation
                } else {
                    SnapshotOpKind::Function
                },
            },
        );
    }

    for decl in op_decls.values() {
        let func = egraph
            .get_function(&decl.name)
            .ok_or_else(|| PersistedSnapshotRestoreError::MissingFunction(decl.name.clone()))?;
        let current_inputs = func
            .schema()
            .input
            .iter()
            .map(|sort| sort.name().to_string())
            .collect::<Vec<_>>();
        let current_output = func.schema().output.name().to_string();
        let output_matches = match decl.kind {
            // Relation restore only consumes fact inputs; egglog's internal backing output sort
            // is not part of the semantic-state contract we persist.
            SnapshotOpKind::Relation => true,
            SnapshotOpKind::Constructor | SnapshotOpKind::Function => {
                current_output == decl.output_sort_name
            }
        };
        if current_inputs != decl.input_sort_names || !output_matches {
            return Err(PersistedSnapshotRestoreError::SchemaMismatch(format!(
                "snapshot schema for `{}` does not match current runtime schema",
                decl.name
            )));
        }
    }

    #[derive(Default)]
    struct RestoreState {
        resolved_values: HashMap<String, egglog::Value>,
        restored_facts: usize,
        restored_function_rows: usize,
        error: Option<PersistedSnapshotRestoreError>,
        applied: bool,
    }

    let restore_state = Arc::new(Mutex::new(RestoreState::default()));
    let facts = snapshot.state.facts.clone();
    let function_rows = snapshot.state.function_rows.clone();
    let op_decls = Arc::new(op_decls);
    let sort_names_by_id = Arc::new(sort_names_by_id);

    run_ephemeral_rust_rule(
        egraph,
        "restore_persisted_snapshot_v1",
        &[],
        egglog::ast::Facts(Vec::new()),
        {
            let restore_state = Arc::clone(&restore_state);
            let op_decls = Arc::clone(&op_decls);
            let sort_names_by_id = Arc::clone(&sort_names_by_id);
            move |ctx, _| {
                let mut state = restore_state.lock().unwrap();
                if state.applied {
                    return Some(());
                }
                state.applied = true;

                let mut pending_facts = facts.clone();
                let mut pending_rows = function_rows.clone();

                loop {
                    let mut progressed = false;

                    let mut next_facts = Vec::new();
                    for fact in pending_facts.into_iter() {
                        let Some(decl) = op_decls.get(&fact.op_id) else {
                            state.error = Some(PersistedSnapshotRestoreError::MissingFunction(
                                format!("op_id {}", fact.op_id),
                            ));
                            return None;
                        };
                        let Some(inputs) = (match resolve_snapshot_values(
                            ctx,
                            &sort_names_by_id,
                            &decl.input_sort_names,
                            &fact.inputs,
                            &state.resolved_values,
                        ) {
                            Ok(inputs) => inputs,
                            Err(err) => {
                                state.error = Some(err);
                                return None;
                            }
                        }) else {
                            next_facts.push(fact);
                            continue;
                        };
                        let unit = ctx.base_to_value(());
                        ctx.insert(&decl.name, inputs.into_iter().chain(std::iter::once(unit)));
                        state.restored_facts += 1;
                        progressed = true;
                    }
                    pending_facts = next_facts;

                    let mut next_rows = Vec::new();
                    for row in pending_rows.into_iter() {
                        let Some(decl) = op_decls.get(&row.op_id) else {
                            state.error = Some(PersistedSnapshotRestoreError::MissingFunction(
                                format!("op_id {}", row.op_id),
                            ));
                            return None;
                        };
                        let Some(inputs) = (match resolve_snapshot_values(
                            ctx,
                            &sort_names_by_id,
                            &decl.input_sort_names,
                            &row.inputs,
                            &state.resolved_values,
                        ) {
                            Ok(inputs) => inputs,
                            Err(err) => {
                                state.error = Some(err);
                                return None;
                            }
                        }) else {
                            next_rows.push(row);
                            continue;
                        };

                        match decl.kind {
                            SnapshotOpKind::Constructor => {
                                let Some(out) = ctx.lookup(&decl.name, &inputs) else {
                                    state.error = Some(PersistedSnapshotRestoreError::Runtime(
                                        format!(
                                            "constructor `{}` lookup failed during snapshot restore",
                                            decl.name
                                        ),
                                    ));
                                    return None;
                                };
                                if let PersistedSnapshotValue::Ref { logical_id, .. } = row.output {
                                    state.resolved_values.entry(logical_id).or_insert(out);
                                }
                                state.restored_function_rows += 1;
                                progressed = true;
                            }
                            SnapshotOpKind::Function => {
                                let output = match match resolve_snapshot_value(
                                    ctx,
                                    &sort_names_by_id,
                                    &decl.output_sort_name,
                                    &row.output,
                                    &state.resolved_values,
                                ) {
                                    Ok(output) => output,
                                    Err(err) => {
                                        state.error = Some(err);
                                        return None;
                                    }
                                } {
                                    Some(value) => value,
                                    None => {
                                        if let PersistedSnapshotValue::Ref { logical_id, .. } = &row.output
                                        {
                                            let Some(value) = ctx.lookup(&decl.name, &inputs) else {
                                                next_rows.push(row);
                                                continue;
                                            };
                                            state
                                                .resolved_values
                                                .entry(logical_id.clone())
                                                .or_insert(value);
                                            value
                                        } else {
                                            next_rows.push(row);
                                            continue;
                                        }
                                    }
                                };
                                ctx.insert(&decl.name, inputs.into_iter().chain(std::iter::once(output)));
                                state.restored_function_rows += 1;
                                progressed = true;
                            }
                            SnapshotOpKind::Relation => unreachable!(),
                        }
                    }
                    pending_rows = next_rows;

                    if pending_facts.is_empty() && pending_rows.is_empty() {
                        break;
                    }
                    if !progressed {
                        state.error = Some(PersistedSnapshotRestoreError::UnresolvedRows {
                            pending_facts: pending_facts.len(),
                            pending_function_rows: pending_rows.len(),
                        });
                        return None;
                    }
                }

                Some(())
            }
        },
    )
    .map_err(|err| {
        restore_state
            .lock()
            .unwrap()
            .error
            .clone()
            .unwrap_or_else(|| PersistedSnapshotRestoreError::Runtime(err.to_string()))
    })?;

    let state = restore_state.lock().unwrap();
    if let Some(err) = &state.error {
        return Err(err.clone());
    }
    Ok(PersistedSnapshotRestoreReport {
        restored_facts: state.restored_facts,
        restored_function_rows: state.restored_function_rows,
        resolved_logical_values: state.resolved_values.len(),
    })
}

fn resolve_snapshot_values(
    ctx: &mut egglog::prelude::RustRuleContext<'_, '_>,
    sort_names_by_id: &HashMap<usize, String>,
    sort_names: &[String],
    values: &[PersistedSnapshotValue],
    resolved_values: &HashMap<String, egglog::Value>,
) -> Result<Option<Vec<egglog::Value>>, PersistedSnapshotRestoreError> {
    let mut out = Vec::with_capacity(values.len());
    for (sort_name, value) in sort_names.iter().zip(values.iter()) {
        let Some(resolved) =
            resolve_snapshot_value(ctx, sort_names_by_id, sort_name, value, resolved_values)?
        else {
            return Ok(None);
        };
        out.push(resolved);
    }
    Ok(Some(out))
}

fn resolve_snapshot_value(
    ctx: &mut egglog::prelude::RustRuleContext<'_, '_>,
    _sort_names_by_id: &HashMap<usize, String>,
    sort_name: &str,
    value: &PersistedSnapshotValue,
    resolved_values: &HashMap<String, egglog::Value>,
) -> Result<Option<egglog::Value>, PersistedSnapshotRestoreError> {
    match value {
        PersistedSnapshotValue::Lit { value, .. } => {
            restore_literal_value(ctx, sort_name, value).map(Some)
        }
        PersistedSnapshotValue::Ref { logical_id, .. } => {
            Ok(resolved_values.get(logical_id).copied())
        }
    }
}

fn restore_literal_value(
    ctx: &mut egglog::prelude::RustRuleContext<'_, '_>,
    sort_name: &str,
    literal: &PersistedSnapshotLiteralValue,
) -> Result<egglog::Value, PersistedSnapshotRestoreError> {
    match sort_name {
        "i64" => literal_i64(literal)
            .map(|value| ctx.base_to_value(value))
            .map_err(|_| PersistedSnapshotRestoreError::UnsupportedLiteral {
                sort: sort_name.to_string(),
                value: literal.value.clone(),
            }),
        "bool" => literal_bool(literal)
            .map(|value| ctx.base_to_value(value))
            .map_err(|_| PersistedSnapshotRestoreError::UnsupportedLiteral {
                sort: sort_name.to_string(),
                value: literal.value.clone(),
            }),
        "String" => {
            let parsed = literal_string(literal).unwrap_or_else(|| literal.value.clone());
            Ok(ctx.base_to_value::<egglog::sort::S>(egglog::sort::S::new(parsed)))
        }
        "f64" => literal_f64(literal)
            .map(|value| ctx.base_to_value(egglog::sort::F::new(egglog::sort::OrderedFloat(value))))
            .map_err(|_| PersistedSnapshotRestoreError::UnsupportedLiteral {
                sort: sort_name.to_string(),
                value: literal.value.clone(),
            }),
        "BigInt" => literal_bigint(literal)
            .map(egglog::sort::Z::new)
            .map(|value| ctx.base_to_value(value))
            .map_err(|_| PersistedSnapshotRestoreError::UnsupportedLiteral {
                sort: sort_name.to_string(),
                value: literal.value.clone(),
            }),
        "BigRat" => literal_bigrat(literal)
            .map(egglog::sort::Q::new)
            .map(|value| ctx.base_to_value(value))
            .map_err(|_| PersistedSnapshotRestoreError::UnsupportedLiteral {
                sort: sort_name.to_string(),
                value: literal.value.clone(),
            }),
        "Unit" | "()" => Ok(ctx.base_to_value(())),
        _ => {
            if let Some(hook) = user_base_sort_restore_hook(sort_name) {
                let Some(machine_value) = literal.machine_value.as_ref() else {
                    return Err(PersistedSnapshotRestoreError::UnsupportedLiteral {
                        sort: sort_name.to_string(),
                        value: literal.value.clone(),
                    });
                };
                return hook.restore_machine_value(ctx, machine_value).map_err(|_| {
                    PersistedSnapshotRestoreError::UnsupportedLiteral {
                        sort: sort_name.to_string(),
                        value: literal.value.clone(),
                    }
                });
            }
            Err(PersistedSnapshotRestoreError::UnsupportedLiteral {
                sort: sort_name.to_string(),
                value: literal.value.clone(),
            })
        }
    }
}

fn is_relation_decl(function: &EngineFunctionSchema) -> bool {
    inventory::iter::<Decl>.into_iter().any(|decl| match decl {
        Decl::EgglogRelationTy { name, input, .. } => {
            *name == function.name
                && input
                    .iter()
                    .map(|sort| normalize_snapshot_sort_name(sort))
                    .collect::<Vec<_>>()
                    == function.input
        }
        _ => false,
    })
}

fn persisted_snapshot_user_base_sort_metadata(sort_name: &str) -> Option<JsonValue> {
    let support = user_base_sort_restore_support(sort_name)?;
    let support_name = match support {
        PersistedSnapshotUserBaseSortSupport::RegisteredWithoutHook => "registered_without_hook",
        PersistedSnapshotUserBaseSortSupport::RegisteredWithHook => "registered_with_hook",
    };
    let mut restore = serde_json::Map::new();
    restore.insert("support".to_string(), json!(support_name));
    if let Some(hook) = user_base_sort_restore_hook(sort_name) {
        restore.insert(
            "capability_label".to_string(),
            json!(hook.capability_label()),
        );
    }
    Some(json!({
        "persisted_snapshot_restore": JsonValue::Object(restore),
    }))
}

fn persisted_snapshot_function_metadata(
    function: &EngineFunctionSchema,
    dsl_schema: &DslSchemaManifest,
) -> Option<JsonValue> {
    if function.kind == ArtifactFunctionKind::Constructor {
        if let Some(variant) = dsl_schema.variants.iter().find(|variant| {
            variant.owner_ty == function.output && variant.variant_name == function.name
        }) {
            return Some(json!({
                "typst_template": variant.typst_template,
                "precedence": variant.precedence,
            }));
        }
    }

    let mut fallback = None;
    for decl in inventory::iter::<Decl> {
        let candidate = match decl {
            Decl::EgglogFuncTy {
                name,
                input,
                output,
                typst_template,
                precedence,
                ..
            } if *name == function.name
                && input
                    .iter()
                    .map(|sort| normalize_snapshot_sort_name(sort))
                    .collect::<Vec<_>>()
                    == function.input
                && normalize_snapshot_sort_name(output) == function.output =>
            {
                Some((*typst_template, *precedence))
            }
            Decl::EgglogRelationTy {
                name,
                input,
                typst_template,
                precedence,
            } if *name == function.name
                && input
                    .iter()
                    .map(|sort| normalize_snapshot_sort_name(sort))
                    .collect::<Vec<_>>()
                    == function.input =>
            {
                Some((*typst_template, *precedence))
            }
            _ => None,
        };

        let Some((typst_template, precedence)) = candidate else {
            continue;
        };
        let metadata = json!({
            "typst_template": typst_template,
            "precedence": precedence,
        });
        if typst_template.is_some() || precedence != u16::MAX {
            return Some(metadata);
        }
        fallback = Some(metadata);
    }
    fallback
}

fn derive_persisted_snapshot_capability_summary(
    snapshot: &PersistedSnapshot,
) -> PersistedSnapshotCapabilitySummary {
    let mut summary = PersistedSnapshotCapabilitySummary {
        required_preconditions: vec![
            "restore target must be fresh with respect to semantic state".to_string(),
            "source_schema alignment proof is required for v1 restore".to_string(),
        ],
        ..Default::default()
    };

    summary
        .guaranteed_restorable
        .push(PersistedSnapshotCapabilityEntry {
            key: "state.function_rows".to_string(),
            detail: "common-path semantic rows are part of the supported v1 restore contract"
                .to_string(),
        });
    summary
        .guaranteed_restorable
        .push(PersistedSnapshotCapabilityEntry {
            key: "state.facts.eggplant_native_relations".to_string(),
            detail: "eggplant-native relation facts are part of the supported v1 restore contract"
                .to_string(),
        });

    let used_literal_sort_ids = snapshot
        .state
        .facts
        .iter()
        .flat_map(|fact| fact.inputs.iter())
        .chain(
            snapshot
                .state
                .function_rows
                .iter()
                .flat_map(|row| row.inputs.iter().chain(std::iter::once(&row.output))),
        )
        .filter_map(|value| match value {
            PersistedSnapshotValue::Lit { sort_id, .. } => Some(*sort_id),
            PersistedSnapshotValue::Ref { .. } => None,
        })
        .collect::<BTreeSet<_>>();

    let sort_name_by_id = snapshot
        .schema
        .sort_decls
        .iter()
        .map(|decl| (decl.sort_id, decl.name.clone()))
        .collect::<HashMap<_, _>>();

    for sort_id in used_literal_sort_ids {
        let Some(sort_name) = sort_name_by_id.get(&sort_id) else {
            continue;
        };
        if let Some(PersistedSnapshotUserBaseSortSupport::RegisteredWithHook) =
            user_base_sort_restore_support(sort_name)
        {
            let capability_label = user_base_sort_restore_hook(sort_name)
                .map(|hook| hook.capability_label())
                .unwrap_or("hooked");
            summary
                .guaranteed_restorable
                .push(PersistedSnapshotCapabilityEntry {
                    key: format!("state.literal_sorts.{sort_name}"),
                    detail: format!(
                        "user-defined base sort is restorable through hook-backed capability `{capability_label}`"
                    ),
                });
        } else if matches!(
            sort_name.as_str(),
            "i64" | "bool" | "String" | "f64" | "BigInt" | "BigRat" | "Unit" | "()"
        ) {
            summary
                .guaranteed_restorable
                .push(PersistedSnapshotCapabilityEntry {
                    key: format!("state.literal_sorts.{sort_name}"),
                    detail: "built-in/common-path literal sort is restorable with machine payloads"
                        .to_string(),
                });
        }
    }

    for diagnostic in &snapshot.diagnostics {
        let entry = PersistedSnapshotCapabilityEntry {
            key: diagnostic
                .path
                .clone()
                .unwrap_or_else(|| diagnostic.code.clone()),
            detail: diagnostic.message.clone(),
        };
        match diagnostic.code.as_str() {
            "plain-source-non-goal" => summary.non_goals.push(entry),
            "user-base-restore-missing-hook" => summary.missing_hooks.push(entry),
            "unsupported-feature" | "restore-coverage-gap" => summary.other_limitations.push(entry),
            _ => summary.other_limitations.push(entry),
        }
    }
    if let Some(payload) = &snapshot.eq_class_payload {
        match payload.semantics {
            PersistedSnapshotEqClassSemantics::InspectOnly => {
                summary.other_limitations.push(PersistedSnapshotCapabilityEntry {
                    key: "eq_class_payload".to_string(),
                    detail:
                        "v2 eq_class_payload is inspect-only metadata and does not participate in restore semantics"
                            .to_string(),
                });
            }
        }
    }

    summary
}

fn derive_persisted_snapshot_eq_class_payload(
    snapshot: &PersistedSnapshot,
) -> PersistedSnapshotEqClassPayload {
    let mut classes = snapshot
        .restore_mapping
        .value_ids
        .iter()
        .map(|value_id| PersistedSnapshotEqClass {
            sort_id: value_id.sort_id,
            logical_id: value_id.logical_id.clone(),
            debug_value: value_id.debug_value.clone(),
            members: Vec::new(),
        })
        .collect::<Vec<_>>();
    let class_by_logical_id = classes
        .iter()
        .enumerate()
        .map(|(idx, class)| (class.logical_id.clone(), idx))
        .collect::<HashMap<_, _>>();

    for row in &snapshot.state.function_rows {
        let PersistedSnapshotValue::Ref { logical_id, .. } = &row.output else {
            continue;
        };
        let Some(class_idx) = class_by_logical_id.get(logical_id).copied() else {
            continue;
        };
        classes[class_idx]
            .members
            .push(PersistedSnapshotEqClassMemberRow {
                op_id: row.op_id,
                inputs: row.inputs.clone(),
            });
    }

    classes.retain(|class| !class.members.is_empty());
    classes.sort_by(|lhs, rhs| lhs.logical_id.cmp(&rhs.logical_id));
    for class in &mut classes {
        class.members.sort_by(|lhs, rhs| {
            lhs.op_id.cmp(&rhs.op_id).then_with(|| {
                serde_json::to_string(&lhs.inputs)
                    .unwrap()
                    .cmp(&serde_json::to_string(&rhs.inputs).unwrap())
            })
        });
    }

    PersistedSnapshotEqClassPayload {
        semantics: PersistedSnapshotEqClassSemantics::InspectOnly,
        classes,
    }
}

fn is_plain_source_relation_like_decl(
    function: &EngineFunctionSchema,
    sort_unionable: &HashMap<String, bool>,
) -> bool {
    !is_relation_decl(function)
        && matches!(function.kind, ArtifactFunctionKind::Constructor)
        && sort_unionable.get(&function.output) == Some(&false)
}

fn persisted_snapshot_machine_literal(
    egraph: &EGraph,
    sort_name: &str,
    value: egglog::Value,
) -> Option<JsonValue> {
    match sort_name {
        "i64" => Some(json!(egraph.value_to_base::<i64>(value))),
        "bool" => Some(json!(egraph.value_to_base::<bool>(value))),
        "String" => Some(json!(egraph.value_to_base::<egglog::sort::S>(value).0)),
        "f64" => Some(json!(egraph.value_to_base::<egglog::sort::F>(value).0.0)),
        "BigInt" => Some(json!({
            "decimal": egraph.value_to_base::<egglog::sort::Z>(value).0.to_string()
        })),
        "BigRat" => {
            let q = egraph.value_to_base::<egglog::sort::Q>(value);
            Some(json!({
                "numer": q.numer().to_string(),
                "denom": q.denom().to_string(),
            }))
        }
        "Unit" | "()" => Some(JsonValue::Null),
        _ => user_base_sort_restore_hook(sort_name)
            .and_then(|hook| hook.export_machine_value(egraph, value)),
    }
}

fn literal_i64(literal: &PersistedSnapshotLiteralValue) -> Result<i64, ()> {
    literal
        .machine_value
        .as_ref()
        .and_then(|value| value.as_i64())
        .or_else(|| literal.value.parse::<i64>().ok())
        .ok_or(())
}

fn literal_bool(literal: &PersistedSnapshotLiteralValue) -> Result<bool, ()> {
    literal
        .machine_value
        .as_ref()
        .and_then(|value| value.as_bool())
        .or_else(|| literal.value.parse::<bool>().ok())
        .ok_or(())
}

fn literal_string(literal: &PersistedSnapshotLiteralValue) -> Option<String> {
    literal
        .machine_value
        .as_ref()
        .and_then(|value| value.as_str().map(ToOwned::to_owned))
        .or_else(|| serde_json::from_str::<String>(&literal.value).ok())
}

fn literal_f64(literal: &PersistedSnapshotLiteralValue) -> Result<f64, ()> {
    literal
        .machine_value
        .as_ref()
        .and_then(|value| value.as_f64())
        .or_else(|| literal.value.parse::<f64>().ok())
        .ok_or(())
}

fn literal_bigint(literal: &PersistedSnapshotLiteralValue) -> Result<num::BigInt, ()> {
    literal
        .machine_value
        .as_ref()
        .and_then(|value| value.get("decimal"))
        .and_then(|value| value.as_str())
        .and_then(|value| value.parse().ok())
        .or_else(|| literal.value.parse().ok())
        .ok_or(())
}

fn literal_bigrat(literal: &PersistedSnapshotLiteralValue) -> Result<num::BigRational, ()> {
    literal
        .machine_value
        .as_ref()
        .and_then(|value| {
            let numer = value.get("numer")?.as_str()?.parse().ok()?;
            let denom = value.get("denom")?.as_str()?.parse().ok()?;
            Some(num::BigRational::new(numer, denom))
        })
        .ok_or(())
}

fn normalize_snapshot_sort_name(sort: &str) -> String {
    match sort {
        "Q" => "BigRat".to_string(),
        "Z" => "BigInt".to_string(),
        _ => sort.to_string(),
    }
}

fn persisted_snapshot_value(
    egraph: &EGraph,
    sort_ids: &HashMap<String, usize>,
    sort_name: &str,
    value: egglog::Value,
    logical_value_ids: &mut HashMap<String, String>,
    value_ids: &mut Vec<PersistedSnapshotValueId>,
    unsupported_restore_literal_sorts: &mut BTreeMap<String, usize>,
) -> PersistedSnapshotValue {
    let sort = egraph
        .get_sort_by_name(sort_name)
        .unwrap_or_else(|| panic!("missing sort {sort_name} while exporting snapshot"));
    let sort_id = sort_ids[sort_name];

    if !sort.is_eq_sort() {
        let machine_value = persisted_snapshot_machine_literal(egraph, sort_name, value);
        if machine_value.is_none() {
            *unsupported_restore_literal_sorts
                .entry(sort_name.to_string())
                .or_default() += 1;
        }
        return PersistedSnapshotValue::Lit {
            sort_id,
            value: PersistedSnapshotLiteralValue {
                tag: sort_name.to_string(),
                value: egraph.base_value_print(value, sort),
                machine_value,
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
