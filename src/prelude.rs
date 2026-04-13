pub use crate::artifact::{
    ArtifactChangeKind, ArtifactCompatibilityIssue, ArtifactCompatibilityReport,
    ArtifactSchemaLayer, BinaryArtifactCodecError, BinaryArtifactCompatibilityPolicy,
    BinaryArtifactHeader, BinaryArtifactIoError, BinaryArtifactPayloadCodec,
    BinaryArtifactPayloadKind, DslSchemaManifest, EGGPLANT_ARTIFACT_FORMAT_VERSION,
    EGGPLANT_BINARY_CODEC_MAGIC, EGGPLANT_BINARY_CODEC_VERSION, EGGPLANT_DSL_MACRO_REV,
    EGGPLANT_PERSISTED_SNAPSHOT_FORMAT, EGGPLANT_PERSISTED_SNAPSHOT_PROFILE,
    EGGPLANT_PERSISTED_SNAPSHOT_VERSION, EGGPLANT_SERIALIZED_ARTIFACT_FORMAT, EngineSchemaManifest,
    PersistedSnapshot, PersistedSnapshotDiagnostic, PersistedSnapshotDictionary,
    PersistedSnapshotFact, PersistedSnapshotFunctionDecl, PersistedSnapshotFunctionRow,
    PersistedSnapshotLiteralValue, PersistedSnapshotProducer, PersistedSnapshotRestoreError,
    PersistedSnapshotRestoreMapping, PersistedSnapshotRestoreReport, PersistedSnapshotRulesetDecl,
    PersistedSnapshotRun, PersistedSnapshotSchema, PersistedSnapshotSortDecl,
    PersistedSnapshotSortKind, PersistedSnapshotState, PersistedSnapshotUnion,
    PersistedSnapshotValue, PersistedSnapshotValueId, SerializedEggplantArtifact,
    build_persisted_snapshot_v1, build_serialized_eggplant_artifact, compare_artifact_to_current,
    current_dsl_schema_manifest, current_engine_schema_manifest, dsl_metadata_fingerprint,
    dsl_runtime_fingerprint, engine_schema_fingerprint, read_binary_artifact_header,
    read_binary_artifact_header_from_file, restore_persisted_snapshot_v1,
};
pub use crate::instances::pat_rec::*;
pub use crate::instances::tx::*;
pub use crate::instances::tx_minimal::*;
pub use crate::instances::tx_rx_vt::*;
pub use crate::instances::tx_rx_vt_pr::*;
pub use crate::instances::tx_rx_vt_pr_slot::*;
pub use crate::schema::{
    ArtifactCompatibility, ArtifactLoadError, ArtifactSchemaHeader, DslVariantManifest,
    EngineConstructorManifest, EngineFunctionManifest, EngineSortKind, EngineSortManifest,
    ManifestCompatibility, ManifestDiff, SchemaCompatibilityError, SchemaFingerprints,
    SerializedArtifactEnvelope,
};
#[cfg(feature = "viewer")]
pub use crate::wrap::EGraphViewSgl;
pub use crate::wrap::constraint::{
    Compare, IntoHandleTy, SetExprExt, VecExprExt, prim_call, prim_fact, set_empty, set_of,
    vec_empty, vec_of,
};
pub use crate::wrap::sorts::set::SetContainer;
pub use crate::wrap::sorts::vec::VecContainer;
pub use crate::wrap::{
    AsHandle, BaseVar, Commit, EgglogNode, ExtractBackend, ExtractNodeSgl, ExtractSgl, FromBase,
    Insertable, LocateVersion, PEq, PatRecSgl, QuerySlot, RenderedTemplateField, RuleRunnerSgl,
    RuleSetId, RunConfig, RustsatExtractConfig, RxSgl, SingletonGetter, SlotVarID,
    SlottedPatRecSgl, ToDot, ToDotSgl, TxCommit, TxCommitSgl, TxSgl, Value,
    render_template_with_precedence, render_variant_display, render_variant_typst,
};

pub use dashmap;
pub use derive_more;
pub use egglog;
pub use egglog::ast::{RustSpan, Span};
pub use eggplant_macros::*;
pub use inventory;
pub use serde;
pub use serde_json;
pub use strum;
pub use strum_macros;
