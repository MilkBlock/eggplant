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
pub use crate::helpers::bench_cli::{
    DEFAULT_EXTRACTORS, ExtractBenchCliArgs, TimelineExportCliArgs, parse_extract_bench_args,
    parse_timeline_export_args, timeline_markdown_asset_path, timeline_markdown_output_path,
    timeline_plot_output_path,
};
pub use crate::helpers::progress::{
    BenchProgress, TimelineExtractMetric, format_bytes, format_duration, format_optional_bytes,
    format_optional_duration, format_optional_ms,
};
#[cfg(feature = "timeline-plot")]
pub use crate::helpers::report::write_timeline_plot_png;
pub use crate::helpers::report::{
    ExtractReportRow, print_extract_comparison_report, print_extract_run_configuration,
    render_timeline_markdown_report, render_timeline_markdown_report_with_plot,
    write_timeline_markdown_report, write_timeline_markdown_report_with_plot,
};
pub use crate::helpers::runtime::{
    current_peak_memory_bytes, duration_from_ms, duration_to_ms, elapsed_ms, gib_to_bytes,
    run_with_timeout_payload,
};
pub use crate::instances::pat_rec::*;
pub use crate::instances::session_runtime::Session;
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
    AsHandle, BaseVar, Commit, EBoostExtractConfig, EBoostLayeredConfig, EgglogCompatExt,
    EgglogEnumVariantTy, EgglogNode, EgglogTy, ExtractBackend, ExtractNodeSgl, ExtractSgl,
    FromBase, FunctionId, Insertable, LocateVersion, NonPatRecSgl, OwnedFunctionRow, PEq,
    PatRecSgl, ProofRuleTemplate, ProofRuleTemplateMatch, ProofRulesTemplateIndex,
    ProofSvgFormatter, QuerySlot, RawEGraphNode, RenderedTemplateField, RuleRunnerSgl, RuleSetId,
    RunConfig, RunSchedule, RunScheduleBuilder, RustsatExtractConfig, RxSgl, SchemaFunctionKind,
    SchemaSortKind, SingletonGetter, SlotVarID, SlottedPatRecSgl, ToDot, ToDotSgl, TxCommit,
    TxCommitSgl, TxSgl, Value, add_ruleset, clear_compat_state, compile_typst_document_to_svg,
    compile_typst_document_to_svg_string, compile_typst_math_to_svg, extract_raw_with_backend,
    render_proof_svg_from_rules_template, render_proof_svg_from_rules_template_with_options,
    render_proof_text_svg, render_proof_text_svg_with_options, render_proof_text_typst,
    render_proof_text_typst_with_options, render_template_with_precedence,
    render_value_proof_text_svg, render_value_proof_text_svg_with_options,
    render_value_proof_text_typst, render_value_proof_text_typst_with_options,
    render_variant_display, render_variant_typst, run_ephemeral_rust_rule, run_ruleset, rust_rule,
    rust_rule_with_metadata,
};
pub use crate::{basic_tx_rx_vt_pr_pf, tx_rx_vt_pr_pf};

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
