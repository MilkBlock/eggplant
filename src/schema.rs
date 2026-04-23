use crate::wrap::{Decl, EgglogTy, SchemaFieldKind, UserBaseSort};
use egglog::sort::{Q, Z};
use serde::{Deserialize, Serialize, de::DeserializeOwned};
use sha2::{Digest, Sha256};
use std::collections::BTreeMap;
use std::fmt::{Display, Formatter};

const ARTIFACT_SCHEMA_FORMAT_VERSION: u32 = 1;
const DSL_MACRO_REVISION: &str = concat!("eggplant-macros@", env!("CARGO_PKG_VERSION"));

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum EngineSortKind {
    Base,
    EqSort,
    Container,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct EngineSortManifest {
    pub key: String,
    pub name: String,
    pub kind: EngineSortKind,
    pub element_sort: Option<String>,
    pub container_kind: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct EngineConstructorManifest {
    pub key: String,
    pub name: String,
    pub output_sort: String,
    pub input_sorts: Vec<String>,
    pub cost: Option<u64>,
    pub unextractable: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct EngineFunctionManifest {
    pub key: String,
    pub name: String,
    pub input_sorts: Vec<String>,
    pub output_sort: String,
    pub merge: Option<String>,
    pub hidden: bool,
    pub let_binding: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Default)]
pub struct EngineSchemaManifest {
    pub sorts: Vec<EngineSortManifest>,
    pub constructors: Vec<EngineConstructorManifest>,
    pub functions: Vec<EngineFunctionManifest>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct DslVariantManifest {
    pub key: String,
    pub name: String,
    pub output_sort: String,
    pub field_names: Vec<String>,
    pub field_sorts: Vec<String>,
    pub field_kinds: Vec<SchemaFieldKind>,
    pub display_template: Option<String>,
    pub typst_template: Option<String>,
    pub precedence: u16,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct DslSchemaManifest {
    pub macro_revision: String,
    pub variants: Vec<DslVariantManifest>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct SchemaFingerprints {
    pub engine: String,
    pub dsl: String,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct ArtifactSchemaHeader {
    pub format_version: u32,
    pub engine: EngineSchemaManifest,
    pub dsl: DslSchemaManifest,
    pub fingerprints: SchemaFingerprints,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct SerializedArtifactEnvelope<P> {
    pub schema: ArtifactSchemaHeader,
    pub payload: P,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct ManifestDiff {
    pub added: Vec<String>,
    pub removed: Vec<String>,
    pub changed: Vec<String>,
}

impl ManifestDiff {
    pub fn is_empty(&self) -> bool {
        self.added.is_empty() && self.removed.is_empty() && self.changed.is_empty()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ManifestCompatibility {
    pub exact_match: bool,
    pub diff: ManifestDiff,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ArtifactCompatibility {
    pub format_version_match: bool,
    pub continuation_allowed: bool,
    pub dsl_runtime_compatible: bool,
    pub engine: ManifestCompatibility,
    pub dsl: ManifestCompatibility,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SchemaCompatibilityError {
    pub compatibility: ArtifactCompatibility,
}

#[derive(Debug)]
pub enum ArtifactLoadError {
    Parse(serde_json::Error),
    IncompatibleSchema(SchemaCompatibilityError),
}

impl Display for SchemaCompatibilityError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if !self.compatibility.format_version_match {
            writeln!(
                f,
                "artifact schema format mismatch: saved header version is incompatible with current runtime"
            )?;
        }
        writeln!(
            f,
            "serialized artifact is not continuation-compatible with the current eggplant runtime"
        )?;
        writeln!(
            f,
            "engine schema diff:{}",
            format_manifest_diff(&self.compatibility.engine.diff)
        )?;
        write!(
            f,
            "dsl schema diff:{}",
            format_manifest_diff(&self.compatibility.dsl.diff)
        )
    }
}

impl std::error::Error for SchemaCompatibilityError {}

impl Display for ArtifactLoadError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ArtifactLoadError::Parse(err) => {
                write!(f, "failed to parse serialized artifact: {err}")
            }
            ArtifactLoadError::IncompatibleSchema(err) => Display::fmt(err, f),
        }
    }
}

impl std::error::Error for ArtifactLoadError {}

impl ArtifactSchemaHeader {
    pub fn current() -> Self {
        let engine = current_engine_schema_manifest();
        let dsl = current_dsl_schema_manifest();
        let fingerprints = SchemaFingerprints {
            engine: fingerprint(&engine),
            dsl: fingerprint(&dsl),
        };
        Self {
            format_version: ARTIFACT_SCHEMA_FORMAT_VERSION,
            engine,
            dsl,
            fingerprints,
        }
    }

    pub fn refreshed(mut self) -> Self {
        self.refresh_fingerprints();
        self
    }

    pub fn refresh_fingerprints(&mut self) {
        self.fingerprints.engine = fingerprint(&self.engine);
        self.fingerprints.dsl = fingerprint(&self.dsl);
    }

    pub fn compatibility_with_current(&self) -> ArtifactCompatibility {
        let current = Self::current();
        let saved_engine_fingerprint = fingerprint(&self.engine);
        let saved_dsl_fingerprint = fingerprint(&self.dsl);
        let engine = ManifestCompatibility {
            exact_match: saved_engine_fingerprint == current.fingerprints.engine,
            diff: merge_diffs(&[
                diff_serialized_records(&self.engine.sorts, &current.engine.sorts),
                diff_serialized_records(&self.engine.constructors, &current.engine.constructors),
                diff_serialized_records(&self.engine.functions, &current.engine.functions),
            ]),
        };
        let dsl = ManifestCompatibility {
            exact_match: saved_dsl_fingerprint == current.fingerprints.dsl,
            diff: {
                let mut diff = diff_serialized_records(&self.dsl.variants, &current.dsl.variants);
                if self.dsl.macro_revision != current.dsl.macro_revision {
                    diff.changed.push("dsl.macro_revision".to_string());
                    diff.changed.sort();
                }
                diff
            },
        };
        let dsl_runtime_compatible =
            dsl_runtime_fingerprint(&self.dsl) == dsl_runtime_fingerprint(&current.dsl);
        let format_version_match = self.format_version == current.format_version;
        ArtifactCompatibility {
            format_version_match,
            continuation_allowed: format_version_match
                && engine.exact_match
                && dsl_runtime_compatible,
            dsl_runtime_compatible,
            engine,
            dsl,
        }
    }

    pub fn ensure_continuation_compatible(&self) -> Result<(), SchemaCompatibilityError> {
        let compatibility = self.compatibility_with_current();
        if compatibility.continuation_allowed {
            Ok(())
        } else {
            Err(SchemaCompatibilityError { compatibility })
        }
    }
}

impl<P> SerializedArtifactEnvelope<P> {
    pub fn new(payload: P) -> Self {
        Self {
            schema: ArtifactSchemaHeader::current(),
            payload,
        }
    }

    pub fn compatibility_with_current(&self) -> ArtifactCompatibility {
        self.schema.compatibility_with_current()
    }

    pub fn ensure_continuation_compatible(&self) -> Result<(), SchemaCompatibilityError> {
        self.schema.ensure_continuation_compatible()
    }

    pub fn map_payload<Q>(self, f: impl FnOnce(P) -> Q) -> SerializedArtifactEnvelope<Q> {
        SerializedArtifactEnvelope {
            schema: self.schema,
            payload: f(self.payload),
        }
    }
}

impl<P> SerializedArtifactEnvelope<P>
where
    P: Serialize,
{
    pub fn to_json_string(&self) -> serde_json::Result<String> {
        serde_json::to_string(self)
    }
}

impl<P> SerializedArtifactEnvelope<P>
where
    P: DeserializeOwned,
{
    pub fn from_json_str_unchecked(json: &str) -> serde_json::Result<Self> {
        serde_json::from_str(json)
    }

    pub fn from_json_str_checked(json: &str) -> Result<Self, ArtifactLoadError> {
        let envelope = Self::from_json_str_unchecked(json).map_err(ArtifactLoadError::Parse)?;
        envelope
            .ensure_continuation_compatible()
            .map_err(ArtifactLoadError::IncompatibleSchema)?;
        Ok(envelope)
    }
}

pub fn current_engine_schema_manifest() -> EngineSchemaManifest {
    let mut sorts = BTreeMap::<String, EngineSortManifest>::new();
    for name in builtin_base_sorts() {
        sorts.insert(
            name.to_string(),
            EngineSortManifest {
                key: name.to_string(),
                name: name.to_string(),
                kind: EngineSortKind::Base,
                element_sort: None,
                container_kind: None,
            },
        );
    }
    for sort in inventory::iter::<UserBaseSort> {
        sorts.insert(
            sort.name.to_string(),
            EngineSortManifest {
                key: sort.name.to_string(),
                name: sort.name.to_string(),
                kind: EngineSortKind::Base,
                element_sort: None,
                container_kind: None,
            },
        );
    }

    let mut constructors = Vec::new();
    let mut functions = Vec::new();
    for decl in inventory::iter::<Decl> {
        match *decl {
            Decl::EgglogMultiConTy { name, cons } => {
                sorts
                    .entry(name.to_string())
                    .or_insert_with(|| EngineSortManifest {
                        key: name.to_string(),
                        name: name.to_string(),
                        kind: EngineSortKind::EqSort,
                        element_sort: None,
                        container_kind: None,
                    });
                for con in cons.iter() {
                    constructors.push(EngineConstructorManifest {
                        key: constructor_key(con.output, con.cons_name, con.input),
                        name: con.cons_name.to_string(),
                        output_sort: normalize_ty_name(con.output),
                        input_sorts: con.input.iter().map(|ty| normalize_ty_name(ty)).collect(),
                        cost: con.cost,
                        unextractable: con.unextractable,
                    });
                }
            }
            Decl::EgglogContainerTy {
                name,
                ele_ty_name,
                constructor_str,
                ..
            } => {
                sorts.insert(
                    name.to_string(),
                    EngineSortManifest {
                        key: name.to_string(),
                        name: name.to_string(),
                        kind: EngineSortKind::Container,
                        element_sort: Some(normalize_ty_name(ele_ty_name)),
                        container_kind: Some(constructor_str.to_string()),
                    },
                );
            }
            Decl::EgglogFuncTy {
                name,
                input,
                output,
                merge,
                hidden,
                let_binding,
            } => {
                let normalized_input = input
                    .iter()
                    .map(|ty| normalize_ty_name(ty))
                    .collect::<Vec<_>>();
                let normalized_output = normalize_ty_name(output);
                functions.push(EngineFunctionManifest {
                    key: function_key(name, &normalized_input, &normalized_output),
                    name: name.to_string(),
                    input_sorts: normalized_input,
                    output_sort: normalized_output,
                    merge: merge.map(str::to_string),
                    hidden,
                    let_binding,
                });
            }
            Decl::EgglogRelationTy { .. } => {}
            Decl::EgglogRule { .. } => {}
        }
    }

    let mut manifest = EngineSchemaManifest {
        sorts: sorts.into_values().collect(),
        constructors,
        functions,
    };
    manifest.sorts.sort_by(|lhs, rhs| lhs.key.cmp(&rhs.key));
    manifest
        .constructors
        .sort_by(|lhs, rhs| lhs.key.cmp(&rhs.key));
    manifest.functions.sort_by(|lhs, rhs| lhs.key.cmp(&rhs.key));
    manifest
}

pub fn current_dsl_schema_manifest() -> DslSchemaManifest {
    let mut variants = Vec::new();
    for decl in inventory::iter::<Decl> {
        if let Decl::EgglogMultiConTy { cons, .. } = *decl {
            for con in cons.iter() {
                variants.push(DslVariantManifest {
                    key: constructor_key(con.output, con.cons_name, con.input),
                    name: con.cons_name.to_string(),
                    output_sort: normalize_ty_name(con.output),
                    field_names: con
                        .input_field_names
                        .iter()
                        .map(|name| (*name).to_string())
                        .collect(),
                    field_sorts: con.input.iter().map(|ty| normalize_ty_name(ty)).collect(),
                    field_kinds: con.input_field_kinds.to_vec(),
                    display_template: con.display_template.map(str::to_string),
                    typst_template: con.typst_template.map(str::to_string),
                    precedence: con.precedence,
                });
            }
        }
    }
    variants.sort_by(|lhs, rhs| lhs.key.cmp(&rhs.key));
    DslSchemaManifest {
        macro_revision: DSL_MACRO_REVISION.to_string(),
        variants,
    }
}

fn builtin_base_sorts() -> [&'static str; 7] {
    [
        "StaticStr",
        <String as EgglogTy>::TY_NAME,
        <i64 as EgglogTy>::TY_NAME,
        <f64 as EgglogTy>::TY_NAME,
        <bool as EgglogTy>::TY_NAME,
        <Q as EgglogTy>::TY_NAME,
        <Z as EgglogTy>::TY_NAME,
    ]
}

fn normalize_ty_name(ty: &str) -> String {
    match ty {
        "Q" => <Q as EgglogTy>::TY_NAME.to_string(),
        "Z" => <Z as EgglogTy>::TY_NAME.to_string(),
        _ => ty.to_string(),
    }
}

fn constructor_key(output_sort: &str, name: &str, input_sorts: &[&str]) -> String {
    let input = input_sorts
        .iter()
        .map(|ty| normalize_ty_name(ty))
        .collect::<Vec<_>>()
        .join(",");
    format!("{}::{name}({input})", normalize_ty_name(output_sort))
}

fn function_key(name: &str, input_sorts: &[String], output_sort: &str) -> String {
    format!("{name}({})->{output_sort}", input_sorts.join(","))
}

fn fingerprint<T: Serialize>(value: &T) -> String {
    let bytes = serde_json::to_vec(value).expect("schema manifest should serialize");
    let mut hasher = Sha256::new();
    hasher.update(bytes);
    format!("{:x}", hasher.finalize())
}

#[derive(Serialize)]
struct DslRuntimeManifest<'a> {
    macro_revision: &'a str,
    variants: Vec<DslRuntimeVariantManifest<'a>>,
}

#[derive(Serialize)]
struct DslRuntimeVariantManifest<'a> {
    key: &'a str,
    name: &'a str,
    output_sort: &'a str,
    field_sorts: &'a [String],
    field_kinds: &'a [SchemaFieldKind],
}

fn dsl_runtime_fingerprint(dsl: &DslSchemaManifest) -> String {
    let runtime = DslRuntimeManifest {
        macro_revision: &dsl.macro_revision,
        variants: dsl
            .variants
            .iter()
            .map(|variant| DslRuntimeVariantManifest {
                key: &variant.key,
                name: &variant.name,
                output_sort: &variant.output_sort,
                field_sorts: &variant.field_sorts,
                field_kinds: &variant.field_kinds,
            })
            .collect(),
    };
    fingerprint(&runtime)
}

fn diff_serialized_records<T>(saved: &[T], current: &[T]) -> ManifestDiff
where
    T: Serialize,
{
    let saved_map = saved
        .iter()
        .map(record_to_entry)
        .collect::<BTreeMap<_, _>>();
    let current_map = current
        .iter()
        .map(record_to_entry)
        .collect::<BTreeMap<_, _>>();

    let mut diff = ManifestDiff::default();
    for key in saved_map.keys() {
        if !current_map.contains_key(key) {
            diff.removed.push(key.clone());
        }
    }
    for key in current_map.keys() {
        if !saved_map.contains_key(key) {
            diff.added.push(key.clone());
        }
    }
    for (key, saved_value) in &saved_map {
        if let Some(current_value) = current_map.get(key)
            && saved_value != current_value
        {
            diff.changed.push(key.clone());
        }
    }
    diff
}

fn record_to_entry<T: Serialize>(record: &T) -> (String, serde_json::Value) {
    let value = serde_json::to_value(record).expect("schema record should serialize");
    let key = value
        .get("key")
        .and_then(serde_json::Value::as_str)
        .expect("schema record must contain a string key")
        .to_string();
    (key, value)
}

fn format_manifest_diff(diff: &ManifestDiff) -> String {
    let mut lines = Vec::new();
    if !diff.added.is_empty() {
        lines.push(format!(" added={}", diff.added.join(", ")));
    }
    if !diff.removed.is_empty() {
        lines.push(format!(" removed={}", diff.removed.join(", ")));
    }
    if !diff.changed.is_empty() {
        lines.push(format!(" changed={}", diff.changed.join(", ")));
    }
    if lines.is_empty() {
        " <none>".to_string()
    } else {
        lines.join("")
    }
}

fn merge_diffs(diffs: &[ManifestDiff]) -> ManifestDiff {
    let mut merged = ManifestDiff::default();
    for diff in diffs {
        merged.added.extend(diff.added.iter().cloned());
        merged.removed.extend(diff.removed.iter().cloned());
        merged.changed.extend(diff.changed.iter().cloned());
    }
    merged.added.sort();
    merged.removed.sort();
    merged.changed.sort();
    merged
}
