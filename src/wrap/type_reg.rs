use std::collections::{BTreeSet, HashMap};
use std::fmt::Debug;
use std::hash::Hash;
use std::marker::PhantomData;

use derive_more::Deref;
use egglog::{
    EGraph, Term, TermDag, TermId,
    ast::{Command, Literal, Parser, RustSpan, Schema, Span, Subdatatypes, Variant},
    prelude::BaseSort,
    sort::{Q, Z},
    span, var,
};
use serde::{Deserialize, Serialize, de::DeserializeOwned};

use crate::wrap::{
    BindingNames, EgglogEnumVariantTy, FromIndexedValues, FromPlainValues, PatRecSgl, PatVars,
    TermToNode, ToStrArcSort, Value,
};

pub trait EgglogContainerTy: EgglogTy {
    type EleTy: EgglogTy;
}
pub trait EgglogMultiConTy: EgglogTy {
    const CONSTRUCTORS: TyConstructors;
}
impl EgglogTy for i64 {
    const TY_NAME: &'static str = "i64";
    const TY_NAME_LOWER: &'static str = "i64";
    type Valued = Value<Self>;
    type EnumVariantMarker = ();
}
impl EgglogTy for f64 {
    const TY_NAME: &'static str = "f64";
    const TY_NAME_LOWER: &'static str = "f64";
    type Valued = Value<Self>;
    type EnumVariantMarker = ();
}
impl EgglogTy for bool {
    const TY_NAME: &'static str = "bool";
    const TY_NAME_LOWER: &'static str = "bool";
    type Valued = Value<Self>;
    type EnumVariantMarker = ();
}
impl EgglogTy for String {
    const TY_NAME: &'static str = "String";
    const TY_NAME_LOWER: &'static str = "string";
    type Valued = Value<Self>;
    type EnumVariantMarker = ();
}
impl EgglogTy for &'static str {
    const TY_NAME: &'static str = "&'static str";
    const TY_NAME_LOWER: &'static str = "&'static str";
    type Valued = Value<Self>;
    type EnumVariantMarker = ();
}
/// basic type only need default [`EgglogTy::Valued`]
/// while for EnumTy they need to specify ValuedVars when pattern recognized to be values
pub trait EgglogTy: 'static {
    const TY_NAME: &'static str;
    const TY_NAME_LOWER: &'static str;
    type Valued: FromPlainValues + FromIndexedValues;
    type EnumVariantMarker: EgglogEnumVariantTy;
    fn get_arc_sort(egraph: &EGraph) -> egglog::ArcSort {
        egraph
            .get_sort_by_name(Self::TY_NAME)
            .expect("sort should be registered before get")
            .clone()
    }
}

pub const EGGPLANT_TIMESTAMP_FUNCTION_PREFIX: &str = "__eggplant_timestamp_";
pub const EGGPLANT_TIMESTAMP_COUNTER_FUNCTION: &str = "__eggplant_timestamp_counter";

pub fn eggplant_timestamp_function_name(sort_name: &str) -> String {
    let escaped = sort_name
        .chars()
        .map(|ch| {
            if ch.is_ascii_alphanumeric() || ch == '_' {
                ch
            } else {
                '_'
            }
        })
        .collect::<String>();
    format!("{EGGPLANT_TIMESTAMP_FUNCTION_PREFIX}{escaped}")
}

pub fn is_eggplant_timestamp_function(function_name: &str) -> bool {
    function_name == EGGPLANT_TIMESTAMP_COUNTER_FUNCTION
        || function_name.starts_with(EGGPLANT_TIMESTAMP_FUNCTION_PREFIX)
}

impl<T: EgglogTy + ToStrArcSort + BindingNames, PR: PatRecSgl> PatVars<PR> for T
where
    T::Valued: crate::wrap::DecodeWithPlanMetas<PR>,
{
    type Valued = T::Valued;
    fn metas_iter(&self) -> impl Iterator<Item = PR::MetaTy> {
        std::iter::empty()
    }
}
impl<T: EgglogTy> ToStrArcSort for T {
    fn to_str_arcsort(&self, _egraph: &egglog::EGraph) -> Vec<(super::VarName, egglog::ArcSort)> {
        todo!()
    }
}
impl EgglogTy for Q {
    // egglog base sort name (see `egglog::sort::BigRatSort`).
    const TY_NAME: &'static str = "BigRat";
    const TY_NAME_LOWER: &'static str = "big_rational";
    type Valued = Value<Self>;
    type EnumVariantMarker = ();
}
impl EgglogTy for Z {
    // egglog base sort name (see `egglog::sort::BigIntSort`).
    const TY_NAME: &'static str = "BigInt";
    const TY_NAME_LOWER: &'static str = "big_int";
    type Valued = Value<Self>;
    type EnumVariantMarker = ();
}

#[derive(Deref, Debug)]
pub struct TyConstructors(pub &'static [TyConstructor]);
pub struct TySortString(pub &'static str);
pub struct FuncSortString(pub &'static str);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum SchemaFieldKind {
    Base,
    Complex,
    Container,
}
#[derive(Debug)]
pub struct TyConstructor {
    pub cons_name: &'static str,
    pub input: &'static [&'static str],
    pub input_field_names: &'static [&'static str],
    pub input_field_kinds: &'static [SchemaFieldKind],
    pub output: &'static str,
    pub cost: Option<u64>,
    pub unextractable: bool,
    pub display_template: Option<&'static str>,
    pub typst_template: Option<&'static str>,
    pub precedence: u16,
    pub term_to_node: TermToNode,
}
pub struct UserBaseSort {
    pub name: &'static str,
    pub sort_insert_fn: fn(&mut EGraph),
    pub persisted_snapshot_restore_hook: Option<&'static dyn PersistedSnapshotUserBaseSortHook>,
}
pub struct UserContainerSort {
    pub sort_insert_fn: fn(&mut EGraph),
}

pub trait PersistedSnapshotUserBaseSortHook: Send + Sync {
    fn capability_label(&self) -> &'static str;
    fn export_machine_value(
        &self,
        egraph: &EGraph,
        value: egglog::Value,
    ) -> Option<serde_json::Value>;
    fn restore_machine_value(
        &self,
        ctx: &mut egglog::prelude::RustRuleContext<'_, '_>,
        machine_value: &serde_json::Value,
    ) -> Result<egglog::Value, String>;
}

pub struct PersistedSnapshotUserBaseSortHookRegistration {
    pub name: &'static str,
    pub hook: &'static dyn PersistedSnapshotUserBaseSortHook,
}

impl PersistedSnapshotUserBaseSortHookRegistration {
    pub const fn new(
        name: &'static str,
        hook: &'static dyn PersistedSnapshotUserBaseSortHook,
    ) -> Self {
        Self { name, hook }
    }
}

pub struct SerdeJsonUserBaseSortHook<T> {
    capability_label: &'static str,
    _marker: PhantomData<fn() -> T>,
}

impl<T> SerdeJsonUserBaseSortHook<T> {
    pub const fn new(capability_label: &'static str) -> Self {
        Self {
            capability_label,
            _marker: PhantomData,
        }
    }
}

impl<T> PersistedSnapshotUserBaseSortHook for SerdeJsonUserBaseSortHook<T>
where
    T: Clone + Hash + Eq + Debug + Send + Sync + Serialize + DeserializeOwned + 'static,
{
    fn capability_label(&self) -> &'static str {
        self.capability_label
    }

    fn export_machine_value(
        &self,
        egraph: &EGraph,
        value: egglog::Value,
    ) -> Option<serde_json::Value> {
        let value = egraph.value_to_base::<egglog::sort::Boxed<T>>(value);
        serde_json::to_value(&value.0).ok()
    }

    fn restore_machine_value(
        &self,
        ctx: &mut egglog::prelude::RustRuleContext<'_, '_>,
        machine_value: &serde_json::Value,
    ) -> Result<egglog::Value, String> {
        let decoded =
            serde_json::from_value::<T>(machine_value.clone()).map_err(|err| err.to_string())?;
        Ok(ctx.base_to_value(egglog::sort::Boxed::new(decoded)))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PersistedSnapshotUserBaseSortSupport {
    RegisteredWithoutHook,
    RegisteredWithHook,
}

pub fn user_base_sort_restore_support(
    sort_name: &str,
) -> Option<PersistedSnapshotUserBaseSortSupport> {
    if user_base_sort_restore_hook(sort_name).is_some() {
        return Some(PersistedSnapshotUserBaseSortSupport::RegisteredWithHook);
    }
    inventory::iter::<UserBaseSort>
        .into_iter()
        .find(|sort| sort.name == sort_name)
        .map(|_| PersistedSnapshotUserBaseSortSupport::RegisteredWithoutHook)
}

pub fn user_base_sort_restore_hook(
    sort_name: &str,
) -> Option<&'static dyn PersistedSnapshotUserBaseSortHook> {
    if let Some(hook) = inventory::iter::<PersistedSnapshotUserBaseSortHookRegistration>
        .into_iter()
        .find(|registration| registration.name == sort_name)
        .map(|registration| registration.hook)
    {
        return Some(hook);
    }
    inventory::iter::<UserBaseSort>
        .into_iter()
        .find(|sort| sort.name == sort_name)
        .and_then(|sort| sort.persisted_snapshot_restore_hook)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DslFieldKind {
    Base,
    UserBase,
    Complex,
    Container,
}

#[derive(Debug)]
pub struct DslFieldDecl {
    pub name: &'static str,
    pub ty: &'static str,
    pub kind: DslFieldKind,
}

#[derive(Debug)]
pub struct DslVariantDecl {
    pub owner_ty: &'static str,
    pub variant_name: &'static str,
    pub fields: &'static [DslFieldDecl],
    pub display_template: Option<&'static str>,
    pub typst_template: Option<&'static str>,
    pub precedence: u16,
}

// collect all sorts into inventory, so that we could send the definitions of types.
inventory::collect!(Decl);
inventory::collect!(UserBaseSort);
inventory::collect!(UserContainerSort);
inventory::collect!(DslVariantDecl);
inventory::collect!(PersistedSnapshotUserBaseSortHookRegistration);

#[derive(Debug)]
pub enum Decl {
    EgglogMultiConTy {
        name: &'static str,
        cons: &'static TyConstructors,
    },
    EgglogContainerTy {
        name: &'static str,
        ele_ty_name: &'static str,
        constructor_str: &'static str,
        ty_str: &'static str,
        term_to_node: TermToNode,
    },
    EgglogFuncTy {
        name: &'static str,
        input: &'static [&'static str],
        output: &'static str,
        /// `None` means `:no-merge`. Otherwise this is the merge function name (e.g. `"new"`).
        merge: Option<&'static str>,
        hidden: bool,
        let_binding: bool,
        typst_template: Option<&'static str>,
        precedence: u16,
    },
    EgglogRelationTy {
        name: &'static str,
        input: &'static [&'static str],
        typst_template: Option<&'static str>,
        precedence: u16,
    },
    EgglogRule {
        name: &'static str,
        input: &'static [&'static str],
        output: &'static str,
    },
}

#[derive(Debug)]
pub struct EgglogTypeRegistry {
    enum_node_fns_map: HashMap<&'static str, TermToNode>,
    variant2type_map: HashMap<&'static str, &'static str>,
    container_node_fns_map: HashMap<(&'static str, &'static str), TermToNode>,
}
impl EgglogTypeRegistry {
    fn normalize_ty_name(ty: &str) -> String {
        match ty {
            "Q" => <Q as EgglogTy>::TY_NAME.to_string(),
            "Z" => <Z as EgglogTy>::TY_NAME.to_string(),
            _ => ty.to_string(),
        }
    }

    // Upstream egglog proof encoding cannot register sorts declared through a presort
    // (Set/Vec/Map/etc.), and it also rejects any datatype that depends on one of those
    // sorts. We compute that unsupported closure up front so proof-mode registration can
    // skip the whole invalid subtree before typechecking runs.
    fn collect_unsupported_proof_sorts() -> BTreeSet<String> {
        let mut unsupported = BTreeSet::new();
        for decl in inventory::iter::<Decl> {
            if let Decl::EgglogContainerTy { name, .. } = decl {
                unsupported.insert(Self::normalize_ty_name(name));
            }
        }

        loop {
            let mut changed = false;
            for decl in inventory::iter::<Decl> {
                let Decl::EgglogMultiConTy { name, cons } = decl else {
                    continue;
                };
                let name = Self::normalize_ty_name(name);
                if unsupported.contains(&name) {
                    continue;
                }
                let depends_on_unsupported = cons.iter().any(|con| {
                    con.input
                        .iter()
                        .map(|ty| Self::normalize_ty_name(ty))
                        .any(|ty| unsupported.contains(&ty))
                });
                if depends_on_unsupported {
                    unsupported.insert(name);
                    changed = true;
                }
            }
            if !changed {
                break;
            }
        }

        unsupported
    }

    fn sort_decl_supported(name: &str, unsupported: &BTreeSet<String>) -> bool {
        // Normalize aliases like Q/Z before checking so the filter matches egglog's
        // canonical sort names.
        !unsupported.contains(&Self::normalize_ty_name(name))
    }

    fn field_types_supported(types: &[&str], unsupported: &BTreeSet<String>) -> bool {
        types
            .iter()
            .map(|ty| Self::normalize_ty_name(ty))
            .all(|ty| !unsupported.contains(&ty))
    }

    pub fn new_with_inventory() -> Self {
        let (enum_node_fns_map, variant2type_map) = Self::collect_enum_fns();
        let container_node_fns_map = Self::collect_container_fns();
        log::debug!("container node:{:?}", container_node_fns_map);
        Self {
            enum_node_fns_map,
            container_node_fns_map,
            variant2type_map,
        }
    }
    pub fn collect_enum_fns() -> (
        HashMap<&'static str, TermToNode>,
        HashMap<&'static str, &'static str>,
    ) {
        let mut fns_map = HashMap::new();
        let mut variant2type_map = HashMap::new();
        inventory::iter::<Decl>
            .into_iter()
            .for_each(|decl| match decl {
                Decl::EgglogMultiConTy { name, cons } => cons.iter().for_each(|con| {
                    fns_map.insert(con.cons_name, con.term_to_node);
                    variant2type_map.insert(con.cons_name, *name);
                }),
                _ => {}
            });
        (fns_map, variant2type_map)
    }
    pub fn collect_container_fns() -> HashMap<(&'static str, &'static str), TermToNode> {
        let mut map = HashMap::new();
        inventory::iter::<Decl>
            .into_iter()
            .for_each(|decl| match *decl {
                Decl::EgglogContainerTy {
                    name: _,
                    ele_ty_name,
                    constructor_str,
                    term_to_node,
                    ty_str: _,
                } => {
                    map.insert((ele_ty_name, constructor_str), term_to_node);
                }
                _ => {}
            });
        map
    }
    fn collect_type_defs_impl(proof_mode: bool) -> Vec<Command> {
        let mut commands = vec![];
        let mut timestamp_sorts = BTreeSet::<String>::new();
        // Proof mode registers only the schema that egglog's proof encoder accepts.
        // The normal path keeps the full inventory, including container presorts and
        // other declarations that proof mode cannot typecheck.
        let unsupported_sorts = if proof_mode {
            Self::collect_unsupported_proof_sorts()
        } else {
            BTreeSet::new()
        };
        // split decls to avoid undefined sort
        let mut types = Vec::<(Span, String, Subdatatypes)>::new();
        for decl in inventory::iter::<Decl> {
            match decl {
                Decl::EgglogMultiConTy { name, cons } => {
                    if proof_mode && !Self::sort_decl_supported(name, &unsupported_sorts) {
                        continue;
                    }
                    // Keep only datatypes whose constructors stay entirely inside the
                    // proof-safe sort subset.
                    timestamp_sorts.insert(Self::normalize_ty_name(name));
                    types.push((
                        span!(),
                        name.to_string(),
                        Subdatatypes::Variants(
                            cons.iter()
                                .map(|x| Variant {
                                    span: span!(),
                                    name: x.cons_name.to_string(),
                                    types: x
                                        .input
                                        .iter()
                                        .map(|ty| Self::normalize_ty_name(ty))
                                        .collect(),
                                    cost: x.cost,
                                    unextractable: x.unextractable,
                                })
                                .collect(),
                        ),
                    ));
                }
                Decl::EgglogContainerTy {
                    name,
                    ele_ty_name,
                    constructor_str: _,
                    term_to_node: _,
                    ty_str,
                } => {
                    if proof_mode {
                        // Container sorts are represented as presorts in egglog, and proof
                        // encoding rejects presort-backed sorts entirely.
                        continue;
                    }
                    timestamp_sorts.insert(Self::normalize_ty_name(name));
                    let ele_ty = ele_ty_name.to_owned();
                    let ele = var!(ele_ty);
                    types.push((
                        span!(),
                        name.to_string(),
                        Subdatatypes::NewSort(ty_str.to_string(), vec![ele]),
                    ));
                }
                _ => {
                    // do nothing
                }
            }
        }
        if !types.is_empty() {
            commands.push(Command::Datatypes {
                span: span!(),
                datatypes: types,
            });
        }
        let mut parser = Parser::default();
        let timestamp_merge = parser
            .get_expr_from_string(None, "(max old new)")
            .expect("internal timestamp merge expression should parse");
        commands.push(Command::Function {
            span: span!(),
            name: EGGPLANT_TIMESTAMP_COUNTER_FUNCTION.to_string(),
            schema: Schema {
                input: Vec::new(),
                output: <i64 as EgglogTy>::TY_NAME.to_string(),
            },
            merge: Some(timestamp_merge.clone()),
            hidden: true,
            let_binding: false,
            term_constructor: None,
            unextractable: false,
        });
        for sort_name in timestamp_sorts {
            commands.push(Command::Function {
                span: span!(),
                name: eggplant_timestamp_function_name(&sort_name),
                schema: Schema {
                    input: vec![sort_name],
                    output: <i64 as EgglogTy>::TY_NAME.to_string(),
                },
                merge: Some(timestamp_merge.clone()),
                hidden: true,
                let_binding: false,
                term_constructor: None,
                unextractable: false,
            });
        }
        for decl in inventory::iter::<Decl> {
            match decl {
                Decl::EgglogFuncTy {
                    name,
                    input,
                    output,
                    merge,
                    hidden,
                    let_binding,
                    ..
                } => {
                    if proof_mode
                        && (merge.is_none()
                            || !Self::field_types_supported(input, &unsupported_sorts)
                            || !Self::sort_decl_supported(output, &unsupported_sorts))
                    {
                        // Proof mode requires a merge function for every non-global
                        // function, and all argument / result sorts must already be in the
                        // proof-safe subset.
                        continue;
                    }
                    commands.push(Command::Function {
                        span: span!(),
                        name: name.to_string(),
                        schema: Schema {
                            input: input.iter().map(|ty| Self::normalize_ty_name(ty)).collect(),
                            output: Self::normalize_ty_name(output),
                        },
                        merge: merge.map(|m| {
                            parser.get_expr_from_string(None, m).unwrap_or_else(|err| {
                                panic!("failed to parse :merge expr for `{name}`: {err}")
                            })
                        }),
                        hidden: *hidden,
                        let_binding: *let_binding,
                        term_constructor: None,
                        unextractable: false,
                    });
                }
                Decl::EgglogRelationTy { name, input, .. } => {
                    if proof_mode && !Self::field_types_supported(input, &unsupported_sorts) {
                        // Relations are only safe to register if every field sort survives
                        // the proof-mode filter above.
                        continue;
                    }
                    commands.push(Command::Relation {
                        span: span!(),
                        name: name.to_string(),
                        inputs: input.iter().map(|ty| Self::normalize_ty_name(ty)).collect(),
                    });
                }
                _ => {}
            }
        }
        commands
    }

    /// Build the full schema for normal eggplant execution.
    pub fn collect_type_defs() -> Vec<Command> {
        Self::collect_type_defs_impl(false)
    }

    /// Build the proof-safe schema subset for `EGraph::new_with_proofs`.
    ///
    /// This is a compatibility filter for upstream egglog proof encoding; it does not
    /// mean eggplant has full proof support for every schema shape.
    pub fn collect_type_defs_for_proofs() -> Vec<Command> {
        Self::collect_type_defs_impl(true)
    }

    pub fn variant_to_type_name(&self, variant_name: &str) -> Option<&'static str> {
        self.variant2type_map.get(variant_name).copied()
    }
    /// warnning: This funciton returns things like Expr<(),Num> which means you should reform
    /// it into () after
    pub fn get_fn(&self, term_id: TermId, term_dag: &TermDag) -> Option<TermToNode> {
        match term_dag.get(term_id) {
            Term::Lit(_) => None,
            Term::Var(_) => None,
            Term::App(name, items) => self.enum_node_fns_map.get(name.as_str()).or_else(|| {
                self.container_node_fns_map.get(&(
                    match term_dag.get(*items.get(0).unwrap()) {
                        Term::Lit(literal) => match literal {
                            Literal::Int(_) => "i64",
                            Literal::Float(_) => "f64",
                            Literal::String(_) => "string",
                            Literal::Bool(_) => "bool",
                            Literal::Unit => {
                                panic!()
                            }
                        },
                        Term::Var(_) => {
                            panic!()
                        }
                        Term::App(succ_variant, _) => {
                            log::trace!("sub_name {}", succ_variant);
                            let ty = self.variant2type_map.get(succ_variant.as_str()).unwrap();
                            ty
                        }
                    },
                    name.as_str(),
                ))
            }),
        }
        .cloned()
    }
}

impl<T> FromPlainValues for Value<T> {
    fn from_plain_values(values: &mut impl Iterator<Item = egglog::Value>) -> Self {
        Value::new(values.next().unwrap())
    }
}

impl<T> FromIndexedValues for Value<T> {
    fn from_indexed_values(values: &[egglog::Value], value_idx: &mut usize) -> Self {
        let value = values.get(*value_idx).copied().unwrap();
        *value_idx += 1;
        Value::new(value)
    }
}

#[derive(Debug)]
pub struct StaticStrSort;
impl BaseSort for StaticStrSort {
    type Base = &'static str;

    fn name(&self) -> &str {
        "StaticStr"
    }

    fn reconstruct_termdag(
        &self,
        base_values: &egglog::sort::BaseValues,
        value: egglog::Value,
        term_dag: &mut TermDag,
    ) -> TermId {
        let str: &'static str = base_values.unwrap(value);
        term_dag.lit(Literal::String(str.to_string()))
    }
}
