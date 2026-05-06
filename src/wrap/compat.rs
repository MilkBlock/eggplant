use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::sync::{
    atomic::{AtomicUsize, Ordering},
    Mutex, OnceLock,
};

use egglog::prelude::{
    add_ruleset as upstream_add_ruleset, run_ruleset as upstream_run_ruleset,
    rust_rule as upstream_rust_rule,
};
use egglog::{self, ArcSort, CommandOutput, EGraph, Error, SerializeConfig, TermDag, Value};

use crate::wrap::{is_eggplant_timestamp_function, EGGPLANT_TIMESTAMP_COUNTER_FUNCTION};
use crate::wrap::{Decl, EgglogTy};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SchemaSortKind {
    Eq,
    Container,
    Base,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SchemaSortManifest {
    pub name: String,
    pub kind: SchemaSortKind,
    pub inner_sorts: Vec<String>,
    pub unionable: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SchemaFunctionKind {
    Constructor,
    Function,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SchemaFunctionManifest {
    pub key: String,
    pub name: String,
    pub kind: SchemaFunctionKind,
    pub input: Vec<String>,
    pub output: String,
    pub merge: Option<String>,
    pub cost: Option<u64>,
    pub unextractable: bool,
    pub hidden: bool,
    pub let_binding: bool,
    pub term_constructor: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EngineSchemaManifest {
    pub sorts: Vec<SchemaSortManifest>,
    pub functions: Vec<SchemaFunctionManifest>,
}

#[derive(Debug, Clone)]
pub struct RawEGraphNode {
    pub inputs_complex: Vec<Value>,
    pub basics: (),
    pub output: Value,
    pub subsumed: bool,
}

#[derive(Debug, Clone)]
pub struct OwnedFunctionRow {
    pub vals: Vec<Value>,
    pub subsumed: bool,
}

pub type FunctionId = String;
pub type RuleId = usize;

#[derive(Debug, Clone)]
pub struct RustRuleHandle {
    pub outputs: Vec<CommandOutput>,
    pub rule_id: RuleId,
    pub generated_rule_name: String,
}

#[derive(Debug, Clone)]
struct CompatEGraphState {
    rulesets: BTreeSet<String>,
    timestamp: u32,
}

impl Default for CompatEGraphState {
    fn default() -> Self {
        Self {
            rulesets: BTreeSet::new(),
            timestamp: 1,
        }
    }
}

static REGISTRY: OnceLock<Mutex<HashMap<usize, CompatEGraphState>>> = OnceLock::new();
static NEXT_RULE_ID: AtomicUsize = AtomicUsize::new(1);
static NEXT_EPHEMERAL_RULESET_ID: AtomicUsize = AtomicUsize::new(1);

fn registry() -> &'static Mutex<HashMap<usize, CompatEGraphState>> {
    REGISTRY.get_or_init(|| Mutex::new(HashMap::new()))
}

fn registry_key(egraph: &EGraph) -> usize {
    egraph as *const EGraph as usize
}

fn with_state_mut<R>(egraph: &EGraph, f: impl FnOnce(&mut CompatEGraphState) -> R) -> R {
    let key = registry_key(egraph);
    let mut registry = registry().lock().unwrap();
    let state = registry.entry(key).or_default();
    f(state)
}

fn bump_timestamp(egraph: &EGraph) {
    with_state_mut(egraph, |state| {
        state.timestamp = state.timestamp.saturating_add(1);
    });
}

fn next_rule_id() -> RuleId {
    NEXT_RULE_ID.fetch_add(1, Ordering::Relaxed)
}

pub fn clear_compat_state(egraph: &EGraph) {
    registry().lock().unwrap().remove(&registry_key(egraph));
}

pub fn add_ruleset(egraph: &mut EGraph, ruleset: &str) -> Result<Vec<CommandOutput>, Error> {
    let outputs = upstream_add_ruleset(egraph, ruleset)?;
    with_state_mut(egraph, |state| {
        state.rulesets.insert(ruleset.to_string());
        state.timestamp = state.timestamp.saturating_add(1);
    });
    Ok(outputs)
}

pub fn run_ruleset(egraph: &mut EGraph, ruleset: &str) -> Result<Vec<CommandOutput>, Error> {
    let outputs = upstream_run_ruleset(egraph, ruleset)?;
    bump_timestamp(egraph);
    Ok(outputs)
}

pub fn rust_rule(
    egraph: &mut EGraph,
    rule_name: &str,
    ruleset: &str,
    vars: &[(&str, ArcSort)],
    facts: egglog::ast::Facts<String, String>,
    func: impl Fn(&mut egglog::prelude::RustRuleContext<'_, '_>, &[Value]) -> Option<()>
        + Clone
        + Send
        + Sync
        + 'static,
) -> Result<Vec<CommandOutput>, Error> {
    Ok(rust_rule_with_metadata(egraph, rule_name, ruleset, vars, facts, func)?.outputs)
}

pub fn rust_rule_with_metadata(
    egraph: &mut EGraph,
    rule_name: &str,
    ruleset: &str,
    vars: &[(&str, ArcSort)],
    facts: egglog::ast::Facts<String, String>,
    func: impl Fn(&mut egglog::prelude::RustRuleContext<'_, '_>, &[Value]) -> Option<()>
        + Clone
        + Send
        + Sync
        + 'static,
) -> Result<RustRuleHandle, Error> {
    let outputs = upstream_rust_rule(egraph, rule_name, ruleset, vars, facts, func)?;
    with_state_mut(egraph, |state| {
        state.rulesets.insert(ruleset.to_string());
        state.timestamp = state.timestamp.saturating_add(1);
    });
    Ok(RustRuleHandle {
        outputs,
        rule_id: next_rule_id(),
        generated_rule_name: rule_name.to_string(),
    })
}

pub fn run_ephemeral_rust_rule(
    egraph: &mut EGraph,
    rule_name: &str,
    vars: &[(&str, ArcSort)],
    facts: egglog::ast::Facts<String, String>,
    func: impl Fn(&mut egglog::prelude::RustRuleContext<'_, '_>, &[Value]) -> Option<()>
        + Clone
        + Send
        + Sync
        + 'static,
) -> Result<Vec<CommandOutput>, Error> {
    let ruleset = format!(
        "ephemeral_rust_rule_ruleset_{}",
        NEXT_EPHEMERAL_RULESET_ID.fetch_add(1, Ordering::Relaxed)
    );
    add_ruleset(egraph, &ruleset)?;
    let mut outputs = rust_rule(egraph, rule_name, &ruleset, vars, facts, func)?;
    outputs.extend(run_ruleset(egraph, &ruleset)?);
    registry()
        .lock()
        .unwrap()
        .entry(registry_key(egraph))
        .and_modify(|state| {
            state.rulesets.remove(&ruleset);
        });
    Ok(outputs)
}

pub trait EgglogCompatExt {
    fn schema_manifest(&self) -> EngineSchemaManifest;
    fn serialize_raw(&self, config: SerializeConfig) -> HashMap<String, Vec<RawEGraphNode>>;
    fn function_rows(&self, func_name: &str) -> Vec<OwnedFunctionRow>;
    fn get_all_rulesets(&self) -> Vec<(String, Vec<RuleId>)>;
    fn base_value_print(&self, base_value: Value, sort: &ArcSort) -> String;
    fn current_timestamp(&self) -> u32;
    fn proof_view_name(&self, func_name: &str) -> Result<String, Error>;
    fn proof_view_proof_name(&self, func_name: &str) -> Result<String, Error>;
    fn prove_values_equal_pretty(
        &mut self,
        sort_name: &str,
        lhs: Value,
        rhs: Value,
    ) -> Result<String, Error>;
    fn prove_value_pretty(&mut self, sort_name: &str, value: Value) -> Result<String, Error>;
    fn constrain_rule_atom_timestamp_range(
        &mut self,
        rule: RuleId,
        atom_index: usize,
        min_inclusive: Option<u32>,
        max_exclusive: Option<u32>,
    ) -> Result<(), Error>;
}

impl EgglogCompatExt for EGraph {
    fn schema_manifest(&self) -> EngineSchemaManifest {
        let mut sorts = self
            .get_arcsorts_by(|_| true)
            .into_iter()
            .map(|sort| {
                let kind = if sort.is_container_sort() {
                    SchemaSortKind::Container
                } else if sort.is_eq_sort() {
                    SchemaSortKind::Eq
                } else {
                    SchemaSortKind::Base
                };
                let inner_sorts = if sort.is_container_sort() {
                    sort.inner_sorts()
                        .into_iter()
                        .map(|inner| inner.name().to_string())
                        .collect()
                } else {
                    Vec::new()
                };
                SchemaSortManifest {
                    name: sort.name().to_string(),
                    kind,
                    inner_sorts,
                    unionable: sort.is_eq_sort(),
                }
            })
            .collect::<Vec<_>>();
        sorts.sort_by(|a, b| a.name.cmp(&b.name));

        let mut constructor_meta = BTreeMap::<String, SchemaFunctionManifest>::new();
        let mut function_meta = BTreeMap::<String, SchemaFunctionManifest>::new();
        for decl in crate::inventory::iter::<Decl> {
            match *decl {
                Decl::EgglogMultiConTy { cons, .. } => {
                    for con in cons.iter() {
                        let input = con
                            .input
                            .iter()
                            .map(|ty| normalize_ty_name(ty))
                            .collect::<Vec<_>>();
                        let output = normalize_ty_name(con.output);
                        let key = function_key(con.cons_name, &input, &output);
                        constructor_meta.insert(
                            key.clone(),
                            SchemaFunctionManifest {
                                key,
                                name: con.cons_name.to_string(),
                                kind: SchemaFunctionKind::Constructor,
                                input,
                                output,
                                merge: None,
                                cost: con.cost,
                                unextractable: con.unextractable,
                                hidden: false,
                                let_binding: false,
                                term_constructor: None,
                            },
                        );
                    }
                }
                Decl::EgglogFuncTy {
                    name,
                    input,
                    output,
                    merge,
                    hidden,
                    let_binding,
                    ..
                } => {
                    let input = input
                        .iter()
                        .map(|ty| normalize_ty_name(ty))
                        .collect::<Vec<_>>();
                    let output = normalize_ty_name(output);
                    let key = function_key(name, &input, &output);
                    function_meta.insert(
                        key.clone(),
                        SchemaFunctionManifest {
                            key,
                            name: name.to_string(),
                            kind: SchemaFunctionKind::Function,
                            input,
                            output,
                            merge: merge.map(str::to_string),
                            cost: None,
                            unextractable: false,
                            hidden,
                            let_binding,
                            term_constructor: None,
                        },
                    );
                }
                _ => {}
            }
        }

        let mut functions = Vec::new();
        for name in self.get_function_names() {
            if is_eggplant_timestamp_function(&name) {
                continue;
            }
            let Some(function) = self.get_function(&name) else {
                continue;
            };
            let input = function
                .schema()
                .input
                .iter()
                .map(|sort| sort.name().to_string())
                .collect::<Vec<_>>();
            let output = function.schema().output.name().to_string();
            let key = function_key(&name, &input, &output);
            if let Some(meta) = constructor_meta.get(&key).cloned() {
                functions.push(meta);
                continue;
            }
            if let Some(meta) = function_meta.get(&key).cloned() {
                functions.push(meta);
                continue;
            }
            functions.push(SchemaFunctionManifest {
                key,
                name,
                kind: SchemaFunctionKind::Function,
                input,
                output,
                merge: None,
                cost: None,
                unextractable: false,
                hidden: false,
                let_binding: function.is_let_binding(),
                term_constructor: None,
            });
        }
        functions.sort_by(|a, b| a.key.cmp(&b.key));

        EngineSchemaManifest { sorts, functions }
    }

    fn serialize_raw(&self, config: SerializeConfig) -> HashMap<String, Vec<RawEGraphNode>> {
        let mut out = HashMap::new();
        let max_calls_per_function = config.max_calls_per_function.unwrap_or(usize::MAX);
        for name in self.get_function_names() {
            if is_eggplant_timestamp_function(&name) {
                continue;
            }
            let mut rows = Vec::new();
            if self
                .function_for_each(&name, |row| {
                    if rows.len() >= max_calls_per_function {
                        return;
                    }
                    let (output, inputs) =
                        row.vals.split_last().expect("function row has no output");
                    rows.push(RawEGraphNode {
                        inputs_complex: inputs.to_vec(),
                        basics: (),
                        output: *output,
                        subsumed: row.subsumed,
                    });
                })
                .is_ok()
            {
                out.insert(name, rows);
            }
        }
        out
    }

    fn function_rows(&self, func_name: &str) -> Vec<OwnedFunctionRow> {
        if is_eggplant_timestamp_function(func_name) {
            return Vec::new();
        }
        let resolved_name = self
            .proof_view_name(func_name)
            .unwrap_or_else(|_| func_name.to_owned());
        let mut out = Vec::new();
        let _ = self.function_for_each(&resolved_name, |row| {
            out.push(OwnedFunctionRow {
                vals: row.vals.to_vec(),
                subsumed: row.subsumed,
            });
        });
        out
    }

    fn get_all_rulesets(&self) -> Vec<(String, Vec<RuleId>)> {
        let state = registry()
            .lock()
            .unwrap()
            .get(&registry_key(self))
            .cloned()
            .unwrap_or_default();
        state
            .rulesets
            .into_iter()
            .map(|name| (name, Vec::new()))
            .collect()
    }

    fn base_value_print(&self, base_value: Value, sort: &ArcSort) -> String {
        match sort.name() {
            "i64" => format!("{:?}", self.value_to_base::<i64>(base_value)),
            "f64" => format!("{:?}", self.value_to_base::<egglog::sort::F>(base_value)),
            "bool" => format!("{:?}", self.value_to_base::<bool>(base_value)),
            "String" => format!("{:?}", self.value_to_base::<egglog::sort::S>(base_value)),
            "BigInt" | "Z" => format!("{:?}", self.value_to_base::<egglog::sort::Z>(base_value)),
            "BigRat" | "Q" => format!("{:?}", self.value_to_base::<egglog::sort::Q>(base_value)),
            "Unit" | "()" => format!("{:?}", self.value_to_base::<()>(base_value)),
            _ => format!("{base_value:?}"),
        }
    }

    fn current_timestamp(&self) -> u32 {
        let counter_name = self
            .proof_view_name(EGGPLANT_TIMESTAMP_COUNTER_FUNCTION)
            .unwrap_or_else(|_| EGGPLANT_TIMESTAMP_COUNTER_FUNCTION.to_owned());
        let mut current: Option<i64> = None;
        let _ = self.function_for_each(&counter_name, |row| {
            if let Some(value) = row.vals.first() {
                let ts = self.value_to_base::<i64>(*value);
                current = Some(current.map_or(ts, |cur| cur.max(ts)));
            }
        });
        current.map(|ts| ts.max(1) as u32).unwrap_or_else(|| {
            registry()
                .lock()
                .unwrap()
                .get(&registry_key(self))
                .map(|state| state.timestamp)
                .unwrap_or(1)
        })
    }

    fn proof_view_name(&self, func_name: &str) -> Result<String, Error> {
        EGraph::proof_view_name(self, func_name)
    }

    fn proof_view_proof_name(&self, func_name: &str) -> Result<String, Error> {
        EGraph::proof_view_proof_name(self, func_name)
    }

    fn prove_values_equal_pretty(
        &mut self,
        sort_name: &str,
        lhs: Value,
        rhs: Value,
    ) -> Result<String, Error> {
        if !self.are_proofs_enabled() {
            return Err(Error::BackendError(
                "prove_values_equal_pretty requires EGraph::new_with_proofs".into(),
            ));
        }
        let sort = self
            .get_sort_by_name(sort_name)
            .ok_or_else(|| Error::BackendError(format!("unknown sort {sort_name}")))?
            .clone();
        if !sort.is_eq_sort() {
            return Err(Error::BackendError(format!(
                "prove_values_equal_pretty requires eq sort, got {}",
                sort.name()
            )));
        }
        let mut termdag = TermDag::default();
        let mut cache = HashMap::new();
        let mut active = HashSet::new();
        let lhs_term = exact_value_term(self, &mut termdag, &sort, lhs, &mut cache, &mut active)?;
        let rhs_term = exact_value_term(self, &mut termdag, &sort, rhs, &mut cache, &mut active)?;
        let lhs_src = termdag.to_string(lhs_term);
        let rhs_src = termdag.to_string(rhs_term);
        let program = format!("(prove (= {lhs_src} {rhs_src}))");
        self.push();
        let result = (|| {
            let outputs = self.parse_and_run_program(None, &program)?;
            outputs
                .into_iter()
                .find_map(|output| match output {
                    CommandOutput::ProveExists {
                        proof_store,
                        proof_id,
                    } => Some(proof_store.proof_to_string(proof_id)),
                    _ => None,
                })
                .ok_or_else(|| Error::BackendError("prove command did not produce a proof".into()))
        })();
        let pop_result = self.pop();
        match (result, pop_result) {
            (Ok(proof), Ok(())) => Ok(proof),
            (Err(err), _) => Err(err),
            (Ok(_), Err(err)) => Err(err),
        }
    }

    fn prove_value_pretty(&mut self, sort_name: &str, value: Value) -> Result<String, Error> {
        if !self.are_proofs_enabled() {
            return Err(Error::BackendError(
                "prove_value_pretty requires EGraph::new_with_proofs".into(),
            ));
        }
        let sort = self
            .get_sort_by_name(sort_name)
            .ok_or_else(|| Error::BackendError(format!("unknown sort {sort_name}")))?
            .clone();
        if !sort.is_eq_sort() {
            return Err(Error::BackendError(format!(
                "prove_value_pretty requires eq sort, got {}",
                sort.name()
            )));
        }
        let mut termdag = TermDag::default();
        let mut cache = HashMap::new();
        let mut active = HashSet::new();
        let term = exact_value_term(self, &mut termdag, &sort, value, &mut cache, &mut active)?;
        let term_src = termdag.to_string(term);
        let program = format!("(prove {term_src})");
        self.push();
        let result = (|| {
            let outputs = self.parse_and_run_program(None, &program)?;
            outputs
                .into_iter()
                .find_map(|output| match output {
                    CommandOutput::ProveExists {
                        proof_store,
                        proof_id,
                    } => Some(proof_store.proof_to_string(proof_id)),
                    _ => None,
                })
                .ok_or_else(|| Error::BackendError("prove command did not produce a proof".into()))
        })();
        let pop_result = self.pop();
        match (result, pop_result) {
            (Ok(proof), Ok(())) => Ok(proof),
            (Err(err), _) => Err(err),
            (Ok(_), Err(err)) => Err(err),
        }
    }

    fn constrain_rule_atom_timestamp_range(
        &mut self,
        _rule: RuleId,
        _atom_index: usize,
        _min_inclusive: Option<u32>,
        _max_exclusive: Option<u32>,
    ) -> Result<(), Error> {
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct ExactTermKey {
    sort_name: String,
    value: Value,
}

fn exact_value_term(
    egraph: &EGraph,
    termdag: &mut TermDag,
    sort: &ArcSort,
    value: Value,
    cache: &mut HashMap<ExactTermKey, egglog::TermId>,
    active: &mut HashSet<ExactTermKey>,
) -> Result<egglog::TermId, Error> {
    if sort.is_container_sort() {
        return Err(Error::BackendError(format!(
            "exact proof term reconstruction does not support container sort `{}`",
            sort.name()
        )));
    }

    if !sort.is_eq_sort() {
        return exact_base_term(egraph, termdag, sort, value);
    }

    let key = ExactTermKey {
        sort_name: sort.name().to_string(),
        value,
    };
    if let Some(term) = cache.get(&key) {
        return Ok(*term);
    }
    if !active.insert(key.clone()) {
        return Err(Error::BackendError(format!(
            "cycle while reconstructing exact proof term for sort `{}`",
            sort.name()
        )));
    }

    let mut last_error = None;
    let mut func_names = egraph.get_function_names();
    func_names.sort();
    for func_name in func_names {
        if is_eggplant_timestamp_function(&func_name) {
            continue;
        }
        let Some(function) = egraph.get_function(&func_name) else {
            continue;
        };
        if function.schema().output.name() != sort.name() {
            continue;
        }

        let mut matching_rows = Vec::new();
        let _ = egraph.function_for_each(&func_name, |row| {
            let Some(row_output) = row.vals.last().copied() else {
                return;
            };
            if row_output == value && row.vals.len() == function.schema().input.len() + 1 {
                matching_rows.push(row.vals.to_vec());
            }
        });
        matching_rows.sort_by(|a, b| format!("{a:?}").cmp(&format!("{b:?}")));

        for row in matching_rows {
            let mut child_terms = Vec::with_capacity(function.schema().input.len());
            let mut ok = true;
            for (child_value, child_sort) in row
                .iter()
                .take(function.schema().input.len())
                .copied()
                .zip(function.schema().input.iter())
            {
                match exact_value_term(egraph, termdag, child_sort, child_value, cache, active) {
                    Ok(child_term) => child_terms.push(child_term),
                    Err(err) => {
                        last_error = Some(err);
                        ok = false;
                        break;
                    }
                }
            }

            if ok {
                let term = termdag.app(func_name.clone(), child_terms);
                cache.insert(key.clone(), term);
                active.remove(&key);
                return Ok(term);
            }
        }
    }

    active.remove(&key);
    Err(last_error.unwrap_or_else(|| {
        Error::BackendError(format!(
            "could not reconstruct an exact term for sort `{}` and value {value:?}",
            sort.name()
        ))
    }))
}

fn exact_base_term(
    egraph: &EGraph,
    termdag: &mut TermDag,
    sort: &ArcSort,
    value: Value,
) -> Result<egglog::TermId, Error> {
    use egglog::ast::Literal;

    Ok(match sort.name() {
        "i64" => termdag.lit(Literal::Int(egraph.value_to_base::<i64>(value))),
        "bool" => termdag.lit(Literal::Bool(egraph.value_to_base::<bool>(value))),
        "String" => termdag.lit(Literal::String(
            egraph.value_to_base::<egglog::sort::S>(value).0,
        )),
        "f64" => termdag.lit(Literal::Float(
            egraph.value_to_base::<egglog::sort::F>(value).0,
        )),
        "Unit" | "()" => termdag.lit(Literal::Unit),
        "BigInt" | "Z" => {
            let bigint = egraph.value_to_base::<egglog::sort::Z>(value);
            let as_string = termdag.lit(Literal::String(bigint.0.to_string()));
            termdag.app("from-string".to_owned(), vec![as_string])
        }
        "BigRat" | "Q" => {
            let rat = egraph.value_to_base::<egglog::sort::Q>(value);
            let numer_as_string = termdag.lit(Literal::String(rat.numer().to_string()));
            let denom_as_string = termdag.lit(Literal::String(rat.denom().to_string()));
            let numer_term = termdag.app("from-string".to_owned(), vec![numer_as_string]);
            let denom_term = termdag.app("from-string".to_owned(), vec![denom_as_string]);
            termdag.app("bigrat".to_owned(), vec![numer_term, denom_term])
        }
        other => {
            return Err(Error::BackendError(format!(
                "exact proof term reconstruction does not support base sort `{other}`"
            )));
        }
    })
}

fn normalize_ty_name(ty: &str) -> String {
    match ty {
        "Q" => <egglog::sort::Q as EgglogTy>::TY_NAME.to_string(),
        "Z" => <egglog::sort::Z as EgglogTy>::TY_NAME.to_string(),
        _ => ty.to_string(),
    }
}

fn function_key(name: &str, input: &[String], output: &str) -> String {
    format!("{name}({})->{output}", input.join(","))
}
