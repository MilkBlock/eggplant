use crate::wrap::{
    self, BoxedContainer, BoxedValue, EgglogContainerTy, EgglogEnumVariantTy, EgglogNode,
    Insertable, IntoConstraintFact, PatRecSgl, RetypeValue,
};
use crate::wrap::{BoxedBase, EgglogTy, NodeDropperSgl, PatVars, WithPatRecSgl};
use egglog::ContainerValue;
use egglog::ast::{Expr, Fact, ResolvedVar};
use egglog::{
    BaseValue,
    ast::{RustSpan, Span},
    prelude::RustRuleContext,
    sort::{EqSort, Sort},
    span,
};
use egglog_reports::RunReport;
use serde::{Deserialize, Serialize};
use std::cell::{RefCell, UnsafeCell};
use std::collections::{HashMap, HashSet};
use std::marker::PhantomData;
use std::ops::Deref;
use std::panic::Location;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::sync::{Mutex, OnceLock};
use wrap::Value;

pub(crate) fn empty_premise_proofs() -> Arc<[egglog::Value]> {
    static EMPTY: std::sync::OnceLock<Arc<[egglog::Value]>> = std::sync::OnceLock::new();
    Arc::clone(EMPTY.get_or_init(|| Arc::from(Vec::<egglog::Value>::new().into_boxed_slice())))
}

thread_local! {
    pub(crate) static CURRENT_PREMISE_PROOFS: RefCell<Vec<Arc<[egglog::Value]>>> = RefCell::new(Vec::new());
    static CURRENT_ACTION_EFFECT_ID: RefCell<Option<String>> = const { RefCell::new(None) };
}

pub(crate) struct PremiseProofScope;

impl PremiseProofScope {
    pub(crate) fn enter(premise_proofs: Arc<[egglog::Value]>) -> Self {
        CURRENT_PREMISE_PROOFS.with(|cell| cell.borrow_mut().push(premise_proofs));
        PremiseProofScope
    }
}

impl Drop for PremiseProofScope {
    fn drop(&mut self) {
        CURRENT_PREMISE_PROOFS.with(|cell| {
            let _ = cell.borrow_mut().pop();
        });
    }
}

// eggplant rule context is a wrapper of egglog rule context.
// it contains the Tx to which the rule is applied
pub struct PRRuleCtx<'a, 'b, 'c, 'p, PR: PatRecSgl> {
    pub ctx: RuleCtx<'a, 'b, 'c, 'p>,
    _p: PhantomData<PR>,
}
pub struct RuleCtx<'a, 'b, 'c, 'p> {
    pub rule_ctx: UnsafeCell<&'c mut RustRuleContext<'a, 'b, 'p>>,
    hook: RuleHookObj,
}
unsafe impl Send for RuleHookObj {}
unsafe impl Sync for RuleHookObj {}
pub struct RuleHookObj(pub Option<Box<dyn RuleCtxHook>>);
impl Clone for RuleHookObj {
    fn clone(&self) -> Self {
        match &self.0 {
            Some(hook) => RuleHookObj(Some(hook.dyn_clone())),
            None => RuleHookObj(None),
        }
    }
}

pub trait RuleCtxHook {
    fn on_insert(&self, table: &str, key: &[egglog::Value]);
    fn on_union(&self, x: egglog::Value, y: egglog::Value);
    fn on_subsume(&self, table: &str, key: &[egglog::Value]);
    fn on_remove(&self, table: &str, key: &[egglog::Value]);
    fn dyn_clone(&self) -> Box<dyn RuleCtxHook>;
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum ActionSampleEvent {
    Insert {
        event_id: String,
        effect_id: Option<String>,
        table: String,
        key_debug: Vec<String>,
    },
    Union {
        event_id: String,
        effect_id: Option<String>,
        lhs_debug: String,
        rhs_debug: String,
    },
    Subsume {
        event_id: String,
        effect_id: Option<String>,
        table: String,
        key_debug: Vec<String>,
    },
    Remove {
        event_id: String,
        effect_id: Option<String>,
        table: String,
        key_debug: Vec<String>,
    },
    DynamicUnknown {
        event_id: String,
        effect_id: Option<String>,
        reason: String,
    },
}

#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct ActionSampleTrace {
    pub version: u32,
    pub events: Vec<ActionSampleEvent>,
}

#[derive(Debug, Default)]
struct ActionSampleRecorderState {
    next_event_id: u64,
    trace: ActionSampleTrace,
}

#[derive(Clone, Debug, Default)]
pub struct ActionSampleRecorder {
    state: Arc<Mutex<ActionSampleRecorderState>>,
}

impl ActionSampleRecorder {
    pub fn snapshot(&self) -> Vec<ActionSampleEvent> {
        self.state.lock().unwrap().trace.events.clone()
    }

    pub fn trace(&self) -> ActionSampleTrace {
        self.state.lock().unwrap().trace.clone()
    }

    fn push_event(&self, build: impl FnOnce(String, Option<String>) -> ActionSampleEvent) {
        let mut state = self.state.lock().unwrap();
        let event_id = format!("evt_{}", state.next_event_id);
        state.next_event_id += 1;
        let effect_id = current_action_effect_id();
        state.trace.version = 1;
        state.trace.events.push(build(event_id, effect_id));
    }

    pub fn record_dynamic_unknown(&self, reason: impl Into<String>) {
        let reason = reason.into();
        self.push_event(|event_id, effect_id| ActionSampleEvent::DynamicUnknown {
            event_id,
            effect_id,
            reason,
        });
    }
}

impl RuleCtxHook for ActionSampleRecorder {
    fn on_insert(&self, table: &str, key: &[egglog::Value]) {
        let table = table.to_owned();
        let key_debug = key.iter().map(|value| format!("{value:?}")).collect();
        self.push_event(|event_id, effect_id| ActionSampleEvent::Insert {
            event_id,
            effect_id,
            table,
            key_debug,
        });
    }

    fn on_union(&self, x: egglog::Value, y: egglog::Value) {
        let lhs_debug = format!("{x:?}");
        let rhs_debug = format!("{y:?}");
        self.push_event(|event_id, effect_id| ActionSampleEvent::Union {
            event_id,
            effect_id,
            lhs_debug,
            rhs_debug,
        });
    }

    fn on_subsume(&self, table: &str, key: &[egglog::Value]) {
        let table = table.to_owned();
        let key_debug = key.iter().map(|value| format!("{value:?}")).collect();
        self.push_event(|event_id, effect_id| ActionSampleEvent::Subsume {
            event_id,
            effect_id,
            table,
            key_debug,
        });
    }

    fn on_remove(&self, table: &str, key: &[egglog::Value]) {
        let table = table.to_owned();
        let key_debug = key.iter().map(|value| format!("{value:?}")).collect();
        self.push_event(|event_id, effect_id| ActionSampleEvent::Remove {
            event_id,
            effect_id,
            table,
            key_debug,
        });
    }

    fn dyn_clone(&self) -> Box<dyn RuleCtxHook> {
        Box::new(self.clone())
    }
}

pub struct ActionEffectScope {
    previous: Option<String>,
}

impl ActionEffectScope {
    pub fn enter(location: &'static Location<'static>) -> Self {
        let next = resolve_action_effect_id(location);
        let previous = CURRENT_ACTION_EFFECT_ID.with(|cell| cell.replace(next));
        Self { previous }
    }
}

impl Drop for ActionEffectScope {
    fn drop(&mut self) {
        CURRENT_ACTION_EFFECT_ID.with(|cell| {
            let _ = cell.replace(self.previous.take());
        });
    }
}

pub fn current_action_effect_id() -> Option<String> {
    CURRENT_ACTION_EFFECT_ID.with(|cell| cell.borrow().clone())
}

fn resolve_action_effect_id(location: &'static Location<'static>) -> Option<String> {
    let fallback = format!(
        "effect@caller:{}:{}:{}",
        location.file(),
        location.line(),
        location.column()
    );
    let Some(cache) = load_source_cache(location.file()) else {
        return Some(fallback);
    };
    let Some(start) = cache.offset_of(location.line() as usize, location.column() as usize) else {
        return Some(fallback);
    };
    let end = scan_action_call_end(&cache.text, start).unwrap_or(start);
    Some(format!("effect@{start}:{end}"))
}

#[derive(Clone)]
struct SourceCache {
    text: Arc<str>,
    line_starts: Arc<[usize]>,
}

impl SourceCache {
    fn offset_of(&self, line: usize, column: usize) -> Option<usize> {
        let line_start = *self.line_starts.get(line.checked_sub(1)?)?;
        Some(line_start + column.checked_sub(1)?)
    }
}

fn load_source_cache(path: &str) -> Option<SourceCache> {
    static CACHE: OnceLock<Mutex<HashMap<PathBuf, SourceCache>>> = OnceLock::new();
    let cache = CACHE.get_or_init(|| Mutex::new(HashMap::new()));
    let path_buf = Path::new(path).to_path_buf();
    if let Some(existing) = cache.lock().unwrap().get(&path_buf).cloned() {
        return Some(existing);
    }
    let text = std::fs::read_to_string(&path_buf).ok()?;
    let mut line_starts = vec![0usize];
    for (idx, ch) in text.char_indices() {
        if ch == '\n' {
            line_starts.push(idx + 1);
        }
    }
    let entry = SourceCache {
        text: Arc::<str>::from(text),
        line_starts: Arc::from(line_starts.into_boxed_slice()),
    };
    cache.lock().unwrap().insert(path_buf, entry.clone());
    Some(entry)
}

fn scan_action_call_end(text: &str, start: usize) -> Option<usize> {
    let slice = text.get(start..)?;
    let mut depth = 0usize;
    let mut opened = false;
    let mut in_string = false;
    let mut escape = false;
    for (offset, ch) in slice.char_indices() {
        if in_string {
            if escape {
                escape = false;
                continue;
            }
            match ch {
                '\\' => escape = true,
                '"' => in_string = false,
                _ => {}
            }
            continue;
        }
        match ch {
            '"' => in_string = true,
            '(' => {
                depth += 1;
                opened = true;
            }
            ')' => {
                if depth == 0 {
                    return None;
                }
                depth -= 1;
                if opened && depth == 0 {
                    return Some(start + offset + ch.len_utf8());
                }
            }
            _ => {}
        }
    }
    None
}

impl<'a, 'b, 'c, 'p, PR: PatRecSgl> PRRuleCtx<'a, 'b, 'c, 'p, PR> {
    pub fn new(rule_ctx: &'c mut RustRuleContext<'a, 'b, 'p>, hook: RuleHookObj) -> Self {
        Self {
            _p: PhantomData::default(),
            ctx: RuleCtx::new(rule_ctx, hook),
        }
    }
    pub fn devalue<'d, B: BoxedValue, D: RetypeValue<Target = B>>(
        &'d self,
        val: Value<D>,
    ) -> B::Output<'d> {
        self.ctx.devalue(val)
    }
    pub fn intern_base<T: EgglogTy, B: BoxedBase>(&self, base: B) -> wrap::Value<T> {
        self.ctx.intern_base(base)
    }
    pub fn intern_container<T: EgglogContainerTy, C: BoxedContainer>(
        &self,
        container: C,
    ) -> wrap::Value<T> {
        self.ctx.intern_container(container)
    }
    pub fn _intern_base<T: EgglogTy, B: BaseValue>(&self, base: B) -> egglog::Value {
        self.ctx._intern_base::<T, B>(base)
    }
    pub fn _intern_container<C: ContainerValue>(&self, container: C) -> egglog::Value {
        self.ctx._intern_container(container)
    }
    pub fn insert(&self, table: &'static str, key: &[egglog::Value]) -> egglog::Value {
        self.ctx.insert(table, key)
    }
    pub fn lookup(&self, table: &str, key: &[egglog::Value]) -> Option<egglog::Value> {
        self.ctx.lookup(table, key)
    }
    pub fn lookup_expect(&self, table: &str, key: &[egglog::Value]) -> egglog::Value {
        self.ctx.lookup_expect(table, key)
    }
    pub fn insert_func_tbl(&self, table: &str, key: &[egglog::Value]) {
        self.ctx.insert_func_tbl(table, key);
    }
    #[track_caller]
    pub fn union<T0: EgglogTy, T1: EgglogTy>(
        &self,
        x: impl Insertable<T0, MetaTy = PR::MetaTy>,
        y: impl Insertable<T1, MetaTy = PR::MetaTy>,
    ) {
        let _effect_scope = ActionEffectScope::enter(Location::caller());
        PR::on_ctx_union(
            (
                <T0::EnumVariantMarker as EgglogEnumVariantTy>::TY_NAME,
                x.to_value(&self.ctx).val,
                x.meta(),
            ),
            (
                T1::EnumVariantMarker::TY_NAME,
                y.to_value(&self.ctx).val,
                y.meta(),
            ),
        );
        self.ctx.union(x, y);
    }
    pub fn subsume(&self, table: &str, key: &[egglog::Value]) {
        self.ctx.subsume(table, key);
    }
    pub fn remove(&self, table: &str, key: &[egglog::Value]) {
        self.ctx.remove(table, key);
    }
    pub fn _devalue_container<T: ContainerValue>(
        &self,
        val: egglog::Value,
    ) -> Option<impl Deref<Target = T>> {
        self.ctx._devalue_container(val)
    }
    pub fn _devalue_base<T: BaseValue>(&self, val: egglog::Value) -> T {
        self.ctx._devalue_base(val)
    }
}
impl<'a, 'b, 'c, 'p> RuleCtx<'a, 'b, 'c, 'p> {
    pub fn new(egglog_ctx: &'c mut RustRuleContext<'a, 'b, 'p>, hook: RuleHookObj) -> Self {
        RuleCtx {
            rule_ctx: UnsafeCell::new(egglog_ctx),
            hook,
        }
    }
    pub fn devalue<'d, B: BoxedValue, D: RetypeValue<Target = B>>(
        &'d self,
        val: Value<D>,
    ) -> B::Output<'d> {
        let val = D::retype_value(val.val);
        B::devalue(&self, val.val)
    }
    pub fn intern_base<T: EgglogTy, B: BoxedBase>(&self, base: B) -> wrap::Value<T> {
        let boxed = base.box_it(&self);
        wrap::Value::new(self._intern_base::<T, B::Boxed>(boxed))
    }
    pub fn intern_container<T: EgglogContainerTy, C: BoxedContainer>(
        &self,
        container: C,
    ) -> wrap::Value<T> {
        let boxed_container = BoxedContainer::box_it(container, &self);
        wrap::Value::new(self._intern_container::<C::Boxed>(boxed_container))
    }
    pub fn _intern_base<T: EgglogTy, B: BaseValue>(&self, base: B) -> egglog::Value {
        unsafe { (*self.rule_ctx.get()).base_to_value(base) }
    }
    pub fn _intern_container<C: ContainerValue>(&self, container: C) -> egglog::Value {
        unsafe { (*self.rule_ctx.get()).container_to_value(container) }
    }
    pub fn insert(&self, table: &str, key: &[egglog::Value]) -> egglog::Value {
        self.lookup_expect(table, key)
    }
    #[track_caller]
    pub fn lookup(&self, table: &str, key: &[egglog::Value]) -> Option<egglog::Value> {
        let _effect_scope = ActionEffectScope::enter(Location::caller());
        self.hook.0.as_ref().map(|x| x.on_insert(table, key));
        unsafe { (*self.rule_ctx.get()).lookup(table, key) }
    }
    #[track_caller]
    pub fn lookup_expect(&self, table: &str, key: &[egglog::Value]) -> egglog::Value {
        self.lookup(table, key).unwrap_or_else(|| {
            panic!(
                "ctx.lookup_expect: missing row in table `{}` for key (len={}); \
note: `ctx.set_*` uses staged insert (`insert_func_tbl`) so rows may not be visible until after the rule callback completes or a later ruleset/run.",
                table,
                key.len()
            )
        })
    }
    #[track_caller]
    pub fn insert_func_tbl(&self, table: &str, key: &[egglog::Value]) {
        let _effect_scope = ActionEffectScope::enter(Location::caller());
        self.hook.0.as_ref().map(|x| x.on_insert(table, key));
        unsafe { (*self.rule_ctx.get()).insert(table, key.iter().cloned()) }
    }
    pub fn union<T0: EgglogTy, T1: EgglogTy>(
        &self,
        x: impl Insertable<T0>,
        y: impl Insertable<T1>,
    ) {
        let x = x.to_value(&self);
        let y = y.to_value(&self);
        self.hook.0.as_ref().map(|hook| hook.on_union(x.val, y.val));
        unsafe {
            CURRENT_PREMISE_PROOFS.with(|cell| {
                let premise_proofs_stack = cell.borrow();
                if let Some(premise_proofs) = premise_proofs_stack.last() {
                    (*self.rule_ctx.get()).union_typed(
                        T0::TY_NAME,
                        x.val,
                        y.val,
                        premise_proofs.as_ref(),
                    );
                } else {
                    (*self.rule_ctx.get()).union(x.val, y.val);
                }
            });
        }
    }
    #[track_caller]
    pub fn subsume(&self, table: &str, key: &[egglog::Value]) {
        let _effect_scope = ActionEffectScope::enter(Location::caller());
        self.hook.0.as_ref().map(|hook| hook.on_subsume(table, key));
        unsafe { (*self.rule_ctx.get()).subsume(table, key) }
    }
    #[track_caller]
    pub fn remove(&self, table: &str, key: &[egglog::Value]) {
        let _effect_scope = ActionEffectScope::enter(Location::caller());
        self.hook.0.as_ref().map(|hook| hook.on_remove(table, key));
        unsafe { (*self.rule_ctx.get()).remove(table, key) }
    }
    pub fn _devalue_container<T: ContainerValue>(
        &self,
        val: egglog::Value,
    ) -> Option<impl Deref<Target = T>> {
        unsafe { (*self.rule_ctx.get()).value_to_container(val) }
    }
    pub fn _devalue_base<T: BaseValue>(&self, val: egglog::Value) -> T {
        unsafe { (*self.rule_ctx.get()).value_to_base(val) }
    }
}
pub trait RuleRunner<PR: PatRecSgl> {
    /// pass info from query pattern variables to valued pattern variables in action
    fn add_rule<P: PatVars<PR>>(
        &self,
        rule_name: &str,
        rule_set: RuleSetId,
        pat: impl Fn() -> P,
        action: impl Fn(&PRRuleCtx<PR>, &P::Valued) + Send + Sync + 'static + Clone,
        ctx_hook: Option<Box<dyn RuleCtxHook>>,
    );
    fn new_ruleset(&self, rule_set: &'static str) -> RuleSetId;
    fn run_ruleset(&self, rule_set_id: RuleSetId, run_config: RunConfig) -> RunReport;
    fn value<T: EgglogNode>(&self, node: &T) -> Value<T>;
}
pub trait RuleRunnerSgl: WithPatRecSgl + NodeDropperSgl {
    fn add_rule<P: PatVars<Self::PatRecSgl>>(
        rule_name: &str,
        rule_set: RuleSetId,
        pat: impl Fn() -> P,
        action: impl Fn(&PRRuleCtx<Self::PatRecSgl>, &P::Valued) + Send + Sync + 'static + Clone,
    ) {
        Self::add_rule_op_hook(rule_name, rule_set, pat, action, None);
    }
    fn add_rule_with_hook<P: PatVars<Self::PatRecSgl>>(
        rule_name: &str,
        rule_set: RuleSetId,
        pat: impl Fn() -> P,
        action: impl Fn(&PRRuleCtx<Self::PatRecSgl>, &P::Valued) + Send + Sync + 'static + Clone,
        hook: Box<dyn RuleCtxHook>,
    ) {
        Self::add_rule_op_hook(rule_name, rule_set, pat, action, Some(hook));
    }
    fn add_rule_op_hook<P: PatVars<Self::PatRecSgl>>(
        rule_name: &str,
        rule_set: RuleSetId,
        pat: impl Fn() -> P,
        action: impl Fn(&PRRuleCtx<Self::PatRecSgl>, &P::Valued) + Send + Sync + 'static + Clone,
        ctx_hook: Option<Box<dyn RuleCtxHook>>,
    );
    fn new_ruleset(rule_set: &'static str) -> RuleSetId;
    fn run_ruleset(rule_set_id: RuleSetId, run_config: RunConfig) -> RunReport;
    fn value<T: EgglogNode>(node: &T) -> Value<T>;
}
impl<T: WithPatRecSgl + NodeDropperSgl> RuleRunnerSgl for T
where
    T::RetTy: RuleRunner<Self::PatRecSgl>,
{
    fn add_rule_op_hook<P: PatVars<T::PatRecSgl>>(
        rule_name: &str,
        rule_set: RuleSetId,
        pat: impl Fn() -> P,
        action: impl Fn(&PRRuleCtx<Self::PatRecSgl>, &P::Valued) + Send + Sync + 'static + Clone,
        ctx_hook: Option<Box<dyn RuleCtxHook>>,
    ) {
        Self::sgl().add_rule::<P>(rule_name, rule_set, pat, action, ctx_hook);
    }
    fn new_ruleset(rule_set: &'static str) -> RuleSetId {
        Self::sgl().new_ruleset(rule_set)
    }
    fn run_ruleset(rule_set_id: RuleSetId, run_config: RunConfig) -> RunReport {
        Self::sgl().run_ruleset(rule_set_id, run_config)
    }

    fn value<N: EgglogNode>(node: &N) -> Value<N> {
        Self::sgl().value(node)
    }
}

#[derive(Clone, Copy)]
pub struct RuleSetId(pub &'static str);

pub enum RunConfig {
    Sat,
    Times(u32),
    Once,
}

pub struct FactsBuilder {
    table_facts: Vec<TableFactSpec>,
    constraint_facts: Vec<Box<dyn IntoConstraintFact>>,
}
pub type TableName = String;
pub type SortName = String;
pub type VarName = String;
#[derive(Clone, Debug)]
pub enum TableFactKind {
    Function,
    Relation,
}

#[derive(Clone, Debug)]
pub struct TableFactSpec {
    pub table: TableName,
    pub vars: Vec<(VarName, SortName)>,
    pub kind: TableFactKind,
}

impl FactsBuilder {
    pub fn new() -> Self {
        Self {
            table_facts: Vec::new(),
            constraint_facts: Vec::new(),
        }
    }
    #[allow(unused)]
    fn to_resolved_var(
        egraph: &egglog::EGraph,
        var_name: VarName,
        sort_name: SortName,
    ) -> ResolvedVar {
        ResolvedVar {
            name: var_name,
            sort: egraph
                .get_sort_by_name(&sort_name)
                .cloned()
                .unwrap_or(Arc::new(EqSort {
                    name: sort_name.clone(),
                }) as Arc<dyn Sort>),
            is_global_ref: false,
        }
    }
    pub fn add_constraint_facts(&mut self, facts: Vec<Box<dyn IntoConstraintFact>>) {
        self.constraint_facts.extend(facts);
    }
    /// procedural macro call this function to add atom
    pub fn add_table_fact(&mut self, query_table: TableName, vars: Vec<(VarName, SortName)>) {
        self.table_facts.push(TableFactSpec {
            table: query_table,
            vars,
            kind: TableFactKind::Function,
        });
    }

    pub fn add_relation_fact(&mut self, query_table: TableName, vars: Vec<(VarName, SortName)>) {
        self.table_facts.push(TableFactSpec {
            table: query_table,
            vars,
            kind: TableFactKind::Relation,
        });
    }

    pub fn vars_with_sorts(&self) -> Vec<(VarName, SortName)> {
        let mut out = Vec::new();
        let mut seen: HashSet<&str> = HashSet::new();
        for fact in self.table_facts.iter() {
            for (var, sort) in fact.vars.iter() {
                if seen.insert(var.as_str()) {
                    out.push((var.clone(), sort.clone()));
                }
            }
        }
        out
    }

    /// Build comparison constraint atom
    fn build_function_fact(
        _egraph: &egglog::EGraph,
        table: TableName,
        vars: Vec<(VarName, SortName)>,
    ) -> Fact {
        let (output, inputs) = vars.split_last().unwrap();
        Expr::Call(
            span!(),
            table,
            inputs
                .iter()
                .map(|(var_name, _sort_name)| Expr::Var(span!(), var_name.clone()))
                .collect(),
        )
        .eq_var(output.0.clone())
    }

    fn build_relation_fact(
        _egraph: &egglog::EGraph,
        table: TableName,
        vars: Vec<(VarName, SortName)>,
    ) -> Fact {
        Fact::Fact(Expr::Call(
            span!(),
            table,
            vars.into_iter()
                .map(|(var_name, _sort_name)| Expr::Var(span!(), var_name))
                .collect(),
        ))
    }

    pub fn build(self, egraph: &egglog::EGraph) -> Vec<Fact> {
        let mut v = Vec::new();
        for table_fact in self.table_facts {
            v.push(match table_fact.kind {
                TableFactKind::Function => {
                    Self::build_function_fact(egraph, table_fact.table, table_fact.vars)
                }
                TableFactKind::Relation => {
                    Self::build_relation_fact(egraph, table_fact.table, table_fact.vars)
                }
            });
        }
        // Add constraints to the query
        for constraint_fact in self.constraint_facts {
            v.extend(constraint_fact.into_constraint_fact(egraph));
        }
        v
    }
    pub fn build_fact() {}
}

trait ExprEq {
    fn eq_var(self, var: VarName) -> Fact;
}
impl ExprEq for Expr {
    fn eq_var(self, var: VarName) -> Fact {
        Fact::Eq(span!(), Expr::Var(span!(), var), self)
    }
}
