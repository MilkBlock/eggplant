use crate::wrap::{
    self, BoxedContainer, BoxedValue, EgglogContainerTy, EgglogNode, Insertable,
    IntoConstraintFact, PatRecSgl, RetypeValue,
};
use crate::wrap::{BoxedBase, EgglogTy, NodeDropperSgl, PatVars, WithPatRecSgl};
use egglog::ast::{Expr, Fact, ResolvedVar};
use egglog::{
    BaseValue,
    ast::{RustSpan, Span},
    prelude::RustRuleContext,
    sort::{EqSort, Sort},
    span,
};
use egglog::{ContainerValue, RunReport};
use std::cell::UnsafeCell;
use std::ops::Deref;
use std::sync::Arc;
use wrap::Value;

// eggplant rule context is a wrapper of egglog rule context.
// it contains the Tx to which the rule is applied
pub struct RuleCtx<'a, 'b, 'c> {
    pub rule_ctx: UnsafeCell<&'c mut RustRuleContext<'a, 'b>>,
    hook: RuleCtxObj,
}
unsafe impl Send for RuleCtxObj {}
unsafe impl Sync for RuleCtxObj {}
pub struct RuleCtxObj(pub Option<Box<dyn RuleCtxHook>>);
impl Clone for RuleCtxObj {
    fn clone(&self) -> Self {
        match &self.0 {
            Some(hook) => RuleCtxObj(Some(hook.dyn_clone())),
            None => RuleCtxObj(None),
        }
    }
}

pub trait RuleCtxHook {
    fn on_insert(&self, table: &str, key: &[egglog::Value]);
    fn on_union(&self, x: egglog::Value, y: egglog::Value);
    fn on_subsume(&self, table: &str, key: &[egglog::Value]);
    fn dyn_clone(&self) -> Box<dyn RuleCtxHook>;
}

impl<'a, 'b, 'c> RuleCtx<'a, 'b, 'c> {
    pub fn new(egglog_ctx: &'c mut RustRuleContext<'a, 'b>, hook: RuleCtxObj) -> Self {
        Self {
            rule_ctx: UnsafeCell::new(egglog_ctx),
            hook,
        }
    }
    pub fn devalue<'d, B: BoxedValue, D: RetypeValue<Target = B>>(
        &'d self,
        val: Value<D>,
    ) -> B::Output<'d> {
        let val = D::retype_value(val.val);
        B::devalue(self, val.val)
    }
    pub fn intern_base<T: EgglogTy, B: BoxedBase>(&self, base: B) -> wrap::Value<T> {
        let boxed = base.box_it(self);
        wrap::Value::new(self._intern_base::<T, B::Boxed>(boxed))
    }
    pub fn intern_container<T: EgglogContainerTy, C: BoxedContainer>(
        &self,
        container: C,
    ) -> wrap::Value<T> {
        let boxed_container = BoxedContainer::box_it(container, self);
        wrap::Value::new(self._intern_container::<C::Boxed>(boxed_container))
    }
    pub fn _intern_base<T: EgglogTy, B: BaseValue>(&self, base: B) -> egglog::Value {
        unsafe { (*self.rule_ctx.get()).base_to_value(base) }
    }
    pub fn _intern_container<C: ContainerValue>(&self, container: C) -> egglog::Value {
        unsafe { (*self.rule_ctx.get()).container_to_value(container) }
    }
    pub fn insert(&self, table: &str, key: &[egglog::Value]) -> egglog::Value {
        self.hook.0.as_ref().map(|x| x.on_insert(table, key));
        unsafe { (*self.rule_ctx.get()).lookup(table, key).unwrap() }
    }
    pub fn insert_func_tbl(&self, table: &str, key: &[egglog::Value]) {
        self.hook.0.as_ref().map(|x| x.on_insert(table, key));
        unsafe { (*self.rule_ctx.get()).insert(table, key.iter().cloned()) }
    }
    pub fn union<T0, T1>(&self, x: impl Insertable<T0>, y: impl Insertable<T1>) {
        let x = x.to_value(self);
        let y = y.to_value(self);
        self.hook.0.as_ref().map(|hook| hook.on_union(x.val, y.val));
        unsafe {
            (*self.rule_ctx.get()).union(x.val, y.val);
        }
    }
    pub fn subsume(&self, table: &str, key: &[egglog::Value]) {
        self.hook.0.as_ref().map(|hook| hook.on_subsume(table, key));
        unsafe { (*self.rule_ctx.get()).subsume(table, key) }
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
pub trait RuleRunner {
    fn add_rule<T: PatRecSgl, P: PatVars<T>>(
        &self,
        rule_name: &str,
        rule_set: RuleSetId,
        pat: impl Fn() -> P,
        action: impl Fn(&RuleCtx, &P::Valued) + Send + Sync + 'static + Clone,
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
        action: impl Fn(&RuleCtx, &P::Valued) + Send + Sync + 'static + Clone,
    ) {
        Self::add_rule_op_hook(rule_name, rule_set, pat, action, None);
    }
    fn add_rule_with_hook<P: PatVars<Self::PatRecSgl>>(
        rule_name: &str,
        rule_set: RuleSetId,
        pat: impl Fn() -> P,
        action: impl Fn(&RuleCtx, &P::Valued) + Send + Sync + 'static + Clone,
        hook: Box<dyn RuleCtxHook>,
    ) {
        Self::add_rule_op_hook(rule_name, rule_set, pat, action, Some(hook));
    }
    fn add_rule_op_hook<P: PatVars<Self::PatRecSgl>>(
        rule_name: &str,
        rule_set: RuleSetId,
        pat: impl Fn() -> P,
        action: impl Fn(&RuleCtx, &P::Valued) + Send + Sync + 'static + Clone,
        ctx_hook: Option<Box<dyn RuleCtxHook>>,
    );
    fn new_ruleset(rule_set: &'static str) -> RuleSetId;
    fn run_ruleset(rule_set_id: RuleSetId, run_config: RunConfig) -> RunReport;
    fn value<T: EgglogNode>(node: &T) -> Value<T>;
}
impl<T: WithPatRecSgl + NodeDropperSgl> RuleRunnerSgl for T
where
    T::RetTy: RuleRunner,
{
    fn add_rule_op_hook<P: PatVars<T::PatRecSgl>>(
        rule_name: &str,
        rule_set: RuleSetId,
        pat: impl Fn() -> P,
        action: impl Fn(&RuleCtx, &P::Valued) + Send + Sync + 'static + Clone,
        ctx_hook: Option<Box<dyn RuleCtxHook>>,
    ) {
        Self::sgl().add_rule::<T::PatRecSgl, P>(rule_name, rule_set, pat, action, ctx_hook);
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
    table_facts: Vec<(TableName, Vec<(VarName, SortName)>)>,
    constraint_facts: Vec<Box<dyn IntoConstraintFact>>,
}
pub type TableName = String;
pub type SortName = String;
pub type VarName = String;
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
        self.table_facts.push((query_table, vars));
    }

    /// Build comparison constraint atom
    fn build_table_fact(
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
    pub fn build(self, egraph: &egglog::EGraph) -> Vec<Fact> {
        let mut v = Vec::new();
        for table_fact in self.table_facts {
            v.push(Self::build_table_fact(egraph, table_fact.0, table_fact.1));
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
