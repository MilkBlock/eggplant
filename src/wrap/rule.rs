use crate::wrap::{
    self, BoxedContainer, BoxedValue, EgglogContainerTy, EgglogNode, Insertable,
    IntoConstraintFact, PatRecSgl, RetypeValue,
};
use crate::wrap::{BoxedBase, EgglogTy, NodeDropperSgl, PatVars, WithPatRecSgl};
use egglog::ast::ResolvedVar;
use egglog::ast::{ResolvedExpr, ResolvedFact};
use egglog::prelude::{EqProofId, FuncType, ResolvedCall, TermProofId};
use egglog::{
    BaseValue,
    ast::FunctionSubtype,
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
    listener: RuleCtxObj,
}
unsafe impl Send for RuleCtxObj {}
unsafe impl Sync for RuleCtxObj {}
pub struct RuleCtxObj(pub Option<Box<dyn RuleCtxListener>>);
impl Clone for RuleCtxObj {
    fn clone(&self) -> Self {
        match &self.0 {
            Some(listener) => RuleCtxObj(Some(listener.dyn_clone())),
            None => RuleCtxObj(None),
        }
    }
}

pub trait RuleCtxListener {
    fn on_insert(&self, table: &str, key: &[egglog::Value]);
    fn on_union(&self, x: egglog::Value, y: egglog::Value);
    fn on_subsume(&self, table: &str, key: &[egglog::Value]);
    fn dyn_clone(&self) -> Box<dyn RuleCtxListener>;
}

impl<'a, 'b, 'c> RuleCtx<'a, 'b, 'c> {
    pub fn new(egglog_ctx: &'c mut RustRuleContext<'a, 'b>, listener: RuleCtxObj) -> Self {
        Self {
            rule_ctx: UnsafeCell::new(egglog_ctx),
            listener,
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
        self.listener.0.as_ref().map(|x| x.on_insert(table, key));
        unsafe { (*self.rule_ctx.get()).lookup(table, key).unwrap() }
    }
    pub fn union<T0, T1>(&self, x: impl Insertable<T0>, y: impl Insertable<T1>) {
        let x = x.to_value(self);
        let y = y.to_value(self);
        self.listener
            .0
            .as_ref()
            .map(|listener| listener.on_union(x.val, y.val));
        unsafe {
            (*self.rule_ctx.get()).union(x.val, y.val);
        }
    }
    pub fn subsume(&self, table: &str, key: &[egglog::Value]) {
        self.listener
            .0
            .as_ref()
            .map(|listener| listener.on_subsume(table, key));
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
    type EqProof;
    type TermProof;
    fn add_rule<T: PatRecSgl, P: PatVars<T>>(
        &self,
        rule_name: &str,
        rule_set: RuleSetId,
        pat: impl Fn() -> P,
        action: impl Fn(&RuleCtx, &P::Valued) + Send + Sync + 'static + Clone,
        ctx_listener: Option<Box<dyn RuleCtxListener>>,
    );
    fn new_ruleset(&self, rule_set: &'static str) -> RuleSetId;
    fn run_ruleset(&self, rule_set_id: RuleSetId, run_config: RunConfig) -> RunReport;
    fn explain<T: EgglogTy>(&self, value: wrap::Value<T>) -> TermProofId;
    fn explain_eq<T1: EgglogTy, T2: EgglogTy>(
        &self,
        v1: wrap::Value<T1>,
        v2: wrap::Value<T2>,
    ) -> EqProofId;
    fn value<T: EgglogNode>(&self, node: &T) -> Value<T>;
}
pub trait RuleRunnerSgl: WithPatRecSgl + NodeDropperSgl {
    type EqProof;
    type TermProof;
    fn add_rule<P: PatVars<Self::PatRecSgl>>(
        rule_name: &str,
        rule_set: RuleSetId,
        pat: impl Fn() -> P,
        action: impl Fn(&RuleCtx, &P::Valued) + Send + Sync + 'static + Clone,
    ) {
        Self::add_rule_op_listener(rule_name, rule_set, pat, action, None);
    }
    fn add_rule_with_listener<P: PatVars<Self::PatRecSgl>>(
        rule_name: &str,
        rule_set: RuleSetId,
        pat: impl Fn() -> P,
        action: impl Fn(&RuleCtx, &P::Valued) + Send + Sync + 'static + Clone,
        listener: Box<dyn RuleCtxListener>,
    ) {
        Self::add_rule_op_listener(rule_name, rule_set, pat, action, Some(listener));
    }
    fn add_rule_op_listener<P: PatVars<Self::PatRecSgl>>(
        rule_name: &str,
        rule_set: RuleSetId,
        pat: impl Fn() -> P,
        action: impl Fn(&RuleCtx, &P::Valued) + Send + Sync + 'static + Clone,
        ctx_listener: Option<Box<dyn RuleCtxListener>>,
    );
    fn new_ruleset(rule_set: &'static str) -> RuleSetId;
    fn run_ruleset(rule_set_id: RuleSetId, run_config: RunConfig) -> RunReport;
    fn explain<T: EgglogTy>(value: Value<T>) -> TermProofId;
    fn explain_raw(value: u32) -> TermProofId;
    fn explain_eq<T1: EgglogTy, T2: EgglogTy>(v1: Value<T1>, v2: Value<T2>) -> EqProofId;
    fn explain_eq_raw(v1: u32, v2: u32) -> EqProofId;
    fn value<T: EgglogNode>(node: &T) -> Value<T>;
}
impl<T: WithPatRecSgl + NodeDropperSgl, EP, TP> RuleRunnerSgl for T
where
    T::RetTy: RuleRunner<EqProof = EP, TermProof = TP>,
{
    type EqProof = <T::RetTy as RuleRunner>::EqProof;
    type TermProof = <T::RetTy as RuleRunner>::TermProof;
    fn add_rule_op_listener<P: PatVars<T::PatRecSgl>>(
        rule_name: &str,
        rule_set: RuleSetId,
        pat: impl Fn() -> P,
        action: impl Fn(&RuleCtx, &P::Valued) + Send + Sync + 'static + Clone,
        ctx_listener: Option<Box<dyn RuleCtxListener>>,
    ) {
        Self::sgl().add_rule::<T::PatRecSgl, P>(rule_name, rule_set, pat, action, ctx_listener);
    }
    fn new_ruleset(rule_set: &'static str) -> RuleSetId {
        Self::sgl().new_ruleset(rule_set)
    }
    fn run_ruleset(rule_set_id: RuleSetId, run_config: RunConfig) -> RunReport {
        Self::sgl().run_ruleset(rule_set_id, run_config)
    }

    fn explain<Ty: EgglogTy>(value: Value<Ty>) -> TermProofId {
        Self::sgl().explain(value)
    }
    fn explain_raw(value: u32) -> TermProofId {
        Self::sgl().explain(Value::<i64>::new(egglog::Value::new_const(value)))
    }

    fn value<N: EgglogNode>(node: &N) -> Value<N> {
        Self::sgl().value(node)
    }

    fn explain_eq<T1: EgglogTy, T2: EgglogTy>(v1: Value<T1>, v2: Value<T2>) -> EqProofId {
        Self::sgl().explain_eq(v1, v2)
    }

    fn explain_eq_raw(v1: u32, v2: u32) -> EqProofId {
        Self::sgl().explain_eq(
            Value::<i64>::new(egglog::Value::new_const(v1)),
            Value::<i64>::new(egglog::Value::new_const(v2)),
        )
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
        egraph: &egglog::EGraph,
        table: TableName,
        vars: Vec<(VarName, SortName)>,
    ) -> ResolvedFact {
        let (output, inputs) = vars.split_last().unwrap();
        ResolvedFact::Fact(ResolvedExpr::Call(
            span!(),
            ResolvedCall::Func(FuncType {
                name: table,
                subtype: FunctionSubtype::Constructor,
                input: inputs
                    .iter()
                    .map(|(_, sort_name)| {
                        egraph
                            .get_sort_by_name(&sort_name)
                            .cloned()
                            .unwrap_or(Arc::new(EqSort {
                                name: sort_name.clone(),
                            }) as Arc<dyn Sort>)
                    })
                    .collect(),
                output: Arc::new(EqSort {
                    name: output.1.clone(),
                }),
                recommend_var_name: Some(output.0.clone()),
            }),
            inputs
                .iter()
                .map(|(var_name, sort_name)| {
                    ResolvedExpr::Var(
                        span!(),
                        Self::to_resolved_var(&egraph, var_name.clone(), sort_name.clone()),
                    )
                })
                .collect(),
        ))
    }
    pub fn build(self, egraph: &egglog::EGraph) -> Vec<ResolvedFact> {
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
