use crate::wrap::{
    self, BoxedContainer, BoxedValue, DeValue, EgglogContainerTy, EgglogNode, PatRecSgl, ToValue,
};
use crate::wrap::{BoxedBase, EgglogTy, NodeDropperSgl, PatVars, WithPatRecSgl};
use egglog::ast::ResolvedVar;
use egglog::prelude::{
    EqProofId, FuncType, GenericAtom, GenericAtomTerm, Query, ResolvedCall, TermProofId,
};
use egglog::{
    BaseValue,
    ast::FunctionSubtype,
    prelude::RustRuleContext,
    sort::{EqSort, Sort},
    span,
};
use egglog::{ContainerValue, RunReport};
use std::sync::Arc;
use wrap::Value;

// eggplant rule context is a wrapper of egglog rule context.
// it contains the Tx to which the rule is applied
pub struct RuleCtx<'a, 'b, 'c> {
    pub rule_ctx: &'c mut RustRuleContext<'a, 'b>,
}
impl<'a, 'b, 'c> RuleCtx<'a, 'b, 'c> {
    pub fn new(egglog_ctx: &'c mut RustRuleContext<'a, 'b>) -> Self {
        Self {
            rule_ctx: egglog_ctx,
        }
    }
    pub fn devalue<'d, B: BoxedValue, D: DeValue<Target = B>>(
        &'d mut self,
        val: Value<D>,
    ) -> B::Output<'d> {
        let val = D::retype_value(val.val);
        B::devalue(self, val.val)
    }
    pub fn intern_base<T: EgglogTy, B: BoxedBase>(&mut self, base: B) -> wrap::Value<T> {
        let boxed = base.box_it(self);
        wrap::Value::new(self._intern_base::<T, B::Boxed>(boxed))
    }
    pub fn intern_container<T: EgglogContainerTy, C: BoxedContainer>(
        &mut self,
        container: C,
    ) -> wrap::Value<T> {
        let boxed_container = BoxedContainer::box_it(container, self);
        wrap::Value::new(self._intern_container::<C::Boxed>(boxed_container))
    }
    pub fn _intern_base<T: EgglogTy, B: BaseValue>(&self, base: B) -> egglog::Value {
        self.rule_ctx.base_to_value(base)
    }
    pub fn _intern_container<C: ContainerValue>(&mut self, container: C) -> egglog::Value {
        self.rule_ctx.container_to_value(container)
    }
    pub fn insert(&mut self, table: &str, key: &[egglog::Value]) -> egglog::Value {
        self.rule_ctx.lookup(table, key).unwrap()
    }
    pub fn union<T0, T1>(&mut self, x: impl ToValue<T0>, y: impl ToValue<T1>) {
        let x = x.to_value(self);
        let y = y.to_value(self);
        self.rule_ctx.union(x.val, y.val);
    }
    pub fn subsume(&mut self, table: &str, key: &[egglog::Value]) {
        self.rule_ctx.subsume(table, key)
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
        action: impl Fn(&mut RuleCtx, &P::Valued) + Send + Sync + 'static + Clone,
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
        action: impl Fn(&mut RuleCtx, &P::Valued) + Send + Sync + 'static + Clone,
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
    fn add_rule<P: PatVars<T::PatRecSgl>>(
        rule_name: &str,
        rule_set: RuleSetId,
        pat: impl Fn() -> P,
        action: impl Fn(&mut RuleCtx, &P::Valued) + Send + Sync + 'static + Clone,
    ) {
        Self::sgl().add_rule::<T::PatRecSgl, P>(rule_name, rule_set, pat, action);
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

pub struct QueryBuilder {
    atoms: Vec<(TableName, Vec<(VarName, SortName)>)>,
}
pub type TableName = String;
pub type SortName = String;
pub type VarName = String;
impl QueryBuilder {
    pub fn new() -> Self {
        Self { atoms: Vec::new() }
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
    /// procedural macro call this function to add atom
    pub fn add_atom(&mut self, query_table: TableName, vars: Vec<(VarName, SortName)>) {
        self.atoms.push((query_table, vars));
    }

    /// for every variable we don't care whether it is selected as Action Input
    fn build_atom(
        egraph: &egglog::EGraph,
        table: TableName,
        vars: Vec<(VarName, SortName)>,
    ) -> GenericAtom<ResolvedCall, ResolvedVar> {
        log::debug!("table_name:{}", table);
        log::debug!("vars::{:?}", vars);
        let (output, inputs) = vars.split_last().unwrap();
        GenericAtom {
            span: span!(),
            head: ResolvedCall::Func(FuncType {
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
            }),
            args: vars
                .iter()
                .map(|(var_name, sort_name)| {
                    GenericAtomTerm::Var(
                        span!(),
                        Self::to_resolved_var(&egraph, var_name.clone(), sort_name.clone()),
                    )
                })
                .collect(),
        }
    }
    pub fn build(self, egraph: &egglog::EGraph) -> Query<ResolvedCall, ResolvedVar> {
        let mut v = Vec::new();
        for atom in self.atoms {
            v.push(Self::build_atom(egraph, atom.0, atom.1));
        }
        Query { atoms: v }
    }
}
