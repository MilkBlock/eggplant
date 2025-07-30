use std::sync::Arc;

use crate::{BoxUnbox, EgglogTy, NodeDropperSgl, PatVars, WithPatRecSgl, wrap::wrap};
use egglog::{
    BaseValue, Value,
    ast::{FunctionSubtype, GenericFact, ResolvedVar},
    core::{GenericAtom, Query, ResolvedCall},
    prelude::RustRuleContext,
    sort::{EqSort, Sort},
    span,
    typechecking::FuncType,
};

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
    pub fn devalue<B: BoxUnbox + EgglogTy>(&mut self, val: wrap::Value<B>) -> B::UnBoxed {
        B::unbox(self.rule_ctx.value_to_base(val.val), self)
    }
    pub fn intern_base<T: EgglogTy, B: BoxUnbox>(&mut self, base: B) -> wrap::Value<T> {
        let boxed = base.box_it(self);
        wrap::Value::new(self._intern_base::<T, B::Boxed>(boxed))
    }
    pub fn _intern_base<T: EgglogTy, B: BaseValue>(&self, base: B) -> egglog::Value {
        self.rule_ctx.base_to_value(base)
    }
    pub fn insert(&mut self, table: &str, key: &[Value]) -> Value {
        self.rule_ctx
            .lookup(table, key.iter().copied().collect())
            .unwrap()
    }
    pub fn union<T0, T1>(&mut self, x: wrap::Value<T0>, y: wrap::Value<T1>) {
        self.rule_ctx.union(x.val, y.val);
    }
}
pub trait RuleRunner {
    fn add_rule<T: WithPatRecSgl, P: PatVars<T::PatRecSgl>>(
        &self,
        rule_name: &str,
        rule_set: RuleSetId,
        pat: impl Fn() -> P,
        action: impl Fn(&mut RuleCtx, &P::Valued) + Send + Sync + 'static + Clone,
    );
    fn new_ruleset(&self, rule_set: &'static str) -> RuleSetId;
    fn run_ruleset(&self, rule_set_id: RuleSetId, run_config: RunConfig) -> Vec<String>;
}
pub trait RuleRunnerSgl: WithPatRecSgl + NodeDropperSgl {
    fn add_rule<P: PatVars<Self::PatRecSgl>>(
        rule_name: &str,
        rule_set: RuleSetId,
        pat: impl Fn() -> P,
        action: impl Fn(&mut RuleCtx, &P::Valued) + Send + Sync + 'static + Clone,
    );
    fn new_ruleset(rule_set: &'static str) -> RuleSetId;
    fn run_ruleset(rule_set_id: RuleSetId, run_config: RunConfig) -> Vec<String>;
}
impl<T: WithPatRecSgl + NodeDropperSgl> RuleRunnerSgl for T
where
    T::RetTy: RuleRunner,
{
    fn add_rule<P: PatVars<T::PatRecSgl>>(
        rule_name: &str,
        rule_set: RuleSetId,
        pat: impl Fn() -> P,
        action: impl Fn(&mut RuleCtx, &P::Valued) + Send + Sync + 'static + Clone,
    ) {
        Self::sgl().add_rule::<T, P>(rule_name, rule_set, pat, action);
    }
    fn new_ruleset(rule_set: &'static str) -> RuleSetId {
        Self::sgl().new_ruleset(rule_set)
    }
    fn run_ruleset(rule_set_id: RuleSetId, run_config: RunConfig) -> Vec<String> {
        Self::sgl().run_ruleset(rule_set_id, run_config)
    }
}

#[derive(Clone, Copy)]
pub struct RuleSetId(pub &'static str);

pub type RunConfig = Option<Vec<GenericFact<String, String>>>;

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
            head: egglog::core::ResolvedCall::Func(FuncType {
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
                    egglog::core::GenericAtomTerm::Var(
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
