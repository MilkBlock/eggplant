use crate::{EgglogTy, NodeDropperSgl, PatVars, WithPatRecSgl, wrap::wrap};
use egglog::{BaseValue, Value, ast::GenericFact, prelude::RustRuleContext};

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
    pub fn devalue<T: EgglogTy, B: BaseValue>(&self, val: wrap::Value<T>) -> B {
        self.rule_ctx.value_to_base(val.val)
    }
    pub fn intern_base<T: EgglogTy, B: BaseValue>(&self, base: B) -> wrap::Value<T> {
        wrap::Value::new(self.rule_ctx.base_to_value(base))
    }
    pub fn insert(&mut self, table: &str, key: &[Value]) -> Value {
        self.rule_ctx.insert(table, key.iter().copied());
        self.rule_ctx
            .lookup(table, key.iter().copied().collect())
            .unwrap()
    }
    pub fn union(&mut self, x: Value, y: Value) {
        self.rule_ctx.union(x, y);
    }
}
pub trait RuleRunner {
    fn add_rule<T: WithPatRecSgl, P: PatVars<T::PatRecSgl>>(
        &self,
        rule_set: RuleSetId,
        pat: impl Fn() -> P,
        action: impl Fn(&mut RuleCtx, &P::Valued) -> Option<()> + Send + Sync + 'static + Clone,
    );
    fn new_ruleset(&self, rule_set: &'static str) -> RuleSetId;
    fn run_ruleset(&self, rule_set_id: RuleSetId, run_config: RunConfig) -> Vec<String>;
}
pub trait RuleRunnerSgl: WithPatRecSgl + NodeDropperSgl {
    fn add_rule<P: PatVars<Self::PatRecSgl>>(
        rule_set: RuleSetId,
        pat: impl Fn() -> P,
        action: impl Fn(&mut RuleCtx, &P::Valued) -> Option<()> + Send + Sync + 'static + Clone,
    );
    fn new_rule_set(rule_set: &'static str) -> RuleSetId;
    fn run_ruleset(rule_set_id: RuleSetId, run_config: RunConfig) -> Vec<String>;
}
impl<T: WithPatRecSgl + NodeDropperSgl> RuleRunnerSgl for T
where
    T::RetTy: RuleRunner,
{
    fn add_rule<P: PatVars<T::PatRecSgl>>(
        rule_set: RuleSetId,
        pat: impl Fn() -> P,
        action: impl Fn(&mut RuleCtx, &P::Valued) -> Option<()> + Send + Sync + 'static + Clone,
    ) {
        Self::sgl().add_rule::<T, P>(rule_set, pat, action);
    }
    fn new_rule_set(rule_set: &'static str) -> RuleSetId {
        Self::sgl().new_ruleset(rule_set)
    }
    fn run_ruleset(rule_set_id: RuleSetId, run_config: RunConfig) -> Vec<String> {
        Self::sgl().run_ruleset(rule_set_id, run_config)
    }
}

#[derive(Clone, Copy)]
pub struct RuleSetId(pub &'static str);

pub type RunConfig = Option<Vec<GenericFact<String, String>>>;
