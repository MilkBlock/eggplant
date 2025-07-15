use crate::{EgglogTy, NodeDropperSgl, PatVars, WithPatRecSgl, wrap::wrap};
use egglog::{BaseValue, ast::GenericFact, prelude::RustRuleContext};

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
    pub fn devalue<T: EgglogTy + BaseValue>(&self, val: wrap::Value<T>) -> T {
        self.rule_ctx.value_to_base(val.val)
    }
}
pub trait RuleRunner {
    fn add_rule<T: WithPatRecSgl, P: PatVars<T::PatRecSgl>>(
        &self,
        rule_set: RuleSetId,
        pat: impl Fn() -> P,
        action: impl Fn(RuleCtx, &P::Valued) -> Option<()> + Send + Sync + 'static + Clone,
    );
    fn new_ruleset(&self, rule_set: &'static str) -> RuleSetId;
    fn run_ruleset(&self, rule_set_id: RuleSetId, run_config: RunConfig) -> Vec<String>;
}
pub trait RuleRunnerSgl: WithPatRecSgl + NodeDropperSgl {
    fn add_rule<P: PatVars<Self::PatRecSgl>>(
        rule_set: RuleSetId,
        pat: impl Fn() -> P,
        action: impl Fn(RuleCtx, &P::Valued) -> Option<()> + Send + Sync + 'static + Clone,
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
        action: impl Fn(RuleCtx, &P::Valued) -> Option<()> + Send + Sync + 'static + Clone,
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
