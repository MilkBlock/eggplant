use egglog::extract::DefaultCost;
use egglog::{ArcSort, EGraph, TermDag, TermId, Value};
#[cfg(feature = "rustsat-extract")]
use std::collections::HashMap;

#[derive(Debug, Clone, Default)]
pub struct EBoostExtractConfig {}

#[cfg(feature = "rustsat-extract")]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(super) struct EBoostEqKey {
    pub sort_name: String,
    pub value: Value,
}

#[cfg(feature = "rustsat-extract")]
#[derive(Debug, Clone)]
pub(super) struct EBoostCandidate {
    pub term_name: String,
    pub output: EBoostEqKey,
    pub inputs: Vec<(ArcSort, Value)>,
    pub head_cost: DefaultCost,
}

#[cfg(feature = "rustsat-extract")]
#[derive(Debug, Clone)]
pub(super) struct EBoostCostSet {
    pub total: DefaultCost,
    pub candidate_idx: usize,
}

#[cfg(feature = "rustsat-extract")]
#[derive(Debug, Clone)]
pub(super) struct EBoostPrepared {
    pub root_key: EBoostEqKey,
    pub reachable_candidates: Vec<EBoostCandidate>,
    pub best_by_class: HashMap<EBoostEqKey, EBoostCostSet>,
    pub candidate_scores: HashMap<usize, DefaultCost>,
}

fn unsupported_eboost() -> egglog::Error {
    egglog::Error::BackendError(
        "eboost extraction requires fork-egglog schema/raw introspection and is disabled on stable-big-pr"
            .to_string(),
    )
}

pub fn eboost_extract_value_prototype(
    _egraph: &EGraph,
    _sort: &ArcSort,
    _value: Value,
    _config: EBoostExtractConfig,
) -> Result<(TermDag, TermId, DefaultCost), egglog::Error> {
    Err(unsupported_eboost())
}

#[cfg(feature = "rustsat-extract")]
pub(super) fn prepare_eboost_candidates(
    egraph: &EGraph,
    sort: &ArcSort,
    value: Value,
) -> Result<EBoostPrepared, egglog::Error> {
    let _ = (egraph, sort, value);
    Err(unsupported_eboost())
}

#[cfg(feature = "rustsat-extract")]
pub(super) fn collect_candidates(
    egraph: &EGraph,
) -> Result<Vec<EBoostCandidate>, egglog::Error> {
    let _ = egraph;
    Err(unsupported_eboost())
}
