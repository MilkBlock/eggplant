use crate::wrap::{EgglogCompatExt, SchemaFunctionKind};
use egglog::extract::DefaultCost;
use egglog::{ArcSort, EGraph, TermDag, TermId, Value, ast::Literal};
use std::collections::{HashMap, HashSet, VecDeque};

#[derive(Debug, Clone, Default)]
pub struct EBoostExtractConfig {}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(super) struct EBoostEqKey {
    pub sort_name: String,
    pub value: Value,
}

#[derive(Debug, Clone)]
pub(super) struct EBoostCandidate {
    pub term_name: String,
    pub output: EBoostEqKey,
    pub inputs: Vec<(ArcSort, Value)>,
    pub head_cost: DefaultCost,
}

#[derive(Debug, Clone)]
pub(super) struct EBoostCostSet {
    pub costs: HashMap<EBoostEqKey, DefaultCost>,
    pub total: DefaultCost,
    pub candidate_idx: usize,
}

#[derive(Debug, Clone)]
pub(super) struct EBoostPrepared {
    pub root_key: EBoostEqKey,
    pub reachable_candidates: Vec<EBoostCandidate>,
    pub best_by_class: HashMap<EBoostEqKey, EBoostCostSet>,
    pub candidate_scores: HashMap<usize, DefaultCost>,
}

pub fn eboost_extract_value_prototype(
    egraph: &EGraph,
    sort: &ArcSort,
    value: Value,
    _config: EBoostExtractConfig,
) -> Result<(TermDag, TermId, DefaultCost), egglog::Error> {
    if !sort.is_eq_sort() {
        return Err(egglog::Error::BackendError(
            "eboost heuristic extraction currently only supports eq-sort roots".to_string(),
        ));
    }

    let prepared = prepare_eboost_candidates(egraph, sort, value)?;
    let Some(best_root) = prepared.best_by_class.get(&prepared.root_key) else {
        return Err(egglog::Error::BackendError(format!(
            "eboost heuristic found no acyclic witness for root e-class `{}`",
            prepared.root_key.sort_name
        )));
    };

    let mut termdag = TermDag::default();
    let mut cache = HashMap::<EBoostEqKey, TermId>::new();
    let mut active = HashSet::<EBoostEqKey>::new();
    let root_term = decode_eqclass(
        egraph,
        &prepared.root_key,
        &prepared.best_by_class,
        &prepared.reachable_candidates,
        &mut cache,
        &mut active,
        &mut termdag,
    )?;

    Ok((termdag, root_term, best_root.total))
}

pub(super) fn prepare_eboost_candidates(
    egraph: &EGraph,
    sort: &ArcSort,
    value: Value,
) -> Result<EBoostPrepared, egglog::Error> {
    let root_key = EBoostEqKey {
        sort_name: sort.name().to_string(),
        value: egraph.get_canonical_value(value, sort),
    };

    let candidates = collect_candidates(egraph)?;
    let by_output = index_candidates(&candidates);
    let reachable_classes = collect_reachable_classes(&root_key, &candidates, &by_output)?;
    let reachable_candidate_ids = collect_reachable_candidate_ids(&reachable_classes, &candidates);

    let mut reachable_candidates = reachable_candidate_ids
        .into_iter()
        .map(|idx| candidates[idx].clone())
        .collect::<Vec<_>>();
    reachable_candidates.sort_by(|a, b| {
        let a_key = format!("{}|{}|{:?}", a.output.sort_name, a.term_name, a.inputs);
        let b_key = format!("{}|{}|{:?}", b.output.sort_name, b.term_name, b.inputs);
        a_key.cmp(&b_key)
    });

    let (best_by_class, candidate_scores) =
        compute_best_cost_sets(&root_key, &reachable_candidates)?;

    Ok(EBoostPrepared {
        root_key,
        reachable_candidates,
        best_by_class,
        candidate_scores,
    })
}

pub(super) fn collect_candidates(egraph: &EGraph) -> Result<Vec<EBoostCandidate>, egglog::Error> {
    let manifests = egraph
        .schema_manifest()
        .functions
        .into_iter()
        .map(|function| (function.name.clone(), function))
        .collect::<HashMap<_, _>>();

    let mut out = Vec::<EBoostCandidate>::new();
    for (func_name, manifest) in manifests {
        if manifest.kind != SchemaFunctionKind::Constructor
            || manifest.hidden
            || manifest.unextractable
        {
            continue;
        }
        if manifest.term_constructor.is_some() {
            return Err(egglog::Error::BackendError(format!(
                "eboost heuristic extraction does not yet support term-constructor / view-table extraction (`{func_name}`)"
            )));
        }

        let Some(function) = egraph.get_function(&func_name) else {
            continue;
        };
        let output_sort = function.schema().output.clone();
        if !output_sort.is_eq_sort() {
            continue;
        }

        for input_sort in &function.schema().input {
            if input_sort.is_container_sort() {
                return Err(egglog::Error::BackendError(format!(
                    "eboost heuristic extraction does not yet support constructor `{func_name}` with container children"
                )));
            }
        }

        for row in egraph.function_rows(&func_name) {
            if row.subsumed {
                continue;
            }
            if row.vals.len() != function.schema().input.len() + 1 {
                return Err(egglog::Error::BackendError(format!(
                    "row/schema arity mismatch while collecting eboost candidates for `{func_name}`"
                )));
            }

            let output_value = egraph.get_canonical_value(*row.vals.last().unwrap(), &output_sort);
            let inputs = row
                .vals
                .iter()
                .take(function.schema().input.len())
                .copied()
                .zip(function.schema().input.iter())
                .map(|(raw_value, input_sort)| {
                    let canonical_value = if input_sort.is_eq_sort() {
                        egraph.get_canonical_value(raw_value, input_sort)
                    } else {
                        raw_value
                    };
                    (input_sort.clone(), canonical_value)
                })
                .collect::<Vec<_>>();

            out.push(EBoostCandidate {
                term_name: func_name.clone(),
                output: EBoostEqKey {
                    sort_name: output_sort.name().to_string(),
                    value: output_value,
                },
                inputs,
                head_cost: manifest.cost.unwrap_or(1),
            });
        }
    }

    Ok(out)
}

pub(super) fn index_candidates(candidates: &[EBoostCandidate]) -> HashMap<EBoostEqKey, Vec<usize>> {
    let mut by_output = HashMap::<EBoostEqKey, Vec<usize>>::new();
    for (idx, candidate) in candidates.iter().enumerate() {
        by_output
            .entry(candidate.output.clone())
            .or_default()
            .push(idx);
    }
    by_output
}

fn collect_reachable_classes(
    root_key: &EBoostEqKey,
    candidates: &[EBoostCandidate],
    by_output: &HashMap<EBoostEqKey, Vec<usize>>,
) -> Result<HashSet<EBoostEqKey>, egglog::Error> {
    let mut reachable = HashSet::<EBoostEqKey>::new();
    let mut queue = VecDeque::<EBoostEqKey>::from([root_key.clone()]);

    while let Some(key) = queue.pop_front() {
        if !reachable.insert(key.clone()) {
            continue;
        }
        let Some(candidate_ids) = by_output.get(&key) else {
            return Err(egglog::Error::BackendError(format!(
                "eboost heuristic found no constructor candidates for reachable e-class `{}`",
                key.sort_name
            )));
        };
        for idx in candidate_ids {
            for (child_sort, child_value) in &candidates[*idx].inputs {
                if child_sort.is_eq_sort() {
                    queue.push_back(EBoostEqKey {
                        sort_name: child_sort.name().to_string(),
                        value: *child_value,
                    });
                }
            }
        }
    }

    Ok(reachable)
}

fn collect_reachable_candidate_ids(
    reachable_classes: &HashSet<EBoostEqKey>,
    candidates: &[EBoostCandidate],
) -> Vec<usize> {
    let mut reachable = candidates
        .iter()
        .enumerate()
        .filter_map(|(idx, candidate)| reachable_classes.contains(&candidate.output).then_some(idx))
        .collect::<Vec<_>>();
    reachable.sort_unstable();
    reachable
}

fn compute_best_cost_sets(
    root_key: &EBoostEqKey,
    candidates: &[EBoostCandidate],
) -> Result<
    (
        HashMap<EBoostEqKey, EBoostCostSet>,
        HashMap<usize, DefaultCost>,
    ),
    egglog::Error,
> {
    let mut parents = HashMap::<EBoostEqKey, Vec<usize>>::new();
    let mut pending = VecDeque::<usize>::new();
    let mut queued = HashSet::<usize>::new();
    let mut best_by_class = HashMap::<EBoostEqKey, EBoostCostSet>::new();
    let mut candidate_scores = HashMap::<usize, DefaultCost>::new();

    for (idx, candidate) in candidates.iter().enumerate() {
        let mut has_eq_child = false;
        for child in unique_eq_children(candidate) {
            has_eq_child = true;
            parents.entry(child).or_default().push(idx);
        }
        if !has_eq_child {
            pending.push_back(idx);
            queued.insert(idx);
        }
    }

    while let Some(idx) = pending.pop_front() {
        queued.remove(&idx);
        let candidate = &candidates[idx];
        let Some(cost_set) = compute_candidate_cost_set(candidate, idx, &best_by_class) else {
            continue;
        };
        candidate_scores.insert(idx, cost_set.total);
        let should_update = best_by_class
            .get(&candidate.output)
            .map(|existing| cost_set.total < existing.total)
            .unwrap_or(true);
        if should_update {
            best_by_class.insert(candidate.output.clone(), cost_set);
            if let Some(parent_ids) = parents.get(&candidate.output) {
                for parent_idx in parent_ids {
                    if queued.insert(*parent_idx) {
                        pending.push_back(*parent_idx);
                    }
                }
            }
        }
    }

    if !best_by_class.contains_key(root_key) {
        return Err(egglog::Error::BackendError(format!(
            "eboost heuristic could not derive any acyclic extraction for root e-class `{}`",
            root_key.sort_name
        )));
    }

    Ok((best_by_class, candidate_scores))
}

fn compute_candidate_cost_set(
    candidate: &EBoostCandidate,
    candidate_idx: usize,
    best_by_class: &HashMap<EBoostEqKey, EBoostCostSet>,
) -> Option<EBoostCostSet> {
    let eq_children = unique_eq_children(candidate);
    if eq_children.is_empty() {
        let mut costs = HashMap::new();
        costs.insert(candidate.output.clone(), candidate.head_cost);
        return Some(EBoostCostSet {
            total: candidate.head_cost,
            costs,
            candidate_idx,
        });
    }

    let mut child_sets = Vec::<&EBoostCostSet>::with_capacity(eq_children.len());
    for child in &eq_children {
        child_sets.push(best_by_class.get(child)?);
    }

    let mut result = child_sets
        .iter()
        .max_by_key(|set| set.costs.len())
        .map(|set| set.costs.clone())
        .unwrap_or_default();

    for child_set in child_sets {
        for (key, value) in &child_set.costs {
            result.entry(key.clone()).or_insert(*value);
        }
    }

    if result.contains_key(&candidate.output) {
        return None;
    }
    result.insert(candidate.output.clone(), candidate.head_cost);

    let total = result
        .values()
        .fold(0_u64, |sum, cost| sum.saturating_add(*cost));

    Some(EBoostCostSet {
        total,
        costs: result,
        candidate_idx,
    })
}

fn unique_eq_children(candidate: &EBoostCandidate) -> Vec<EBoostEqKey> {
    let mut out = Vec::<EBoostEqKey>::new();
    let mut seen = HashSet::<EBoostEqKey>::new();
    for (child_sort, child_value) in &candidate.inputs {
        if child_sort.is_eq_sort() {
            let key = EBoostEqKey {
                sort_name: child_sort.name().to_string(),
                value: *child_value,
            };
            if seen.insert(key.clone()) {
                out.push(key);
            }
        }
    }
    out
}

fn decode_eqclass(
    egraph: &EGraph,
    key: &EBoostEqKey,
    best_by_class: &HashMap<EBoostEqKey, EBoostCostSet>,
    candidates: &[EBoostCandidate],
    cache: &mut HashMap<EBoostEqKey, TermId>,
    active: &mut HashSet<EBoostEqKey>,
    termdag: &mut TermDag,
) -> Result<TermId, egglog::Error> {
    if let Some(term) = cache.get(key) {
        return Ok(*term);
    }
    if !active.insert(key.clone()) {
        return Err(egglog::Error::BackendError(format!(
            "eboost decode re-entered e-class `{}` while it was still active",
            key.sort_name
        )));
    }

    let Some(best) = best_by_class.get(key) else {
        active.remove(key);
        return Err(egglog::Error::BackendError(format!(
            "eboost decode could not find a best candidate for `{}`",
            key.sort_name
        )));
    };
    let chosen = &candidates[best.candidate_idx];

    let mut child_terms = Vec::with_capacity(chosen.inputs.len());
    for (child_sort, child_value) in &chosen.inputs {
        let child_term = if child_sort.is_eq_sort() {
            decode_eqclass(
                egraph,
                &EBoostEqKey {
                    sort_name: child_sort.name().to_string(),
                    value: *child_value,
                },
                best_by_class,
                candidates,
                cache,
                active,
                termdag,
            )?
        } else {
            base_term(egraph, termdag, child_sort, *child_value)?
        };
        child_terms.push(child_term);
    }

    let term = termdag.app(chosen.term_name.clone(), child_terms);
    cache.insert(key.clone(), term);
    active.remove(key);
    Ok(term)
}

fn base_term(
    egraph: &EGraph,
    termdag: &mut TermDag,
    sort: &ArcSort,
    value: Value,
) -> Result<TermId, egglog::Error> {
    match sort.name() {
        "i64" => Ok(termdag.lit(Literal::Int(egraph.value_to_base::<i64>(value)))),
        "bool" => Ok(termdag.lit(Literal::Bool(egraph.value_to_base::<bool>(value)))),
        "String" => Ok(termdag.lit(Literal::String(
            egraph.value_to_base::<egglog::sort::S>(value).0,
        ))),
        "f64" => Ok(termdag.lit(Literal::Float(
            egraph.value_to_base::<egglog::sort::F>(value).0,
        ))),
        "Unit" | "()" => Ok(termdag.lit(Literal::Unit)),
        other => Err(egglog::Error::BackendError(format!(
            "eboost heuristic extraction does not yet support base sort `{other}` in decode"
        ))),
    }
}
