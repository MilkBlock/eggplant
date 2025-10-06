use crate::wrap::{
    EgglogFunc, EgglogFuncInputs, EgglogFuncOutput, constraint::IntoConstraintFact,
    etc::generate_dot_by_graph,
};

use super::*;
use dashmap::DashMap;
use derive_more::Debug;
use egglog::util::IndexSet;
use petgraph::prelude::StableDiGraph;
use std::{
    collections::HashMap,
    path::PathBuf,
    sync::{Mutex, atomic::AtomicU32},
};

#[derive(Debug)]
pub struct PatRecorder {
    #[debug(skip)]
    map: DashMap<Sym, PatRecordNode>,
    /// place_holders field records current building pattern's all place holders
    pub patterns: Mutex<HashMap<PatId, HashMap<Sym, &'static str>>>,
    /// one pattern may have multiple root nodes
    #[debug(skip)]
    pub root_table: DashMap<PatId, Vec<Sym>>,
    #[debug(skip)]
    pub constraint_table: DashMap<PatId, Vec<Box<dyn IntoConstraintFact>>>,
    _registry: EgglogTypeRegistry,
    /// next_pat_id increment when on_record_end is called
    next_pat_id: AtomicU32,
}
struct PatRecordNode {
    work_node: WorkAreaNode,
    pat_id: PatId,
    /// if one node dropped in pattern defining function then it is not selected as action args
    selected: bool,
}

impl PatRecordNode {
    pub fn new(node: Box<dyn EgglogNode>, pat_id: u32) -> Self {
        Self {
            work_node: WorkAreaNode {
                preds: Syms::default(),
                egglog: node,
                next: None,
                prev: None,
                pulled_by: None,
            },
            pat_id: PatId(pat_id),
            selected: true,
        }
    }
    pub fn succs_mut(&mut self) -> impl Iterator<Item = &mut Sym> {
        self.work_node.egglog.succs_mut().into_iter()
    }
    #[allow(unused)]
    pub fn preds_mut(&mut self) -> impl Iterator<Item = &mut Sym> {
        self.work_node.preds.iter_mut()
    }
    pub fn succs(&self) -> impl Iterator<Item = Sym> {
        self.work_node.egglog.succs().into_iter()
    }
    pub fn preds(&self) -> impl Iterator<Item = Sym> {
        self.work_node.preds.iter().cloned()
    }
    pub fn next(&self) -> Option<&Sym> {
        self.work_node.next.as_ref()
    }
}

/// Pattern Recorder, so that you could define pattern in a function
impl PatRecorder {
    pub fn new() -> Self {
        Self {
            map: DashMap::default(),
            _registry: EgglogTypeRegistry::new_with_inventory(),
            patterns: Mutex::new(Default::default()),
            next_pat_id: AtomicU32::new(0),
            root_table: DashMap::default(),
            constraint_table: DashMap::default(),
        }
    }
    // collect all ancestors of cur_sym, without cur_sym
    pub fn collect_latest_ancestors(&self, cur_sym: Sym, index_set: &mut IndexSet<Sym>) {
        let node = self.map.get(&cur_sym).unwrap();
        let succss = node.work_node.preds.clone();
        drop(node);
        for pred in succss {
            if index_set.contains(&pred) || self.map.get(&pred).unwrap().next().is_some() {
                // do nothing
            } else {
                index_set.insert(pred);
                self.collect_latest_ancestors(pred, index_set)
            }
        }
    }
    pub fn wag_build_petgraph(&self) -> StableDiGraph<WorkAreaNode, ()> {
        // 1. collect all nodes
        let v = self
            .map
            .iter()
            .map(|x| x.value().work_node.clone())
            .collect::<Vec<_>>();
        let mut g = StableDiGraph::new();
        let mut idxs = Vec::new();
        // 2. map from WorkAreaNode cur_sym to petgraph::NodeIndex
        use std::collections::HashMap;
        let mut sym2idx = HashMap::new();
        for node in &v {
            let idx = g.add_node(node.clone());
            idxs.push(idx);
            sym2idx.insert(node.egglog.cur_sym(), idx);
            log::debug!("sym2idx insert {}", node.egglog.cur_sym());
        }
        // 3. append edge (succs)
        for node in &v {
            let from = node.egglog.cur_sym();
            let from_idx = sym2idx[&from];
            log::debug!("succs of {} is {:?}", from, node.egglog.succs());
            for to in node.egglog.succs() {
                if let Some(&to_idx) = sym2idx.get(&to) {
                    g.add_edge(from_idx, to_idx, ());
                } else {
                    panic!("{} not found in wag", to)
                }
            }
        }
        g
    }
    pub fn pats_to_dot(&self, path: PathBuf) {
        let g = self.wag_build_petgraph();
        generate_dot_by_graph(&g, path, &[]);
    }

    /// topo all input nodes with specified direction
    pub fn topo_sort(&self, index_set: &IndexSet<Sym>, direction: TopoDirection) -> Vec<Sym> {
        // init in degrees and out degrees
        let mut ins = Vec::new();
        let mut outs = Vec::new();
        ins.resize(index_set.len(), 0);
        outs.resize(index_set.len(), 0);
        for (i, (in_degree, out_degree)) in ins.iter_mut().zip(outs.iter_mut()).enumerate() {
            let sym = index_set[i];
            let node = self.map.get(&sym).unwrap();
            *in_degree = Self::degree_in_subgraph(node.preds().into_iter().map(|x| x), index_set);
            *out_degree = Self::degree_in_subgraph(node.succs().into_iter(), index_set);
        }
        let (mut _ins, mut outs) = match direction {
            TopoDirection::Up => (ins, outs),
            TopoDirection::Down => (outs, ins),
        };
        let mut rst = Vec::new();
        let mut wait_for_release = Vec::new();
        // start node should not have any out edges in subgraph
        for (idx, _value) in outs.iter().enumerate() {
            if 0 == outs[idx] {
                wait_for_release.push(index_set[idx]);
            }
        }
        while !wait_for_release.is_empty() {
            let popped = wait_for_release.pop().unwrap();
            for target in self.map.get(&popped).unwrap().preds() {
                if let Some(idx) = index_set.get_index_of(&target) {
                    outs[idx] -= 1;
                    if outs[idx] == 0 {
                        log::debug!("{} found to be 0", target);
                        wait_for_release.push(target);
                    }
                }
            }
            rst.push(popped);
        }
        log::debug!("{:?}", rst);
        rst
    }

    /// calculate the edges in the subgraph
    pub fn degree_in_subgraph(nodes: impl Iterator<Item = Sym>, index_set: &IndexSet<Sym>) -> u32 {
        nodes.fold(0, |acc, item| {
            if index_set.contains(&item) {
                acc + 1
            } else {
                acc
            }
        })
    }

    fn add_node(&self, node: &(impl EgglogNode + 'static)) {
        let node = node.clone_dyn();
        let mut node = PatRecordNode::new(
            node,
            self.next_pat_id.load(std::sync::atomic::Ordering::Acquire),
        );
        let sym = node.work_node.cur_sym();
        for succ_node in node.succs_mut() {
            self.map
                .get_mut(succ_node)
                .unwrap_or_else(|| panic!("node {} not found", succ_node.as_str()))
                .work_node
                .preds
                .push(sym);
        }
        self.map.insert(node.work_node.cur_sym(), node);
    }

    fn current_pat_id(&self) -> PatId {
        PatId(self.next_pat_id.load(std::sync::atomic::Ordering::Acquire))
    }
}

unsafe impl Send for PatRecorder {}
unsafe impl Sync for PatRecorder {}
// MARK: Tx
impl Tx for PatRecorder {
    fn send(&self, _: TxCommand) {
        panic!("should not impl send")
    }

    fn on_new(&self, symnode: &(impl EgglogNode + 'static)) {
        self.add_node(symnode);
    }

    #[track_caller]
    fn on_func_set<'a, F: EgglogFunc>(
        &self,
        _: <F::Input as EgglogFuncInputs>::Ref<'a>,
        _: <F::Output as EgglogFuncOutput>::Ref<'a>,
    ) {
        panic!("should not impl on_func_set");
    }

    fn on_union(&self, _: &(impl EgglogNode + 'static), _: &(impl EgglogNode + 'static)) {
        panic!("should not impl on_union");
    }
}

impl NodeDropper for PatRecorder {
    fn on_drop(&self, dropped: &mut (impl EgglogNode + 'static)) {
        self.map
            .get_mut(&dropped.cur_sym())
            .expect("should have been inserted")
            .selected = false;
    }
}
impl NodeOwner for PatRecorder {
    type OwnerSpecDataInNode<T: EgglogTy, V: EgglogEnumVariantTy> = u32;
}
impl NodeSetter for PatRecorder {
    fn on_set(&self, _node: &mut (impl EgglogNode + 'static)) {
        // do nothing
    }
}

impl PatRec for PatRecorder {
    fn on_new_query_leaf(&self, node: &(impl EgglogNode + 'static)) {
        self.add_node(node);
    }
    fn on_new_constraint(&self, constraint: impl IntoConstraintFact) {
        log::debug!("constraint: {:?}", constraint);
        self.constraint_table
            .entry(self.current_pat_id())
            .or_default()
            .push(Box::new(constraint));
    }

    fn on_record_start(&self) {
        log::debug!("record start");
    }

    fn on_record_end<T: PatRecSgl>(&self, _pat_vars: &impl PatVars<T>) -> PatId {
        log::debug!("record end");
        let current_pat_id = self.current_pat_id();
        // build root_table, put all nodes with 0 indegree into root_table
        let sym_set = IndexSet::from_iter(self.map.iter().map(|entry| *entry.key()));
        let mut roots = Vec::new();
        for node in self.map.iter() {
            let in_deg = Self::degree_in_subgraph(node.value().preds(), &sym_set);
            if in_deg == 0 {
                // push root node
                roots.push(node.value().work_node.cur_sym());
            }
        }
        self.root_table.insert(current_pat_id, roots);

        PatId(
            self.next_pat_id
                .fetch_add(1, std::sync::atomic::Ordering::SeqCst),
        )
    }
    // find pattern defined in current Tx and transformed it into [Facts<String,String>]
    // one pattern may has multiple roots
    fn pat2query(&self, pat_id: PatId) -> FactsBuilder {
        // build TermDag from roots
        let pat_nodes = IndexSet::from_iter(self.map.iter().filter_map(|x| {
            if x.value().pat_id == pat_id {
                Some(*x.key())
            } else {
                None
            }
        }));
        let mut facts_builder = FactsBuilder::new();
        let topo_syms = self.topo_sort(&pat_nodes, TopoDirection::Up);
        for sym in pat_nodes {
            let node = &self.map.get(&sym).unwrap().work_node.egglog;
            node.add_table_fact(&mut facts_builder);
        }
        log::debug!("topo:{:?}", topo_syms);

        match self.constraint_table.remove(&pat_id) {
            Some(constraint_facts) => {
                facts_builder.add_constraint_facts(constraint_facts.1);
            }
            None => {}
        }
        facts_builder
    }
}
