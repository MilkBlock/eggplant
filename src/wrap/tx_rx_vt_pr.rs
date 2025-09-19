use crate::wrap::{
    EgglogFunc, EgglogFuncInputs, EgglogFuncOutput,
    etc::{Escape, quote, topo_sort},
};

use super::*;
use core::panic;
use dashmap::DashMap;
use egglog::{
    EGraph, PrettyPrintConfig, RunReport, SerializeConfig,
    core::Query,
    prelude::{EqProofId, ProofStore, TermProofId, add_ruleset, run_ruleset},
    span,
    util::{IndexMap, IndexSet},
};
use graphviz_rust::dot_structures::Attribute;
use log::info;
use petgraph::{EdgeType, prelude::StableDiGraph};
use std::{
    collections::HashMap,
    fs::File,
    io::Write,
    path::PathBuf,
    sync::{Arc, Mutex},
};

/// support features:
/// 1. Tx: send command to egraph
/// 2. Rx: get value from egraph
/// 3. VersionCtl: version control for nodes
/// 4. PR: Pattern recorder
/// 5. generate proof (opt.)
pub struct TxRxVTPR {
    pub egraph: Mutex<EGraph>,
    map: DashMap<Sym, WorkAreaNode>,
    /// used to store newly staged node among committed nodes (Not only the currently latest node but also nodes of old versions)
    staged_set_map: DashMap<Sym, Box<dyn EgglogNode>>,
    staged_new_map: Mutex<IndexMap<Sym, Box<dyn EgglogNode>>>,
    checkpoints: Mutex<Vec<CommitCheckPoint>>,
    registry: EgglogTypeRegistry,
    /// mapping from sym to value, used to query [`Value`] in EGraph of specified [`Sym`]
    sym2value_map: Arc<DashMap<Sym, egglog::Value>>,
    proof_store: Mutex<ProofStore>,
    commit_counter: Mutex<u32>,
}

#[allow(unused)]
#[derive(Debug)]
pub struct CommitCheckPoint {
    committed_node_root: Sym,
    staged_set_nodes: Vec<Sym>,
    staged_new_nodes: Vec<Sym>,
}

/// Tx with version ctl feature
impl TxRxVTPR {
    pub fn clear_egraph(&self) {
        let mut egraph = self.egraph.lock().unwrap();
        self.sym2value_map.clear();
        *egraph = EGraph::default();
    }
    // collect all lastest ancestors of cur_sym, without cur_sym
    pub fn collect_latest_ancestors(&self, cur_sym: Sym, index_set: &mut IndexSet<Sym>) {
        let sym_node = self.map.get(&cur_sym).unwrap();
        let v = sym_node.preds.clone();
        drop(sym_node);
        for pred in v {
            // if pred has been accessed or it's not the lastest version
            if index_set.contains(&pred) || self.map.get(&pred).unwrap().next.is_some() {
                // do nothing
            } else {
                index_set.insert(pred.clone());
                self.collect_latest_ancestors(pred, index_set)
            }
        }
    }
    // collect all ancestors of cur_sym, without cur_sym
    pub fn collect_ancestors(&self, cur_sym: Sym, index_set: &mut IndexSet<Sym>) {
        let sym_node = self.map.get(&cur_sym).unwrap();
        let v = sym_node.preds.clone();
        drop(sym_node);
        for pred in v {
            // if pred has been accessed or it's not the lastest version
            if index_set.contains(&pred) {
                // do nothing
            } else {
                index_set.insert(pred.clone());
                self.collect_ancestors(pred, index_set)
            }
        }
    }
    /// collect all strict descendants of cur_sym, without cur_sym
    pub fn collect_descendants(&self, cur_sym: Sym, index_set: &mut IndexSet<Sym>) {
        let succs = self
            .staged_set_map
            .get(&cur_sym)
            .map(|x| x.succs())
            .unwrap_or(self.map.get(&cur_sym).unwrap().succs());
        for succ in succs {
            if index_set.contains(&succ) || self.map.get(&succ).unwrap().next.is_some() {
                // do nothing this succ node has been accessed
            } else {
                index_set.insert(succ.clone());
                self.collect_descendants(succ, index_set)
            }
        }
    }
    /// topo all input nodes
    pub fn topo_sort(&self, index_set: &IndexSet<Sym>, direction: TopoDirection) -> Vec<Sym> {
        // init in degrees and out degrees
        let mut ins = Vec::new();
        let mut outs = Vec::new();
        ins.resize(index_set.len(), 0);
        outs.resize(index_set.len(), 0);
        for (i, (in_degree, out_degree)) in ins.iter_mut().zip(outs.iter_mut()).enumerate() {
            let sym = index_set[i];
            let node = self.map.get(&sym).unwrap();
            *in_degree =
                TxRxVTPR::degree_in_subgraph(node.preds().into_iter().map(|x| *x), index_set);
            *out_degree = TxRxVTPR::degree_in_subgraph(node.succs().into_iter(), index_set);
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
            log::debug!(
                "popped is {} preds:{:?}",
                popped,
                &self.map.get(&popped).unwrap().preds
            );
            for target in &self.map.get(&popped).unwrap().preds {
                if let Some(idx) = index_set.get_index_of(target) {
                    outs[idx] -= 1;
                    if outs[idx] == 0 {
                        log::debug!("{} found to be 0", target);
                        wait_for_release.push(*target);
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
    pub fn new() -> Self {
        let tx = Self {
            egraph: Mutex::new({
                let mut e = EGraph::default();
                Self::add_eggplant_sorts(&mut e);
                e
            }),
            registry: EgglogTypeRegistry::new_with_inventory(),
            map: DashMap::new(),
            staged_set_map: DashMap::new(),
            staged_new_map: Mutex::new(IndexMap::default()),
            checkpoints: Mutex::new(vec![]),
            sym2value_map: Arc::new(DashMap::new()),
            proof_store: Mutex::new(ProofStore::default()),
            commit_counter: Mutex::new(0),
        };
        let type_defs = EgglogTypeRegistry::collect_type_defs();
        for def in type_defs {
            tx.send(TxCommand::NativeCommand { command: def });
        }
        tx
    }
    fn add_eggplant_sorts(e: &mut EGraph) {
        egglog::prelude::add_leaf_sort(e, StaticStrSort, span!()).unwrap();
        for sort_fn in inventory::iter::<UserBaseSort> {
            (sort_fn.sort_insert_fn)(e)
        }
    }
    /// this tracing is implemented by Proof Table writing, which is quick but without full proof
    pub fn new_with_fast_proof() -> Self {
        let tx = Self {
            egraph: Mutex::new({
                let mut e = EGraph::with_tracing();
                Self::add_eggplant_sorts(&mut e);
                e
            }),
            registry: EgglogTypeRegistry::new_with_inventory(),
            map: DashMap::new(),
            staged_set_map: DashMap::new(),
            staged_new_map: Mutex::new(IndexMap::default()),
            checkpoints: Mutex::new(vec![]),
            sym2value_map: Arc::new(DashMap::new()),
            proof_store: Mutex::new(ProofStore::default()),
            commit_counter: Mutex::new(0),
        };
        let type_defs = EgglogTypeRegistry::collect_type_defs();
        for def in type_defs {
            tx.send(TxCommand::NativeCommand { command: def });
        }
        tx
    }
    /// this tracing is implemented by TableAction tracing without proof table writing, might be slow
    /// but with full feature
    pub fn new_with_accurate_proof() -> Self {
        let tx = Self {
            egraph: Mutex::new({
                let mut e = EGraph::with_tracing();
                Self::add_eggplant_sorts(&mut e);
                e
            }),
            registry: EgglogTypeRegistry::new_with_inventory(),
            map: DashMap::new(),
            staged_set_map: DashMap::new(),
            staged_new_map: Mutex::new(IndexMap::default()),
            checkpoints: Mutex::new(vec![]),
            sym2value_map: Arc::new(DashMap::new()),
            proof_store: Mutex::new(ProofStore::default()),
            commit_counter: Mutex::new(0),
        };
        let type_defs = EgglogTypeRegistry::collect_type_defs();
        for def in type_defs {
            tx.send(TxCommand::NativeCommand { command: def });
        }
        tx
    }
    // if auto_latest is true, it will locate the latest version of the node and add it to the map
    fn add_node(&self, mut node: WorkAreaNode, auto_latest: bool) {
        let sym = node.cur_sym();
        for node in node.succs_mut() {
            log::debug!("succ is {}", node);
            let latest = if auto_latest {
                &self.locate_latest(*node)
            } else {
                &*node
            };
            self.map
                .get_mut(node)
                .unwrap_or_else(|| panic!("node {} not found", latest.as_str()))
                .preds
                .push(sym);
            *node = *latest;
        }
        log::debug!("map insert {:?}", node.egglog);
        if let Some(node) = self.map.insert(node.cur_sym(), node) {
            panic!("repeat insertion of node {:?}", node);
        }
    }
    /// update all ancestors recursively in guest and send updated term by egglog native command to host
    /// when you update the node
    /// return all WorkAreaNodes created
    fn update_nodes(
        &self,
        root: Sym,
        staged_latest_syms_and_staged_nodes: Vec<(Sym, Box<dyn EgglogNode>)>,
    ) -> IndexSet<Sym> {
        if staged_latest_syms_and_staged_nodes.len() == 0 {
            return IndexSet::default();
        }

        log::debug!("update_nodes:{:#?}", self.map);
        // collect all ancestors that need copy
        let mut ancestors = IndexSet::default();
        for (latest_sym, _) in &staged_latest_syms_and_staged_nodes {
            log::debug!("collect ancestors of {:?}", latest_sym);
            self.collect_ancestors(*latest_sym, &mut ancestors);
        }
        let mut root_ancestors = IndexSet::default();
        self.collect_ancestors(root, &mut root_ancestors);
        if !root_ancestors.is_empty() {
            panic!("commit should be applied to root");
        }
        root_ancestors.insert(root);
        let mut root_descendants = IndexSet::default();
        self.collect_descendants(root, &mut root_descendants);
        root_descendants.insert(root);
        let intersection = IndexSet::from_iter(
            ancestors
                .intersection(&root_descendants)
                .cloned()
                .into_iter(),
        );
        let mut ancestors =
            IndexSet::from_iter(intersection.union(&root_ancestors).into_iter().cloned());
        let mut staged_latest_sym_map = IndexMap::default();
        // here we insert all staged_latest_sym because latest_ancestors do may not include all of them
        for (staged_latest_sym, staged_node) in staged_latest_syms_and_staged_nodes {
            ancestors.insert(staged_latest_sym);
            staged_latest_sym_map.insert(staged_latest_sym, staged_node);
        }

        // NB: ancestors set now contains all nodes that need to create
        log::debug!("all latest_ancestors {:?}", ancestors);

        let mut next_syms = IndexSet::default();
        for ancestor in ancestors {
            let mut latest_node = self.map.get_mut(&self.locate_latest(ancestor)).unwrap();
            let latest_sym = latest_node.cur_sym();
            let mut next_latest_node = latest_node.clone();
            let next_sym = next_latest_node.roll_sym();

            // set next, chain latest version to next latest version
            latest_node.next = Some(next_sym);
            drop(latest_node);

            // set prev, chain next latest version to latest version
            next_latest_node.prev = Some(latest_sym);

            next_syms.insert(next_sym);
            if !staged_latest_sym_map.contains_key(&ancestor) {
                log::debug!("map insert {},{:?}", next_sym, next_latest_node);
                if let Some(node) = self.map.insert(next_sym, next_latest_node) {
                    panic!("repeat insertion of node {:?}", node);
                }
            } else {
                let mut staged_node = staged_latest_sym_map.get(&ancestor).unwrap().clone_dyn();
                *staged_node.cur_sym_mut() = next_sym;

                let mut staged_node = WorkAreaNode::new(staged_node);
                // set prev, chain next latest version to latest version
                staged_node.prev = Some(latest_sym);
                staged_node.preds = self.map.get(&ancestor).unwrap().preds.clone();

                log::debug!("map insert {},{:?}", next_sym, staged_node);
                if let Some(node) = self.map.insert(next_sym, staged_node) {
                    panic!("repeat insertion of node {:?}", node);
                }
            }
        }
        log::debug!("mid update_nodes:{:#?}", self.map);

        // update all preds
        let mut succ_preds_map = HashMap::new();
        for &next_sym in &next_syms {
            let sym_node = self.map.get(&next_sym).unwrap();
            for &sym in sym_node.preds() {
                let latest_sym = self.locate_latest(sym);
                if sym != latest_sym && !succ_preds_map.contains_key(&latest_sym) {
                    succ_preds_map.insert(sym, latest_sym);
                }
            }
            for sym in sym_node.succs() {
                let latest_sym = self.locate_latest(sym);
                if sym != latest_sym && !succ_preds_map.contains_key(&latest_sym) {
                    succ_preds_map.insert(sym, latest_sym);
                }
            }
        }
        log::debug!("preds 「map」to be {:?}", succ_preds_map);

        for &next_sym in &next_syms {
            let mut sym_node = self.map.get_mut(&next_sym).unwrap();
            for sym in sym_node.preds_mut() {
                if let Some(found) = succ_preds_map.get(sym) {
                    *sym = *found;
                }
            }
            for sym in sym_node.succs_mut() {
                if let Some(found) = succ_preds_map.get(sym) {
                    *sym = *found;
                }
            }
        }
        log::debug!("after update_nodes:{:#?}", self.map);
        next_syms
    }
    pub fn wag_build_petgraph(&self) -> StableDiGraph<WorkAreaNode, ()> {
        // 1. collect all nodes
        let v = self
            .map
            .iter()
            .map(|x| x.value().clone())
            .collect::<Vec<_>>();
        let mut g = StableDiGraph::new();
        let mut idxs = Vec::new();
        // 2. map from WorkAreaNode cur_sym to petgraph::NodeIndex
        use std::collections::HashMap;
        let mut sym2idx = HashMap::new();
        log::debug!("map:{:?}", self.map);
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
}

unsafe impl Send for TxRxVTPR {}
unsafe impl Sync for TxRxVTPR {}
impl VersionCtl for TxRxVTPR {
    /// locate the lastest version of the symbol
    fn locate_latest(&self, old: Sym) -> Sym {
        let map = &self.map;
        let mut cur = old;
        while let Some(newer) = map.get(&cur).unwrap().next {
            cur = newer;
        }
        cur
    }

    // locate next version
    fn locate_next(&self, node: Sym) -> Sym {
        let map = &self.map;
        let mut cur = node;
        if let Some(newer) = map.get(&cur).unwrap().next {
            cur = newer;
        } else {
            // do nothing because current version is the latest
        }
        cur
    }

    fn set_latest(&self, node: &mut Sym) {
        *node = self.locate_latest(*node);
    }

    fn set_next(&self, node: &mut Sym) {
        *node = self.locate_next(*node);
    }
    fn locate_prev(&self, node: Sym) -> Sym {
        let map = &self.map;
        let mut cur = node;
        if let Some(older) = map.get(&cur).unwrap().prev {
            cur = older;
        } else {
            // do nothing because current version is the oldest
        }
        cur
    }
    fn set_prev(&self, node: &mut Sym) {
        *node = self.locate_prev(*node);
    }
}

// MARK: Tx
impl Tx for TxRxVTPR {
    fn send(&self, transmitted: TxCommand) {
        let mut egraph = self.egraph.lock().unwrap();
        match transmitted {
            TxCommand::StringCommand { command } => {
                log::info!("{}", command);
                egraph.parse_and_run_program(None, &command).unwrap();
            }
            TxCommand::NativeCommand { command } => {
                log::debug!("{}", command.to_string());
                egraph.run_program(vec![command]).unwrap();
            }
        }
    }

    fn on_new(&self, node: &(impl EgglogNode + 'static)) {
        self.staged_new_map
            .lock()
            .unwrap()
            .insert(node.cur_sym(), node.clone_dyn());
    }

    #[track_caller]
    fn on_func_set<'a, F: EgglogFunc>(
        &self,
        _input: <F::Input as EgglogFuncInputs>::Ref<'a>,
        _output: <F::Output as EgglogFuncOutput>::Ref<'a>,
    ) {
        // let input_nodes = input.as_evalues();
        // let input_syms = input_nodes.iter().map(|x| x.get_symlit());
        // let output = output.as_evalue().get_symlit();
        // self.send(TxCommand::StringCommand {
        //     command: format!(
        //         "(set ({} {}) {} )",
        //         F::FUNC_NAME,
        //         input_syms.map(|x| format!("{}", x)).collect::<String>(),
        //         output
        //     ),
        // });
        todo!("proof not yet implemented")
    }

    fn on_union(&self, node1: &(impl EgglogNode + 'static), node2: &(impl EgglogNode + 'static)) {
        self.send(TxCommand::StringCommand {
            command: format!("(union {} {})", node1.cur_sym(), node2.cur_sym()),
        });
    }
}

impl TxCommit for TxRxVTPR {
    /// commit behavior:
    /// 1. commit all descendants (if you also call set fn on subnodes they will also be committed)
    /// 2. commit basing on the latest ersion of the working graph (working graph records all versions)
    /// 3. if TxCommit is implemented you can change egraph by `commit` rather than `set`. It's lazy because it uses a buffer to store all `staged set`.
    /// 4. if you didn't stage `set` on nodes, it will do nothing on commited node only flush all staged_new_node buffer
    /// 5. after commit, you can get [`egglog::Value`] of [`Sym`] because they has been committed to egrpah
    fn on_commit<T: EgglogNode>(&self, commit_root: &T) {
        // log::debug!("on_commit {:?}", commit_root.to_egglog_string());
        let check_point = CommitCheckPoint {
            committed_node_root: commit_root.cur_sym(),
            staged_set_nodes: self.staged_set_map.iter().map(|a| *a.key()).collect(),
            staged_new_nodes: self
                .staged_new_map
                .lock()
                .unwrap()
                .iter()
                .map(|a| *a.0)
                .collect(),
        };
        log::debug!("{:?}", check_point);
        log::debug!("staged_set_map:{:?}", self.staged_set_map);
        log::debug!("staged_new_map:{:?}", self.staged_new_map.lock().unwrap());
        self.checkpoints.lock().unwrap().push(check_point);

        // process new nodes
        let mut news = self.staged_new_map.lock().unwrap();
        // set of staged_new_map to avoid repeat stage since they have been topo ordered.
        // we don't need to do topo sort as below's
        let mut backup_staged_new_syms = IndexSet::default();
        let len = news.len();
        for (new, new_node) in news.drain(0..len) {
            self.add_node(WorkAreaNode::new(new_node.clone_dyn()), false);
            backup_staged_new_syms.insert(new);
        }
        // collect all staged ndoes
        let all_staged = IndexSet::from_iter(self.staged_set_map.iter().map(|a| *a.key()));
        let mut descendants = IndexSet::default();
        self.collect_descendants(commit_root.cur_sym(), &mut descendants);
        descendants.insert(commit_root.cur_sym());

        let staged_descendants_old = descendants.intersection(&all_staged).collect::<Vec<_>>();
        let staged_descendants_latest = staged_descendants_old
            .iter()
            .map(|x| self.locate_latest(**x))
            .collect::<Vec<_>>();

        let iter_impl = staged_descendants_latest.iter().cloned().zip(
            staged_descendants_old
                .iter()
                .map(|x| self.staged_set_map.remove(*x).unwrap().1),
        );
        let created = self.update_nodes(commit_root.cur_sym(), iter_impl.collect());
        log::trace!("created {:#?}", created);

        let topo_sorted_nodes = self.topo_sort(&created, TopoDirection::Up);
        log::trace!("nodes to topo:{:?}", topo_sorted_nodes);

        // create ruleset
        let rule_name = format!(
            "commit_rule_{}",
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos()
        );
        // build check point
        let ruleset_name = format!("{}_ruleset", rule_name);

        let sym2value_map = Arc::clone(&self.sym2value_map);
        let topo_sorted_nodes_clone = topo_sorted_nodes.clone();
        log::debug!("sorted to be {:?}", topo_sorted_nodes);
        let map_clone = self.map.clone();

        // sue add_rule API to create rule
        let mut egraph = self.egraph.lock().unwrap();
        egglog::prelude::add_ruleset(&mut egraph, &ruleset_name).unwrap();
        let rule_rst = egraph.raw_add_rule_with_name(
            format!("commit{}", self.commit_counter.lock().unwrap()),
            ruleset_name.to_string(),
            Query::default(),
            &[],
            move |ctx, _| {
                let mut ctx = RuleCtx::new(ctx);
                let sym2value_map = sym2value_map.clone();
                for &sym in &backup_staged_new_syms {
                    log::debug!("topo_insert:{}", sym);
                    if let Some(node) = map_clone.get(&sym) {
                        // add node to egraph using native_egglog API
                        let value = node.egglog.native_egglog(&mut ctx, &sym2value_map);
                        // store sym value pair to sym_to_value_map
                        sym2value_map.insert(sym, value);
                        log::debug!("Added node {} to sym_to_value_map using native_egglog", sym);
                    }
                }

                // append all topo sorted nodes
                for &sym in &topo_sorted_nodes_clone {
                    log::debug!("topo_update:{}", sym);
                    if let Some(node) = map_clone.get(&sym) {
                        // add node to egraph using native_egglog API
                        let value = node.egglog.native_egglog(&mut ctx, &sym2value_map);
                        // store sym value pair to sym_to_value_map
                        sym2value_map.insert(sym, value);
                        log::debug!(
                            "Update node {} to sym_to_value_map using native_egglog",
                            sym
                        );
                    }
                }

                Some(())
            },
        );
        log::debug!("Commit Rule define {:?}", rule_rst);

        // execute commit rule
        let rst = run_ruleset(&mut egraph, &ruleset_name);
        log::debug!("Commit Rule execution results: {:?}", rst);
        *self.commit_counter.lock().unwrap() += 1;
    }

    fn on_stage<T: EgglogNode + ?Sized>(&self, node: &T) {
        self.staged_set_map.insert(node.cur_sym(), node.clone_dyn());
    }
}

// MARK: Rx
impl Rx for TxRxVTPR {
    fn on_func_get<'a, 'b, F: EgglogFunc>(
        &self,
        input: <F::Input as EgglogFuncInputs>::Ref<'a>,
    ) -> F::Output {
        let input_nodes = input.as_evalues();
        let output = {
            let egraph = &mut self.egraph.lock().unwrap();
            let output = get_func_value(egraph, F::FUNC_NAME, input_nodes);
            output
        };
        let sym = self.on_pull_value(Value::<F::Output>::new(output));
        match sym {
            SymLit::Sym(sym) => {
                let node = &self.map.get(&sym).unwrap().egglog;
                let output: &F::Output =
                    unsafe { &*(node.as_ref() as *const dyn EgglogNode as *const F::Output) };
                output.clone()
            }
            SymLit::Lit(literal) => F::Output::from_literal(&literal),
        }
    }

    fn on_funcs_get<'a, 'b, F: EgglogFunc>(
        &self,
        _max_size: Option<usize>,
    ) -> Vec<(
        <F::Input as EgglogFuncInputs>::Ref<'b>,
        <F::Output as EgglogFuncOutput>::Ref<'b>,
    )> {
        todo!()
    }
    fn on_pull_value<T: EgglogTy>(&self, value: Value<T>) -> SymLit {
        log::debug!("pulling value {:?}", value);
        let egraph = self.egraph.lock().unwrap();
        let sort = egraph.get_sort_by_name(T::TY_NAME).unwrap();
        let mut term2sym = HashMap::new();
        let (term_dag, start_term, cost) = egraph
            .extract_value(
                sort,
                egraph
                    .backend
                    // here we should get canno repr of specified value because egglog-backend will merge two equivalent e-node with one canno repr
                    .get_canon_repr(value.val, egglog::sort::ColumnTy::Id),
            )
            .unwrap();
        log::debug!("pulled dag: {:?}", term_dag);

        let root_idx = term_dag.lookup(&start_term);
        log::debug!("term_dag:{:?}, {:?}", term_dag, start_term);
        let mut ret_sym = None;

        let topo = topo_sort(&term_dag);
        for &i in &topo {
            let new_fn = self
                .registry
                .get_fn(i, &term_dag)
                .unwrap_or_else(|| panic!("didn't found fn of term {:?}", term_dag.get(i)));
            let boxed_node = new_fn(i, &term_dag, &mut term2sym);
            if i == root_idx {
                ret_sym = Some(boxed_node.cur_sym())
            }
            log::info!("pulled add node: {:?}", boxed_node);
            self.add_node(WorkAreaNode::new_pulled(boxed_node, value.erase()), false);
        }
        log::debug!(
            "term:{:?}, term_dag:{:?}, cost:{}",
            start_term,
            term_dag,
            cost
        );
        match ret_sym {
            Some(sym) => {
                // situation 1
                // func ret a Variant Node
                SymLit::Sym(sym)
            }
            None => {
                // situtaion 2
                // func ret a BaseTy
                SymLit::Lit(match term_dag.get(0) {
                    egglog::Term::Lit(literal) => literal.clone(),
                    _ => {
                        panic!("termdag[0] should be a literal")
                    }
                })
            }
        }
    }
    fn on_pull_sym<T: EgglogTy>(&self, sym: Sym) -> SymLit {
        if let Some(value) = self.sym2value_map.get(&sym) {
            self.on_pull_value(Value::<T>::new(*value))
        } else {
            panic!("{}'s value not found in sym2value_map", sym)
        }
    }
}

impl NodeDropper for TxRxVTPR {}
impl NodeOwner for TxRxVTPR {
    type OwnerSpecDataInNode<T: EgglogTy, V: EgglogEnumVariantTy> = ();
}

impl NodeSetter for TxRxVTPR {
    fn on_set(&self, _node: &mut (impl EgglogNode + 'static)) {
        // do nothing
        // the node may be set but we don't care
        // the rst will be committed throguh commit API
    }
}

impl RuleRunner for TxRxVTPR {
    type EqProof = EqProofId;
    type TermProof = TermProofId;

    fn add_rule<PR: PatRecSgl, P: PatVars<PR>>(
        &self,
        rule_name: &str,
        rule_set: RuleSetId,
        pat: impl Fn() -> P,
        action: impl Fn(&mut RuleCtx, &P::Valued) + Send + Sync + 'static + Clone,
    ) {
        let mut egraph = self.egraph.lock().unwrap();
        PR::on_record_start();
        let pat_vars = pat();
        let pat_id = PR::on_record_end(&pat_vars);

        let query = PR::pat2query(pat_id).build(&egraph);
        let vars = pat_vars.to_str_arcsort(&egraph);
        log::debug!("{:#?}", query);
        log::debug!("{:#?}", vars);

        let rst = egraph.raw_add_rule_with_name(
            rule_name.to_string(),
            rule_set.0.to_string(),
            query,
            vars.as_slice(),
            move |ctx, values| {
                let mut ctx = RuleCtx::new(ctx);
                let valued_pat_vars = P::Valued::from_plain_values(&mut values.iter().cloned());
                action(&mut ctx, &valued_pat_vars);
                Some(())
            },
        );
        let (s, rule_id) = rst.expect("add_rule_set err");
        info!("reg rule rule_id {rule_id:?} {s}");
    }

    fn new_ruleset(&self, rule_set: &'static str) -> RuleSetId {
        let mut egraph = self.egraph.lock().unwrap();
        add_ruleset(&mut egraph, rule_set).unwrap();
        RuleSetId(rule_set)
    }

    #[track_caller]
    fn run_ruleset(&self, ruleset_id: RuleSetId, until: RunConfig) -> RunReport {
        let mut egraph = self.egraph.lock().unwrap();
        match until {
            RunConfig::Sat => {
                let mut run_report = RunReport::default();
                loop {
                    let iter_report = egraph.step_rules(ruleset_id.0);
                    let updated = iter_report.updated;
                    run_report.union(iter_report);
                    if !updated {
                        break run_report;
                    }
                }
            }
            RunConfig::Times(times) => {
                let mut run_report = RunReport::default();
                for _ in 0..times {
                    let iter_report = egraph.step_rules(ruleset_id.0);
                    run_report.union(iter_report);
                }
                run_report
            }
            RunConfig::Once => {
                let run_report = egraph.step_rules(ruleset_id.0);
                run_report
            }
        }
    }

    fn explain<T: EgglogTy>(&self, value: Value<T>) -> TermProofId {
        let mut prf_store = self.proof_store.lock().unwrap();
        let prf_id = self
            .egraph
            .lock()
            .unwrap()
            .backend
            .explain_term(value.erase(), &mut prf_store)
            .unwrap();
        prf_store
            .print_term_proof(prf_id, &mut std::io::stdout())
            .unwrap();
        prf_id
    }

    fn value<T: EgglogNode>(&self, node: &T) -> Value<T> {
        Value::new(
            *self
                .sym2value_map
                .get(&node.cur_sym())
                .expect("sym should be comitted before get value")
                .value(),
        )
    }

    fn explain_eq<T1: EgglogTy, T2: EgglogTy>(
        &self,
        v1: super::Value<T1>,
        v2: super::Value<T2>,
    ) -> egglog::prelude::EqProofId {
        let mut prf_store = self.proof_store.lock().unwrap();
        let prf_id = self
            .egraph
            .lock()
            .unwrap()
            .backend
            .explain_terms_equal(v1.erase(), v2.erase(), &mut prf_store)
            .unwrap();
        prf_store
            .print_eq_proof_pretty(
                prf_id,
                &PrettyPrintConfig::default(),
                &mut std::io::stdout(),
            )
            .unwrap();
        println!("");
        prf_id
    }
}

impl ToDot for TxRxVTPR {
    /// transform EGraph into dot file
    fn egraph_to_dot(&self, path: PathBuf) {
        let egraph = self.egraph.lock().unwrap();
        let serialized = egraph.serialize_tracing(SerializeConfig::default());
        let dot_path = path;
        serialized
            .to_dot_file(dot_path.clone())
            .unwrap_or_else(|_| panic!("Failed to write dot file to {dot_path:?}"));
    }

    /// transform WorkAreaGraph into dot file with enhanced visualization
    fn wag_to_dot(&self, path: PathBuf) {
        use graphviz_rust::attributes::NodeAttributes;
        use graphviz_rust::{
            attributes::*,
            dot_generator::*,
            dot_structures::{
                Edge, EdgeTy, Graph, GraphAttributes as GA, Id, Node, NodeId, Stmt, Subgraph,
                Vertex,
            },
            printer::{DotPrinter, PrinterContext},
        };

        // Number of colors in the graphviz color scheme
        // https://graphviz.org/doc/info/colors.html
        const N_COLORS: usize = 12;
        // Initial color to use for the first type
        const INITIAL_COLOR: usize = 2;

        // 1. Group nodes by node.pulled_by
        let mut groupped_nodes = IndexMap::default();
        let mut node_to_type = HashMap::new();
        let mut version_chains = HashMap::new();
        // println!("{:#?}", self.map);

        for pair in &self.map {
            let (sym, node) = (pair.key(), pair.value().clone());
            // println!(
            //     "Node: {:?}, Type: {}, Variant: {:?}, Pulled By: {:?}",
            //     sym,
            //     node.egglog.ty_name(),
            //     node.egglog.variant_name(),
            //     node.pulled_by
            // );
            // let typ = node.egglog.ty_name();
            // let variant = node.egglog.variant_name().unwrap_or("Unknown");
            let node_info = node.pulled_by;

            node_to_type.insert(*sym, node_info.clone());
            groupped_nodes
                .entry(node_info.clone())
                .or_insert_with(Vec::new)
                .push((*sym, node.clone()));

            // Track version chains
            if let Some(next) = node.next {
                version_chains.insert(*sym, next);
            }
        }

        // 2. Start with configuration
        let mut stmts = vec![
            stmt!(GraphAttributes::compound(true)),
            stmt!(GraphAttributes::fontname("helvetica".to_string())),
            stmt!(GraphAttributes::fontsize(10.0)),
            stmt!(GraphAttributes::margin(2.0)),
            stmt!(GraphAttributes::nodesep(0.3)),
            stmt!(GraphAttributes::ranksep(0.6)),
            stmt!(GraphAttributes::colorscheme("set312".to_string())),
            stmt!(GA::Edge(vec![
                EdgeAttributes::arrowsize(0.8),
                EdgeAttributes::fontsize(8.0),
                EdgeAttributes::fontname("helvetica".to_string())
            ])),
            stmt!(GA::Graph(vec![GraphAttributes::style(quote(
                "dashed,rounded,filled"
            ))])),
            stmt!(GA::Node(vec![
                NodeAttributes::shape(shape::none),
                NodeAttributes::style(quote("rounded")),
                NodeAttributes::margin(0.0),
                NodeAttributes::fontname("helvetica".to_string()),
                NodeAttributes::fontsize(15.0)
            ])),
        ];

        // 3. Add each type as a subgraph
        let mut type_colors = HashMap::new();

        for (_i, (pulled_from, nodes)) in groupped_nodes.iter().enumerate() {
            let mut inner_stmts = vec![stmt!(SubgraphAttributes::label(quote(
                if pulled_from.is_some() {
                    format!("pulled_from_{:?}", pulled_from)
                } else {
                    String::new()
                }
                .as_str()
            )))];
            // let subgraph_color = (i + INITIAL_COLOR) % N_COLORS + 1;
            // inner_stmts.push(stmt!(attr!("fillcolor", subgraph_color)));

            // Add nodes for this type
            for (j, (sym, node)) in nodes.iter().enumerate() {
                let color_idx = (j + INITIAL_COLOR) % N_COLORS + 1;
                let type_color = type_colors
                    .entry(node.egglog.variant_name().clone())
                    .or_insert(color_idx);
                inner_stmts.push(stmt!(attr!("fillcolor", type_color)));

                let node_label_big = {
                    format!(
                        "{} {}",
                        node.egglog.variant_name().unwrap_or(node.egglog.ty_name()),
                        if node.egglog.basic_field_names().len() == 0 {
                            String::new()
                        } else {
                            node.egglog
                                .to_egglog_string()
                                .unwrap_or_else(|| String::new())
                                .split_once(node.egglog.variant_name().unwrap_or_default())
                                .unwrap()
                                .1
                                .to_string()
                                .trim()
                                .trim_end_matches(")")
                                .to_string()
                        }
                    )
                };
                // let node_label_small = if node.egglog.complex_field_names().len() == 0 {
                //     format!(
                //         "{}",
                //         node.egglog
                //             .to_egglog_string()
                //             .unwrap_or_else(|| String::new())
                //     )
                // } else {
                //     String::new()
                // };

                let tooltip = format!(
                    "Type: {}\nSym: {}\nVersion: {:?}",
                    node.egglog.ty_name(),
                    sym,
                    if node.next.is_some() {
                        "Next"
                    } else {
                        "Latest"
                    }
                );

                // <TR><TD BALIGN=\"left\" CELLPADDING=\"4\" FONT_SIZE=\"3\">{}</TD></TR>\
                // Create HTML label with version info
                let html_label = format!(
                    "<<TABLE BGCOLOR=\"white\" CELLBORDER=\"0\" CELLSPACING=\"0\" CELLPADDING=\"2\" style=\"rounded\">\
                    <TR><TD BALIGN=\"left\" CELLPADDING=\"2\" FONT_SIZE=\"30\">{}</TD></TR>\
                    </TABLE>>",
                    Escape(&node_label_big),
                    // Escape(&node_label_small),
                );

                let qouted_sym = quote(sym.as_str());
                let node_stmt = node!(
                    qouted_sym;
                    NodeAttributes::label(html_label),
                    NodeAttributes::tooltip(quote(&tooltip))
                    // NodeAttributes::fillcolor(color_name::aqua)
                );
                inner_stmts.push(stmt!(subgraph!(
                    format!("cluster_{}", sym.as_str()),
                    vec![
                        stmt!(SubgraphAttributes::label(quote(""))),
                        stmt!(node_stmt),
                    ]
                )));
            }

            // Create subgraph for this type
            let subgraph_id = match &pulled_from {
                Some(pulled_from) => format!("cluster_pulled_from_value{:?}", pulled_from),
                None => {
                    format!("main")
                }
            };
            let quoted_subgraph_id = quote(format!("cluster_{}", &subgraph_id).as_str());
            // let subgraph_label = quote(&subgraph_id);
            let outer_subgraph_id = quote(&format!("outer_{}", subgraph_id));

            let subgraph = subgraph!(outer_subgraph_id;
                // Disable label for now, to reduce size
                // NodeAttributes::label(subgraph_html_label(&typ)),

                // Nest in empty sub-graph so that we can use rank=same
                // https://stackoverflow.com/a/55562026/907060
                // SubgraphAttributes::label(subgraph_label),
                subgraph!(quoted_subgraph_id,  inner_stmts),

                // Make outer subgraph a cluster but make it invisible, so just used for padding
                // https://forum.graphviz.org/t/how-to-add-space-between-clusters/1209/3
                SubgraphAttributes::style(quote("invis")),
                attr!("cluster", "true")
            );
            stmts.push(stmt!(subgraph));
        }

        // 4. Add edges for dependencies (succs)
        for pair in self.map.iter() {
            let (sym, node) = (pair.key(), pair.value());
            let from_id = quote(sym.as_str());
            for succ in node.egglog.succs() {
                let to_id = quote(succ.as_str());
                let edge = edge!(node_id!(from_id) => node_id!(to_id);
                    EdgeAttributes::color(color_name::black),
                    EdgeAttributes::style(quote("solid"))
                );
                stmts.push(stmt!(edge));
            }
        }

        // 7. Create the final graph
        let graph = graph!(di id!(), stmts);
        let dot_string = graph.print(&mut PrinterContext::default());

        // 8. Write to file
        std::fs::write(path, dot_string).expect("Failed to write dot file");
    }

    fn proof_to_dot(&self, path: PathBuf) {
        let egraph = self.egraph.lock().unwrap();
        let proof_graph = egraph.backend.get_proof_graph().unwrap();

        pub fn generate_dot_by_graph<N: std::fmt::Debug, E: std::fmt::Debug, Ty: EdgeType>(
            g: &petgraph::Graph<N, E, Ty>,
            name: PathBuf,
            graph_config: &[petgraph::dot::Config],
        ) {
            let dot_name = name.clone();
            let mut f = File::create(dot_name.clone()).unwrap();
            let dot_string = format!("{:?}", petgraph::dot::Dot::with_config(&g, &graph_config));
            f.write_all(dot_string.as_bytes()).expect("写入失败");
        }
        generate_dot_by_graph(&proof_graph, path, &[]);
    }
    fn table_view(&self) {
        let egraph = self.egraph.lock().unwrap();
        egraph.backend.dump_debug_info();
    }
}
