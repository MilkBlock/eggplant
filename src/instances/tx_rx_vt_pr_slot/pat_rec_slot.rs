use crate::{
    butler_portugal::{DeBru, DeBrus, Tensor, TensorIndex},
    etc::generate_dot_by_graph,
    prelude::slotted::{FuncName, SlotPendingOps, SlottedCtx},
    wrap::*,
};
use dashmap::DashMap;
use derive_more::{Debug, Deref};
use egglog::{EGraph, util::IndexSet};
use petgraph::prelude::StableDiGraph;
use std::{
    collections::HashMap,
    marker::PhantomData,
    path::Path,
    sync::{Arc, Mutex, atomic::AtomicU32},
};

#[derive(Debug)]
pub struct SlottedPatRecorder {
    #[debug(skip)]
    map: DashMap<Sym, SlottedPatRecNode>,
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

    // slotted ctx
    pub slotted_ctx: Arc<SlottedCtx>,
}
struct SlottedPatRecNode {
    work_node: WorkAreaNode,
    pat_id: PatId,

    slot_meta: ArcSlotMetaInner,

    /// if one node dropped in pattern defining function then it is not selected as action args
    selected: bool,
}

impl SlottedPatRecNode {
    pub fn new(node: Box<dyn EgglogNode>, pat_id: u32, slot_meta: ArcSlotMetaInner) -> Self {
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
            slot_meta,
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
impl SlottedPatRecorder {
    pub fn new() -> Self {
        Self {
            map: DashMap::default(),
            _registry: EgglogTypeRegistry::new_with_inventory(),
            patterns: Mutex::new(Default::default()),
            next_pat_id: AtomicU32::new(0),
            root_table: DashMap::default(),
            constraint_table: DashMap::default(),
            slotted_ctx: Default::default(),
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
    pub fn pats_to_dot(&self, path: impl AsRef<Path>) {
        let g = self.wag_build_petgraph();
        generate_dot_by_graph(&g, path.as_ref().to_path_buf(), &[]);
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

    fn add_node_with_slot_meta(
        &self,
        node: &(impl EgglogNode + 'static),
        slot_meta: ArcSlotMetaInner,
    ) {
        let node = node.clone_dyn();
        let mut node = SlottedPatRecNode::new(
            node,
            self.next_pat_id.load(std::sync::atomic::Ordering::Acquire),
            slot_meta,
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
    }

    fn current_pat_id(&self) -> PatId {
        PatId(self.next_pat_id.load(std::sync::atomic::Ordering::Acquire))
    }
}

unsafe impl Send for SlottedPatRecorder {}
unsafe impl Sync for SlottedPatRecorder {}
// MARK: Tx
impl Tx for SlottedPatRecorder {
    fn send(&self, _: TxCommand) {
        panic!("should not impl send")
    }

    fn on_new(&self, node: &(impl EgglogNode + 'static)) {
        let sub_metas: Vec<ArcSlotMetaInner> = {
            node.succs()
                .iter()
                .map(|sub| {
                    self.map
                        .get(sub)
                        .unwrap_or_else(|| panic!("slot node not found"))
                        .slot_meta
                        .clone()
                })
                .collect()
        };
        let var_id_set = {
            node.succs()
                .iter()
                .map(|sub| {
                    self.map
                        .get_mut(sub)
                        .unwrap_or_else(|| panic!("slot node not found"))
                        .slot_meta
                        .var_id_set
                        .clone()
                })
                .flat_map(|x| x.into_iter())
                .collect()
        };
        self.add_node_with_slot_meta(
            node,
            ArcSlotMetaInner::new(SlotMetaInner {
                sub_metas,
                var_id_set,
            }),
        );
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
    fn canonical_raw(&self, _node1: &(impl EgglogNode + 'static)) -> egglog::Value {
        todo!("not yet implemented");
    }

    fn replace_meta(&self, sym: Sym, meta: Box<dyn std::any::Any>) {
        panic!("new meta operation is done in query_slot")
    }
}

impl NodeDropper for SlottedPatRecorder {
    fn on_drop(&self, dropped: &mut (impl EgglogNode + 'static)) {
        self.map
            .get_mut(&dropped.cur_sym())
            .expect("should have been inserted")
            .selected = false;
    }
}
impl NodeOwner for SlottedPatRecorder {
    type OwnerSpecDataInNode<T: EgglogTy, V: EgglogEnumVariantTy> = u32;
}
impl NodeSetter for SlottedPatRecorder {
    fn on_set(&self, _node: &mut (impl EgglogNode + 'static)) {
        // do nothing
    }
}

impl PatRec for SlottedPatRecorder {
    type MetaTy<PR: PatRecSgl> = SlotMeta<PR>;
    fn on_new_query_leaf(&self, node: &(impl EgglogNode + 'static)) {
        self.add_node_with_slot_meta(
            node,
            ArcSlotMetaInner::new(SlotMetaInner {
                sub_metas: Default::default(),
                var_id_set: Default::default(),
            }),
        );
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
    fn pat2fact_builder(&self, pat_id: PatId) -> FactsBuilder {
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

    fn meta_of<PR: PatRecSgl>(&self, node: &(impl EgglogNode + 'static)) -> Self::MetaTy<PR> {
        let inner = self
            .map
            .get(&node.cur_sym())
            .unwrap_or_else(|| panic!("meta of {} not found", node.cur_sym()))
            .slot_meta
            .clone();
        SlotMeta {
            inner,
            _p: PhantomData,
        }
    }

    fn on_ctx_insert<PR: PatRecSgl>(
        &self,
        inputs: Vec<(FuncName, egglog::Value, Self::MetaTy<PR>)>,
        output: (FuncName, egglog::Value),
    ) -> Self::MetaTy<PR> {
        // self.slotted_ctx.insert(cano_value, meta);
        let inner_inputs = inputs
            .iter()
            .map(|(x, y, z)| (*x, y.clone(), z.inner.clone()))
            .collect();
        let merged = SlotMeta::merge(&mut inputs.into_iter().map(|(_x, _y, z)| z));
        self.slotted_ctx.push_pending(SlotPendingOps::Insert {
            inputs: inner_inputs,
            output: (output.0, output.1.clone(), merged.inner.clone()),
        });
        merged
    }

    fn on_ctx_union<PR: PatRecSgl>(
        &self,
        x: (FuncName, egglog::Value, Self::MetaTy<PR>),
        y: (FuncName, egglog::Value, Self::MetaTy<PR>),
    ) {
        self.slotted_ctx.push_pending(SlotPendingOps::Union(
            (x.0, x.1, x.2.inner),
            (y.0, y.1, y.2.inner),
        ))
    }

    fn flush_pending(&self, egraph: &EGraph) -> bool {
        self.slotted_ctx.flush_pending(egraph);
        false
    }
}

impl SlottedPatRec for SlottedPatRecorder {
    fn on_new_query_slot(&self, node: &(impl EgglogNode + 'static), slot_id: SlotVarID) {
        let slot_meta = SlotMetaInner {
            sub_metas: Default::default(),
            var_id_set: {
                let mut set = IndexSet::default();
                set.insert(slot_id);
                set
            },
        };
        self.add_node_with_slot_meta(node, ArcSlotMetaInner::new(slot_meta));
    }
}

#[derive(Clone, Debug, Default)]
pub struct SlotMetaInner {
    pub sub_metas: Vec<ArcSlotMetaInner>,
    pub var_id_set: IndexSet<crate::wrap::SlotVarID>,
}
#[derive(Clone, Debug, Deref, Default)]
pub struct ArcSlotMetaInner {
    pub inner: Arc<SlotMetaInner>,
}
impl ArcSlotMetaInner {
    fn new(inner: SlotMetaInner) -> Self {
        Self {
            inner: Arc::new(inner),
        }
    }
    pub fn get_current_layer_de_bruijn(&self) -> Vec<Vec<usize>> {
        let mut a = vec![];
        for meta in self.inner.sub_metas.iter() {
            a.push(
                meta.var_id_set
                    .iter()
                    .map(|x| self.inner.var_id_set.get_index_of(x))
                    .map(Option::unwrap)
                    .collect(),
            );
        }
        a
    }
    pub fn tensor(&self) -> Tensor {
        Tensor::new(
            self.get_current_layer_de_bruijn()
                .iter()
                .flat_map(|v| v.iter().copied())
                .enumerate()
                .map(|(i, x)| TensorIndex::new(DeBru::new(x), i))
                .collect(),
        )
    }
}
pub struct SlotMeta<PR: PatRecSgl> {
    inner: ArcSlotMetaInner,
    _p: PhantomData<PR>,
}
impl<PR: PatRecSgl> std::fmt::Debug for SlotMeta<PR> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SlotMeta")
            .field("vars", &self.inner.var_id_set)
            .field("de Bruijn", &self.get_current_layer_de_bruijn())
            .finish()
    }
}
impl<PR: PatRecSgl> Meta for SlotMeta<PR> {
    fn merge(metas: &mut impl Iterator<Item = Self>) -> Self {
        Self::from_metas(&mut metas.map(|x| x))
    }
}
unsafe impl<PR: PatRecSgl> Send for SlotMeta<PR> {}
unsafe impl<PR: PatRecSgl> Sync for SlotMeta<PR> {}
impl<PR: PatRecSgl> Clone for SlotMeta<PR> {
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
            _p: self._p.clone(),
        }
    }
}
impl<PR: PatRecSgl> Default for SlotMeta<PR> {
    fn default() -> Self {
        Self {
            inner: Default::default(),
            _p: Default::default(),
        }
    }
}

impl<PR: PatRecSgl> FromMetas<PR> for SlotMeta<PR> {
    /// here we should merge mapping
    /// for example  
    ///    Add           Add(2) with mapping [x=>1, y=>2]
    ///   x    y    =>  [1]  [2]
    ///
    ///      Sub                 Sub                         Sub(2)
    ///      /\                  / \                         {x,y} detected y in both mapping
    ///     /  \                /   \                       /   \
    ///    Add  Var y         Add(2)  Var(1)            Add(2)  Var(1)
    ///   /  \                {x,y}   {y}                {x,y}   {y}
    ///  /    \                /   \                    /   \
    /// Var  Var       =>  Var(1) Var(1)          => Var(1)  Var(1)
    ///  x     y             {x}   {y}                {x}     {y}
    ///              
    ///      Sub                 Sub                         Sub(2)
    ///      /\                  / \                         {x,y,z}
    ///     /  \                /   \                       /   \
    ///    Add  Var z         Add(2)  Var(1)            Add(2)   Var(1)
    ///   /  \                {x,y}     {z}             {x,y}     {z}
    ///  /    \                /   \                    /   \
    /// Var  Var       =>  Var(1) Var(1)          => Var(1)  Var(1)
    ///  x     y             {x}   {y}                 {x}    {y}
    ///
    fn from_metas(sub_metas: &mut impl Iterator<Item = SlotMeta<PR>>) -> Self {
        let sub_metas = sub_metas.map(|x| x.inner);
        // length of sub_metas should be same to merged mapping length
        Self {
            inner: ArcSlotMetaInner::from_metas(sub_metas),
            _p: PhantomData,
        }
    }
}
impl ArcSlotMetaInner {
    pub fn from_metas(sub_metas: impl Iterator<Item = ArcSlotMetaInner>) -> Self {
        let sub_metas: Vec<_> = sub_metas.collect();
        let var_id_set = sub_metas
            .iter()
            .map(|meta| &meta.var_id_set)
            .flat_map(|x| x.iter().copied())
            .collect();

        ArcSlotMetaInner {
            inner: Arc::new(SlotMetaInner {
                sub_metas,
                var_id_set,
            }),
        }
    }
}
impl<PR: PatRecSgl> SlotMeta<PR> {
    /// so that we can verify one eclass-enode pair whether it's the true one
    /// for example given two eclass
    ///   eclass A(x,y)     eclass A(x)
    ///    add(self)         add            
    ///  (x),(y)           (x),(x)         <= ret value of this function
    ///   /   \             /   \
    /// Var x Var y       Var x Var x  (these two should be same eclass)
    ///
    /// After de Bruijn:
    ///   eclass A{x,y}     eclass A{x}
    ///    add(self)         add            
    ///  (0),(1)           (0),(0)         <= ret value of this function
    ///   /   \             /   \
    /// Var x Var y       Var x Var x  (these two should be same eclass)
    ///
    /// Or more complex:
    /// After de Bruijn:
    ///   eclass A{x,y}     eclass A{x,y}
    ///    add(self)         add            
    ///  (0,1),(1,0)       (0,1),(0,1)     <= ret value of this function  还需要调用 f eclass 的 canonicalize
    ///   /   \             /   \
    /// f      f          f      f
    ///
    ///  they have different de Bruijn form so we can distinguish them
    pub fn get_current_layer_de_bruijn(&self) -> Vec<Vec<usize>> {
        let mut a = vec![];
        for meta in self.inner.inner.sub_metas.iter() {
            a.push(
                meta.var_id_set
                    .iter()
                    .map(|x| self.inner.var_id_set.get_index_of(x))
                    .map(Option::unwrap)
                    .collect(),
            );
        }
        a
    }
    pub fn tensor(&self) -> Tensor {
        Tensor::new(
            self.get_current_layer_de_bruijn()
                .iter()
                .flat_map(|v| v.iter().copied())
                .enumerate()
                .map(|(i, x)| TensorIndex::new(DeBru::new(x), i))
                .collect(),
        )
    }
}
