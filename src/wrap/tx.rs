use crate::wrap::{EgglogFunc, EgglogFuncInputs, EgglogFuncOutput};
use egglog::ast::Command;

use super::*;
use dashmap::DashMap;
use egglog::{EGraph, SerializeConfig, util::IndexSet};
use std::{path::{Path, PathBuf}, sync::Mutex};

#[allow(unused)]
pub struct TxNoVT {
    pub egraph: Mutex<EGraph>,
    map: DashMap<Sym, WorkAreaNode>,
    registry: EgglogTypeRegistry,
}

/// Tx without version ctl feature
impl TxNoVT {
    pub fn new_with_type_defs(type_defs: Vec<Command>) -> Self {
        Self {
            egraph: Mutex::new({
                let mut e = EGraph::default();
                log::info!("{:?}", type_defs);
                e.run_program(type_defs).unwrap();
                e
            }),
            map: DashMap::default(),
            registry: EgglogTypeRegistry::new_with_inventory(),
        }
    }
    pub fn new() -> Self {
        Self::new_with_type_defs(EgglogTypeRegistry::collect_type_defs())
    }
    pub fn to_dot(&self, file_name: impl AsRef<Path>) {
        let egraph = self.egraph.lock().unwrap();
        let serialized = egraph.serialize(SerializeConfig::default());
        let dot_path = file_name.as_ref().with_extension("dot");
        serialized
            .egraph
            .to_dot_file(dot_path.clone())
            .unwrap_or_else(|_| panic!("Failed to write dot file to {dot_path:?}"));
    }
    // collect all ancestors of cur_sym, without cur_sym
    pub fn collect_latest_ancestors(&self, cur_sym: Sym, index_set: &mut IndexSet<Sym>) {
        let node = self.map.get(&cur_sym).unwrap();
        let succss = node.preds.clone();
        drop(node);
        for pred in succss {
            if index_set.contains(&pred) || self.map.get(&pred).unwrap().next.is_some() {
                // do nothing
            } else {
                index_set.insert(pred);
                self.collect_latest_ancestors(pred, index_set)
            }
        }
    }
    /// start nodes is asserted to be zero input degree
    pub fn topo_sort(&self, starts: IndexSet<Sym>, index_set: &IndexSet<Sym>) -> Vec<Sym> {
        let map = &self.map;
        // init in degrees and out degrees
        let mut ins = Vec::new();
        let mut outs = Vec::new();
        ins.resize(index_set.len(), 0);
        outs.resize(index_set.len(), 0);
        for (i, (in_degree, out_degree)) in ins.iter_mut().zip(outs.iter_mut()).enumerate() {
            let sym = index_set[i];
            let node = map.get(&sym).unwrap();
            *in_degree =
                TxNoVT::degree_in_subgraph(node.preds().into_iter().map(|x| *x), index_set);
            *out_degree = TxNoVT::degree_in_subgraph(node.succs().into_iter(), index_set);
        }
        let mut rst = Vec::new();
        let mut wait_for_release = Vec::new();
        // start node should not have any out edges in subgraph
        for start in starts {
            assert_eq!(0, outs[index_set.get_index_of(&start).unwrap()]);
            wait_for_release.push(start);
        }
        while !wait_for_release.is_empty() {
            let popped = wait_for_release.pop().unwrap();
            for target in &map.get(&popped).unwrap().preds {
                let idx = index_set.get_index_of(target).unwrap();
                outs[idx] -= 1;
                if outs[idx] == 0 {
                    wait_for_release.push(*target);
                }
            }
            rst.push(popped);
        }
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
        self.send(TxCommand::NativeCommand {
            command: Command::Action(node.to_egglog()),
        });
        let mut node = WorkAreaNode::new(node.clone_dyn());
        let sym = node.cur_sym();
        for succ_node in node.succs_mut() {
            self.map
                .get_mut(succ_node)
                .unwrap_or_else(|| panic!("node {} not found", succ_node.as_str()))
                .preds
                .push(sym);
        }
        self.map.insert(node.cur_sym(), node);
    }
}

unsafe impl Send for TxNoVT {}
unsafe impl Sync for TxNoVT {}
// MARK: Tx
impl Tx for TxNoVT {
    fn send(&self, received: TxCommand) {
        match received {
            TxCommand::StringCommand { command } => {
                {
                    log::info!("{}", command);
                    let mut egraph = self.egraph.lock().unwrap();
                    egraph
                        .parse_and_run_program(None, command.as_str())
                        .unwrap();
                };
            }
            TxCommand::NativeCommand { command } => {
                let mut egraph = self.egraph.lock().unwrap();
                egraph.run_program(vec![command]).unwrap();
            }
        }
    }

    fn on_new(&self, symnode: &(impl EgglogNode + 'static)) {
        self.add_node(symnode);
    }

    #[track_caller]
    fn on_func_set<'a, F: EgglogFunc>(
        &self,
        input: <F::Input as EgglogFuncInputs>::Ref<'a>,
        output: <F::Output as EgglogFuncOutput>::Ref<'a>,
    ) {
        let input_nodes = input.as_evalues();
        let input_syms = input_nodes.iter().map(|x| x.get_symlit());
        let output = output.as_evalue().get_symlit();
        self.send(TxCommand::StringCommand {
            command: format!(
                "(set ({} {}) {} )",
                F::FUNC_NAME,
                input_syms.map(|x| format!("{}", x)).collect::<String>(),
                output
            ),
        });
    }

    fn on_union(&self, node1: &(impl EgglogNode + 'static), node2: &(impl EgglogNode + 'static)) {
        self.send(TxCommand::StringCommand {
            command: format!("(union {} {})", node1.cur_sym(), node2.cur_sym()),
        });
    }
}

impl NodeDropper for TxNoVT {}
impl NodeOwner for TxNoVT {
    type OwnerSpecDataInNode<T: EgglogTy, V: EgglogEnumVariantTy> = ();
}
impl NodeSetter for TxNoVT {
    fn on_set(&self, _node: &mut (impl EgglogNode + 'static)) {
        // do nothing
    }
}
