use super::*;
use crate::wrap::{EgglogFunc, EgglogFuncInputs, EgglogFuncOutput};
use egglog::{EGraph, SerializeConfig, ast::Command};
use std::{path::{Path, PathBuf}, sync::Mutex};

pub struct TxMinimal {
    egraph: Mutex<EGraph>,
}

/// tx with miminal feature (only new function is supported)
/// it's even not a buffer, just a Mutex<EGraph>
impl TxMinimal {
    pub fn new_with_type_defs(commands: Vec<Command>) -> Self {
        Self {
            egraph: Mutex::new({
                let mut e = EGraph::default();
                e.run_program(commands).unwrap();
                e
            }),
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
}

unsafe impl Send for TxMinimal {}
unsafe impl Sync for TxMinimal {}
// MARK: Tx
impl Tx for TxMinimal {
    fn send(&self, transmitted: TxCommand) {
        log::debug!("{:?}", transmitted);
        let mut egraph = self.egraph.lock().unwrap();
        match transmitted {
            TxCommand::StringCommand { command } => {
                egraph.parse_and_run_program(None, &command).unwrap();
            }
            TxCommand::NativeCommand { command } => {
                egraph.run_program(vec![command]).unwrap();
            }
        }
    }

    fn on_new(&self, node: &(impl EgglogNode + 'static)) {
        self.send(TxCommand::NativeCommand {
            command: Command::Action(node.to_egglog()),
        });
    }

    fn on_func_set<'a, F: EgglogFunc>(
        &self,
        input: <F::Input as EgglogFuncInputs>::Ref<'a>,
        output: <F::Output as EgglogFuncOutput>::Ref<'a>,
    ) {
        let input_nodes = input.as_evalues();
        let input_syms = input_nodes.iter().map(|ev| ev.get_symlit());
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

impl NodeDropper for TxMinimal {}
impl NodeOwner for TxMinimal {
    type OwnerSpecDataInNode<T: EgglogTy, V: EgglogEnumVariantTy> = ();
}
impl NodeSetter for TxMinimal {
    fn on_set(&self, _node: &mut (impl EgglogNode + 'static)) {
        // do nothing
    }
}
