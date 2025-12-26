use crate::{
    butler_portugal::{Tensor, canonicalize},
    prelude::SlotMeta,
    wrap::{EgglogNode, PatRec, PatRecSgl, Sym, Syms},
};
use dashmap::DashMap;
use derive_more::{Deref, DerefMut};
use egglog::{EGraph, Value, util::IndexMap};
use std::sync::atomic::{AtomicUsize, Ordering};

pub type FuncName = &'static str;

pub type SEClassID = usize;
pub type SENodeID = usize;
#[derive(Debug, Clone)]
pub struct SENode {
    seclass_id: SEClassID,
    senode_id: SENodeID,
    ty_name: FuncName,
    grp: Group,
}
#[derive(Debug, Clone)]
pub struct SEClass {
    seclass_id: SEClassID,
    senode_ids: Vec<SENodeID>,
}

#[derive(Debug, Clone)]
pub struct SEClassesWithCanoValue {
    // one erased egraph may contains several slotted eclass
    senodes: IndexMap<SENodeID, SENode>,
    // maintain a mapping from seclass 2 senodes and a mapping from senode
    seclass2senodes: IndexMap<SEClassID, SEClass>,
    ty2senodes: IndexMap<FuncName, Vec<SENodeID>>,
    cano_value: Value,
}

impl SEClassesWithCanoValue {
    fn new(cano_value: Value) -> Self {
        Self {
            cano_value,
            senodes: {
                let mut senodes = IndexMap::default();
                senodes
            },
            seclass2senodes: {
                let mut seclass2senodes: IndexMap<usize, SEClass> = IndexMap::default();
                seclass2senodes
            },
            ty2senodes: {
                let mut ty2senodes: IndexMap<FuncName, Vec<usize>> = IndexMap::default();
                ty2senodes
            },
        }
    }
}

#[derive(Clone, Debug)]
struct Group {
    tensor: Tensor, // struct of bulter_protugal implementation
}
impl Group {
    fn new(tensor: Tensor) -> Self {
        Self { tensor }
    }
    // check that
    fn contains(&self, target: Tensor) -> bool {
        if target.rank() != self.tensor.rank() {
            panic!("try to compare tensor with different args num");
        }
        let canno_target = canonicalize(&target).unwrap();
        self.tensor == canno_target
    }
}

#[derive(Debug)]
pub struct SlottedCtx {
    // slotted id info
    next_seclass_id: AtomicUsize,
    next_senode_id: AtomicUsize,

    pub cano_value2seclasses: DashMap<Value, SEClassesWithCanoValue>,

    pub pending_ops: crossbeam::queue::SegQueue<SlotPendingOps>,
}
impl Default for SlottedCtx {
    fn default() -> Self {
        Self::new()
    }
}
impl SlottedCtx {
    pub fn new() -> Self {
        Self {
            next_seclass_id: AtomicUsize::new(0),
            next_senode_id: AtomicUsize::new(0),
            cano_value2seclasses: Default::default(),
            pending_ops: Default::default(),
        }
    }
    // pending insert
    // pub fn insert<PR: SlottedPatRecSgl>(&mut self, cano_value: Value, meta: Vec<SlotMeta<PR>>) {}
    pub fn push_pending(&self, pended: SlotPendingOps) {
        self.pending_ops.push(pended);
    }
    pub fn flush_pending(&self, egraph: &EGraph) {
        println!("flush_pending");
        while let Some(pended) = self.pending_ops.pop() {
            println!("processing {:?}", pended);
            match pended {
                SlotPendingOps::Insert { inputs, output } => {
                    let output_cano_value = egraph.get_canonical_value(
                        output.1,
                        &egraph.get_function(output.0).unwrap().schema().output,
                    );
                    if output.1 != output_cano_value {
                        panic!("output is not equal to cano_value");
                    }
                    // find seclasses
                    let mut seclasses = self
                        .cano_value2seclasses
                        .entry(output_cano_value)
                        .or_insert(SEClassesWithCanoValue::new(output_cano_value));

                    // then find the satisfied enode or create new
                    if let Some(enode_id) = find_satisfied_enode(&seclasses, &inputs, &output) {
                    } else {
                        // create new
                        let senode_id = self.next_senode_id.fetch_add(1, Ordering::SeqCst);
                        let seclass_id = self.next_seclass_id.fetch_add(1, Ordering::SeqCst);
                        seclasses.senodes.insert(
                            senode_id,
                            SENode {
                                seclass_id,
                                senode_id,
                                ty_name: output.0,
                                grp: Group::new(output.2.tensor()),
                            },
                        );
                        seclasses
                            .seclass2senodes
                            .entry(seclass_id)
                            .insert_entry(SEClass {
                                seclass_id,
                                senode_ids: vec![senode_id],
                            });
                        seclasses
                            .ty2senodes
                            .entry(output.0)
                            .or_default()
                            .push(senode_id);
                    }

                    println!("inputs:{:?} output:{:?}", inputs, output);
                }
                SlotPendingOps::Union(a, b) => {
                    println!("union :{:?} {:?}", a, b);
                }
            }
        }
    }
    pub fn add_pending_ops() {}
}

fn find_satisfied_enode(
    seclasses: &dashmap::mapref::one::RefMut<'_, Value, SEClassesWithCanoValue>,
    inputs: &[(&'static str, Value, SlotMeta)],
    (output_func, output_cano_val, output_meta): &(&'static str, Value, SlotMeta),
) -> Option<SENodeID> {
    // if let Some(senode_ids) = seclasses.ty2senodes.get(output_func) {
    //     for enode_id in senode_ids.iter() {
    //         match seclasses.senodes.get(enode_id) {
    //             Some(senode) => {
    //                 assert_eq!(senode.ty_name, output_func);
    //                 if senode.grp.tensor

    //             }
    //             None => {
    //                 panic!(
    //                     "enode_id {} should be contained by seclasses {:?}",
    //                     enode_id, seclasses
    //                 )
    //             }
    //         }
    //     }
    // } else {
    //     None;
    // }
    None
}

pub type _FuncValueMeta<PR: PatRecSgl> = (FuncName, egglog::Value, Option<PR::MetaTy>);
pub type FuncValueMeta<Pr: PatRec> = (FuncName, egglog::Value, Option<Pr::MetaTy>);
pub type FuncValueMetaInner = (FuncName, egglog::Value, SlotMeta);
#[derive(Clone, Debug)]
pub enum SlotPendingOps {
    Insert {
        inputs: Vec<FuncValueMetaInner>,
        output: FuncValueMetaInner,
    },
    Union(FuncValueMetaInner, FuncValueMetaInner),
}

#[derive(DerefMut, Deref)]
pub struct SlotWorkAreaNode {
    pub next: Option<Sym>,
    pub prev: Option<Sym>,
    pub preds: Syms,
    #[deref]
    #[deref_mut]
    pub egglog: Box<dyn EgglogNode>,
    pub pulled_by: Option<egglog::Value>,
}

impl SlotWorkAreaNode {
    pub fn new(node: Box<dyn EgglogNode>) -> Self {
        Self {
            preds: Syms::default(),
            egglog: node,
            next: None,
            prev: None,
            pulled_by: None,
        }
    }
    pub fn new_pulled(node: Box<dyn EgglogNode>, pulled_by: egglog::Value) -> Self {
        Self {
            preds: Syms::default(),
            egglog: node,
            next: None,
            prev: None,
            pulled_by: Some(pulled_by),
        }
    }
    pub fn succs_mut(&mut self) -> impl Iterator<Item = &mut Sym> {
        self.egglog.succs_mut().into_iter()
    }
    pub fn preds_mut(&mut self) -> impl Iterator<Item = &mut Sym> {
        self.preds.iter_mut()
    }
    pub fn preds(&self) -> impl Iterator<Item = &Sym> {
        self.preds.iter()
    }
}

impl std::fmt::Debug for SlotWorkAreaNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} {} | {} | pulled_by {:?}",
            self.variant_name().unwrap_or(self.ty_name()),
            self.cur_sym(),
            self.to_egglog_string().unwrap_or(
                self.egglog
                    .basic_field_types()
                    .iter()
                    .map(|s| s.to_string())
                    .collect::<Vec<_>>()
                    .join(" ")
            ),
            self.pulled_by
        )
    }
}

impl Clone for SlotWorkAreaNode {
    fn clone(&self) -> Self {
        Self {
            next: self.next.clone(),
            preds: self.preds.clone(),
            egglog: self.egglog.clone_dyn(),
            prev: None,
            pulled_by: self.pulled_by,
        }
    }
}
