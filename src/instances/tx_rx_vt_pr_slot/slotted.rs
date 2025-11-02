use super::pat_rec_slot::SlotMeta;
use crate::{
    butler_portugal::{Tensor, canonicalize},
    prelude::ArcSlotMetaInner,
    wrap::{PatRec, PatRecSgl, SlottedPatRecSgl},
};
use egglog::{Value, util::IndexMap};

pub type FuncName = &'static str;

pub type SEClassID = usize;
pub type SENodeID = usize;
#[derive(Debug, Clone)]
pub struct SENode {
    seclass_id: SEClassID,
    senode_id: SENodeID,
    ty_name: FuncName,
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
    fn make_single_seclass(
        func: FuncName,
        grp: Group,
        cano_value: Value,
        seclass_id: SEClassID,
        senode_id: SENodeID,
    ) -> Self {
        Self {
            cano_value,
            senodes: {
                let mut senodes = IndexMap::default();
                senodes.insert(
                    senode_id,
                    SENode {
                        seclass_id,
                        senode_id,
                        ty_name: func,
                    },
                );
                senodes
            },
            seclass2senodes: {
                let mut seclass2senodes: IndexMap<usize, SEClass> = IndexMap::default();
                seclass2senodes.entry(seclass_id).insert_entry(SEClass {
                    seclass_id,
                    senode_ids: vec![senode_id],
                });
                seclass2senodes
            },
            ty2senodes: {
                let mut ty2senodes: IndexMap<FuncName, Vec<usize>> = IndexMap::default();
                ty2senodes.entry(func).or_default().push(senode_id);
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
    next_seclass_id: usize,
    next_senode_id: usize,

    cano_value2seclasses: IndexMap<Value, SEClassesWithCanoValue>,

    pub pending_ops: crossbeam::queue::SegQueue<SlotPendingOps>,
}
impl SlottedCtx {
    pub fn new() -> Self {
        Self {
            next_seclass_id: 0,
            next_senode_id: 0,
            cano_value2seclasses: Default::default(),
            pending_ops: Default::default(),
        }
    }
    // pending insert
    // pub fn insert<PR: SlottedPatRecSgl>(&mut self, cano_value: Value, meta: Vec<SlotMeta<PR>>) {}
    pub fn push_pending(&self, pended: SlotPendingOps) {
        self.pending_ops.push(pended);
    }
    pub fn flush_pending(&self) {
        while let Some(pended) = self.pending_ops.pop() {
            println!("processing {:?}", pended);
            match pended {
                SlotPendingOps::Insert { inputs, output } => {
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
pub type _FuncValueMeta<PR: PatRecSgl> = (FuncName, egglog::Value, PR::MetaTy);
pub type FuncValueMeta<Pr: PatRec, PR: PatRecSgl> = (FuncName, egglog::Value, Pr::MetaTy<PR>);
pub type FuncValueMetaInner = (FuncName, egglog::Value, ArcSlotMetaInner);
#[derive(Clone, Debug)]
pub enum SlotPendingOps {
    Insert {
        inputs: Vec<FuncValueMetaInner>,
        output: FuncValueMetaInner,
    },
    Union(FuncValueMetaInner, FuncValueMetaInner),
}
