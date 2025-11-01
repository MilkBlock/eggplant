use super::pat_rec_slot::SlotMeta;
use crate::{
    butler_portugal::{Tensor, canonicalize},
    wrap::SlottedPatRecSgl,
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

#[derive(Debug, Clone)]
pub struct SlottedCtx {
    // slotted id info
    next_seclass_id: usize,
    next_senode_id: usize,

    cano_value2seclasses: IndexMap<Value, SEClassesWithCanoValue>,
}
impl SlottedCtx {
    pub fn new() -> Self {
        Self {
            next_seclass_id: 0,
            next_senode_id: 0,
            cano_value2seclasses: Default::default(),
        }
    }
    // pending insert
    pub fn insert<PR: SlottedPatRecSgl>(&mut self, cano_value: Value, meta: SlotMeta<PR>) {
        match self.cano_value2seclasses.get(&cano_value) {
            Some(seclasses) => {
                // seclasses.ty2senodes.get
            }
            None => {}
        }
    }
}
