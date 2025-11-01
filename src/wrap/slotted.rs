use eggplant::egglog::{
    Value,
    util::{IndexMap, IndexSet},
};

use crate::bulter_portugal::{Tensor, canonicalize};

pub type TyName = &'static str;

struct ErasedClass {
    classes: IndexMap<TyName, Vec<Group>>,
    value: Value,
}

impl ErasedClass {
    fn make_slotted_class(ty: TyName, permu: Tensor) {}
}

struct ErasedNode {}

struct SlottedClass {
    grp: Group,
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
