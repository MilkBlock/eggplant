use std::{marker::PhantomData, mem};

use crate::wrap::{BoxedValue, EgglogTy, RetypeValue, RuleCtx, Value};

type Ref<'a, T> = Box<dyn std::ops::Deref<Target = T> + 'a>;
impl<T: EgglogTy> BoxedValue for VecContainer<T> {
    fn devalue<'b>(rule_ctx: &'b RuleCtx, value: egglog::Value) -> Self::Output<'b> {
        let container = rule_ctx
            ._devalue_container::<egglog::sort::VecContainer>(value)
            .unwrap();
        let c: Ref<'b, egglog::sort::VecContainer> = Box::new(container);
        // safety : VecContainer<T>'s memory layout is same with VecContainer so this is legal
        unsafe { mem::transmute(c) }
    }

    type Output<'a> = Box<dyn std::ops::Deref<Target = Self> + 'a>;
}
pub struct VecContainer<T: EgglogTy + 'static> {
    pub(crate) inner: egglog::sort::VecContainer,
    pub(crate) _p: PhantomData<T>,
}

impl<'a, T: EgglogTy + 'static> IntoIterator for &'a VecContainer<T> {
    type Item = &'a egglog::Value;
    type IntoIter = std::slice::Iter<'a, egglog::Value>;

    fn into_iter(self) -> Self::IntoIter {
        self.inner.data.iter()
    }
}
impl<Ty: EgglogTy> VecContainer<Ty> {
    //     fn rebuild_contents(&mut self, rebuilder: &dyn egglog::sort::Rebuilder) -> bool {
    //         self.inner.rebuild_contents(rebuilder)
    //     }

    //     fn iter(&self) -> impl Iterator<Item = egglog::Value> + '_ {
    //         self.inner.iter()
    //     }
    pub fn push<T: EgglogTy + RetypeValue<Target = Ty>>(&mut self, val: Value<T>) {
        self.inner.data.push(val.erase())
    }
}
