use std::{marker::PhantomData, mem};

use crate::wrap::{BoxedContainer, BoxedValue, EgglogTy, RetypeValue, RuleCtx, Value};

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

impl<Ty: EgglogTy> FromIterator<egglog::Value> for VecContainer<Ty> {
    fn from_iter<T: IntoIterator<Item = egglog::Value>>(iter: T) -> Self {
        VecContainer::from(iter.into_iter().collect::<Vec<_>>())
    }
}

unsafe impl<T: EgglogTy> Send for VecContainer<T> {}
unsafe impl<T: EgglogTy> Sync for VecContainer<T> {}
impl<T: EgglogTy> Clone for VecContainer<T> {
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
            _p: self._p.clone(),
        }
    }
}

impl<T: EgglogTy> VecContainer<T> {
    pub fn new() -> VecContainer<T> {
        VecContainer {
            inner: egglog::sort::VecContainer {
                do_rebuild: false,
                data: vec![],
            },
            _p: PhantomData,
        }
    }
    pub fn iter(&self) -> impl Iterator<Item = Value<T>> {
        self.inner
            .data
            .as_slice()
            .iter()
            .map(|x| Value::new(*x))
            .into_iter()
    }
}
impl<T: EgglogTy> From<Vec<egglog::Value>> for VecContainer<T> {
    fn from(value: Vec<egglog::Value>) -> Self {
        VecContainer {
            inner: egglog::sort::VecContainer {
                do_rebuild: false,
                data: value,
            },
            _p: PhantomData,
        }
    }
}

impl<T: EgglogTy> From<Vec<Value<T>>> for VecContainer<T> {
    fn from(value: Vec<Value<T>>) -> Self {
        VecContainer {
            inner: egglog::sort::VecContainer {
                do_rebuild: false,
                data: unsafe { mem::transmute(value) },
            },
            _p: PhantomData,
        }
    }
}
impl<T: EgglogTy> BoxedContainer for VecContainer<T> {
    type Boxed = egglog::sort::VecContainer;
    const CONSTRUCTOR_STR: &'static str = "vec-of";
    const TY_STR: &'static str = "Vec";

    fn unbox(boxed: Self::Boxed, _ctx: &RuleCtx) -> Self {
        unsafe { mem::transmute(boxed) }
    }

    fn box_it(self, _ctx: &RuleCtx) -> Self::Boxed {
        unsafe { mem::transmute(self) }
    }
}
