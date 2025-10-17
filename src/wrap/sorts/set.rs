use std::{
    collections::{BTreeSet, btree_set::Iter},
    marker::PhantomData,
    mem,
};

use crate::wrap::{BoxedContainer, BoxedValue, EgglogTy, RetypeValue, RuleCtx, Value};

type Ref<'a, T> = Box<dyn std::ops::Deref<Target = T> + 'a>;
impl<T: EgglogTy> BoxedValue for SetContainer<T> {
    fn devalue<'b>(rule_ctx: &'b RuleCtx, value: egglog::Value) -> Self::Output<'b> {
        let container = rule_ctx
            ._devalue_container::<egglog::sort::SetContainer>(value)
            .unwrap();
        let c: Ref<'b, egglog::sort::SetContainer> = Box::new(container);
        // safety : SetContainer<T>'s memory layout is same with SetContainer so this is legal
        unsafe { mem::transmute(c) }
    }

    type Output<'a> = Box<dyn std::ops::Deref<Target = Self> + 'a>;
}
pub struct SetContainer<T: EgglogTy + 'static> {
    pub(crate) inner: egglog::sort::SetContainer,
    pub(crate) _p: PhantomData<T>,
}

impl<'a, T: EgglogTy + 'static> IntoIterator for &'a SetContainer<T> {
    type Item = &'a egglog::Value;
    type IntoIter = Iter<'a, egglog::Value>;

    fn into_iter(self) -> Self::IntoIter {
        self.inner.data.iter()
    }
}
impl<Ty: EgglogTy> SetContainer<Ty> {
    //     fn rebuild_contents(&mut self, rebuilder: &dyn egglog::sort::Rebuilder) -> bool {
    //         self.inner.rebuild_contents(rebuilder)
    //     }

    //     fn iter(&self) -> impl Iterator<Item = egglog::Value> + '_ {
    //         self.inner.iter()
    //     }
    pub fn insert<T: EgglogTy + RetypeValue<Target = Ty>>(&mut self, val: Value<T>) -> bool {
        self.inner.data.insert(val.erase())
    }
}

impl<Ty: EgglogTy> FromIterator<egglog::Value> for SetContainer<Ty> {
    fn from_iter<T: IntoIterator<Item = egglog::Value>>(iter: T) -> Self {
        SetContainer::from(iter.into_iter().collect::<BTreeSet<_>>())
    }
}

unsafe impl<T: EgglogTy> Send for SetContainer<T> {}
unsafe impl<T: EgglogTy> Sync for SetContainer<T> {}
impl<T: EgglogTy> Clone for SetContainer<T> {
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
            _p: self._p.clone(),
        }
    }
}

impl<T: EgglogTy> SetContainer<T> {
    pub fn new() -> SetContainer<T> {
        SetContainer {
            inner: egglog::sort::SetContainer {
                do_rebuild: false,
                data: Default::default(),
            },
            _p: PhantomData,
        }
    }
    pub fn iter(&self) -> impl Iterator<Item = Value<T>> {
        self.inner.data.iter().map(|x| Value::new(*x)).into_iter()
    }
}
impl<T: EgglogTy> From<BTreeSet<egglog::Value>> for SetContainer<T> {
    fn from(value: BTreeSet<egglog::Value>) -> Self {
        SetContainer {
            inner: egglog::sort::SetContainer {
                do_rebuild: false,
                data: value,
            },
            _p: PhantomData,
        }
    }
}

impl<T: EgglogTy> From<Vec<Value<T>>> for SetContainer<T> {
    fn from(value: Vec<Value<T>>) -> Self {
        SetContainer {
            inner: egglog::sort::SetContainer {
                do_rebuild: false,
                data: unsafe { mem::transmute(value) },
            },
            _p: PhantomData,
        }
    }
}

impl<T: EgglogTy> BoxedContainer for SetContainer<T> {
    type Boxed = egglog::sort::SetContainer;
    const CONSTRUCTOR_STR: &'static str = "set-of";
    const TY_STR: &'static str = "Set";

    fn unbox(boxed: Self::Boxed, _ctx: &RuleCtx) -> Self {
        unsafe { mem::transmute(boxed) }
    }

    fn box_it(self, _ctx: &RuleCtx) -> Self::Boxed {
        unsafe { mem::transmute(self) }
    }
}
