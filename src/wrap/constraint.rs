use std::marker::PhantomData;

use crate::wrap::{EgglogTy, Sym};
use egglog::{
    ast::{Literal, ResolvedVar},
    core::{GenericAtom, ResolvedCall},
};

pub trait PEq {
    type SelfEgglogTy: EgglogTy;
    fn eq<T2: EgglogTy>(
        &self,
        other: impl AsHandle<Target = T2>,
    ) -> EqConstraint<Self::SelfEgglogTy, T2>;
    fn ne<T2: EgglogTy>(
        &self,
        other: impl AsHandle<Target = T2>,
    ) -> NEConstraint<Self::SelfEgglogTy, T2>;
}
#[derive(Debug)]
pub struct EqConstraint<T1: EgglogTy, T2: EgglogTy> {
    a: HandleToConstrain<T1>,
    b: HandleToConstrain<T2>,
}
pub struct NEConstraint<T1: EgglogTy, T2: EgglogTy> {
    a: HandleToConstrain<T1>,
    b: HandleToConstrain<T2>,
}
impl<T1: EgglogTy, T2: EgglogTy> IntoConstraintAtoms for EqConstraint<T1, T2> {
    fn into_atoms(
        &self,
    ) -> Vec<egglog::prelude::GenericAtom<egglog::prelude::ResolvedCall, egglog::ast::ResolvedVar>>
    {
        vec![]
        // 生成一个 atom 这个atom 是Eq
    }
}
impl<T1: EgglogTy> PEq for HandleToConstrain<T1> {
    type SelfEgglogTy = T1;
    fn eq<T2: EgglogTy>(&self, other: impl AsHandle<Target = T2>) -> EqConstraint<T1, T2> {
        EqConstraint {
            a: self.clone(),
            b: other.as_handle().clone(),
        }
    }
    fn ne<T2: EgglogTy>(
        &self,
        other: impl AsHandle<Target = T2>,
    ) -> NEConstraint<Self::SelfEgglogTy, T2> {
        NEConstraint {
            a: self.clone(),
            b: other.as_handle().clone(),
        }
    }
}
pub trait ConstrainClosure<T: EgglogTy, C: IntoConstraintAtoms>:
    Fn(HandleToConstrain<T>) -> C
{
}

#[derive(Clone)]
pub enum HandleTy {
    Base { field_name: &'static str, sym: Sym },
    Complex { sym: Sym },
    Literal { lit: Literal },
}
impl std::fmt::Debug for HandleTy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            HandleTy::Base { field_name, sym } => f
                .debug_struct("Base")
                .field("field_name", field_name)
                .field("sym", sym)
                .finish(),
            HandleTy::Complex { sym } => f.debug_struct("Complex").field("sym", sym).finish(),
            HandleTy::Literal { .. } => f.debug_struct("Literal").finish(),
        }
    }
}
pub struct HandleToConstrain<T: EgglogTy> {
    pub handle: HandleTy, /* TODO complex or base or container?
                          what's the field name ?  field name can be used to query position
                          one queryed node could have several field name in one atom, so we should mege them in eggplant ? NONONON
                          we can make them processed in egglog by GenericExpr but not atom
                          So that we can be happy.
                           */
    pub _p: PhantomData<T>,
}
impl<T: EgglogTy> Clone for HandleToConstrain<T> {
    fn clone(&self) -> Self {
        Self {
            handle: self.handle.clone(),
            _p: self._p.clone(),
        }
    }
}
impl<T: EgglogTy> std::fmt::Debug for HandleToConstrain<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("HandleToConstrain")
            .field("handle", &self.handle)
            .field("_p", &self._p)
            .finish()
    }
}

pub trait IntoConstraintAtoms {
    fn into_atoms(&self) -> Vec<GenericAtom<ResolvedCall, ResolvedVar>>;
}

pub trait AsHandle {
    type Target: EgglogTy;
    fn as_handle(&self) -> HandleToConstrain<Self::Target>;
}
impl<T: EgglogTy> AsHandle for &HandleToConstrain<T> {
    type Target = T;
    fn as_handle(&self) -> HandleToConstrain<Self::Target> {
        (*self).clone()
    }
}

impl AsHandle for i64 {
    type Target = i64;
    fn as_handle(&self) -> HandleToConstrain<Self::Target> {
        HandleToConstrain {
            handle: HandleTy::Literal {
                lit: Literal::Int(*self),
            },
            _p: PhantomData,
        }
    }
}
