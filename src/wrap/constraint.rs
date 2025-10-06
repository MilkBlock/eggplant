use egglog::{
    EGraph,
    ast::{Literal, ResolvedExpr, ResolvedFact, ResolvedVar},
    core::{ResolvedCall, SpecializedPrimitive},
    span,
};
use std::marker::PhantomData;

use crate::wrap::{EgglogTy, FromBase, Sym};
pub trait IntoConstraintFact: 'static + std::fmt::Debug {
    #[track_caller]
    fn into_constraint_fact(&self, egraph: &EGraph) -> Vec<ResolvedFact>;
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
impl<T> AsHandle for &T
where
    Literal: FromBase<T>,
    T: EgglogTy + Clone,
{
    type Target = T;
    fn as_handle(&self) -> HandleToConstrain<Self::Target> {
        HandleToConstrain {
            handle: HandleTy::Literal {
                lit: Literal::from_base(self),
            },
            _p: PhantomData,
        }
    }
}
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
#[derive(derive_more::Debug, Clone)]
pub struct EqConstraint<T1: EgglogTy, T2: EgglogTy> {
    a: HandleToConstrain<T1>,
    b: HandleToConstrain<T2>,
}
#[derive(derive_more::Debug, Clone)]
pub struct NEConstraint<T1: EgglogTy, T2: EgglogTy> {
    a: HandleToConstrain<T1>,
    b: HandleToConstrain<T2>,
}
impl<T1: EgglogTy, T2: EgglogTy> IntoConstraintFact for EqConstraint<T1, T2> {
    fn into_constraint_fact(&self, egraph: &EGraph) -> Vec<ResolvedFact> {
        vec![ResolvedFact::Eq(
            span!(),
            self.a.to_resolved_expr(egraph),
            self.b.to_resolved_expr(egraph),
        )]
        // 生成一个 atom 这个atom 是Eq
    }
}
impl<T1: EgglogTy, T2: EgglogTy> IntoConstraintFact for NEConstraint<T1, T2> {
    fn into_constraint_fact(&self, egraph: &EGraph) -> Vec<ResolvedFact> {
        let neq = egraph.get_primitive("!=").unwrap()[0].clone();
        vec![ResolvedFact::Fact(ResolvedExpr::Call(
            span!(),
            ResolvedCall::Primitive(SpecializedPrimitive {
                primitive: neq,
                input: vec![T1::get_arc_sort(egraph), T2::get_arc_sort(egraph)],
                output: egraph.get_sort_by_name("Unit").unwrap().clone(),
            }),
            vec![
                self.a.to_resolved_expr(egraph),
                self.b.to_resolved_expr(egraph),
            ],
        ))]
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
pub trait ConstrainClosure<T: EgglogTy, C: IntoConstraintFact>:
    Fn(HandleToConstrain<T>) -> C
{
}

#[derive(Clone, derive_more::Debug)]
pub enum HandleTy {
    Base { field_name: &'static str, sym: Sym },
    Complex { sym: Sym },
    Literal { lit: Literal },
}
/// hanle used to generate constraint fact
#[derive(derive_more::Debug)]
pub struct HandleToConstrain<T: EgglogTy> {
    pub handle: HandleTy,
    pub _p: PhantomData<T>,
}
impl<T: EgglogTy> HandleToConstrain<T> {
    pub fn name(&self) -> String {
        match &self.handle {
            HandleTy::Base { field_name, sym } => format!("{}{}", sym, field_name),
            HandleTy::Complex { sym } => format!("{}", sym),
            HandleTy::Literal { lit } => format!("{}.literal.{lit:?}", T::TY_NAME_LOWER),
        }
    }
    pub fn to_resolved_expr(&self, egraph: &EGraph) -> ResolvedExpr {
        match &self.handle {
            HandleTy::Base { field_name, sym } => ResolvedExpr::Var(
                span!(),
                ResolvedVar {
                    name: format!("{}{}", sym, field_name),
                    sort: T::get_arc_sort(egraph),
                    is_global_ref: false,
                },
            ),
            HandleTy::Complex { sym } => ResolvedExpr::Var(
                span!(),
                ResolvedVar {
                    name: format!("{}", sym),
                    sort: T::get_arc_sort(egraph),
                    is_global_ref: false,
                },
            ),
            HandleTy::Literal { lit } => ResolvedExpr::Lit(span!(), lit.clone()),
        }
    }
}
impl<T: EgglogTy> Clone for HandleToConstrain<T> {
    fn clone(&self) -> Self {
        Self {
            handle: self.handle.clone(),
            _p: self._p.clone(),
        }
    }
}
