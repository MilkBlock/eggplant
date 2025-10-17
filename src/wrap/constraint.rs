use egglog::{
    EGraph,
    ast::{Expr, Fact, GenericExpr, GenericFact, Literal, RustSpan, Span},
    span,
};
use std::marker::PhantomData;

use crate::wrap::{EgglogTy, FromBase, Sym};
pub trait IntoConstraintFact: 'static + std::fmt::Debug {
    #[track_caller]
    fn into_constraint_fact(&self, egraph: &EGraph) -> Vec<Fact>;
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
pub trait Compare {
    type SelfEgglogTy: EgglogTy;
    fn lt<T2: EgglogTy>(
        &self,
        other: impl AsHandle<Target = T2>,
    ) -> LtConstraint<Self::SelfEgglogTy, T2>;
    fn le<T2: EgglogTy>(
        &self,
        other: impl AsHandle<Target = T2>,
    ) -> LeConstraint<Self::SelfEgglogTy, T2>;
    fn gt<T2: EgglogTy>(
        &self,
        other: impl AsHandle<Target = T2>,
    ) -> GtConstraint<Self::SelfEgglogTy, T2>;
    fn ge<T2: EgglogTy>(
        &self,
        other: impl AsHandle<Target = T2>,
    ) -> GeConstraint<Self::SelfEgglogTy, T2>;
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

// Macro definition: generate constraint structs
macro_rules! define_constraint_structs {
    ($($name:ident),* $(,)?) => {
        $(
            #[derive(derive_more::Debug, Clone)]
            pub struct $name<T1: EgglogTy, T2: EgglogTy> {
                a: HandleToConstrain<T1>,
                b: HandleToConstrain<T2>,
            }
        )*
    };
}

// Use macro to generate all constraint structs
define_constraint_structs! { LtConstraint, LeConstraint, GtConstraint, GeConstraint }
impl<T1: EgglogTy, T2: EgglogTy> IntoConstraintFact for EqConstraint<T1, T2> {
    fn into_constraint_fact(&self, egraph: &EGraph) -> Vec<Fact> {
        vec![Fact::Eq(
            span!(),
            self.a.to_resolved_expr(egraph),
            self.b.to_resolved_expr(egraph),
        )]
        // Generate an atom, this atom is Eq
    }
}
impl<T1: EgglogTy, T2: EgglogTy> IntoConstraintFact for NEConstraint<T1, T2> {
    fn into_constraint_fact(&self, egraph: &EGraph) -> Vec<Fact> {
        vec![Fact::Fact(Expr::Call(
            span!(),
            "!=".to_string(),
            vec![
                self.a.to_resolved_expr(egraph),
                self.b.to_resolved_expr(egraph),
            ],
        ))]
    }
}

// Macro definition: generate IntoConstraintFact implementations
macro_rules! impl_into_constraint_fact {
    ($($constraint:ident => $op:literal),* $(,)?) => {
        $(
            impl<T1: EgglogTy, T2: EgglogTy> IntoConstraintFact for $constraint<T1, T2> {
                fn into_constraint_fact(&self, egraph: &EGraph) -> Vec<GenericFact<String, String>> {
                    vec![Fact::Fact(GenericExpr::<String,String>::Call(
                        span!(),
                        $op.to_string(),
                        vec![
                            self.a.to_resolved_expr(egraph),
                            self.b.to_resolved_expr(egraph),
                        ],
                    ))]
                }
            }
        )*
    };
}

// Use macro to generate all IntoConstraintFact implementations
impl_into_constraint_fact! {
    LtConstraint => "<",
    LeConstraint => "<=",
    GtConstraint => ">",
    GeConstraint => ">="
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

// Macro definition: generate Compare trait implementations
macro_rules! impl_compare_for_type {
    ($type:ty) => {
        impl Compare for HandleToConstrain<$type> {
            type SelfEgglogTy = $type;
            fn lt<T2: EgglogTy>(
                &self,
                other: impl AsHandle<Target = T2>,
            ) -> LtConstraint<$type, T2> {
                LtConstraint {
                    a: self.clone(),
                    b: other.as_handle().clone(),
                }
            }
            fn le<T2: EgglogTy>(
                &self,
                other: impl AsHandle<Target = T2>,
            ) -> LeConstraint<$type, T2> {
                LeConstraint {
                    a: self.clone(),
                    b: other.as_handle().clone(),
                }
            }
            fn gt<T2: EgglogTy>(
                &self,
                other: impl AsHandle<Target = T2>,
            ) -> GtConstraint<$type, T2> {
                GtConstraint {
                    a: self.clone(),
                    b: other.as_handle().clone(),
                }
            }
            fn ge<T2: EgglogTy>(
                &self,
                other: impl AsHandle<Target = T2>,
            ) -> GeConstraint<$type, T2> {
                GeConstraint {
                    a: self.clone(),
                    b: other.as_handle().clone(),
                }
            }
        }
    };
}

// Use macro to generate Compare implementations for numeric types
impl_compare_for_type!(i64);
impl_compare_for_type!(f64);
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
/// Handle used to generate constraint fact
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
    pub fn to_resolved_expr(&self, _egraph: &EGraph) -> GenericExpr<String, String> {
        match &self.handle {
            HandleTy::Base { field_name, sym } => {
                GenericExpr::Var(span!(), format!("{}{}", sym, field_name))
            }
            HandleTy::Complex { sym } => GenericExpr::Var(span!(), format!("{}", sym)),
            HandleTy::Literal { lit } => GenericExpr::Lit(span!(), lit.clone()),
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
