use egglog::{
    EGraph,
    ast::{Expr, Fact, GenericExpr, GenericFact, Literal, RustSpan, Span},
    span,
};
use std::marker::PhantomData;

use crate::wrap::{EgglogContainerTy, EgglogTy, FromBase, Sym};
pub trait IntoConstraintFact: 'static + std::fmt::Debug {
    #[track_caller]
    fn into_constraint_fact(&self, egraph: &EGraph) -> Vec<Fact>;
}

/// A constraint that asserts a primitive fact call, e.g. `(vec-contains (vec-of 1 2 3) 2)`.
///
/// This is used for egglog primitives whose "result" is `Unit` (i.e. they succeed/fail as a
/// predicate), so they appear in egglog `check` as a bare call, not an equality to `true`.
#[derive(derive_more::Debug, Clone)]
pub struct FactCallConstraint {
    pub op: &'static str,
    pub operands: Vec<HandleTy>,
}

impl IntoConstraintFact for FactCallConstraint {
    fn into_constraint_fact(&self, egraph: &EGraph) -> Vec<Fact> {
        vec![Fact::Fact(Expr::Call(
            span!(),
            self.op.to_string(),
            self.operands
                .iter()
                .map(|h| h.to_resolved_expr(egraph))
                .collect(),
        ))]
    }
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
impl<T: EgglogTy> AsHandle for HandleToConstrain<T> {
    type Target = T;
    fn as_handle(&self) -> HandleToConstrain<Self::Target> {
        self.clone()
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

/// Convert values/handles into an untyped [`HandleTy`] for building primitive-call constraints.
pub trait IntoHandleTy {
    fn into_handle_ty(self) -> HandleTy;
}

impl<T: EgglogTy> IntoHandleTy for HandleToConstrain<T> {
    fn into_handle_ty(self) -> HandleTy {
        self.handle
    }
}

impl<T: EgglogTy> IntoHandleTy for &HandleToConstrain<T> {
    fn into_handle_ty(self) -> HandleTy {
        self.clone().handle
    }
}

impl<T> IntoHandleTy for &T
where
    Literal: FromBase<T>,
    T: EgglogTy + Clone,
{
    fn into_handle_ty(self) -> HandleTy {
        HandleTy::Literal {
            lit: Literal::from_base(self),
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
macro_rules! cartesian_ops {
    (forall,($t1:ty, $t2:ty),$out:ty,$op:tt,$op_egglog:literal,$method:ident) => {
        impl<T2: AsHandle<Target = $t2>> std::ops::$op<T2> for HandleToConstrain<$t1> {
            type Output = HandleToConstrain<$out>;
            fn $method(self, rhs: T2) -> Self::Output {
                HandleToConstrain {
                    handle: HandleTy::Expr {
                        op: $op_egglog,
                        operands: vec![Box::new(self.handle), Box::new(rhs.as_handle().handle)],
                    },
                    _p: PhantomData,
                }
            }
        }
    };
    (plain,($t1:ty, $t2:ty),$out:ty,$op:tt,$op_egglog:literal,$method:ident) => {
        impl std::ops::$op<HandleToConstrain<$t2>> for HandleToConstrain<$t1> {
            type Output = HandleToConstrain<$out>;
            fn $method(self, rhs: HandleToConstrain<$t2>) -> Self::Output {
                HandleToConstrain {
                    handle: HandleTy::Expr {
                        op: $op_egglog,
                        operands: vec![Box::new(self.handle), Box::new(rhs.handle)],
                    },
                    _p: PhantomData,
                }
            }
        }
    }; // // with commutative law
       // (($t1:ty, $t2:ty, $($rest:ty),+),$out:ty,$op:tt,$op_egglog:literal,$method:ident) => {
       //     cartesian_ops!(($t1, $t2),$out,$op,$op_egglog,$method);
       //     cartesian_ops!(($t2, $t1),$out,$op,$op_egglog,$method);
       //     cartesian_ops!(($t1, $t1),$out,$op,$op_egglog,$method);
       //     $( cartesian_ops!(($t1, $rest),$out,$op,$op_egglog,$method);)*
       //     $( cartesian_ops!(($rest, $t1),$out,$op,$op_egglog,$method);)*
       //     cartesian_ops!(($t2, $($rest),*),$out,$op,$op_egglog,$method);
       // };
}
macro_rules! batch_binary_ops_impl {
    ($qualifier:ident,$t1:ty,$t2:ty,$(($out:ty,$op:tt,$op_egglog:literal,$method:ident)),+) => {
        $(
            cartesian_ops!(
                $qualifier,
                ($t1, $t2),
                $out,
                $op,
                $op_egglog,
                $method
            );
        )+
    }
}
batch_binary_ops_impl!(
    forall,
    i64,
    f64,
    (f64, Add, "+", add),
    (f64, Sub, "-", sub),
    (f64, Mul, "*", mul),
    (f64, Div, "/", div)
);
batch_binary_ops_impl!(
    forall,
    f64,
    i64,
    (f64, Add, "+", add),
    (f64, Sub, "-", sub),
    (f64, Mul, "*", mul),
    (f64, Div, "/", div)
);
batch_binary_ops_impl!(
    plain,
    i64,
    i64,
    (i64, Add, "+", add),
    (i64, Sub, "-", sub),
    (i64, Mul, "*", mul),
    (i64, Div, "/", div)
);
batch_binary_ops_impl!(
    plain,
    f64,
    f64,
    (f64, Add, "+", add),
    (f64, Sub, "-", sub),
    (f64, Mul, "*", mul),
    (f64, Div, "/", div)
);

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
    Base {
        field_name: &'static str,
        sym: Sym,
    },
    Complex {
        sym: Sym,
    },
    Literal {
        lit: Literal,
    },
    Expr {
        op: &'static str,
        operands: Vec<Box<HandleTy>>,
    },
}
/// Handle used to generate constraint fact
#[derive(derive_more::Debug)]
pub struct HandleToConstrain<T: EgglogTy> {
    pub handle: HandleTy,
    pub _p: PhantomData<T>,
}
impl HandleTy {
    pub fn to_resolved_expr(&self, egraph: &EGraph) -> GenericExpr<String, String> {
        match &self {
            HandleTy::Base { field_name, sym } => {
                GenericExpr::Var(span!(), format!("{}{}", sym, field_name))
            }
            HandleTy::Complex { sym } => GenericExpr::Var(span!(), format!("{}", sym)),
            HandleTy::Literal { lit } => GenericExpr::Lit(span!(), lit.clone()),
            HandleTy::Expr { op, operands } => GenericExpr::Call(
                span!(),
                op.to_string(),
                operands
                    .iter()
                    .map(|x| x.to_resolved_expr(egraph))
                    .collect(),
            ),
        }
    }
}
impl<T: EgglogTy> HandleToConstrain<T> {
    pub fn name(&self) -> String {
        match &self.handle {
            HandleTy::Base { field_name, sym } => format!("{}{}", sym, field_name),
            HandleTy::Complex { sym } => format!("{}", sym),
            HandleTy::Literal { lit } => format!("{}.literal.{lit:?}", T::TY_NAME_LOWER),
            HandleTy::Expr { op, operands } => format!("{} {:?}", op, operands),
        }
    }
    pub fn to_resolved_expr(&self, egraph: &EGraph) -> GenericExpr<String, String> {
        self.handle.to_resolved_expr(egraph)
    }
}

/// Build a primitive-call handle (e.g. `to-string`, `and`, `bool-<`) to be used in constraints.
///
/// Example:
/// ```rust
/// use eggplant::prelude::*;
/// let h = prim_call::<bool>("and", vec![(&true).into_handle_ty(), (&false).into_handle_ty()]);
/// let c = h.eq(&false);
/// ```
#[track_caller]
pub fn prim_call<Out: EgglogTy>(
    op: &'static str,
    operands: Vec<HandleTy>,
) -> HandleToConstrain<Out> {
    HandleToConstrain {
        handle: HandleTy::Expr {
            op,
            operands: operands.into_iter().map(|h| Box::new(h)).collect(),
        },
        _p: PhantomData,
    }
}

#[track_caller]
pub fn vec_empty<VecTy>() -> HandleToConstrain<VecTy>
where
    VecTy: EgglogContainerTy,
{
    prim_call("vec-empty", vec![])
}

#[track_caller]
pub fn vec_of<VecTy, I, H>(elements: I) -> HandleToConstrain<VecTy>
where
    VecTy: EgglogContainerTy,
    I: IntoIterator<Item = H>,
    H: AsHandle<Target = VecTy::EleTy>,
{
    prim_call(
        "vec-of",
        elements
            .into_iter()
            .map(|element| element.as_handle().into_handle_ty())
            .collect(),
    )
}

#[track_caller]
pub fn set_empty<SetTy>() -> HandleToConstrain<SetTy>
where
    SetTy: EgglogContainerTy,
{
    prim_call("set-empty", vec![])
}

#[track_caller]
pub fn set_of<SetTy, I, H>(elements: I) -> HandleToConstrain<SetTy>
where
    SetTy: EgglogContainerTy,
    I: IntoIterator<Item = H>,
    H: AsHandle<Target = SetTy::EleTy>,
{
    prim_call(
        "set-of",
        elements
            .into_iter()
            .map(|element| element.as_handle().into_handle_ty())
            .collect(),
    )
}

pub trait VecExprExt: AsHandle
where
    Self::Target: EgglogContainerTy,
{
    #[track_caller]
    fn vec_push(
        &self,
        elem: impl AsHandle<Target = <Self::Target as EgglogContainerTy>::EleTy>,
    ) -> HandleToConstrain<Self::Target> {
        prim_call(
            "vec-push",
            vec![
                self.as_handle().into_handle_ty(),
                elem.as_handle().into_handle_ty(),
            ],
        )
    }

    #[track_caller]
    fn vec_append(
        &self,
        rhs: impl AsHandle<Target = Self::Target>,
    ) -> HandleToConstrain<Self::Target> {
        prim_call(
            "vec-append",
            vec![
                self.as_handle().into_handle_ty(),
                rhs.as_handle().into_handle_ty(),
            ],
        )
    }

    #[track_caller]
    fn vec_pop(&self) -> HandleToConstrain<Self::Target> {
        prim_call("vec-pop", vec![self.as_handle().into_handle_ty()])
    }

    #[track_caller]
    fn vec_len(&self) -> HandleToConstrain<i64> {
        prim_call("vec-length", vec![self.as_handle().into_handle_ty()])
    }

    #[track_caller]
    fn vec_get(
        &self,
        idx: impl AsHandle<Target = i64>,
    ) -> HandleToConstrain<<Self::Target as EgglogContainerTy>::EleTy> {
        prim_call(
            "vec-get",
            vec![
                self.as_handle().into_handle_ty(),
                idx.as_handle().into_handle_ty(),
            ],
        )
    }

    #[track_caller]
    fn vec_set(
        &self,
        idx: impl AsHandle<Target = i64>,
        elem: impl AsHandle<Target = <Self::Target as EgglogContainerTy>::EleTy>,
    ) -> HandleToConstrain<Self::Target> {
        prim_call(
            "vec-set",
            vec![
                self.as_handle().into_handle_ty(),
                idx.as_handle().into_handle_ty(),
                elem.as_handle().into_handle_ty(),
            ],
        )
    }

    #[track_caller]
    fn vec_contains(
        &self,
        elem: impl AsHandle<Target = <Self::Target as EgglogContainerTy>::EleTy>,
    ) -> FactCallConstraint {
        FactCallConstraint {
            op: "vec-contains",
            operands: vec![
                self.as_handle().into_handle_ty(),
                elem.as_handle().into_handle_ty(),
            ],
        }
    }

    #[track_caller]
    fn vec_not_contains(
        &self,
        elem: impl AsHandle<Target = <Self::Target as EgglogContainerTy>::EleTy>,
    ) -> FactCallConstraint {
        FactCallConstraint {
            op: "vec-not-contains",
            operands: vec![
                self.as_handle().into_handle_ty(),
                elem.as_handle().into_handle_ty(),
            ],
        }
    }
}

impl<H, VecTy> VecExprExt for H
where
    H: AsHandle<Target = VecTy>,
    VecTy: EgglogContainerTy,
{
}

pub trait SetExprExt: AsHandle
where
    Self::Target: EgglogContainerTy,
{
    #[track_caller]
    fn set_insert(
        &self,
        elem: impl AsHandle<Target = <Self::Target as EgglogContainerTy>::EleTy>,
    ) -> HandleToConstrain<Self::Target> {
        prim_call(
            "set-insert",
            vec![
                self.as_handle().into_handle_ty(),
                elem.as_handle().into_handle_ty(),
            ],
        )
    }

    #[track_caller]
    fn set_union(
        &self,
        rhs: impl AsHandle<Target = Self::Target>,
    ) -> HandleToConstrain<Self::Target> {
        prim_call(
            "set-union",
            vec![
                self.as_handle().into_handle_ty(),
                rhs.as_handle().into_handle_ty(),
            ],
        )
    }

    #[track_caller]
    fn set_len(&self) -> HandleToConstrain<i64> {
        prim_call("set-length", vec![self.as_handle().into_handle_ty()])
    }

    #[track_caller]
    fn set_get(
        &self,
        idx: impl AsHandle<Target = i64>,
    ) -> HandleToConstrain<<Self::Target as EgglogContainerTy>::EleTy> {
        prim_call(
            "set-get",
            vec![
                self.as_handle().into_handle_ty(),
                idx.as_handle().into_handle_ty(),
            ],
        )
    }

    #[track_caller]
    fn set_remove(
        &self,
        elem: impl AsHandle<Target = <Self::Target as EgglogContainerTy>::EleTy>,
    ) -> HandleToConstrain<Self::Target> {
        prim_call(
            "set-remove",
            vec![
                self.as_handle().into_handle_ty(),
                elem.as_handle().into_handle_ty(),
            ],
        )
    }
}

impl<H, SetTy> SetExprExt for H
where
    H: AsHandle<Target = SetTy>,
    SetTy: EgglogContainerTy,
{
}

impl<T: EgglogTy> Clone for HandleToConstrain<T> {
    fn clone(&self) -> Self {
        Self {
            handle: self.handle.clone(),
            _p: self._p.clone(),
        }
    }
}
