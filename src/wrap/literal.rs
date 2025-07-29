use std::any::type_name;

use egglog::{
    ast::Literal,
    sort::{Boxed, OrderedFloat},
};

use crate::BoxUnbox;

pub trait DeLiteral<T> {
    fn deliteral(&self) -> T;
}
pub trait FromBase<T> {
    fn from_base(base: T) -> Self;
}
impl DeLiteral<i64> for Literal {
    fn deliteral(&self) -> i64 {
        match self {
            Literal::Int(i) => *i,
            Literal::Float(_) => {
                panic!("can't deliteral float to {}", type_name::<i64>())
            }
            Literal::String(_) => {
                panic!("can't deliteral string to {}", type_name::<i64>())
            }
            Literal::Bool(_) => {
                panic!("can't deliteral bool to {}", type_name::<i64>())
            }
            Literal::Unit => {
                panic!("can't deliteral bool to {}", type_name::<i64>())
            }
        }
    }
}

impl DeLiteral<f64> for Literal {
    fn deliteral(&self) -> f64 {
        match self {
            Literal::Int(_) => {
                panic!("can't deliteral float to {}", type_name::<f64>())
            }
            Literal::Float(ordered_float) => ordered_float.0,
            Literal::String(_) => {
                panic!("can't deliteral string to {}", type_name::<f64>())
            }
            Literal::Bool(_) => {
                panic!("can't deliteral bool to {}", type_name::<f64>())
            }
            Literal::Unit => {
                panic!("can't deliteral bool to {}", type_name::<f64>())
            }
        }
    }
}

impl DeLiteral<String> for Literal {
    fn deliteral(&self) -> String {
        match self {
            Literal::Int(_) => {
                panic!("can't deliteral float to {}", type_name::<String>())
            }
            Literal::Float(_) => {
                panic!("can't deliteral float to {}", type_name::<String>())
            }
            Literal::String(s) => s.clone(),
            Literal::Bool(_) => {
                panic!("can't deliteral bool to {}", type_name::<String>())
            }
            Literal::Unit => {
                panic!("can't deliteral bool to {}", type_name::<String>())
            }
        }
    }
}

impl DeLiteral<bool> for Literal {
    fn deliteral(&self) -> bool {
        match self {
            Literal::Int(_) => {
                panic!("can't deliteral float to {}", type_name::<bool>())
            }
            Literal::Float(_) => {
                panic!("can't deliteral float to {}", type_name::<bool>())
            }
            Literal::String(_) => {
                panic!("can't deliteral string to {}", type_name::<bool>())
            }
            Literal::Bool(b) => *b,
            Literal::Unit => {
                panic!("can't deliteral bool to {}", type_name::<bool>())
            }
        }
    }
}

impl FromBase<bool> for Literal {
    fn from_base(base: bool) -> Self {
        Literal::Bool(base)
    }
}

impl FromBase<i64> for Literal {
    fn from_base(base: i64) -> Self {
        Literal::Int(base)
    }
}

impl FromBase<f64> for Literal {
    fn from_base(base: f64) -> Self {
        Literal::Float(OrderedFloat(base))
    }
}
impl FromBase<String> for Literal {
    fn from_base(base: String) -> Self {
        Literal::String(base)
    }
}

impl BoxUnbox for f64 {
    type Boxed = Boxed<OrderedFloat<f64>>;
    type UnBoxed = f64;
    fn box_it(self, _ctx: &mut super::RuleCtx) -> Self::Boxed {
        Boxed(OrderedFloat(self))
    }
    fn unbox(boxed: Self::Boxed, _ctx: &mut super::RuleCtx) -> Self::UnBoxed {
        *boxed.0
    }
}
macro_rules! impl_simple_boxunbox_for {
    ($ty:ident) => {
        impl BoxUnbox for $ty {
            type Boxed = $ty;
            type UnBoxed = $ty;

            fn unbox(boxed: Self::Boxed, _ctx: &mut super::RuleCtx) -> Self::UnBoxed {
                boxed
            }

            fn box_it(self, _ctx: &mut super::RuleCtx) -> Self::Boxed {
                self
            }
        }
    };
}
impl_simple_boxunbox_for!(String);
impl_simple_boxunbox_for!(i64);
impl_simple_boxunbox_for!(bool);
