use std::any::type_name;

use egglog::{
    ast::Literal,
    sort::{Boxed, OrderedFloat, Q},
};

use crate::ToValue;

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

impl ToValue<i64> for i64 {
    fn to_value(&self, ctx: &mut super::RuleCtx) -> super::Value<i64> {
        ctx.intern_base(*self)
    }
}

impl ToValue<bool> for bool {
    fn to_value(&self, ctx: &mut super::RuleCtx) -> super::Value<bool> {
        ctx.intern_base(*self)
    }
}

impl ToValue<String> for String {
    fn to_value(&self, ctx: &mut super::RuleCtx) -> super::Value<String> {
        ctx.intern_base(self.clone())
    }
}

impl ToValue<Q> for Q {
    fn to_value(&self, ctx: &mut super::RuleCtx) -> super::Value<Q> {
        ctx.intern_base(self.clone())
    }
}

impl ToValue<f64> for f64 {
    fn to_value(&self, ctx: &mut super::RuleCtx) -> super::Value<f64> {
        ctx.intern_base(Boxed(OrderedFloat(*self)))
    }
}
