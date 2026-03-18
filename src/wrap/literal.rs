use std::any::type_name;

use egglog::{
    ast::Literal,
    sort::{Boxed, OrderedFloat, Q, Z},
};

use crate::wrap::{BoxedBase, BoxedValue, RuleCtx};

pub trait DeLiteral<T> {
    fn deliteral(&self) -> T;
}
pub trait FromBase<T: Clone> {
    fn from_base(base: &T) -> Self;
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
impl DeLiteral<&'static str> for Literal {
    fn deliteral(&self) -> &'static str {
        match self {
            Literal::Int(_) => {
                panic!("can't deliteral float to {}", type_name::<String>())
            }
            Literal::Float(_) => {
                panic!("can't deliteral float to {}", type_name::<String>())
            }
            Literal::String(s) => s.clone().leak(),
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

impl DeLiteral<Q> for Literal {
    fn deliteral(&self) -> Q {
        match self {
            // Note: egglog does not have a dedicated BigRat literal kind; when we need to
            // represent it in a `Literal`, we round-trip through strings.
            Literal::String(s) => Q::new(
                s.parse()
                    .unwrap_or_else(|_| panic!("can't deliteral string to {}", type_name::<Q>())),
            ),
            Literal::Int(i) => Q::new(i.to_string().parse().unwrap()),
            Literal::Float(_) => panic!("can't deliteral float to {}", type_name::<Q>()),
            Literal::Bool(_) => panic!("can't deliteral bool to {}", type_name::<Q>()),
            Literal::Unit => panic!("can't deliteral unit to {}", type_name::<Q>()),
        }
    }
}

impl DeLiteral<Z> for Literal {
    fn deliteral(&self) -> Z {
        match self {
            // Note: egglog does not have a dedicated BigInt literal kind; when we need to
            // represent it in a `Literal`, we round-trip through strings.
            Literal::String(s) => Z::new(
                s.parse()
                    .unwrap_or_else(|_| panic!("can't deliteral string to {}", type_name::<Z>())),
            ),
            Literal::Int(i) => Z::new((*i).into()),
            Literal::Float(_) => panic!("can't deliteral float to {}", type_name::<Z>()),
            Literal::Bool(_) => panic!("can't deliteral bool to {}", type_name::<Z>()),
            Literal::Unit => panic!("can't deliteral unit to {}", type_name::<Z>()),
        }
    }
}

impl FromBase<bool> for Literal {
    fn from_base(base: &bool) -> Self {
        Literal::Bool(base.clone())
    }
}

impl FromBase<i64> for Literal {
    fn from_base(base: &i64) -> Self {
        Literal::Int(*base)
    }
}

impl FromBase<f64> for Literal {
    fn from_base(base: &f64) -> Self {
        Literal::Float(OrderedFloat(*base))
    }
}
impl FromBase<String> for Literal {
    fn from_base(base: &String) -> Self {
        Literal::String(base.clone())
    }
}
impl FromBase<&'static str> for Literal {
    fn from_base(base: &&'static str) -> Self {
        Literal::String(base.to_string())
    }
}

impl FromBase<Q> for Literal {
    fn from_base(base: &Q) -> Self {
        Literal::String(base.0.to_string())
    }
}

impl FromBase<Z> for Literal {
    fn from_base(base: &Z) -> Self {
        Literal::String(base.0.to_string())
    }
}

impl BoxedBase for f64 {
    type Boxed = Boxed<OrderedFloat<f64>>;
    fn box_it(self, _ctx: &super::RuleCtx) -> Self::Boxed {
        Boxed(OrderedFloat(self))
    }
    fn unbox(boxed: Self::Boxed, _ctx: &super::RuleCtx) -> Self {
        *boxed.0
    }
}
impl BoxedValue for f64 {
    type Output<'a> = Self;
    fn devalue<'b>(rule_ctx: &'b super::RuleCtx, value: egglog::Value) -> Self::Output<'b> {
        let value = rule_ctx._devalue_base(value);
        Self::unbox(value, rule_ctx)
    }
}
macro_rules! impl_simple_boxed_base_for {
    ($ty:ty) => {
        impl BoxedBase for $ty {
            type Boxed = $ty;

            fn unbox(boxed: Self::Boxed, _ctx: &super::RuleCtx) -> Self {
                boxed
            }

            fn box_it(self, _ctx: &super::RuleCtx) -> Self::Boxed {
                self
            }
        }
        impl_simple_boxed_value_for_boxed_base!($ty);
    };
}
macro_rules! impl_simple_boxed_value_for_boxed_base {
    ($ty:ty) => {
        impl BoxedValue for $ty {
            type Output<'a> = $ty;
            fn devalue<'b>(rule_ctx: &'b RuleCtx, value: egglog::Value) -> Self::Output<'b> {
                let value = rule_ctx._devalue_base(value);
                Self::unbox(value, rule_ctx)
            }
        }
    };
}
impl BoxedBase for String {
    type Boxed = Boxed<String>;
    fn unbox(boxed: Self::Boxed, _ctx: &RuleCtx) -> Self {
        boxed.0
    }
    fn box_it(self, _ctx: &RuleCtx) -> Self::Boxed {
        Boxed::new(self)
    }
}
impl BoxedValue for String {
    type Output<'a> = Self;
    fn devalue(rule_ctx: &RuleCtx, value: egglog::Value) -> Self {
        let value = rule_ctx._devalue_base(value);
        Self::unbox(value, rule_ctx)
    }
}
impl_simple_boxed_base_for!(i64);
impl_simple_boxed_base_for!(&'static str);
impl_simple_boxed_base_for!(bool);
impl_simple_boxed_base_for!(Q);
impl_simple_boxed_base_for!(Z);
