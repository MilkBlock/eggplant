use crate::{EValue, EgglogNode, EgglogTy};

/// Trait for input types that can be used in egglog functions
pub trait EgglogFuncInput {
    type Ref<'a>: EgglogFuncInputRef;
    fn as_evalue(&self) -> &dyn EValue;
}
/// Trait for input tuple that can be used in egglog functions
pub trait EgglogFuncInputs {
    type Ref<'a>: EgglogFuncInputsRef;
    fn as_evalues(&self) -> Box<[&dyn EValue]>;
}
/// Trait for input types ref that directly used as function argument
pub trait EgglogFuncInputRef {
    type DeRef: EgglogFuncInput + EValue;
    fn as_evalue(&self) -> &dyn EValue;
}
pub trait EgglogFuncInputsRef {
    type DeRef: EgglogFuncInputs;
    fn as_evalues(&self) -> Box<[&dyn EValue]>;
}

/// Trait for output types that can be used in egglog functions
pub trait EgglogFuncOutput: 'static + Clone {
    type Ref<'a>: EgglogFuncOutputRef;
    fn as_evalue(&self) -> &dyn EValue;
    fn clone_downcast(&self) -> Self;
}
impl<T> EgglogFuncOutput for T
where
    T: EgglogNode + 'static + Sized + Clone,
{
    type Ref<'a> = &'a dyn AsRef<T>;
    fn as_evalue(&self) -> &dyn EValue {
        self
    }
    fn clone_downcast(&self) -> T {
        self.clone()
    }
}
impl<T: EgglogFuncOutput + EgglogNode + 'static> EgglogFuncOutputRef for &dyn AsRef<T> {
    type DeRef = T;
    fn as_evalue(&self) -> &dyn EValue {
        self.as_ref()
    }
    fn deref(&self) -> &Self::DeRef {
        self.as_ref()
    }
}
pub trait EgglogFuncOutputRef {
    type DeRef: EgglogFuncOutput;
    fn as_evalue(&self) -> &dyn EValue;
    fn deref(&self) -> &Self::DeRef;
}
pub trait EgglogFunc {
    type Input: EgglogFuncInputs;
    type Output: EgglogFuncOutput;
    type OutputTy: EgglogTy;
    const FUNC_NAME: &'static str;
}
impl<T> EgglogFuncInput for T
where
    T: EgglogNode + 'static,
{
    type Ref<'a> = &'a dyn AsRef<T>;
    fn as_evalue(&self) -> &dyn EValue {
        self
    }
}
impl<T> EgglogFuncInputRef for &dyn AsRef<T> where T:EgglogNode +'static{
    type DeRef = T;

    fn as_evalue(&self) -> &dyn EValue {
        self.as_ref()
    }
}
macro_rules! impl_egglog_for_primitive {
    ($type:ty) => {
        impl EgglogFuncInput for $type {
            type Ref<'a> = &'a $type;
            
            fn as_evalue(&self) -> &dyn EValue {
                self
            }
        }
        
        impl EgglogFuncInputRef for &$type {
            type DeRef = $type;
            
            fn as_evalue(&self) -> &dyn EValue {
                *self
            }
        }
        
        impl EgglogFuncOutput for $type {
            type Ref<'a> = &'a $type;

            fn as_evalue(&self) -> &dyn EValue {
                self
            }

            fn clone_downcast(&self) -> Self {
                self.clone()
            }
        }
        
        impl EgglogFuncOutputRef for &$type {
            type DeRef = $type;

            fn as_evalue(&self) -> &dyn EValue {
                *self
            }

            fn deref(&self) -> &Self::DeRef {
                self
            }
        }
    };
}
impl_egglog_for_primitive!(i64);
impl_egglog_for_primitive!(String);
impl_egglog_for_primitive!(bool);
// 其他基本类型..

macro_rules! impl_input_for_tuples {
    () => {
        #[allow(unused)]
        impl EgglogFuncInputs for () {
            type Ref<'a> = ();
            fn as_evalues(&self) -> Box<[&dyn EValue]>{
                Box::new([])
            }
        }
    };
    ($($T:ident),*) => {
        #[allow(unused)]
        impl<$($T: EValue + EgglogFuncInput),*> EgglogFuncInputs for ($($T),*) {
            type Ref<'a> = ($($T::Ref<'a>),*);

            fn as_evalues(&self) -> Box<[&dyn EValue]> {
                #[allow(non_snake_case)]
                let ($($T),*) = self;
                Box::new([$($T),*])
            }
        }
    };
}

// Continue for as many tuple sizes as needed
macro_rules! impl_input_ref_for_tuples {
    () => {
        #[allow(unused)]
        impl EgglogFuncInputsRef for () {
            type DeRef = ();
            fn as_evalues(&self) -> Box<[&dyn EValue]> {
                Box::new([])
            }
        }
    };

    ($($T:ident),*) => {
        #[allow(unused)]
        impl<$($T: EgglogFuncInputRef),*> EgglogFuncInputsRef for ($($T),*) {
            type DeRef = ($($T::DeRef),*);
            #[allow(non_snake_case)]
            fn as_evalues(&self) -> Box<[&dyn EValue]> {
                let ($($T),*) = self;
                Box::new([$($T.as_evalue()),*])
            }
        }
    };
}

macro_rules! impl_for_tuples {
    ($($T:ident),*) => {
        impl_input_for_tuples!($($T),*);
        impl_input_ref_for_tuples!($($T),*);
    };
}


// Generate implementations
impl_for_tuples!();
impl_for_tuples!(T0);
impl_for_tuples!(T0, T1);
impl_for_tuples!(T0, T1, T2);
impl_for_tuples!(T0, T1, T2, T3);
impl_for_tuples!(T0, T1, T2, T3, T4);
impl_for_tuples!(T0, T1, T2, T3, T4, T5);
impl_for_tuples!(T0, T1, T2, T3, T4, T5, T6);
impl_for_tuples!(T0, T1, T2, T3, T4, T5, T6, T7);
impl_for_tuples!(T0, T1, T2, T3, T4, T5, T6, T7, T8);
