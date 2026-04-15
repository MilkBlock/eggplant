use std::marker::PhantomData;
use std::sync::atomic::{AtomicU32, Ordering};

use egglog::{ArcSort, EGraph};

use crate::wrap::{
    BindingNames, EgglogTy, HandleToConstrain, HandleTy, PatRecSgl, PatVars, SortName, Sym,
    ToStrArcSort, Value, VarName, VarsCollector,
};

static BASE_VAR_COUNTER: AtomicU32 = AtomicU32::new(0);

#[derive(Debug)]
pub struct BaseVar<T: EgglogTy, PR = ()> {
    sym: Sym<T>,
    _p: PhantomData<PR>,
}

impl<T: EgglogTy, PR> Clone for BaseVar<T, PR> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T: EgglogTy, PR> Copy for BaseVar<T, PR> {}

impl<T: EgglogTy, PR> BaseVar<T, PR> {
    #[track_caller]
    pub fn query() -> Self {
        let id = BASE_VAR_COUNTER.fetch_add(1, Ordering::Relaxed);
        let sym: Sym<T> = Sym::new(format!("{}{}", T::TY_NAME_LOWER, id).into());
        Self {
            sym,
            _p: PhantomData,
        }
    }

    #[track_caller]
    pub fn query_named(prefix: &'static str) -> Self {
        let id = BASE_VAR_COUNTER.fetch_add(1, Ordering::Relaxed);
        let sym: Sym<T> = Sym::new(format!("{prefix}{id}").into());
        Self {
            sym,
            _p: PhantomData,
        }
    }

    #[track_caller]
    pub fn named(self, prefix: &'static str) -> Self {
        Self::query_named(prefix)
    }

    pub fn name(&self) -> String {
        self.sym.to_string()
    }

    pub fn handle(&self) -> HandleToConstrain<T> {
        HandleToConstrain {
            handle: HandleTy::Complex {
                sym: self.sym.erase(),
            },
            _p: PhantomData,
        }
    }
}

impl<T: EgglogTy, PR> VarsCollector for BaseVar<T, PR> {
    fn collect_vars(&self, vars: &mut Vec<(VarName, SortName)>) {
        vars.push((self.sym.to_string(), T::TY_NAME.to_string()));
    }
}

impl<T: EgglogTy, PR> BindingNames for BaseVar<T, PR> {
    fn collect_binding_names(&self, names: &mut Vec<VarName>) {
        names.push(self.sym.to_string());
    }
}

impl<T: EgglogTy, PR> ToStrArcSort for BaseVar<T, PR> {
    fn to_str_arcsort(&self, egraph: &EGraph) -> Vec<(VarName, ArcSort)> {
        vec![(
            self.sym.to_string(),
            egraph.get_sort_by_name(T::TY_NAME).unwrap().clone(),
        )]
    }
}

impl<PR: PatRecSgl, T: EgglogTy> PatVars<PR> for BaseVar<T, PR> {
    type Valued = Value<T>;

    fn metas_iter(&self) -> impl Iterator<Item = crate::prelude::SlotMeta> {
        std::iter::empty()
    }
}
