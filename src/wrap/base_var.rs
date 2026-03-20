use std::marker::PhantomData;
use std::sync::atomic::{AtomicU32, Ordering};

use egglog::{ArcSort, EGraph};

use crate::wrap::{
    BindingNames, EgglogTy, HandleToConstrain, HandleTy, PatRecSgl, PatVars, SortName, Sym,
    ToStrArcSort, Value, VarName, VarsCollector,
};

static BASE_VAR_COUNTER: AtomicU32 = AtomicU32::new(0);

/// A pure base-sort variable for rule patterns.
///
/// Unlike using a 1-field AST node to carry an `i64` (which introduces an extra constructor fact),
/// a `BaseVar<T, PR>` only declares a variable (name + sort) and can be used in:
/// - `PatRec::on_new_table_fact` / function-table facts (e.g. `fib(x, f0)`)
/// - constraints via [`BaseVar::handle`]
///
/// The matched value is extracted as [`Value<T>`] in rule callbacks.
#[derive(Clone, Copy, Debug)]
pub struct BaseVar<T: EgglogTy, PR = ()> {
    sym: Sym<T>,
    _p: PhantomData<PR>,
}

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

    /// Return a new variable with the same sort but a nicer debug prefix.
    ///
    /// This is intended for readability (e.g. `fib::x().named("x1")`). The suffix is still
    /// uniquified to avoid accidental collisions across patterns.
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
    fn metas_iter(&self) -> impl Iterator<Item = PR::MetaTy> {
        std::iter::empty()
    }
}
