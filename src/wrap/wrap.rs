use crate::prelude::slotted::{_FuncValueMeta, FuncName, FuncValueMeta};
use crate::prelude::{SlotMeta, TxRxVT};
use crate::wrap::DslVariantDecl;
use crate::wrap::constraint::IntoConstraintFact;
#[cfg(feature = "rustsat-extract")]
use crate::wrap::eboost_extract::{
    EBoostCandidate, EBoostEqKey, EBoostPrepared, collect_candidates, prepare_eboost_candidates,
};
use crate::wrap::eboost_extract::{EBoostExtractConfig, eboost_extract_value_prototype};
use crate::wrap::{
    EValue, EgglogFunc, EgglogFuncInputs, EgglogFuncInputsRef, EgglogFuncOutput, EgglogRelation,
    EgglogTy, FactsBuilder, FromBase, SortName, SymLit, TableName, VarName,
};
use crate::wrap::{RuleCtx, RuleCtxHook, RuleRunnerSgl};
use dashmap::DashMap;
use derive_more::{Debug, Deref, DerefMut, IntoIterator};
use egglog::ast::{RustSpan, Span};
use egglog::extract::{CostModel, DefaultCost, TreeAdditiveCostModel};
use egglog::prelude::span;
use egglog::{
    ArcSort, BaseValue, ContainerValue, EGraph,
    ast::{Command, GenericAction, GenericExpr},
};
use egglog::{Term, TermDag, TermId, ast::Literal};
#[cfg(feature = "rustsat-extract")]
use rustsat::{
    algs::maxsat::SolutionImprovingSearch,
    encodings::pb::BinaryAdder,
    instances::{BasicVarManager, OptInstance},
    types::{
        Assignment as RustsatAssignment, Clause as RustsatClause, Lit as RustsatLit, TernaryVal,
    },
};
#[cfg(feature = "rustsat-extract")]
use rustsat_minisat::core::Minisat as RustsatMinisat;
use serde::{Deserialize, Serialize};
use smallvec::SmallVec;
#[cfg(feature = "rustsat-extract")]
use std::collections::{BTreeSet, HashSet, VecDeque};
use std::sync::Mutex;
use std::{
    any::Any,
    borrow::Borrow,
    borrow::Cow,
    collections::HashMap,
    fmt, fs,
    hash::Hash,
    marker::PhantomData,
    panic::Location,
    path::Path,
    process::Command as ProcessCommand,
    sync::{Arc, atomic::AtomicU32},
    time::{SystemTime, UNIX_EPOCH},
};
use strum::IntoDiscriminant;
use strum_macros::{EnumDiscriminants, EnumIs};
use symbol_table::GlobalSymbol;
pub type EgglogAction = GenericAction<String, String>;
pub type TermToNode = fn(TermId, &TermDag, &mut HashMap<TermId, Sym>) -> Box<dyn EgglogNode>;

#[derive(Debug)]
pub enum TxCommand {
    StringCommand { command: String },
    NativeCommand { command: Command },
}

/// This trait is useful when defining pattern.
/// We assume every node is a placeholder if it doesn't drop after call the defining function.
pub trait NodeDropper: NodeOwner + 'static {
    fn on_drop(&self, _dropped: &mut (impl EgglogNode + 'static)) {
        // do nothing as default
    }

    #[track_caller]
    fn replace_meta(&self, _sym: Sym, _meta: Box<dyn Any>) {
        panic!("no meta suppoerted")
    }
    fn meta_of(&self, _sym: Sym) -> Box<dyn std::any::Any> {
        panic!("no meta supported")
    }
}
pub trait Tx: 'static + NodeOwner + NodeDropper {
    /// receive is guaranteed to not be called in proc macro
    #[track_caller]
    fn send(&self, sended: TxCommand);
    #[track_caller]
    fn on_new(&self, node: &(impl EgglogNode + 'static));
    #[track_caller]
    fn on_func_set<'a, F: EgglogFunc>(
        &self,
        input: <F::Input as EgglogFuncInputs>::Ref<'a>,
        output: <F::Output as EgglogFuncOutput>::Ref<'a>,
    );
    #[track_caller]
    fn on_relation_insert<'a, R: EgglogRelation>(
        &self,
        input: <R::Input as EgglogFuncInputs>::Ref<'a>,
    ) {
        let input_exprs = input
            .as_evalues()
            .iter()
            .map(|value| (*value).get_egglog_expr())
            .collect::<Vec<_>>();
        self.send(TxCommand::NativeCommand {
            command: Command::Action(GenericAction::Expr(
                span!(),
                GenericExpr::Call(span!(), R::REL_NAME.to_string(), input_exprs),
            )),
        });
    }
    #[track_caller]
    fn on_union(&self, node1: &(impl EgglogNode + 'static), node2: &(impl EgglogNode + 'static));
    fn canonical_raw(&self, node1: &(impl EgglogNode + 'static)) -> egglog::Value;
}
pub trait Rx: 'static {
    #[track_caller]
    fn on_func_get<'a, F: EgglogFunc>(
        &self,
        input: <F::Input as EgglogFuncInputs>::Ref<'a>,
    ) -> F::Output;
    #[track_caller]
    fn on_funcs_get<'a, 'b, F: EgglogFunc>(
        &self,
        max_size: Option<usize>,
    ) -> Vec<(
        <F::Input as EgglogFuncInputs>::Ref<'b>,
        <F::Output as EgglogFuncOutput>::Ref<'b>,
    )>;
    #[track_caller]
    fn on_pull<T: EgglogTy>(&self, node: &(impl EgglogNode + 'static)) {
        self.on_pull_sym::<T>(node.cur_sym());
    }

    #[track_caller]
    fn on_pull_sym<T: EgglogTy>(&self, sym: Sym) -> SymLit;
    #[track_caller]
    fn on_pull_value<T: EgglogTy>(&self, value: Value<T>) -> SymLit;
}

pub trait SingletonGetter: 'static {
    type RetTy;
    #[track_caller]
    fn sgl() -> &'static Self::RetTy;
}
pub trait NodeOwnerSgl: SingletonGetter + 'static {
    /// helpful when you want to append additional data to node specific to your NodeOwner
    type OwnerSpecDataInNode<T: EgglogTy, V: EgglogEnumVariantTy>: Default + Copy + Send + Sync;
}
pub trait NodeOwner: 'static {
    /// helpful when you want to append additional data to node specific to your singleton
    type OwnerSpecDataInNode<T: EgglogTy, V: EgglogEnumVariantTy>: Default + Copy + Send + Sync;
}
impl<S: SingletonGetter> NodeOwnerSgl for S
where
    S::RetTy: NodeOwner,
{
    type OwnerSpecDataInNode<T: EgglogTy, V: EgglogEnumVariantTy> =
        <Self::RetTy as NodeOwner>::OwnerSpecDataInNode<T, V>;
}
pub trait NodeDropperSgl: 'static + Sized + SingletonGetter + NodeOwnerSgl {
    fn on_drop(dropped: &mut (impl EgglogNode + 'static));

    fn replace_meta(sym: Sym, meta: Box<dyn Any>);
    fn meta_of(sym: Sym) -> Box<dyn std::any::Any>;
}

pub trait TxSgl: 'static + Sized + NodeDropperSgl + NodeOwnerSgl {
    // delegate all functions from Tx
    fn receive(received: TxCommand);
    #[track_caller]
    fn on_new(node: &(impl EgglogNode + 'static));
    #[track_caller]
    fn on_func_set<'a, F: EgglogFunc>(
        input: <F::Input as EgglogFuncInputs>::Ref<'a>,
        output: <F::Output as EgglogFuncOutput>::Ref<'a>,
    );
    #[track_caller]
    fn on_relation_insert<'a, R: EgglogRelation>(input: <R::Input as EgglogFuncInputs>::Ref<'a>);
    fn on_union(node1: &(impl EgglogNode + 'static), node2: &(impl EgglogNode + 'static));
    fn canonical_raw(node1: &(impl EgglogNode + 'static)) -> egglog::Value;
}
pub trait RxSgl: 'static + Sized + SingletonGetter + NodeDropperSgl + NodeOwnerSgl {
    // delegate all functions from Rx
    #[track_caller]
    fn on_func_get<'a, 'b, F: EgglogFunc>(
        input: <F::Input as EgglogFuncInputs>::Ref<'a>,
    ) -> F::Output;
    #[track_caller]
    fn on_funcs_get<'a, 'b, F: EgglogFunc>(
        max_size: Option<usize>,
    ) -> Vec<(
        <F::Input as EgglogFuncInputs>::Ref<'b>,
        <F::Output as EgglogFuncOutput>::Ref<'b>,
    )>;
    #[track_caller]
    fn on_pull<T: EgglogTy>(node: &(impl EgglogNode + 'static));
}

impl<S: SingletonGetter> NodeDropperSgl for S
where
    S::RetTy: NodeDropper + 'static,
{
    fn on_drop(_dropped: &mut (impl EgglogNode + 'static)) {
        // do nothing as default
        // Self::sgl().on_drop(dropped);
    }
    fn replace_meta(sym: Sym, meta: Box<dyn Any>) {
        Self::sgl().replace_meta(sym, meta)
    }
    fn meta_of(sym: Sym) -> Box<dyn std::any::Any> {
        Self::sgl().meta_of(sym)
    }
}

impl<S: SingletonGetter + 'static> TxSgl for S
where
    S::RetTy: Tx + NodeDropper + NodeSetter + 'static,
{
    fn receive(received: TxCommand) {
        Self::sgl().send(received);
    }
    fn on_new(node: &(impl EgglogNode + 'static)) {
        Self::sgl().on_new(node);
    }

    fn on_func_set<'a, F: EgglogFunc>(
        input: <F::Input as EgglogFuncInputs>::Ref<'a>,
        output: <F::Output as EgglogFuncOutput>::Ref<'a>,
    ) {
        Self::sgl().on_func_set::<F>(input, output);
    }

    fn on_relation_insert<'a, R: EgglogRelation>(input: <R::Input as EgglogFuncInputs>::Ref<'a>) {
        Self::sgl().on_relation_insert::<R>(input);
    }

    fn on_union(node1: &(impl EgglogNode + 'static), node2: &(impl EgglogNode + 'static)) {
        Self::sgl().on_union(node1, node2);
    }
    fn canonical_raw(node1: &(impl EgglogNode + 'static)) -> egglog::Value {
        Self::sgl().canonical_raw(node1)
    }
}
pub trait NodeSetterSgl {
    #[track_caller]
    fn on_set(node: &mut (impl EgglogNode + 'static));
}
impl<S: NodeOwnerSgl> NodeSetterSgl for S
where
    S::RetTy: NodeSetter,
{
    fn on_set(node: &mut (impl EgglogNode + 'static)) {
        Self::sgl().on_set(node);
    }
}
pub trait NodeSetter {
    #[track_caller]
    fn on_set(&self, node: &mut (impl EgglogNode + 'static));
}
impl<S: SingletonGetter + 'static> RxSgl for S
where
    S::RetTy: Rx + NodeDropper + 'static,
{
    fn on_func_get<'a, 'b, F: EgglogFunc>(
        input: <F::Input as EgglogFuncInputs>::Ref<'a>,
    ) -> F::Output {
        Self::sgl().on_func_get::<F>(input)
    }

    fn on_funcs_get<'a, 'b, F: EgglogFunc>(
        max_size: Option<usize>,
    ) -> Vec<(
        <F::Input as EgglogFuncInputs>::Ref<'b>,
        <F::Output as EgglogFuncOutput>::Ref<'b>,
    )> {
        Self::sgl().on_funcs_get::<F>(max_size)
    }
    fn on_pull<T: EgglogTy>(node: &(impl EgglogNode + 'static)) {
        Self::sgl().on_pull::<T>(node)
    }
}

/// version control triat
/// which should be implemented by Tx
pub trait VersionCtl {
    fn locate_latest(&self, node: Sym) -> Sym;
    fn locate_next(&self, node: Sym) -> Sym;
    fn locate_prev(&self, node: Sym) -> Sym;
    fn set_latest(&self, node: &mut Sym);
    fn set_next(&self, node: &mut Sym);
    fn set_prev(&self, node: &mut Sym);
}

pub trait Meta:
    Default
    + Clone
    + Send
    + Sync
    + fmt::Debug
    + Serialize
    + Deserialize<'static>
    + Hash
    + PartialEq
    + Eq
{
    fn merge(metas: &mut impl Iterator<Item = Self>) -> Self;
}
impl Meta for () {
    fn merge(_metas: &mut impl Iterator<Item = Self>) -> Self {
        ()
    }
}
/// pattern recorder triat
/// it's neccessary to impl NodeDropper for PatternCombine feature
/// and also should be implemented by Tx
pub trait PatRec: NodeDropper + Tx {
    type MetaTy: Meta;
    #[track_caller]
    fn on_new_query_leaf(&self, node: &(impl EgglogNode + 'static));
    #[track_caller]
    fn on_new_constraint(&self, constraint: impl IntoConstraintFact);
    #[track_caller]
    fn on_new_table_fact(&self, query_table: TableName, vars: Vec<(VarName, SortName)>) {
        let _ = (query_table, vars);
    }
    #[track_caller]
    fn on_new_relation_fact(&self, query_table: TableName, vars: Vec<(VarName, SortName)>) {
        let _ = (query_table, vars);
    }
    fn on_record_start(&self);
    fn on_record_end<T: PatRecSgl>(&self, pat_vars: &impl PatVars<T>) -> PatId;
    fn pat2fact_builder(&self, pat_id: PatId) -> FactsBuilder;

    #[allow(unused)]
    fn on_ctx_insert<PR: PatRecSgl>(
        &self,
        inputs: Vec<FuncValueMeta<Self>>,
        output: (FuncName, egglog::Value, Option<Self::MetaTy>),
    ) {
    }
    #[allow(unused)]
    fn on_ctx_union(&self, combo1: FuncValueMeta<Self>, combo2: FuncValueMeta<Self>) {}

    /// return whether updated
    fn flush_pending(&self, _egraph: &EGraph) -> bool {
        false
    }
}
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct PatId(pub u32);

pub trait PatRecSgl: NodeDropperSgl + TxSgl {
    type MetaTy: Meta;
    #[track_caller]
    fn on_new_query_leaf(node: &(impl EgglogNode + 'static));
    #[track_caller]
    fn on_new_constraint(constraint: impl IntoConstraintFact);
    #[track_caller]
    fn on_new_table_fact(query_table: TableName, vars: Vec<(VarName, SortName)>);
    #[track_caller]
    fn on_new_relation_fact(query_table: TableName, vars: Vec<(VarName, SortName)>);
    fn on_record_start();
    fn on_record_end(pat_vars: &impl PatVars<Self>) -> PatId;
    fn pat2fact_builder(pat_id: PatId) -> FactsBuilder;

    fn on_ctx_insert(inputs: Vec<_FuncValueMeta<Self>>, output: _FuncValueMeta<Self>);
    fn on_ctx_union(combo1: _FuncValueMeta<Self>, combo2: _FuncValueMeta<Self>);

    /// flush pending and return whether updated
    fn flush_pending(egraph: &EGraph) -> bool;
}
impl<T: WithRxSgl + SingletonGetter> PatRecSgl for T
where
    T::RetTy: PatRec + NodeSetter,
{
    type MetaTy = <T::RetTy as PatRec>::MetaTy;
    fn on_new_query_leaf(node: &(impl EgglogNode + 'static)) {
        Self::sgl().on_new_query_leaf(node);
    }
    fn on_new_constraint(constraint: impl IntoConstraintFact) {
        Self::sgl().on_new_constraint(constraint);
    }
    fn on_new_table_fact(query_table: TableName, vars: Vec<(VarName, SortName)>) {
        Self::sgl().on_new_table_fact(query_table, vars);
    }
    fn on_new_relation_fact(query_table: TableName, vars: Vec<(VarName, SortName)>) {
        Self::sgl().on_new_relation_fact(query_table, vars);
    }
    fn on_record_start() {
        Self::sgl().on_record_start();
    }

    fn on_record_end(pat_vars: &impl PatVars<Self>) -> PatId {
        Self::sgl().on_record_end(pat_vars)
    }

    fn pat2fact_builder(pat_id: PatId) -> FactsBuilder {
        Self::sgl().pat2fact_builder(pat_id)
    }

    fn on_ctx_insert(_inputs: Vec<_FuncValueMeta<Self>>, _output: _FuncValueMeta<Self>) {}

    fn on_ctx_union(combo1: _FuncValueMeta<Self>, combo2: _FuncValueMeta<Self>) {
        Self::sgl().on_ctx_union(combo1, combo2)
    }

    fn flush_pending(egraph: &EGraph) -> bool {
        Self::sgl().flush_pending(egraph)
    }
}

pub trait WithPatRecSgl: SingletonGetter {
    type PatRecSgl: PatRecSgl;
}
pub trait WithRxSgl {
    type RxSgl: RxSgl;
}

// pub trait WithPatternRecorderSgl

/// version control triat
/// which should be implemented by Tx
pub trait VersionCtlSgl {
    fn locate_latest(node: Sym) -> Sym;
    fn locate_next(node: Sym) -> Sym;
    fn locate_prev(node: Sym) -> Sym;
    fn set_latest(node: &mut Sym);
    fn set_next(node: &mut Sym);
    fn set_prev(node: &mut Sym);
}

impl<S: SingletonGetter> VersionCtlSgl for S
where
    S::RetTy: Tx + VersionCtl + 'static,
{
    fn locate_latest(node: Sym) -> Sym {
        Self::sgl().locate_latest(node)
    }
    fn locate_next(node: Sym) -> Sym {
        Self::sgl().locate_next(node)
    }
    fn locate_prev(node: Sym) -> Sym {
        Self::sgl().locate_prev(node)
    }
    fn set_latest(node: &mut Sym) {
        Self::sgl().set_latest(node)
    }
    fn set_next(node: &mut Sym) {
        Self::sgl().set_next(node)
    }
    fn set_prev(node: &mut Sym) {
        Self::sgl().set_prev(node)
    }
}

/// this trait should not be implemented for Node because they have many variants which is recognized as different types by compiler
pub trait UpdateCounter<T: EgglogTy> {
    fn inc_counter(&mut self, counter: &mut TyCounter<T>) -> Sym<T>;
}

impl<T> Sym<T> {
    pub fn erase(&self) -> Sym<()> {
        // safety note: type erasure
        unsafe { *&*(self as *const Sym<T> as *const Sym) }
    }
    pub fn erase_ref(&self) -> &Sym<()> {
        // safety note: type erasure
        unsafe { &*(self as *const Sym<T> as *const Sym) }
    }
    pub fn erase_mut(&mut self) -> &mut Sym<()> {
        // safety note: type erasure
        unsafe { &mut *(self as *mut Sym<T> as *mut Sym) }
    }
}
impl Sym {
    pub fn typed<T: EgglogTy>(self) -> Sym<T> {
        unsafe { *(&self as *const Sym as *const Sym<T>) }
    }
}

/// trait of basic functions to interact with egglog
pub trait ToEgglog {
    fn to_egglog_string(&self) -> Option<String>;
    fn to_egglog(&self) -> EgglogAction;
    fn native_egglog(
        &self,
        ctx: &RuleCtx,
        sym_to_value_map: &DashMap<Sym, egglog::Value>,
    ) -> egglog::Value;
}

/// version control triat
/// which should be implemented by Node
pub trait LocateVersion {
    fn locate_latest(&mut self);
    fn locate_next(&mut self);
    fn locate_prev(&mut self);
}
/// trait of node behavior
pub trait EgglogNode: ToEgglog + Any + EValue + Send + Sync {
    fn succs_mut(&mut self) -> Vec<&mut Sym>;
    fn succs(&self) -> Vec<Sym>;
    /// set new sym and return the new sym
    fn roll_sym(&mut self) -> Sym;
    // return current sym
    fn cur_sym(&self) -> Sym;
    fn cur_sym_mut(&mut self) -> &mut Sym;

    fn clone_dyn(&self) -> Box<dyn EgglogNode>;

    fn ty_name(&self) -> &'static str;
    fn variant_name(&self) -> Option<&'static str>;
    fn ty_name_lower(&self) -> &'static str;
    fn basic_field_names(&self) -> &[&'static str];
    fn basic_field_types(&self) -> &[&'static str];
    fn complex_field_names(&self) -> &[&'static str];
    fn complex_field_types(&self) -> &[&'static str];
    fn precedence(&self) -> u16;

    #[track_caller]
    fn to_term(
        &self,
        term_dag: &mut TermDag,
        sym2term: &mut HashMap<Sym, TermId>,
        sym2ph_name: &HashMap<Sym, &'static str>,
    ) -> TermId;

    #[track_caller]
    fn add_table_fact(&self, query_builder: &mut FactsBuilder);
}
pub trait VarsCollector {
    /// 1. if self is a typed placeholder [`TyPH::VarPH`], collect itself and its basic vars
    /// 2. if self is a typed placeholder [`TyPH::PH`], only collect itself
    /// 3. if self is a [`PatVars`] collect recursively
    fn collect_vars(&self, vars: &mut Vec<(VarName, SortName)>);
}
impl<T: VarsCollector, M> VarsCollector for (T, M) {
    fn collect_vars(&self, vars: &mut Vec<(VarName, SortName)>) {
        self.0.collect_vars(vars);
    }
}

pub trait BindingNames {
    fn collect_binding_names(&self, names: &mut Vec<VarName>);

    fn binding_names(&self) -> Vec<VarName> {
        let mut names = Vec::new();
        self.collect_binding_names(&mut names);
        names
    }
}

impl<T: BindingNames, M> BindingNames for (T, M) {
    fn collect_binding_names(&self, names: &mut Vec<VarName>) {
        self.0.collect_binding_names(names);
    }
}

pub trait EgglogEnumVariantTy: Clone + 'static + Send + Sync {
    const TY_NAME: &'static str;
    /// T represent the type call that call this type
    /// This is useful when we want to specify default for a type
    type ValuedWithDefault<T>: FromPlainValues + FromIndexedValues;
    const DISPLAY_TEMPLATE: Option<&'static str>;
    const TYPST_TEMPLATE: Option<&'static str>;
    const PRECEDENCE: u16;
    /// fields names of valued variant struct
    const BASIC_FIELD_NAMES: &[&'static str];
    const COMPLEX_FIELD_NAMES: &[&'static str];
    const BASIC_FIELD_TYPES: &[&'static str];
    const COMPLEX_FIELD_TYPES: &[&'static str];
}
/// instance of specified [`EgglogTy`] & its VariantTy
#[derive(Debug, Clone)]
pub struct Node<T, R, I, S>
where
    T: EgglogTy,
    R: NodeOwnerSgl,
    I: NodeInner,
    I::Discriminant: Clone + fmt::Debug,
    S: EgglogEnumVariantTy,
{
    // PH => PlaceHolder, Ty => normal node
    pub ty: TyPH<I>,
    pub sgl_specific: R::OwnerSpecDataInNode<T, S>,
    pub span: Option<&'static Location<'static>>,
    pub sym: Sym<T>,
    /// Rule closure requires send and sync. Make them happy.
    pub _p: PhantomData<SendSyncWrap<R>>,
    pub _s: PhantomData<SendSyncWrap<S>>,
}
pub struct SendSyncWrap<T> {
    _p: PhantomData<T>,
}
unsafe impl<T> Send for SendSyncWrap<T> {}
unsafe impl<T> Sync for SendSyncWrap<T> {}

/// allow type erasure on S
impl<T, R, I, S> AsRef<Node<T, R, I, ()>> for Node<T, R, I, S>
where
    T: EgglogTy,
    R: NodeOwnerSgl,
    I: NodeInner,
    I::Discriminant: Clone + fmt::Debug,
    S: EgglogEnumVariantTy,
{
    fn as_ref(&self) -> &Node<T, R, I, ()> {
        // Safety notes:
        // 1. Node's memory layout is unaffected by PhantomData
        // 2. We're only changing the S type parameter from a concrete type to unit type (),
        //    which doesn't affect the actual data
        unsafe { &*(self as *const Node<T, R, I, S> as *const Node<T, R, I, ()>) }
    }
}

#[derive(PartialEq, Eq, Hash, Debug)]
pub struct Sym<T = ()> {
    pub inner: GlobalSymbol,
    pub p: PhantomData<T>,
}

impl<T> Sym<T> {
    pub fn new(global_sym: GlobalSymbol) -> Self {
        Self {
            inner: global_sym,
            p: PhantomData,
        }
    }
    pub fn as_str(&self) -> &'static str {
        self.inner.as_str()
    }
    pub fn to_string(&self) -> String {
        self.inner.as_str().to_string()
    }
}
impl<T> Copy for Sym<T> {}
impl<T> Clone for Sym<T> {
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
            p: PhantomData,
        }
    }
}

/// trait of egglog node inner
pub trait NodeInner: IntoDiscriminant {
    fn succs_mut(&mut self) -> Vec<&mut Sym>;
    fn succs(&self) -> Vec<Sym>;
}
impl<T> std::fmt::Display for Sym<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.inner.as_str())
    }
}
impl<T> From<Sym<T>> for &str {
    fn from(value: Sym<T>) -> Self {
        value.inner.as_str()
    }
}
impl<T: EgglogTy> From<Syms<T>> for Syms {
    fn from(value: Syms<T>) -> Self {
        value.into_iter().map(|s| s.erase()).collect()
    }
}
/// count the number of nodes of specific EgglogTy for specific binding Tx
pub struct TyCounter<T: EgglogTy> {
    counter: AtomicU32,
    t: PhantomData<T>,
}
impl<T: EgglogTy> TyCounter<T> {
    pub const fn new() -> Self {
        TyCounter {
            counter: AtomicU32::new(0),
            t: PhantomData,
        }
    }
    // get next symbol of specified type T
    pub fn next_sym(&self) -> Sym<T> {
        Sym {
            inner: format!("{}{}", T::TY_NAME_LOWER, self.inc()).into(),
            p: PhantomData::<T>,
        }
    }
    pub fn get_counter(&self) -> u32 {
        self.counter.load(std::sync::atomic::Ordering::Acquire)
    }
    /// counter increment atomically
    pub fn inc(&self) -> u32 {
        self.counter
            .fetch_add(1, std::sync::atomic::Ordering::AcqRel)
    }
}

impl EgglogEnumVariantTy for () {
    const TY_NAME: &'static str = "Unknown Func";
    type ValuedWithDefault<T> = Value<T>;
    const DISPLAY_TEMPLATE: Option<&'static str> = None;
    const TYPST_TEMPLATE: Option<&'static str> = None;
    const PRECEDENCE: u16 = u16::MAX;
    const BASIC_FIELD_NAMES: &[&'static str] = &[];
    const BASIC_FIELD_TYPES: &[&'static str] = &[];
    const COMPLEX_FIELD_NAMES: &[&'static str] = &[];
    const COMPLEX_FIELD_TYPES: &[&'static str] = &[];
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RenderedTemplateField<'a> {
    pub text: Cow<'a, str>,
    pub precedence: u16,
}

impl<'a> RenderedTemplateField<'a> {
    pub fn new(text: impl Into<Cow<'a, str>>, precedence: u16) -> Self {
        Self {
            text: text.into(),
            precedence,
        }
    }

    pub fn atom(text: impl Into<Cow<'a, str>>) -> Self {
        Self::new(text, u16::MAX)
    }
}

pub fn render_template_with_precedence(
    template: &str,
    parent_precedence: u16,
    fields: &[(&str, RenderedTemplateField<'_>)],
) -> String {
    let chars = template.chars().collect::<Vec<_>>();
    let mut rendered = String::new();
    let mut idx = 0usize;

    while idx < chars.len() {
        match chars[idx] {
            '{' => {
                if chars.get(idx + 1) == Some(&'{') {
                    rendered.push('{');
                    idx += 2;
                    continue;
                }

                let start = idx + 1;
                let mut end = start;
                while end < chars.len() && chars[end] != '}' {
                    end += 1;
                }
                let placeholder = chars[start..end].iter().collect::<String>();
                let field = fields
                    .iter()
                    .find(|(name, _)| *name == placeholder)
                    .unwrap_or_else(|| panic!("missing render field `{placeholder}`"));

                if field.1.precedence < parent_precedence {
                    rendered.push('(');
                    rendered.push_str(field.1.text.as_ref());
                    rendered.push(')');
                } else {
                    rendered.push_str(field.1.text.as_ref());
                }
                idx = end + 1;
            }
            '}' => {
                if chars.get(idx + 1) == Some(&'}') {
                    rendered.push('}');
                    idx += 2;
                } else {
                    rendered.push('}');
                    idx += 1;
                }
            }
            ch => {
                rendered.push(ch);
                idx += 1;
            }
        }
    }

    rendered
}

pub fn render_variant_typst<V: EgglogEnumVariantTy>(
    fields: &[(&str, RenderedTemplateField<'_>)],
) -> Option<String> {
    V::TYPST_TEMPLATE
        .map(|template| render_template_with_precedence(template, V::PRECEDENCE, fields))
}

pub fn render_variant_display<V: EgglogEnumVariantTy>(
    fields: &[(&str, RenderedTemplateField<'_>)],
) -> Option<String> {
    V::DISPLAY_TEMPLATE
        .map(|template| render_template_with_precedence(template, V::PRECEDENCE, fields))
}

fn dsl_variant_decl(variant_name: &str) -> Option<&'static DslVariantDecl> {
    inventory::iter::<DslVariantDecl>
        .into_iter()
        .find(|decl| decl.variant_name == variant_name)
}

fn literal_to_typst(literal: &Literal) -> String {
    match literal {
        Literal::Int(n) => n.to_string(),
        Literal::Float(n) => n.to_string(),
        Literal::String(s) => {
            if s.chars().count() == 1 {
                s.clone()
            } else {
                format!("\"{s}\"")
            }
        }
        Literal::Bool(b) => b.to_string(),
        Literal::Unit => "()".to_string(),
    }
}

fn render_term_to_typst(
    term_id: TermId,
    term_dag: &TermDag,
) -> Result<RenderedTemplateField<'static>, egglog::Error> {
    match term_dag.get(term_id) {
        Term::Lit(literal) => Ok(RenderedTemplateField::atom(literal_to_typst(literal))),
        Term::Var(name) => Ok(RenderedTemplateField::atom(name.clone())),
        Term::App(name, children) => {
            let decl = dsl_variant_decl(name.as_str()).ok_or_else(|| {
                egglog::Error::BackendError(format!(
                    "missing DslVariantDecl metadata for extracted variant `{name}`"
                ))
            })?;
            let typst_template = decl.typst_template.ok_or_else(|| {
                egglog::Error::BackendError(format!(
                    "variant `{name}` does not provide a #[eggplant::typst(...)] template"
                ))
            })?;
            if decl.fields.len() != children.len() {
                return Err(egglog::Error::BackendError(format!(
                    "variant `{name}` field count does not match extracted term arity"
                )));
            }

            let mut rendered_fields = Vec::with_capacity(children.len());
            for (field, child_term) in decl.fields.iter().zip(children.iter()) {
                rendered_fields.push((field.name, render_term_to_typst(*child_term, term_dag)?));
            }

            Ok(RenderedTemplateField::new(
                render_template_with_precedence(typst_template, decl.precedence, &rendered_fields),
                decl.precedence,
            ))
        }
    }
}

fn compile_typst_math_to_svg(typst_math: &str, output_path: &Path) -> Result<(), egglog::Error> {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map_err(|err| egglog::Error::BackendError(format!("system clock error: {err}")))?
        .as_nanos();
    let input_path = std::env::temp_dir().join(format!("eggplant_typst_render_{unique}.typ"));
    let document = format!(
        "#set page(width: auto, height: auto, margin: 8pt)\n#set text(size: 14pt)\n${}$\n",
        typst_math
    );
    fs::write(&input_path, document).map_err(|err| {
        egglog::Error::BackendError(format!(
            "failed to write temporary typst source `{}`: {err}",
            input_path.display()
        ))
    })?;

    let output = ProcessCommand::new("typst")
        .arg("compile")
        .arg(&input_path)
        .arg(output_path)
        .output()
        .map_err(|err| {
            egglog::Error::BackendError(format!("failed to invoke `typst compile`: {err}"))
        })?;

    let _ = fs::remove_file(&input_path);

    if !output.status.success() {
        return Err(egglog::Error::BackendError(format!(
            "`typst compile` failed: {}",
            String::from_utf8_lossy(&output.stderr)
        )));
    }

    Ok(())
}

#[derive(DerefMut, Deref)]
pub struct WorkAreaNode {
    pub next: Option<Sym>,
    pub prev: Option<Sym>,
    pub preds: Syms,
    #[deref]
    #[deref_mut]
    pub egglog: Box<dyn EgglogNode>,
    pub pulled_by: Option<egglog::Value>,
}

impl Clone for WorkAreaNode {
    fn clone(&self) -> Self {
        Self {
            next: self.next.clone(),
            preds: self.preds.clone(),
            egglog: self.egglog.clone_dyn(),
            prev: None,
            pulled_by: self.pulled_by,
        }
    }
}
impl fmt::Debug for WorkAreaNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} {} | {} | pulled_by {:?}",
            self.variant_name().unwrap_or(self.ty_name()),
            self.cur_sym(),
            self.to_egglog_string().unwrap_or(
                self.egglog
                    .basic_field_types()
                    .iter()
                    .map(|s| s.to_string())
                    .collect::<Vec<_>>()
                    .join(" ")
            ),
            self.pulled_by
        )
    }
}
impl WorkAreaNode {
    pub fn new(node: Box<dyn EgglogNode>) -> Self {
        Self {
            preds: Syms::default(),
            egglog: node,
            next: None,
            prev: None,
            pulled_by: None,
        }
    }
    pub fn new_pulled(node: Box<dyn EgglogNode>, pulled_by: egglog::Value) -> Self {
        Self {
            preds: Syms::default(),
            egglog: node,
            next: None,
            prev: None,
            pulled_by: Some(pulled_by),
        }
    }
    pub fn succs_mut(&mut self) -> impl Iterator<Item = &mut Sym> {
        self.egglog.succs_mut().into_iter()
    }
    pub fn preds_mut(&mut self) -> impl Iterator<Item = &mut Sym> {
        self.preds.iter_mut()
    }
    pub fn preds(&self) -> impl Iterator<Item = &Sym> {
        self.preds.iter()
    }
}

impl Borrow<GlobalSymbol> for Sym {
    fn borrow(&self) -> &GlobalSymbol {
        &self.inner
    }
}

#[derive(Clone, Deref, DerefMut, IntoIterator, Debug, Default)]
pub struct Syms<T = ()> {
    #[into_iterator(owned, ref, ref_mut)]
    inner: SmallVec<[Sym<T>; 4]>,
}

impl From<SmallVec<[Sym; 4]>> for Syms {
    fn from(value: SmallVec<[Sym; 4]>) -> Self {
        Syms { inner: value }
    }
}

impl<S> FromIterator<Sym<S>> for Syms<S> {
    fn from_iter<T: IntoIterator<Item = Sym<S>>>(iter: T) -> Self {
        Syms {
            inner: iter.into_iter().collect(),
        }
    }
}
impl<T> Syms<T> {
    pub fn new() -> Self {
        Syms {
            inner: SmallVec::new(),
        }
    }
}
impl From<Vec<Sym>> for Syms {
    fn from(value: Vec<Sym>) -> Self {
        value.into_iter().collect()
    }
}

/// global commit
/// This trait should be implemented for Tx singleton
/// usage:
/// ```text
/// let last_version_node = node.clone();
/// Tx::commit(&self, node);
/// ```
pub trait TxCommit {
    #[track_caller]
    fn on_stage<T: EgglogNode + ?Sized>(&self, node: &T);
    fn on_commit_op_hook<T: EgglogNode>(&self, node: &T, _: Option<Box<dyn RuleCtxHook>>);
}

pub trait TxCommitSgl {
    #[track_caller]
    fn on_commit<T: EgglogNode>(node: &T);
    #[track_caller]
    fn on_commit_with_hook<T: EgglogNode>(node: &T, hook: Box<dyn RuleCtxHook>);
    #[track_caller]
    fn on_stage<T: EgglogNode>(node: &T);
}

impl<Ret, S> TxCommitSgl for S
where
    Ret: Tx + VersionCtl + TxCommit,
    S: SingletonGetter<RetTy = Ret>,
{
    fn on_commit_with_hook<T: EgglogNode>(node: &T, hook: Box<dyn RuleCtxHook>) {
        S::sgl().on_commit_op_hook(node, Some(hook));
    }
    fn on_stage<T: EgglogNode>(node: &T) {
        S::sgl().on_stage(node);
    }

    fn on_commit<T: EgglogNode>(node: &T) {
        S::sgl().on_commit_op_hook(node, None);
    }
}

/// single node commit
/// This trait should be implemented for Node
/// usage:
/// ```text
/// let last_version_node = node.clone();
/// node.set_a()
///     .set_b()
///     .commit();
/// ```
pub trait Commit {
    #[track_caller]
    fn commit(&self);
    #[track_caller]
    fn commit_with_hook(&self, hook: Box<dyn RuleCtxHook>);
    #[track_caller]
    fn stage(&self);
}

/// In Egglog there are 2 ways to interact with egraph
/// 1. String of egglog code
/// 2. Vector of Egglog Command Struct
/// Use this Interpreter trait to concile them
///
/// Also there are
pub trait Interpreter {
    type Interpreted;
    fn interpret(interpreted: Self::Interpreted);
}

// pub trait EgglogNodeMarker{ }

impl<T: EgglogNode> From<T> for WorkAreaNode {
    fn from(value: T) -> Self {
        WorkAreaNode::new(value.clone_dyn())
    }
}

pub trait ToVar {
    fn to_var(&self) -> GenericExpr<&'static str, &'static str>;
}

impl<T> ToVar for Sym<T> {
    fn to_var(&self) -> GenericExpr<&'static str, &'static str> {
        GenericExpr::Var(span!(), self.inner.into())
    }
}
impl<T> ToVar for T
where
    Literal: FromBase<T>,
    T: Clone,
{
    fn to_var(&self) -> GenericExpr<&'static str, &'static str> {
        GenericExpr::Lit(span!(), Literal::from_base(&self))
    }
}

pub trait ToOwnedStr {
    fn to_owned_str(&self) -> GenericExpr<String, String>;
}

impl ToOwnedStr for GenericExpr<&'static str, &'static str> {
    fn to_owned_str(&self) -> GenericExpr<String, String> {
        match self {
            GenericExpr::Lit(span, literal) => GenericExpr::Lit(span.clone(), literal.clone()),
            GenericExpr::Var(span, v) => GenericExpr::Var(span.clone(), v.to_string()),
            GenericExpr::Call(span, h, generic_exprs) => GenericExpr::Call(
                span.clone(),
                h.to_string(),
                generic_exprs.iter().map(|x| x.to_owned_str()).collect(),
            ),
        }
    }
}

pub trait ToSpan {
    fn to_span(&self) -> Span;
}

impl ToSpan for &'static Location<'static> {
    fn to_span(&self) -> Span {
        Span::Rust(Arc::new(RustSpan {
            file: self.file(),
            line: self.line(),
            column: self.column(),
        }))
    }
}

impl ToSpan for Option<&'static Location<'static>> {
    fn to_span(&self) -> Span {
        match self {
            Some(value) => value.to_span(),
            None => Span::Panic,
        }
    }
}

pub trait FromTerm {
    fn term_to_node(
        term: TermId,
        dag: &TermDag,
        term2sym: &mut HashMap<TermId, Sym>,
    ) -> Box<dyn EgglogNode>;
}

/// used for type erased marker
impl SingletonGetter for () {
    type RetTy = TxRxVT;
    fn sgl() -> &'static Self::RetTy {
        panic!("illegal singleton getter, you can't get singleton of ()");
    }
}

pub enum TopoDirection {
    Up,
    Down,
}

impl std::fmt::Debug for Box<dyn EgglogNode> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{},{}",
            self.cur_sym(),
            self.to_egglog_string().unwrap_or(String::new())
        )
    }
}

// place holder for EgglogNode
#[derive(Deref, DerefMut)]
pub struct PH<N> {
    pub node: N,
}

impl<N: EgglogNode> PH<N> {
    pub fn new(node: N) -> PH<N> {
        Self { node }
    }
}
impl<T> Default for Sym<T> {
    fn default() -> Self {
        Self {
            inner: "".into(),
            p: Default::default(),
        }
    }
}

pub trait SymOrValueConstructor {
    type Constructor<T>;
}

impl SymOrValueConstructor for Sym {
    type Constructor<T> = Sym<T>;
}
// impl<T:EgglogTy> SymOrValueConstructor for Value<T> {
//     type Constructor<Ty:EgglogTy> = Value<Ty>;
// }

/// a wrapper for EgglogBackend Value with type info
/// It's useful for Node Type because in rust_rule's action part you should specify
/// value for Node rather than Sym
pub struct Value<T> {
    pub val: egglog::Value,
    p: PhantomData<T>,
}
impl<T> Value<T> {
    pub fn new(val: egglog::Value) -> Value<T> {
        Value {
            val,
            p: PhantomData,
        }
    }
    pub fn new_from_iter(val: &mut impl Iterator<Item = egglog::Value>) -> Value<T> {
        Value {
            val: val.next().unwrap(),
            p: PhantomData,
        }
    }
    pub fn erase(&self) -> egglog::Value {
        self.val
    }
}
impl<T: EgglogTy> fmt::Debug for Value<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}{}{:?}",
            T::TY_NAME,
            T::EnumVariantMarker::TY_NAME,
            self.val
        )
    }
}

/// a pattern may extract values in EGraph, for example
/// if you record pattern (fib x) then x will be extracted
/// we use [`PatVars`] trait to mark such patterns
pub trait PatVars<PR: PatRecSgl>: ToStrArcSort + BindingNames {
    type Valued: FromPlainValuesMetas<PR> + FromIndexedValuesMetas<PR> + DecodeWithPlanMetas<PR>;
    fn metas_iter(&self) -> impl Iterator<Item = PR::MetaTy>;

    fn build_decode_plan(
        &self,
        binding_var_slots: &HashMap<Arc<str>, usize>,
    ) -> <Self::Valued as DecodeWithPlanMetas<PR>>::DecodePlan {
        let binding_slots = self
            .binding_names()
            .into_iter()
            .map(|name| {
                *binding_var_slots
                    .get(name.as_str())
                    .unwrap_or_else(|| panic!("missing binding layout var {}", name))
            })
            .collect::<Vec<_>>();
        let mut value_idx = 0;
        <Self::Valued as DecodeWithPlanMetas<PR>>::build_decode_plan(&binding_slots, &mut value_idx)
    }

    fn decode_with_plan(
        values: &[egglog::Value],
        metas: &[PR::MetaTy],
        plan: &<Self::Valued as DecodeWithPlanMetas<PR>>::DecodePlan,
    ) -> Self::Valued {
        let mut meta_idx = 0;
        <Self::Valued as DecodeWithPlanMetas<PR>>::decode_with_plan(
            values,
            metas,
            &mut meta_idx,
            plan,
        )
    }
}
impl<T, PV: ToStrArcSort> ToStrArcSort for (PV, T) {
    fn to_str_arcsort(&self, egraph: &EGraph) -> Vec<(VarName, ArcSort)> {
        PV::to_str_arcsort(&self.0, egraph)
    }
}
impl<PR: PatRecSgl, PV: PatVars<PR>> PatVars<PR> for (PV, PR::MetaTy) {
    type Valued = PV::Valued;
    fn metas_iter(&self) -> impl Iterator<Item = PR::MetaTy> {
        self.0.metas_iter().chain(std::iter::once(self.1.clone()))
    }
}

/// a pattern should be transformed into [(str,Arcsort)] when registering rules
pub trait ToStrArcSort {
    fn to_str_arcsort(&self, egraph: &EGraph) -> Vec<(VarName, ArcSort)>;
}

/// This trait is stronger then FromPlainValuesMetas so auto impl that if this is implmented
pub trait FromPlainValues {
    fn from_plain_values(values: &mut impl Iterator<Item = egglog::Value>) -> Self;
}

pub trait FromIndexedValues {
    fn from_indexed_values(values: &[egglog::Value], value_idx: &mut usize) -> Self;
}

pub trait FromIndexedValuesMetas<PR: PatRecSgl> {
    fn from_indexed_values_metas(
        values: &[egglog::Value],
        value_idx: &mut usize,
        metas: &[PR::MetaTy],
        meta_idx: &mut usize,
    ) -> Self;
}

pub trait DecodeWithPlanMetas<PR: PatRecSgl>: Sized {
    type DecodePlan: Clone + Send + Sync + 'static;

    fn build_decode_plan(binding_slots: &[usize], value_idx: &mut usize) -> Self::DecodePlan;

    fn decode_with_plan(
        values: &[egglog::Value],
        metas: &[PR::MetaTy],
        meta_idx: &mut usize,
        plan: &Self::DecodePlan,
    ) -> Self;
}

impl<T: FromPlainValues, PR: PatRecSgl> FromPlainValuesMetas<PR> for (T, PR::MetaTy) {
    fn from_plain_values_metas(
        values: &mut impl Iterator<Item = egglog::Value>,
        metas: &mut impl Iterator<Item = PR::MetaTy>,
    ) -> Self {
        (
            <T as FromPlainValues>::from_plain_values(values),
            metas.next().unwrap(),
        )
    }
}
impl<T: FromPlainValues, PR: PatRecSgl> FromPlainValuesMetas<PR> for T {
    fn from_plain_values_metas(
        values: &mut impl Iterator<Item = egglog::Value>,
        _metas: &mut impl Iterator<Item = PR::MetaTy>,
    ) -> Self {
        <T as FromPlainValues>::from_plain_values(values)
    }
}

impl<T: FromIndexedValues, PR: PatRecSgl> FromIndexedValuesMetas<PR> for (T, PR::MetaTy) {
    fn from_indexed_values_metas(
        values: &[egglog::Value],
        value_idx: &mut usize,
        metas: &[PR::MetaTy],
        meta_idx: &mut usize,
    ) -> Self {
        let value = <T as FromIndexedValues>::from_indexed_values(values, value_idx);
        let meta = metas.get(*meta_idx).cloned().unwrap_or_default();
        *meta_idx += 1;
        (value, meta)
    }
}

impl<T: DecodeWithPlanMetas<PR>, PR: PatRecSgl> DecodeWithPlanMetas<PR> for (T, PR::MetaTy) {
    type DecodePlan = T::DecodePlan;

    fn build_decode_plan(binding_slots: &[usize], value_idx: &mut usize) -> Self::DecodePlan {
        T::build_decode_plan(binding_slots, value_idx)
    }

    fn decode_with_plan(
        values: &[egglog::Value],
        metas: &[PR::MetaTy],
        meta_idx: &mut usize,
        plan: &Self::DecodePlan,
    ) -> Self {
        let value = T::decode_with_plan(values, metas, meta_idx, plan);
        let meta = metas.get(*meta_idx).cloned().unwrap_or_default();
        *meta_idx += 1;
        (value, meta)
    }
}

impl<T: FromIndexedValues, PR: PatRecSgl> FromIndexedValuesMetas<PR> for T {
    fn from_indexed_values_metas(
        values: &[egglog::Value],
        value_idx: &mut usize,
        _metas: &[PR::MetaTy],
        _meta_idx: &mut usize,
    ) -> Self {
        <T as FromIndexedValues>::from_indexed_values(values, value_idx)
    }
}

impl<T, PR: PatRecSgl> DecodeWithPlanMetas<PR> for Value<T> {
    type DecodePlan = usize;

    fn build_decode_plan(binding_slots: &[usize], value_idx: &mut usize) -> Self::DecodePlan {
        let slot = *binding_slots
            .get(*value_idx)
            .unwrap_or_else(|| panic!("missing callback slot for binding #{}", value_idx));
        *value_idx += 1;
        slot
    }

    fn decode_with_plan(
        values: &[egglog::Value],
        _metas: &[PR::MetaTy],
        _meta_idx: &mut usize,
        plan: &Self::DecodePlan,
    ) -> Self {
        Self::new(values[*plan])
    }
}

pub trait FromPlainValuesMetas<PR: PatRecSgl> {
    fn from_plain_values_metas(
        values: &mut impl Iterator<Item = egglog::Value>,
        metas: &mut impl Iterator<Item = PR::MetaTy>,
    ) -> Self;
}

/// Insertable and RetypeValue are quite different, Insertable is used in Union or table insert
/// while RetypeValueonly used when you want operational structure
pub trait Insertable<T>: Clone {
    type MetaTy;
    fn to_value(&self, ctx: &RuleCtx) -> Value<T>;
    fn meta(&self) -> Option<Self::MetaTy>;
}
impl<I: Insertable<T>, T, M: Meta + 'static> Insertable<T> for (I, M) {
    type MetaTy = M;
    fn to_value(&self, ctx: &RuleCtx) -> Value<T> {
        self.0.to_value(ctx)
    }
    fn meta(&self) -> Option<Self::MetaTy> {
        Some(self.1.clone())
    }
}
impl<I: Insertable<T>, T, M: Meta + 'static> Insertable<T> for &(I, M) {
    type MetaTy = M;
    fn to_value(&self, ctx: &RuleCtx) -> Value<T> {
        self.0.to_value(ctx)
    }
    fn meta(&self) -> Option<Self::MetaTy> {
        Some(self.1.clone())
    }
}
pub trait RetypeValue {
    type Target;
    fn retype_value(val: egglog::Value) -> Value<Self::Target>;
}

impl<D: RetypeValue> RetypeValue for Value<D> {
    type Target = D::Target;
    fn retype_value(val: egglog::Value) -> Value<Self::Target> {
        Value::new(val)
    }
}
impl<T: BoxedValue> RetypeValue for T {
    type Target = T;
    fn retype_value(val: egglog::Value) -> Value<Self::Target> {
        Value::new(val)
    }
}

impl<T> Clone for Value<T> {
    fn clone(&self) -> Self {
        Self {
            val: self.val.clone(),
            p: PhantomData,
        }
    }
}
impl<T: EgglogTy> Copy for Value<T> {}

/// if one struct BoxUnBox that means it can be converted to a boxed value in database
/// this is an essential condition to be insert into egglog_backend
/// any struct implements this trait inferred to be [`Insertable`]
pub trait BoxedBase: BoxedValue {
    type Boxed: BaseValue;
    fn unbox(boxed: Self::Boxed, ctx: &RuleCtx) -> Self;
    fn box_it(self, ctx: &RuleCtx) -> Self::Boxed;
}
pub trait BoxedContainer: BoxedValue {
    type Boxed: ContainerValue;
    const CONSTRUCTOR_STR: &'static str;
    const TY_STR: &'static str;
    fn unbox(boxed: Self::Boxed, ctx: &RuleCtx) -> Self;
    fn box_it(self, ctx: &RuleCtx) -> Self::Boxed;
}

pub trait SingleFieldVariant {}

impl<T0, B: BoxedBase<Boxed = T0> + EgglogTy + Clone> Insertable<B> for B {
    type MetaTy = ();
    fn to_value(&self, ctx: &RuleCtx) -> Value<Self> {
        ctx.intern_base(self.clone())
    }
    fn meta(&self) -> Option<Self::MetaTy> {
        None
    }
}

impl<T, Elem> Insertable<T> for super::SetContainer<Elem>
where
    T: super::type_reg::EgglogContainerTy<EleTy = Elem>,
    Elem: EgglogTy,
{
    type MetaTy = ();
    fn to_value(&self, ctx: &RuleCtx) -> Value<T> {
        ctx.intern_container::<T, super::SetContainer<Elem>>(self.clone())
    }
    fn meta(&self) -> Option<Self::MetaTy> {
        None
    }
}

impl<T, Elem> Insertable<T> for super::VecContainer<Elem>
where
    T: super::type_reg::EgglogContainerTy<EleTy = Elem>,
    Elem: EgglogTy,
{
    type MetaTy = ();
    fn to_value(&self, ctx: &RuleCtx) -> Value<T> {
        ctx.intern_container::<T, super::VecContainer<Elem>>(self.clone())
    }
    fn meta(&self) -> Option<Self::MetaTy> {
        None
    }
}

pub trait BoxedValue {
    type Output<'a>;
    fn devalue<'b>(rule_ctx: &'b RuleCtx, value: egglog::Value) -> Self::Output<'b>;
}

#[derive(EnumDiscriminants, EnumIs, Debug, Clone)]
pub enum TyPH<T: strum::IntoDiscriminant>
where
    T::Discriminant: Clone + fmt::Debug,
{
    /// not be leaf node in pattern
    Ty(T),
    /// to discriminate whether this leaf node's basic fields should be recorded as action args
    VarPH(T::Discriminant, Vec<Sym>),
    /// this leaf node's basic fields should not be recorded as action args
    PH,
}

impl<T: strum::IntoDiscriminant> TyPH<T>
where
    T::Discriminant: Clone + fmt::Debug,
{
    pub fn unwrap_ref(&self) -> &T {
        if let TyPH::Ty(ty) = self {
            ty
        } else {
            panic!()
        }
    }
    pub fn ty_ref(&self) -> Option<&T> {
        if let TyPH::Ty(ty) = self {
            Some(ty)
        } else {
            None
        }
    }
    pub fn unwrap_mut(&mut self) -> &mut T {
        if let TyPH::Ty(ty) = self {
            ty
        } else {
            panic!()
        }
    }
    pub fn ty_mut(&mut self) -> Option<&mut T> {
        if let TyPH::Ty(ty) = self {
            Some(ty)
        } else {
            None
        }
    }
    pub fn map_ty_ref_or_else<'a, R>(
        &'a self,
        ph_f: impl FnOnce() -> R,
        var_ph_f: impl FnOnce(&'a T::Discriminant, &'a Vec<Sym>) -> R,
        f: impl FnOnce(&'a T) -> R,
    ) -> R {
        match self {
            Self::Ty(ty) => f(ty),
            Self::PH => ph_f(),
            Self::VarPH(dis, succs) => var_ph_f(dis, succs),
        }
    }
    pub fn map_ty_mut_or_else<'a, R>(
        &'a mut self,
        ph_f: impl FnOnce() -> R,
        var_ph_f: impl FnOnce(&'a mut T::Discriminant, &'a mut Vec<Sym>) -> R,
        f: impl FnOnce(&'a mut T) -> R,
    ) -> R {
        match self {
            Self::Ty(ty) => f(ty),
            Self::PH => ph_f(),
            Self::VarPH(dis, succs) => var_ph_f(dis, succs),
        }
    }
}

pub type SerializedPetGraph = petgraph::Graph<String, String>;
pub trait ToDotSgl {
    fn egraph_to_dot(path: impl AsRef<Path>);
    fn wag_to_dot(path: impl AsRef<Path>);
    fn wag_to_petgraph() -> SerializedPetGraph;
    // fn proof_to_dot(path: impl AsRef<Path>);
    fn table_view();
}
pub trait ToDot {
    fn egraph_to_dot(&self, path: impl AsRef<Path>);
    fn wag_to_dot(&self, path: impl AsRef<Path>);
    fn wag_to_petgraph(&self) -> SerializedPetGraph;
    // fn proof_to_dot(&self, path: impl AsRef<Path>);
    fn table_view(&self);
}
impl<S: SingletonGetter> ToDotSgl for S
where
    S::RetTy: ToDot + 'static,
{
    fn egraph_to_dot(path: impl AsRef<Path>) {
        Self::sgl().egraph_to_dot(path);
    }

    fn wag_to_dot(path: impl AsRef<Path>) {
        Self::sgl().wag_to_dot(path);
    }
    fn wag_to_petgraph() -> SerializedPetGraph {
        Self::sgl().wag_to_petgraph()
    }
    // fn proof_to_dot(path: impl AsRef<Path>) {
    //     Self::sgl().proof_to_dot(path);
    // }
    fn table_view() {
        Self::sgl().table_view();
    }
}

/// a marker trait for those not pattern recorder singleton
/// because currently rust doesn't support `!PatRecSgl` clause
pub trait NonPatRecSgl {
    fn egraph() -> Arc<Mutex<EGraph>>;
}
impl NonPatRecSgl for () {
    fn egraph() -> Arc<Mutex<EGraph>> {
        panic!()
    }
}

#[derive(Debug, Clone, Default)]
pub struct RustsatExtractConfig {}

impl RustsatExtractConfig {
    pub fn new() -> Self {
        Self::default()
    }
}

#[derive(Debug, Clone)]
pub struct EBoostLayeredConfig {
    pub bound: f32,
    pub exact: RustsatExtractConfig,
}

impl Default for EBoostLayeredConfig {
    fn default() -> Self {
        Self {
            bound: 1.25,
            exact: RustsatExtractConfig::default(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ExtractBackend<CM = TreeAdditiveCostModel> {
    CostModel(CM),
    EBoostHeuristic(EBoostExtractConfig),
    #[cfg(feature = "rustsat-extract")]
    EBoostLayered(EBoostLayeredConfig),
    #[cfg(feature = "rustsat-extract")]
    Rustsat(RustsatExtractConfig),
}

impl<CM> ExtractBackend<CM> {
    pub fn cost_model(cost_model: CM) -> Self {
        Self::CostModel(cost_model)
    }

    pub fn eboost_heuristic(config: EBoostExtractConfig) -> Self {
        Self::EBoostHeuristic(config)
    }

    #[cfg(feature = "rustsat-extract")]
    pub fn eboost_layered(config: EBoostLayeredConfig) -> Self {
        Self::EBoostLayered(config)
    }

    #[cfg(feature = "rustsat-extract")]
    pub fn rustsat(config: RustsatExtractConfig) -> Self {
        Self::Rustsat(config)
    }
}

impl Default for ExtractBackend<TreeAdditiveCostModel> {
    fn default() -> Self {
        Self::CostModel(TreeAdditiveCostModel::default())
    }
}

pub fn extract_raw_with_backend<CM: CostModel<DefaultCost> + 'static>(
    egraph: &EGraph,
    sort: &ArcSort,
    value: egglog::Value,
    backend: ExtractBackend<CM>,
) -> Result<(TermDag, TermId, DefaultCost), egglog::Error> {
    match backend {
        ExtractBackend::CostModel(cost_model) => {
            egraph.extract_value_with_cost_model(sort, value, cost_model)
        }
        ExtractBackend::EBoostHeuristic(config) => {
            eboost_extract_value_prototype(egraph, sort, value, config)
        }
        #[cfg(feature = "rustsat-extract")]
        ExtractBackend::EBoostLayered(config) => {
            eboost_layered_extract_value_prototype(egraph, sort, value, config)
        }
        #[cfg(feature = "rustsat-extract")]
        ExtractBackend::Rustsat(config) => {
            rustsat_extract_value_prototype(egraph, sort, value, config)
        }
    }
}

pub trait ExtractSgl: NonPatRecSgl {
    fn extract_value<T: EgglogTy>(
        value: Value<T>,
    ) -> Result<(TermDag, TermId, DefaultCost), egglog::Error> {
        Self::extract_value_with_cost_model(value, TreeAdditiveCostModel::default())
    }

    fn extract_value_with_cost_model<T: EgglogTy, CM: CostModel<DefaultCost> + 'static>(
        value: Value<T>,
        cost_model: CM,
    ) -> Result<(TermDag, TermId, DefaultCost), egglog::Error> {
        let egraph = Self::egraph();
        let egraph = egraph.lock().unwrap();
        let sort = T::get_arc_sort(&egraph);
        egraph.extract_value_with_cost_model(&sort, value.val, cost_model)
    }

    fn extract_value_with_backend<T: EgglogTy, CM: CostModel<DefaultCost> + 'static>(
        value: Value<T>,
        backend: ExtractBackend<CM>,
    ) -> Result<(TermDag, TermId, DefaultCost), egglog::Error> {
        let egraph = Self::egraph();
        let egraph = egraph.lock().unwrap();
        let sort = T::get_arc_sort(&egraph);
        extract_raw_with_backend(&egraph, &sort, value.val, backend)
    }

    fn extract_value_to_string<T: EgglogTy>(
        value: Value<T>,
    ) -> Result<(String, DefaultCost), egglog::Error> {
        Self::extract_value_to_string_with_cost_model(value, TreeAdditiveCostModel::default())
    }

    fn extract_value_to_string_with_cost_model<
        T: EgglogTy,
        CM: CostModel<DefaultCost> + 'static,
    >(
        value: Value<T>,
        cost_model: CM,
    ) -> Result<(String, DefaultCost), egglog::Error> {
        let (termdag, term, cost) = Self::extract_value_with_cost_model(value, cost_model)?;
        Ok((termdag.to_string(term), cost))
    }

    fn extract_value_to_string_with_backend<T: EgglogTy, CM: CostModel<DefaultCost> + 'static>(
        value: Value<T>,
        backend: ExtractBackend<CM>,
    ) -> Result<(String, DefaultCost), egglog::Error> {
        let (termdag, term, cost) = Self::extract_value_with_backend(value, backend)?;
        Ok((termdag.to_string(term), cost))
    }
}
impl<T: NonPatRecSgl> ExtractSgl for T {}

pub trait ExtractNodeSgl: ExtractSgl + TxSgl {
    fn extract_node<N>(node: &N) -> Result<(TermDag, TermId, DefaultCost), egglog::Error>
    where
        N: EgglogNode + EgglogTy + 'static,
    {
        Self::extract_node_with_cost_model(node, TreeAdditiveCostModel::default())
    }

    fn extract_node_with_cost_model<N, CM>(
        node: &N,
        cost_model: CM,
    ) -> Result<(TermDag, TermId, DefaultCost), egglog::Error>
    where
        N: EgglogNode + EgglogTy + 'static,
        CM: CostModel<DefaultCost> + 'static,
    {
        Self::extract_value_with_cost_model(Value::<N>::new(Self::canonical_raw(node)), cost_model)
    }

    fn extract_node_with_backend<N, CM>(
        node: &N,
        backend: ExtractBackend<CM>,
    ) -> Result<(TermDag, TermId, DefaultCost), egglog::Error>
    where
        N: EgglogNode + EgglogTy + 'static,
        CM: CostModel<DefaultCost> + 'static,
    {
        Self::extract_value_with_backend(Value::<N>::new(Self::canonical_raw(node)), backend)
    }

    fn extract_node_to_string<N>(node: &N) -> Result<(String, DefaultCost), egglog::Error>
    where
        N: EgglogNode + EgglogTy + 'static,
    {
        Self::extract_node_to_string_with_cost_model(node, TreeAdditiveCostModel::default())
    }

    fn extract_node_to_string_with_cost_model<N, CM>(
        node: &N,
        cost_model: CM,
    ) -> Result<(String, DefaultCost), egglog::Error>
    where
        N: EgglogNode + EgglogTy + 'static,
        CM: CostModel<DefaultCost> + 'static,
    {
        Self::extract_value_to_string_with_cost_model(
            Value::<N>::new(Self::canonical_raw(node)),
            cost_model,
        )
    }

    fn extract_node_to_string_with_backend<N, CM>(
        node: &N,
        backend: ExtractBackend<CM>,
    ) -> Result<(String, DefaultCost), egglog::Error>
    where
        N: EgglogNode + EgglogTy + 'static,
        CM: CostModel<DefaultCost> + 'static,
    {
        Self::extract_value_to_string_with_backend(
            Value::<N>::new(Self::canonical_raw(node)),
            backend,
        )
    }

    fn extract_node_to_typst_with_backend<N, CM>(
        node: &N,
        backend: ExtractBackend<CM>,
    ) -> Result<(String, DefaultCost), egglog::Error>
    where
        N: EgglogNode + EgglogTy + 'static,
        CM: CostModel<DefaultCost> + 'static,
    {
        let (termdag, term, cost) = Self::extract_node_with_backend(node, backend)?;
        let rendered = render_term_to_typst(term, &termdag)?.text.into_owned();
        Ok((rendered, cost))
    }

    fn extract_node_to_svg_with_backend<N, CM>(
        node: &N,
        backend: ExtractBackend<CM>,
        output_path: impl AsRef<Path>,
    ) -> Result<DefaultCost, egglog::Error>
    where
        N: EgglogNode + EgglogTy + 'static,
        CM: CostModel<DefaultCost> + 'static,
    {
        let (typst, cost) = Self::extract_node_to_typst_with_backend(node, backend)?;
        compile_typst_math_to_svg(&typst, output_path.as_ref())?;
        Ok(cost)
    }
}
impl<T: ExtractSgl + TxSgl> ExtractNodeSgl for T {}

#[cfg(feature = "rustsat-extract")]
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct RustsatEqKey {
    sort_name: String,
    value: egglog::Value,
}

#[cfg(feature = "rustsat-extract")]
#[derive(Debug, Clone)]
struct RustsatCandidateDraft {
    term_name: String,
    output: RustsatEqKey,
    inputs: Vec<(ArcSort, egglog::Value)>,
    penalty: usize,
}

#[cfg(feature = "rustsat-extract")]
#[derive(Debug, Clone)]
struct RustsatCandidate {
    lit: RustsatLit,
    term_name: String,
    output: RustsatEqKey,
    inputs: Vec<(ArcSort, egglog::Value)>,
    penalty: usize,
}

#[cfg(feature = "rustsat-extract")]
impl RustsatCandidateDraft {
    fn sort_key(&self) -> String {
        format!(
            "{}|{:?}|{}|{:?}",
            self.output.sort_name, self.output.value, self.term_name, self.inputs
        )
    }
}

#[cfg(feature = "rustsat-extract")]
impl From<&EBoostEqKey> for RustsatEqKey {
    fn from(value: &EBoostEqKey) -> Self {
        Self {
            sort_name: value.sort_name.clone(),
            value: value.value,
        }
    }
}

#[cfg(feature = "rustsat-extract")]
impl RustsatCandidate {
    fn decode_key(&self) -> String {
        format!(
            "{}|{}|{:?}",
            self.term_name,
            self.inputs.len(),
            self.inputs
                .iter()
                .map(|(sort, value)| format!("{}:{:?}", sort.name(), value))
                .collect::<Vec<_>>()
        )
    }
}

#[cfg(feature = "rustsat-extract")]
fn rustsat_extract_value_prototype(
    egraph: &EGraph,
    sort: &ArcSort,
    value: egglog::Value,
    _config: RustsatExtractConfig,
) -> Result<(TermDag, TermId, DefaultCost), egglog::Error> {
    if !sort.is_eq_sort() {
        return Err(egglog::Error::BackendError(
            "rustsat extraction prototype currently only supports eq-sort roots".to_string(),
        ));
    }

    let root_value = egraph.get_canonical_value(value, sort);
    let root_key = RustsatEqKey {
        sort_name: sort.name().to_string(),
        value: root_value,
    };

    let draft_candidates = rustsat_collect_draft_candidates(egraph)?;
    rustsat_extract_from_drafts(egraph, root_key, draft_candidates)
}

#[cfg(feature = "rustsat-extract")]
fn rustsat_extract_from_drafts(
    egraph: &EGraph,
    root_key: RustsatEqKey,
    draft_candidates: Vec<RustsatCandidateDraft>,
) -> Result<(TermDag, TermId, DefaultCost), egglog::Error> {
    let draft_by_output = rustsat_index_draft_candidates(&draft_candidates);
    let reachable_classes =
        rustsat_collect_reachable_classes(&root_key, &draft_candidates, &draft_by_output)?;
    let reachable_draft_ids =
        rustsat_collect_reachable_candidate_ids(&reachable_classes, &draft_candidates);

    let mut inst = OptInstance::<BasicVarManager>::default();
    let mut reachable_candidates = reachable_draft_ids
        .into_iter()
        .map(|idx| draft_candidates[idx].clone())
        .collect::<Vec<_>>();
    reachable_candidates.sort_by_key(RustsatCandidateDraft::sort_key);

    let mut candidates = Vec::<RustsatCandidate>::with_capacity(reachable_candidates.len());
    for draft in reachable_candidates {
        let lit = inst.new_lit();
        candidates.push(RustsatCandidate {
            lit,
            term_name: draft.term_name,
            output: draft.output,
            inputs: draft.inputs,
            penalty: draft.penalty,
        });
    }

    let candidates_by_output = rustsat_index_candidates(&candidates);
    rustsat_add_cycle_constraints(
        &mut inst,
        &reachable_classes,
        &candidates,
        &candidates_by_output,
    )?;
    let root_candidates = candidates_by_output.get(&root_key).ok_or_else(|| {
        egglog::Error::BackendError(format!(
            "rustsat extraction prototype found no constructor candidates for root {}",
            root_key.sort_name
        ))
    })?;

    let mut root_clause = RustsatClause::with_capacity(root_candidates.len());
    for idx in root_candidates {
        root_clause.add(candidates[*idx].lit);
    }
    inst.constraints_mut().add_clause(root_clause);

    for candidate in &candidates {
        for (child_sort, child_value) in &candidate.inputs {
            if !child_sort.is_eq_sort() {
                continue;
            }
            let child_key = RustsatEqKey {
                sort_name: child_sort.name().to_string(),
                value: *child_value,
            };
            let child_candidates = candidates_by_output.get(&child_key).ok_or_else(|| {
                egglog::Error::BackendError(format!(
                    "rustsat extraction prototype found no constructor candidates for reachable child {}",
                    child_key.sort_name
                ))
            })?;
            let mut clause = RustsatClause::with_capacity(child_candidates.len() + 1);
            clause.add(!candidate.lit);
            for idx in child_candidates {
                clause.add(candidates[*idx].lit);
            }
            inst.constraints_mut().add_clause(clause);
        }
        inst.objective_mut()
            .add_soft_lit(candidate.penalty, candidate.lit);
    }

    let (assignment, objective_cost) = inst
        .solve_maxsat::<SolutionImprovingSearch<RustsatMinisat, BinaryAdder>>()
        .ok_or_else(|| {
            egglog::Error::BackendError(
                "rustsat extraction prototype could not find a satisfying weighted-MaxSAT solution"
                    .to_string(),
            )
        })?;

    let mut termdag = TermDag::default();
    let mut cache = HashMap::<RustsatEqKey, TermId>::new();
    let mut active = HashSet::<RustsatEqKey>::new();
    let root_term = rustsat_decode_eqclass(
        egraph,
        &root_key,
        &assignment,
        &candidates,
        &candidates_by_output,
        &mut cache,
        &mut active,
        &mut termdag,
    )?;
    let cost = u64::try_from(objective_cost).map_err(|_| {
        egglog::Error::BackendError("rustsat objective overflowed DefaultCost".to_string())
    })?;
    Ok((termdag, root_term, cost))
}

#[cfg(feature = "rustsat-extract")]
fn eboost_layered_extract_value_prototype(
    egraph: &EGraph,
    sort: &ArcSort,
    value: egglog::Value,
    config: EBoostLayeredConfig,
) -> Result<(TermDag, TermId, DefaultCost), egglog::Error> {
    if !(config.bound.is_finite() && config.bound >= 1.0) {
        return Err(egglog::Error::BackendError(format!(
            "eboost layered extraction requires bound >= 1.0, got {}",
            config.bound
        )));
    }
    let prepared = prepare_eboost_candidates(egraph, sort, value)?;
    let pruned = eboost_layered_prune_candidates(&prepared, config.bound);
    let drafts = pruned
        .iter()
        .map(rustsat_draft_from_eboost_candidate)
        .collect::<Result<Vec<_>, _>>()?;
    rustsat_extract_from_drafts(egraph, RustsatEqKey::from(&prepared.root_key), drafts)
}

#[cfg(feature = "rustsat-extract")]
fn eboost_layered_prune_candidates(prepared: &EBoostPrepared, bound: f32) -> Vec<EBoostCandidate> {
    let heuristic_choice_ids = prepared
        .best_by_class
        .values()
        .map(|cost_set| cost_set.candidate_idx)
        .collect::<HashSet<_>>();

    let mut min_score_by_class = HashMap::<EBoostEqKey, DefaultCost>::new();
    for (idx, score) in &prepared.candidate_scores {
        let class_key = prepared.reachable_candidates[*idx].output.clone();
        min_score_by_class
            .entry(class_key)
            .and_modify(|existing| {
                if *score < *existing {
                    *existing = *score;
                }
            })
            .or_insert(*score);
    }

    prepared
        .reachable_candidates
        .iter()
        .enumerate()
        .filter(|(idx, candidate)| {
            if heuristic_choice_ids.contains(idx) {
                return true;
            }
            let Some(score) = prepared.candidate_scores.get(idx) else {
                return true;
            };
            let Some(class_min) = min_score_by_class.get(&candidate.output) else {
                return true;
            };
            (*score as f64) <= (*class_min as f64) * (bound as f64)
        })
        .map(|(_, candidate)| candidate.clone())
        .collect()
}

#[cfg(feature = "rustsat-extract")]
fn rustsat_draft_from_eboost_candidate(
    candidate: &EBoostCandidate,
) -> Result<RustsatCandidateDraft, egglog::Error> {
    Ok(RustsatCandidateDraft {
        term_name: candidate.term_name.clone(),
        output: RustsatEqKey::from(&candidate.output),
        inputs: candidate.inputs.clone(),
        penalty: usize::try_from(candidate.head_cost).map_err(|_| {
            egglog::Error::BackendError(format!(
                "cost for `{}` does not fit into rustsat weight domain",
                candidate.term_name
            ))
        })?,
    })
}

#[cfg(feature = "rustsat-extract")]
fn rustsat_collect_draft_candidates(
    egraph: &EGraph,
) -> Result<Vec<RustsatCandidateDraft>, egglog::Error> {
    collect_candidates(egraph)?
        .iter()
        .map(rustsat_draft_from_eboost_candidate)
        .collect()
}

#[cfg(feature = "rustsat-extract")]
fn rustsat_index_draft_candidates(
    candidates: &[RustsatCandidateDraft],
) -> HashMap<RustsatEqKey, Vec<usize>> {
    let mut by_output = HashMap::<RustsatEqKey, Vec<usize>>::new();
    for (idx, candidate) in candidates.iter().enumerate() {
        by_output
            .entry(candidate.output.clone())
            .or_default()
            .push(idx);
    }
    by_output
}

#[cfg(feature = "rustsat-extract")]
fn rustsat_index_candidates(candidates: &[RustsatCandidate]) -> HashMap<RustsatEqKey, Vec<usize>> {
    let mut by_output = HashMap::<RustsatEqKey, Vec<usize>>::new();
    for (idx, candidate) in candidates.iter().enumerate() {
        by_output
            .entry(candidate.output.clone())
            .or_default()
            .push(idx);
    }
    by_output
}

#[cfg(feature = "rustsat-extract")]
fn rustsat_collect_reachable_classes(
    root_key: &RustsatEqKey,
    candidates: &[RustsatCandidateDraft],
    by_output: &HashMap<RustsatEqKey, Vec<usize>>,
) -> Result<HashSet<RustsatEqKey>, egglog::Error> {
    let mut reachable = HashSet::<RustsatEqKey>::new();
    let mut queue = VecDeque::<RustsatEqKey>::from([root_key.clone()]);

    while let Some(key) = queue.pop_front() {
        if !reachable.insert(key.clone()) {
            continue;
        }
        let Some(candidate_ids) = by_output.get(&key) else {
            return Err(egglog::Error::BackendError(format!(
                "rustsat extraction prototype found no constructor candidates for reachable e-class `{}`",
                key.sort_name
            )));
        };
        for idx in candidate_ids {
            for (child_sort, child_value) in &candidates[*idx].inputs {
                if child_sort.is_eq_sort() {
                    queue.push_back(RustsatEqKey {
                        sort_name: child_sort.name().to_string(),
                        value: *child_value,
                    });
                }
            }
        }
    }

    Ok(reachable)
}

#[cfg(feature = "rustsat-extract")]
fn rustsat_collect_reachable_candidate_ids(
    reachable_classes: &HashSet<RustsatEqKey>,
    candidates: &[RustsatCandidateDraft],
) -> Vec<usize> {
    let mut reachable = candidates
        .iter()
        .enumerate()
        .filter_map(|(idx, candidate)| reachable_classes.contains(&candidate.output).then_some(idx))
        .collect::<Vec<_>>();
    reachable.sort_unstable();
    reachable
}

#[cfg(feature = "rustsat-extract")]
fn rustsat_add_cycle_constraints(
    inst: &mut OptInstance<BasicVarManager>,
    reachable_classes: &HashSet<RustsatEqKey>,
    candidates: &[RustsatCandidate],
    by_output: &HashMap<RustsatEqKey, Vec<usize>>,
) -> Result<(), egglog::Error> {
    let adjacency = rustsat_class_adjacency(reachable_classes, candidates);
    let class_cycles = rustsat_enumerate_class_cycles(&adjacency);
    for cycle in class_cycles {
        let cycle_set = cycle.iter().cloned().collect::<HashSet<_>>();
        let mut cycle_clause = RustsatClause::with_capacity(cycle.len());
        for class_key in &cycle {
            let cycle_candidate_ids = by_output
                .get(class_key)
                .into_iter()
                .flatten()
                .copied()
                .filter(|idx| {
                    candidates[*idx]
                        .inputs
                        .iter()
                        .any(|(child_sort, child_value)| {
                            child_sort.is_eq_sort()
                                && cycle_set.contains(&RustsatEqKey {
                                    sort_name: child_sort.name().to_string(),
                                    value: *child_value,
                                })
                        })
                })
                .collect::<Vec<_>>();

            if cycle_candidate_ids.is_empty() {
                continue;
            }
            let disable_lit = inst.new_lit();
            cycle_clause.add(disable_lit);
            for idx in &cycle_candidate_ids {
                inst.constraints_mut()
                    .add_clause(rustsat_clause([!disable_lit, !candidates[*idx].lit]));
            }
            let mut iff_clause = RustsatClause::with_capacity(cycle_candidate_ids.len() + 1);
            iff_clause.add(disable_lit);
            for idx in &cycle_candidate_ids {
                iff_clause.add(candidates[*idx].lit);
            }
            inst.constraints_mut().add_clause(iff_clause);
        }
        if !cycle_clause.is_empty() {
            inst.constraints_mut().add_clause(cycle_clause);
        }
    }
    Ok(())
}

#[cfg(feature = "rustsat-extract")]
fn rustsat_class_adjacency(
    reachable_classes: &HashSet<RustsatEqKey>,
    candidates: &[RustsatCandidate],
) -> HashMap<RustsatEqKey, BTreeSet<RustsatEqKey>> {
    let mut adjacency = HashMap::<RustsatEqKey, BTreeSet<RustsatEqKey>>::new();
    for class_key in reachable_classes {
        adjacency.entry(class_key.clone()).or_default();
    }
    for candidate in candidates {
        let edges = adjacency.entry(candidate.output.clone()).or_default();
        for (child_sort, child_value) in &candidate.inputs {
            if child_sort.is_eq_sort() {
                let child = RustsatEqKey {
                    sort_name: child_sort.name().to_string(),
                    value: *child_value,
                };
                if reachable_classes.contains(&child) {
                    edges.insert(child);
                }
            }
        }
    }
    adjacency
}

#[cfg(feature = "rustsat-extract")]
fn rustsat_enumerate_class_cycles(
    adjacency: &HashMap<RustsatEqKey, BTreeSet<RustsatEqKey>>,
) -> Vec<Vec<RustsatEqKey>> {
    let mut nodes = adjacency.keys().cloned().collect::<Vec<_>>();
    nodes.sort();
    let mut stack = Vec::<RustsatEqKey>::new();
    let mut path_set = HashSet::<RustsatEqKey>::new();
    let mut seen_cycles = BTreeSet::<Vec<RustsatEqKey>>::new();

    fn dfs(
        start: &RustsatEqKey,
        node: &RustsatEqKey,
        adjacency: &HashMap<RustsatEqKey, BTreeSet<RustsatEqKey>>,
        stack: &mut Vec<RustsatEqKey>,
        path_set: &mut HashSet<RustsatEqKey>,
        seen_cycles: &mut BTreeSet<Vec<RustsatEqKey>>,
    ) {
        stack.push(node.clone());
        path_set.insert(node.clone());
        if let Some(children) = adjacency.get(node) {
            for child in children {
                if child == start {
                    let mut cycle = stack.clone();
                    cycle.sort();
                    seen_cycles.insert(cycle);
                } else if !path_set.contains(child) && child >= start {
                    dfs(start, child, adjacency, stack, path_set, seen_cycles);
                }
            }
        }
        stack.pop();
        path_set.remove(node);
    }

    for node in &nodes {
        dfs(
            node,
            node,
            adjacency,
            &mut stack,
            &mut path_set,
            &mut seen_cycles,
        );
    }

    seen_cycles.into_iter().collect()
}

#[cfg(feature = "rustsat-extract")]
fn rustsat_decode_eqclass(
    egraph: &EGraph,
    key: &RustsatEqKey,
    assignment: &RustsatAssignment,
    candidates: &[RustsatCandidate],
    by_output: &HashMap<RustsatEqKey, Vec<usize>>,
    cache: &mut HashMap<RustsatEqKey, TermId>,
    active: &mut HashSet<RustsatEqKey>,
    termdag: &mut TermDag,
) -> Result<TermId, egglog::Error> {
    if let Some(term) = cache.get(key) {
        return Ok(*term);
    }
    if !active.insert(key.clone()) {
        return Err(egglog::Error::BackendError(format!(
            "rustsat decode re-entered e-class `{}` while it was still active; the selected assignment is not acyclic enough to decode",
            key.sort_name
        )));
    }
    let Some(candidate_ids) = by_output.get(key) else {
        active.remove(key);
        return Err(egglog::Error::BackendError(format!(
            "rustsat decode could not find any candidate rows for `{}`",
            key.sort_name
        )));
    };

    let mut selected = candidate_ids
        .iter()
        .copied()
        .filter(|idx| assignment.lit_value(candidates[*idx].lit) == TernaryVal::True)
        .collect::<Vec<_>>();
    selected.sort_by_key(|idx| candidates[*idx].decode_key());
    let Some(chosen_idx) = selected.first().copied() else {
        active.remove(key);
        return Err(egglog::Error::BackendError(format!(
            "rustsat decode found no selected witness for required e-class `{}`",
            key.sort_name
        )));
    };
    let chosen = &candidates[chosen_idx];

    let mut child_terms = Vec::with_capacity(chosen.inputs.len());
    for (child_sort, child_value) in &chosen.inputs {
        let child_term = if child_sort.is_eq_sort() {
            rustsat_decode_eqclass(
                egraph,
                &RustsatEqKey {
                    sort_name: child_sort.name().to_string(),
                    value: *child_value,
                },
                assignment,
                candidates,
                by_output,
                cache,
                active,
                termdag,
            )?
        } else {
            rustsat_base_term(egraph, termdag, child_sort, *child_value)?
        };
        child_terms.push(child_term);
    }

    let term = termdag.app(chosen.term_name.clone(), child_terms);
    cache.insert(key.clone(), term);
    active.remove(key);
    Ok(term)
}

#[cfg(feature = "rustsat-extract")]
fn rustsat_base_term(
    egraph: &EGraph,
    termdag: &mut TermDag,
    sort: &ArcSort,
    value: egglog::Value,
) -> Result<TermId, egglog::Error> {
    match sort.name() {
        "i64" => Ok(termdag.lit(Literal::Int(egraph.value_to_base::<i64>(value)))),
        "bool" => Ok(termdag.lit(Literal::Bool(egraph.value_to_base::<bool>(value)))),
        "String" => Ok(termdag.lit(Literal::String(
            egraph.value_to_base::<egglog::sort::S>(value).0,
        ))),
        "f64" => Ok(termdag.lit(Literal::Float(
            egraph.value_to_base::<egglog::sort::F>(value).0,
        ))),
        "Unit" | "()" => Ok(termdag.lit(Literal::Unit)),
        other => Err(egglog::Error::BackendError(format!(
            "rustsat extraction prototype does not yet support base sort `{other}` in decode"
        ))),
    }
}

#[cfg(feature = "rustsat-extract")]
fn rustsat_clause(lits: impl IntoIterator<Item = RustsatLit>) -> RustsatClause {
    lits.into_iter().collect()
}

pub trait G: TxSgl + NonPatRecSgl + RuleRunnerSgl + RxSgl {}
impl<T: TxSgl + NonPatRecSgl + RuleRunnerSgl + RxSgl> G for T {}

pub type SlotVarID = String;

pub trait QuerySlot {
    fn query_slot(name: SlotVarID) -> Self;
}
pub trait SlottedPatRecSgl: PatRecSgl {
    fn on_new_query_slot(node: &(impl EgglogNode + 'static), var_id: SlotVarID);
}
pub trait SlottedPatRec: PatRec {
    fn on_new_query_slot(&self, node: &(impl EgglogNode + 'static), var_id: SlotVarID);
}
impl<T: PatRecSgl> SlottedPatRecSgl for T
where
    T::RetTy: SlottedPatRec + PatRec,
{
    fn on_new_query_slot(node: &(impl EgglogNode + 'static), var_id: SlotVarID) {
        Self::sgl().on_new_query_slot(node, var_id);
    }
}
pub trait FromMetas {
    fn from_metas(values: &mut impl Iterator<Item = SlotMeta>) -> Self;
}

#[cfg(feature = "viewer")]
impl<S: SingletonGetter> EGraphViewSgl for S
where
    S::RetTy: EGraphView,
{
    fn egraph() -> std::sync::Arc<std::sync::Mutex<egglog::EGraph>> {
        Self::sgl().egraph()
    }
    fn view() -> Result<(), eggplant_viewer::Error> {
        Self::sgl().view()
    }
}

#[cfg(feature = "viewer")]
pub trait EGraphViewSgl {
    fn egraph() -> Arc<Mutex<EGraph>>;
    fn view() -> Result<(), eframe::Error>;
}

#[cfg(feature = "viewer")]
pub trait EGraphView {
    fn egraph(&self) -> Arc<Mutex<EGraph>>;
    fn view(&self) -> Result<(), eframe::Error>;
}
