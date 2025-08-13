use crate::wrap::{
    EValue, EgglogFunc, EgglogFuncInputs, EgglogFuncOutput, EgglogTy, QueryBuilder, RuleCtx,
    SortName, SymLit, VarName, tx_rx_vt::TxRxVT,
};
use dashmap::DashMap;
use derive_more::{Debug, Deref, DerefMut, IntoIterator};
use egglog::{
    ArcSort, BaseValue, EGraph,
    ast::{Command, GenericAction, GenericExpr, RustSpan, Span},
};
use egglog::{TermDag, TermId, ast::Literal, sort::OrderedFloat, span};
use smallvec::SmallVec;
use std::{
    any::Any,
    borrow::Borrow,
    collections::HashMap,
    fmt,
    hash::Hash,
    marker::PhantomData,
    panic::Location,
    sync::{Arc, atomic::AtomicU32},
    usize,
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
    fn on_union(&self, node1: &(impl EgglogNode + 'static), node2: &(impl EgglogNode + 'static));
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
    fn on_union(node1: &(impl EgglogNode + 'static), node2: &(impl EgglogNode + 'static));
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
    fn on_drop(dropped: &mut (impl EgglogNode + 'static)) {
        Self::sgl().on_drop(dropped);
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

    fn on_union(node1: &(impl EgglogNode + 'static), node2: &(impl EgglogNode + 'static)) {
        Self::sgl().on_union(node1, node2);
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

/// pattern recorder triat
/// it's neccessary to impl NodeDropper for PatternCombine feature
/// and also should be implemented by Tx
pub trait PatRec: NodeDropper + Tx {
    #[track_caller]
    fn on_new_query_leaf(&self, node: &(impl EgglogNode + 'static));
    fn on_record_start(&self);
    fn on_record_end<T: PatRecSgl>(&self, pat_vars: &impl PatVars<T>) -> PatId;
    fn pat2query(&self, pat_id: PatId) -> QueryBuilder;
}
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct PatId(pub u32);

pub trait PatRecSgl: NodeDropperSgl + TxSgl {
    #[track_caller]
    fn on_new_query_leaf(node: &(impl EgglogNode + 'static));
    fn on_record_start();
    fn on_record_end(pat_vars: &impl PatVars<Self>) -> PatId;
    fn pat2query(pat_id: PatId) -> QueryBuilder;
}
impl<T: SingletonGetter> PatRecSgl for T
where
    T::RetTy: PatRec + NodeSetter,
{
    fn on_new_query_leaf(node: &(impl EgglogNode + 'static)) {
        Self::sgl().on_new_query_leaf(node);
    }

    fn on_record_start() {
        Self::sgl().on_record_start();
    }

    fn on_record_end(pat_vars: &impl PatVars<Self>) -> PatId {
        Self::sgl().on_record_end(pat_vars)
    }

    fn pat2query(pat_id: PatId) -> QueryBuilder {
        Self::sgl().pat2query(pat_id)
    }
}

pub trait WithPatRecSgl {
    type PatRecSgl: PatRecSgl;
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
        ctx: &mut RuleCtx,
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

    #[track_caller]
    fn to_term(
        &self,
        term_dag: &mut TermDag,
        sym2term: &mut HashMap<Sym, TermId>,
        sym2ph_name: &HashMap<Sym, &'static str>,
    ) -> TermId;

    #[track_caller]
    fn add_atom(&self, query_builder: &mut QueryBuilder);
}
pub trait VarsCollector {
    /// 1. if self is a typed placeholder [`TyPH::VarPH`], collect itself and its basic vars
    /// 2. if self is a typed placeholder [`TyPH::PH`], only collect itself
    /// 3. if self is a [`PatVars`] collect recursively
    fn collect_vars(&self, vars: &mut Vec<(VarName, SortName)>);
}

pub trait EgglogEnumVariantTy: Clone + 'static + Send + Sync {
    const TY_NAME: &'static str;
    /// T represent the type call that call this type
    /// This is useful when we want to specify default for a type
    type ValuedWithDefault<T>: FromPlainValues;
    /// fields names of valued variant struct
    const BASIC_FIELD_NAMES: &[&'static str];
    const BASIC_FIELD_TYPES: &[&'static str];
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
    const TY_NAME: &'static str = "Unknown";
    type ValuedWithDefault<T> = Value<T>;
    const BASIC_FIELD_NAMES: &[&'static str] = &[];
    const BASIC_FIELD_TYPES: &[&'static str] = &[];
}

#[derive(DerefMut, Deref)]
pub struct WorkAreaNode {
    pub next: Option<Sym>,
    pub prev: Option<Sym>,
    pub preds: Syms,
    #[deref]
    #[deref_mut]
    pub egglog: Box<dyn EgglogNode>,
}

impl Clone for WorkAreaNode {
    fn clone(&self) -> Self {
        Self {
            next: self.next.clone(),
            preds: self.preds.clone(),
            egglog: self.egglog.clone_dyn(),
            prev: None,
        }
    }
}
impl fmt::Debug for WorkAreaNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} {} | {}",
            self.variant_name().unwrap_or(self.ty_name()),
            self.cur_sym(),
            self.to_egglog_string().unwrap_or(
                self.egglog
                    .basic_field_types()
                    .iter()
                    .map(|s| s.to_string())
                    .collect::<Vec<_>>()
                    .join(" ")
            )
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
impl Syms {
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
    fn on_commit<T: EgglogNode>(&self, node: &T);
    #[track_caller]
    fn on_stage<T: EgglogNode + ?Sized>(&self, node: &T);
}

pub trait TxCommitSgl {
    #[track_caller]
    fn on_commit<T: EgglogNode>(node: &T);
    #[track_caller]
    fn on_stage<T: EgglogNode>(node: &T);
}

impl<Ret, S> TxCommitSgl for S
where
    Ret: Tx + VersionCtl + TxCommit,
    S: SingletonGetter<RetTy = Ret>,
{
    fn on_commit<T: EgglogNode>(node: &T) {
        S::sgl().on_commit(node);
    }

    fn on_stage<T: EgglogNode>(node: &T) {
        S::sgl().on_stage(node);
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
impl ToVar for i64 {
    fn to_var(&self) -> GenericExpr<&'static str, &'static str> {
        GenericExpr::Lit(span!(), egglog::ast::Literal::Int(*self))
    }
}
impl ToVar for f64 {
    fn to_var(&self) -> GenericExpr<&'static str, &'static str> {
        GenericExpr::Lit(
            span!(),
            egglog::ast::Literal::Float(OrderedFloat::<f64>(*self)),
        )
    }
}
impl ToVar for String {
    fn to_var(&self) -> GenericExpr<&'static str, &'static str> {
        GenericExpr::Lit(span!(), Literal::String(self.to_owned()))
    }
}
impl ToVar for bool {
    fn to_var(&self) -> GenericExpr<&'static str, &'static str> {
        GenericExpr::Lit(span!(), Literal::Bool(*self))
    }
}
impl<T> ToVar for Sym<T> {
    fn to_var(&self) -> GenericExpr<&'static str, &'static str> {
        GenericExpr::Var(span!(), self.inner.into())
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
        panic!("illegal singleton getter")
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
    pub fn detype(&self) -> egglog::Value {
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
pub trait PatVars<T: PatRecSgl>: ToStrArcSort {
    type Valued: FromPlainValues;
}

/// a pattern should be transformed into [(str,Arcsort)] when registering rules
pub trait ToStrArcSort {
    fn to_str_arcsort(&self, egraph: &EGraph) -> Vec<(VarName, ArcSort)>;
}

pub trait FromPlainValues {
    fn from_plain_values(values: &mut impl Iterator<Item = egglog::Value>) -> Self;
}

pub trait ToValue<T> {
    fn to_value(&self, ctx: &mut RuleCtx) -> Value<T>;
}

impl<T: EgglogTy> Clone for Value<T> {
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
/// any struct implements this trait inferred to be [`ToValue`]
pub trait BoxUnbox {
    type Boxed: BaseValue;
    type UnBoxed;
    fn unbox(boxed: Self::Boxed, ctx: &mut RuleCtx) -> Self::UnBoxed;
    fn box_it(self, ctx: &mut RuleCtx) -> Self::Boxed;
}

pub trait SingleFieldVariant {}

impl<T0, T1, B: BoxUnbox<Boxed = T0, UnBoxed = T1> + EgglogTy + Clone> ToValue<B> for B {
    fn to_value(&self, ctx: &mut RuleCtx) -> Value<B> {
        ctx.intern_base(self.clone())
    }
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

/// a marker trait for those not pattern recorder singleton
/// because currently rust doesn't support `!PatRecSgl` clause
pub trait NonPatRecSgl {}
impl NonPatRecSgl for () {}
