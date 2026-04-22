use crate::wrap::{
    EgglogFunc, EgglogFuncInputs, EgglogFuncOutput, EgglogNode, EgglogRelation, EgglogTy,
    FactsBuilder, FromBase, LocateVersion, NodeDropper, NodeOwner, NodeSetter, PatId, PatRec,
    PatRecSgl, PatVars, RuleCtxHook, RuleRunner, RuleSetId, RunConfig, Rx, SortName, Sym, TableName,
    ToDot, Tx, TxCommand, TxCommit, Value, VarName, VersionCtl,
};
use egglog::EGraph;
use egglog_reports::RunReport;
use std::any::{Any, TypeId};
use std::cell::RefCell;
use std::collections::HashMap;
use std::future::Future;
use std::marker::PhantomData;
use std::ops::Deref;
use std::sync::{
    Arc, Condvar, Mutex, OnceLock,
    atomic::{AtomicUsize, Ordering},
};
use std::thread::ThreadId;

type ErasedState = Arc<dyn Any + Send + Sync>;

#[derive(Clone)]
struct ErasedSessionBinding {
    state: ErasedState,
    order: usize,
}

impl ErasedSessionBinding {
    fn new<Tx: SessionAwareTxMarker>(state: Arc<SessionState<Tx>>) -> Self {
        Self {
            state,
            order: NEXT_BINDING_ORDER.fetch_add(1, Ordering::Relaxed),
        }
    }
}

static NEXT_BINDING_ORDER: AtomicUsize = AtomicUsize::new(0);
static DEFAULT_SESSIONS: OnceLock<Mutex<HashMap<TypeId, ErasedState>>> = OnceLock::new();

tokio::task_local! {
    static CURRENT_SESSION_TASKS: RefCell<HashMap<TypeId, ErasedSessionBinding>>;
}

thread_local! {
    static CURRENT_SESSION_THREADS: RefCell<HashMap<TypeId, ErasedSessionBinding>> = RefCell::new(HashMap::new());
}

#[derive(Clone, Copy)]
enum RulesetRegistration {
    Building { owner: ThreadId },
    Ready(RuleSetId),
}

struct SessionState<Tx: SessionAwareTxMarker> {
    runtime: Tx::Runtime,
    pat_recorder: Mutex<Tx::PatRecorder>,
    rulesets: Mutex<HashMap<&'static str, RulesetRegistration>>,
    rulesets_cv: Condvar,
}

impl<Tx: SessionAwareTxMarker> SessionState<Tx> {
    fn new() -> Self {
        Self {
            runtime: Tx::new_runtime(),
            pat_recorder: Mutex::new(Tx::new_pat_recorder()),
            rulesets: Mutex::new(HashMap::new()),
            rulesets_cv: Condvar::new(),
        }
    }
}

fn type_key<T: 'static>() -> TypeId {
    TypeId::of::<T>()
}

fn erase_state<Tx: SessionAwareTxMarker>(state: Arc<SessionState<Tx>>) -> ErasedState {
    state
}

fn downcast_state<Tx: SessionAwareTxMarker>(state: ErasedState) -> Arc<SessionState<Tx>> {
    state.downcast::<SessionState<Tx>>().unwrap_or_else(|_| {
        panic!(
            "pseudo-singleton state type mismatch for {}",
            std::any::type_name::<Tx>()
        )
    })
}

fn default_state<Tx: SessionAwareTxMarker>() -> Arc<SessionState<Tx>> {
    let key = type_key::<Tx>();
    let mut registry = DEFAULT_SESSIONS
        .get_or_init(|| Mutex::new(HashMap::new()))
        .lock()
        .unwrap();
    if let Some(existing) = registry.get(&key) {
        return downcast_state::<Tx>(Arc::clone(existing));
    }

    let state = Arc::new(SessionState::<Tx>::new());
    registry.insert(key, erase_state(Arc::clone(&state)));
    state
}

fn task_bindings_snapshot() -> HashMap<TypeId, ErasedSessionBinding> {
    CURRENT_SESSION_TASKS
        .try_with(|slot| slot.borrow().clone())
        .unwrap_or_default()
}

fn current_state<Tx: SessionAwareTxMarker>() -> Arc<SessionState<Tx>> {
    let key = type_key::<Tx>();
    let thread_binding = CURRENT_SESSION_THREADS.with(|slot| slot.borrow().get(&key).cloned());
    let task_binding = CURRENT_SESSION_TASKS
        .try_with(|slot| slot.borrow().get(&key).cloned())
        .ok()
        .flatten();

    match (thread_binding, task_binding) {
        (Some(thread), Some(task)) => {
            if thread.order > task.order {
                downcast_state::<Tx>(thread.state)
            } else {
                downcast_state::<Tx>(task.state)
            }
        }
        (Some(thread), None) => downcast_state::<Tx>(thread.state),
        (None, Some(task)) => downcast_state::<Tx>(task.state),
        (None, None) => default_state::<Tx>(),
    }
}

pub fn with_runtime<Tx: SessionAwareTxMarker, R>(f: impl FnOnce(&Tx::Runtime) -> R) -> R {
    let state = current_state::<Tx>();
    f(&state.runtime)
}

pub fn with_pat_recorder<Tx: SessionAwareTxMarker, R>(
    f: impl FnOnce(&Tx::PatRecorder) -> R,
) -> R {
    let state = current_state::<Tx>();
    let pat_recorder = state.pat_recorder.lock().unwrap();
    f(&pat_recorder)
}

pub struct Session<Tx: SessionAwareTxMarker> {
    state: Arc<SessionState<Tx>>,
}

impl<Tx: SessionAwareTxMarker> Clone for Session<Tx> {
    fn clone(&self) -> Self {
        Self {
            state: Arc::clone(&self.state),
        }
    }
}

impl<Tx: SessionAwareTxMarker> Session<Tx> {
    pub fn new() -> Self {
        Self {
            state: Arc::new(SessionState::<Tx>::new()),
        }
    }

    pub fn default() -> Self {
        Self {
            state: default_state::<Tx>(),
        }
    }

    pub fn run<R>(&self, f: impl FnOnce() -> R) -> R {
        let key = type_key::<Tx>();
        CURRENT_SESSION_THREADS.with(|slot| {
            let previous = slot
                .borrow_mut()
                .insert(key, ErasedSessionBinding::new::<Tx>(Arc::clone(&self.state)));
            let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(f));
            let mut bindings = slot.borrow_mut();
            match previous {
                Some(previous) => {
                    bindings.insert(key, previous);
                }
                None => {
                    bindings.remove(&key);
                }
            }
            match result {
                Ok(value) => value,
                Err(payload) => std::panic::resume_unwind(payload),
            }
        })
    }

    pub async fn run_async<R>(&self, fut: impl Future<Output = R>) -> R {
        let mut bindings = task_bindings_snapshot();
        for (key, binding) in CURRENT_SESSION_THREADS.with(|slot| slot.borrow().clone()) {
            match bindings.get(&key) {
                Some(existing) if existing.order > binding.order => {}
                _ => {
                    bindings.insert(key, binding);
                }
            }
        }
        bindings.insert(
            type_key::<Tx>(),
            ErasedSessionBinding::new::<Tx>(Arc::clone(&self.state)),
        );
        CURRENT_SESSION_TASKS.scope(RefCell::new(bindings), fut).await
    }

    pub fn spawn<R: Send + 'static>(
        &self,
        f: impl FnOnce() -> R + Send + 'static,
    ) -> std::thread::JoinHandle<R> {
        let session = Session {
            state: Arc::clone(&self.state),
        };
        std::thread::spawn(move || session.run(f))
    }

    pub fn spawn_async<R: Send + 'static>(
        &self,
        fut: impl Future<Output = R> + Send + 'static,
    ) -> tokio::task::JoinHandle<R> {
        let mut bindings = task_bindings_snapshot();
        for (key, binding) in CURRENT_SESSION_THREADS.with(|slot| slot.borrow().clone()) {
            match bindings.get(&key) {
                Some(existing) if existing.order > binding.order => {}
                _ => {
                    bindings.insert(key, binding);
                }
            }
        }
        bindings.insert(
            type_key::<Tx>(),
            ErasedSessionBinding::new::<Tx>(Arc::clone(&self.state)),
        );
        tokio::spawn(CURRENT_SESSION_TASKS.scope(RefCell::new(bindings), fut))
    }

    pub fn get_or_register_ruleset(
        &self,
        key: &'static str,
        build: impl FnOnce() -> RuleSetId,
    ) -> RuleSetId {
        get_or_register_ruleset_state::<Tx>(&self.state, key, build)
    }
}

fn get_or_register_ruleset_state<Tx: SessionAwareTxMarker>(
    state: &Arc<SessionState<Tx>>,
    key: &'static str,
    build: impl FnOnce() -> RuleSetId,
) -> RuleSetId {
    let current_thread = std::thread::current().id();

    loop {
        let mut rulesets = state.rulesets.lock().unwrap();
        match rulesets.get(key).copied() {
            Some(RulesetRegistration::Ready(ruleset)) => return ruleset,
            Some(RulesetRegistration::Building { owner }) => {
                if owner == current_thread {
                    drop(rulesets);
                    panic!("reentrant ruleset registration for key `{key}` is not supported");
                }
                let guard = state.rulesets_cv.wait(rulesets).unwrap();
                drop(guard);
            }
            None => {
                rulesets.insert(
                    key,
                    RulesetRegistration::Building {
                        owner: current_thread,
                    },
                );
                drop(rulesets);

                let build_result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(build));

                let mut rulesets = state.rulesets.lock().unwrap();
                match build_result {
                    Ok(ruleset) => {
                        rulesets.insert(key, RulesetRegistration::Ready(ruleset));
                        state.rulesets_cv.notify_all();
                        return ruleset;
                    }
                    Err(payload) => {
                        rulesets.remove(key);
                        state.rulesets_cv.notify_all();
                        std::panic::resume_unwind(payload);
                    }
                }
            }
        }
    }
}

pub fn get_or_register_ruleset<Tx: SessionAwareTxMarker>(
    key: &'static str,
    build: impl FnOnce() -> RuleSetId,
) -> RuleSetId {
    let state = current_state::<Tx>();
    get_or_register_ruleset_state::<Tx>(&state, key, build)
}

pub fn reset_for_bench<Tx: SessionAwareTxMarker>() {
    let state = current_state::<Tx>();
    Tx::reset_runtime_for_bench(&state.runtime);
    *state.pat_recorder.lock().unwrap() = Tx::new_pat_recorder();
    let mut rulesets = state.rulesets.lock().unwrap();
    rulesets.clear();
    state.rulesets_cv.notify_all();
}

pub fn egraph<Tx: SessionAwareTxMarker>() -> Arc<Mutex<EGraph>> {
    with_runtime::<Tx, _>(Tx::runtime_egraph)
}

pub trait SessionAwareTxMarker: Sized + 'static {
    type Runtime: Tx
        + Rx
        + VersionCtl
        + TxCommit
        + NodeOwner
        + NodeDropper
        + NodeSetter
        + ToDot
        + RuleRunner<Self::PatRecMarker>
        + Send
        + Sync
        + 'static;
    type PatRecorder: PatRec<MetaTy = <Self::PatRecMarker as SessionAwarePatRecMarker>::MetaTy>
        + NodeOwner
        + NodeDropper
        + NodeSetter
        + Tx
        + Send
        + Sync
        + 'static;
    type PatRecMarker: SessionAwarePatRecMarker<TxMarker = Self> + PatRecSgl;

    fn new_runtime() -> Self::Runtime;
    fn new_pat_recorder() -> Self::PatRecorder;
    fn runtime_egraph(runtime: &Self::Runtime) -> Arc<Mutex<EGraph>>;
    fn reset_runtime_for_bench(runtime: &Self::Runtime);
}

pub trait SessionAwarePatRecMarker: Sized + 'static {
    type TxMarker: SessionAwareTxMarker<PatRecMarker = Self>;
    type MetaTy: crate::wrap::Meta;
}

pub trait SessionAwareTxSgl: SessionAwareTxMarker {
    fn new_session() -> Session<Self> {
        Session::new()
    }

    fn default_session() -> Session<Self> {
        Session::default()
    }
}

impl<T: SessionAwareTxMarker> SessionAwareTxSgl for T {}

pub struct SessionTxFacade<TxMarker: SessionAwareTxMarker>(PhantomData<TxMarker>);

impl<TxMarker: SessionAwareTxMarker> SessionTxFacade<TxMarker> {
    pub const fn new() -> Self {
        Self(PhantomData)
    }
}

impl<TxMarker: SessionAwareTxMarker> NodeOwner for SessionTxFacade<TxMarker> {
    type OwnerSpecDataInNode<T: EgglogTy, V: crate::wrap::EgglogEnumVariantTy> =
        <TxMarker::Runtime as NodeOwner>::OwnerSpecDataInNode<T, V>;
}

impl<TxMarker: SessionAwareTxMarker> NodeDropper for SessionTxFacade<TxMarker> {
    fn on_drop(&self, dropped: &mut (impl EgglogNode + 'static)) {
        with_runtime::<TxMarker, _>(|runtime| runtime.on_drop(dropped));
    }
}

impl<TxMarker: SessionAwareTxMarker> NodeSetter for SessionTxFacade<TxMarker> {
    fn on_set(&self, node: &mut (impl EgglogNode + 'static)) {
        with_runtime::<TxMarker, _>(|runtime| runtime.on_set(node));
    }
}

impl<TxMarker: SessionAwareTxMarker> Tx for SessionTxFacade<TxMarker> {
    fn send(&self, sended: TxCommand) {
        with_runtime::<TxMarker, _>(|runtime| runtime.send(sended));
    }

    fn on_new(&self, node: &(impl EgglogNode + 'static)) {
        with_runtime::<TxMarker, _>(|runtime| runtime.on_new(node));
    }

    fn on_func_set<'a, F: EgglogFunc>(
        &self,
        input: <F::Input as EgglogFuncInputs>::Ref<'a>,
        output: <F::Output as EgglogFuncOutput>::Ref<'a>,
    ) {
        with_runtime::<TxMarker, _>(|runtime| runtime.on_func_set::<F>(input, output));
    }

    fn on_relation_insert<'a, R: EgglogRelation>(
        &self,
        input: <R::Input as EgglogFuncInputs>::Ref<'a>,
    ) {
        with_runtime::<TxMarker, _>(|runtime| runtime.on_relation_insert::<R>(input));
    }

    fn on_union(&self, node1: &(impl EgglogNode + 'static), node2: &(impl EgglogNode + 'static)) {
        with_runtime::<TxMarker, _>(|runtime| runtime.on_union(node1, node2));
    }

    fn canonical_raw(&self, node1: &(impl EgglogNode + 'static)) -> egglog::Value {
        with_runtime::<TxMarker, _>(|runtime| runtime.canonical_raw(node1))
    }
}

impl<TxMarker: SessionAwareTxMarker> Rx for SessionTxFacade<TxMarker> {
    fn on_func_get<'a, F: EgglogFunc>(
        &self,
        input: <F::Input as EgglogFuncInputs>::Ref<'a>,
    ) -> F::Output {
        with_runtime::<TxMarker, _>(|runtime| runtime.on_func_get::<F>(input))
    }

    fn on_funcs_get<'a, 'b, F: EgglogFunc>(
        &self,
        max_size: Option<usize>,
    ) -> Vec<(
        <F::Input as EgglogFuncInputs>::Ref<'b>,
        <F::Output as EgglogFuncOutput>::Ref<'b>,
    )> {
        with_runtime::<TxMarker, _>(|runtime| runtime.on_funcs_get::<F>(max_size))
    }

    fn on_pull_sym<T: EgglogTy>(&self, sym: Sym) -> crate::wrap::SymLit {
        with_runtime::<TxMarker, _>(|runtime| runtime.on_pull_sym::<T>(sym))
    }

    fn on_pull_value<T: EgglogTy>(&self, value: Value<T>) -> crate::wrap::SymLit {
        with_runtime::<TxMarker, _>(|runtime| runtime.on_pull_value::<T>(value))
    }
}

impl<TxMarker: SessionAwareTxMarker> VersionCtl for SessionTxFacade<TxMarker> {
    fn locate_latest(&self, node: Sym) -> Sym {
        with_runtime::<TxMarker, _>(|runtime| runtime.locate_latest(node))
    }
    fn locate_next(&self, node: Sym) -> Sym {
        with_runtime::<TxMarker, _>(|runtime| runtime.locate_next(node))
    }
    fn locate_prev(&self, node: Sym) -> Sym {
        with_runtime::<TxMarker, _>(|runtime| runtime.locate_prev(node))
    }
    fn set_latest(&self, node: &mut Sym) {
        with_runtime::<TxMarker, _>(|runtime| runtime.set_latest(node));
    }
    fn set_next(&self, node: &mut Sym) {
        with_runtime::<TxMarker, _>(|runtime| runtime.set_next(node));
    }
    fn set_prev(&self, node: &mut Sym) {
        with_runtime::<TxMarker, _>(|runtime| runtime.set_prev(node));
    }
}

impl<TxMarker: SessionAwareTxMarker> TxCommit for SessionTxFacade<TxMarker> {
    fn on_stage<T: EgglogNode + ?Sized>(&self, node: &T) {
        with_runtime::<TxMarker, _>(|runtime| runtime.on_stage(node));
    }

    fn on_commit_op_hook<T: EgglogNode>(&self, node: &T, hook: Option<Box<dyn RuleCtxHook>>) {
        with_runtime::<TxMarker, _>(|runtime| runtime.on_commit_op_hook(node, hook));
    }
}

impl<TxMarker> RuleRunner<TxMarker::PatRecMarker> for SessionTxFacade<TxMarker>
where
    TxMarker: SessionAwareTxMarker,
    TxMarker::PatRecMarker: PatRecSgl,
{
    fn add_rule<P: PatVars<TxMarker::PatRecMarker>>(
        &self,
        rule_name: &str,
        rule_set: RuleSetId,
        pat: impl Fn() -> P,
        action: impl Fn(&crate::wrap::PRRuleCtx<TxMarker::PatRecMarker>, &P::Valued)
            + Send
            + Sync
            + 'static
            + Clone,
        ctx_hook: Option<Box<dyn RuleCtxHook>>,
    ) {
        with_runtime::<TxMarker, _>(|runtime| {
            runtime.add_rule::<P>(rule_name, rule_set, pat, action, ctx_hook)
        });
    }

    fn new_ruleset(&self, rule_set: &'static str) -> RuleSetId {
        with_runtime::<TxMarker, _>(|runtime| runtime.new_ruleset(rule_set))
    }

    fn run_ruleset(&self, rule_set_id: RuleSetId, run_config: RunConfig) -> RunReport {
        with_runtime::<TxMarker, _>(|runtime| runtime.run_ruleset(rule_set_id, run_config))
    }

    fn value<T: EgglogNode>(&self, node: &T) -> Value<T> {
        with_runtime::<TxMarker, _>(|runtime| runtime.value(node))
    }
}

impl<TxMarker: SessionAwareTxMarker> ToDot for SessionTxFacade<TxMarker> {
    fn egraph_to_dot(&self, path: impl AsRef<std::path::Path>) {
        with_runtime::<TxMarker, _>(|runtime| runtime.egraph_to_dot(path));
    }

    fn wag_to_dot(&self, path: impl AsRef<std::path::Path>) {
        with_runtime::<TxMarker, _>(|runtime| runtime.wag_to_dot(path));
    }

    fn wag_to_petgraph(&self) -> crate::wrap::SerializedPetGraph {
        with_runtime::<TxMarker, _>(|runtime| runtime.wag_to_petgraph())
    }

    fn table_view(&self) {
        with_runtime::<TxMarker, _>(|runtime| runtime.table_view());
    }
}

pub struct SessionPatRecFacade<PatMarker: SessionAwarePatRecMarker>(PhantomData<PatMarker>);

impl<PatMarker: SessionAwarePatRecMarker> SessionPatRecFacade<PatMarker> {
    pub const fn new() -> Self {
        Self(PhantomData)
    }
}

impl<PatMarker: SessionAwarePatRecMarker> NodeOwner for SessionPatRecFacade<PatMarker> {
    type OwnerSpecDataInNode<T: EgglogTy, V: crate::wrap::EgglogEnumVariantTy> =
        <<PatMarker::TxMarker as SessionAwareTxMarker>::PatRecorder as NodeOwner>::OwnerSpecDataInNode<T, V>;
}

impl<PatMarker: SessionAwarePatRecMarker> NodeDropper for SessionPatRecFacade<PatMarker> {
    fn on_drop(&self, dropped: &mut (impl EgglogNode + 'static)) {
        with_pat_recorder::<PatMarker::TxMarker, _>(|pat_rec| pat_rec.on_drop(dropped));
    }
}

impl<PatMarker: SessionAwarePatRecMarker> NodeSetter for SessionPatRecFacade<PatMarker> {
    fn on_set(&self, node: &mut (impl EgglogNode + 'static)) {
        with_pat_recorder::<PatMarker::TxMarker, _>(|pat_rec| pat_rec.on_set(node));
    }
}

impl<PatMarker: SessionAwarePatRecMarker> Tx for SessionPatRecFacade<PatMarker> {
    fn send(&self, sended: TxCommand) {
        with_pat_recorder::<PatMarker::TxMarker, _>(|pat_rec| pat_rec.send(sended));
    }

    fn on_new(&self, node: &(impl EgglogNode + 'static)) {
        with_pat_recorder::<PatMarker::TxMarker, _>(|pat_rec| pat_rec.on_new(node));
    }

    fn on_func_set<'a, F: EgglogFunc>(
        &self,
        input: <F::Input as EgglogFuncInputs>::Ref<'a>,
        output: <F::Output as EgglogFuncOutput>::Ref<'a>,
    ) {
        with_pat_recorder::<PatMarker::TxMarker, _>(|pat_rec| pat_rec.on_func_set::<F>(input, output));
    }

    fn on_union(&self, node1: &(impl EgglogNode + 'static), node2: &(impl EgglogNode + 'static)) {
        with_pat_recorder::<PatMarker::TxMarker, _>(|pat_rec| pat_rec.on_union(node1, node2));
    }

    fn canonical_raw(&self, node1: &(impl EgglogNode + 'static)) -> egglog::Value {
        with_pat_recorder::<PatMarker::TxMarker, _>(|pat_rec| pat_rec.canonical_raw(node1))
    }
}

impl<PatMarker: SessionAwarePatRecMarker> PatRec for SessionPatRecFacade<PatMarker> {
    type MetaTy = <<PatMarker::TxMarker as SessionAwareTxMarker>::PatRecorder as PatRec>::MetaTy;

    fn on_new_query_leaf(&self, node: &(impl EgglogNode + 'static)) {
        with_pat_recorder::<PatMarker::TxMarker, _>(|pat_rec| pat_rec.on_new_query_leaf(node));
    }

    fn on_new_constraint(&self, constraint: impl crate::wrap::IntoConstraintFact) {
        with_pat_recorder::<PatMarker::TxMarker, _>(|pat_rec| pat_rec.on_new_constraint(constraint));
    }

    fn on_new_table_fact(&self, query_table: TableName, vars: Vec<(VarName, SortName)>) {
        with_pat_recorder::<PatMarker::TxMarker, _>(|pat_rec| pat_rec.on_new_table_fact(query_table, vars));
    }

    fn on_new_relation_fact(&self, query_table: TableName, vars: Vec<(VarName, SortName)>) {
        with_pat_recorder::<PatMarker::TxMarker, _>(|pat_rec| pat_rec.on_new_relation_fact(query_table, vars));
    }

    fn on_record_start(&self) {
        with_pat_recorder::<PatMarker::TxMarker, _>(|pat_rec| pat_rec.on_record_start());
    }

    fn on_record_end<T: PatRecSgl>(&self, pat_vars: &impl PatVars<T>) -> PatId {
        with_pat_recorder::<PatMarker::TxMarker, _>(|pat_rec| pat_rec.on_record_end(pat_vars))
    }

    fn pat2fact_builder(&self, pat_id: PatId) -> FactsBuilder {
        with_pat_recorder::<PatMarker::TxMarker, _>(|pat_rec| pat_rec.pat2fact_builder(pat_id))
    }

    fn on_ctx_insert<PR: PatRecSgl>(
        &self,
        inputs: Vec<crate::prelude::slotted::FuncValueMeta<Self>>,
        output: (
            crate::prelude::slotted::FuncName,
            egglog::Value,
            Option<Self::MetaTy>,
        ),
    ) {
        with_pat_recorder::<PatMarker::TxMarker, _>(|pat_rec| {
            pat_rec.on_ctx_insert::<PR>(inputs, output)
        });
    }

    fn on_ctx_union(
        &self,
        combo1: crate::prelude::slotted::FuncValueMeta<Self>,
        combo2: crate::prelude::slotted::FuncValueMeta<Self>,
    ) {
        with_pat_recorder::<PatMarker::TxMarker, _>(|pat_rec| pat_rec.on_ctx_union(combo1, combo2));
    }

    fn flush_pending(&self, egraph: &EGraph) -> bool {
        with_pat_recorder::<PatMarker::TxMarker, _>(|pat_rec| pat_rec.flush_pending(egraph))
    }
}
