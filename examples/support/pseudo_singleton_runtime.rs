#![allow(dead_code)]

use eggplant::egglog::EGraph;
use eggplant::instances::{pat_rec::PatRecorder, tx_rx_vt_pr::TxRxVTPR};
use eggplant::prelude::{RuleSetId, SingletonGetter};
use eggplant::wrap::{EgglogNode, EgglogTy, FactsBuilder, IntoConstraintFact, PatRec, PatRecSgl};
use std::cell::RefCell;
use std::collections::HashMap;
use std::future::Future;
use std::sync::{
    Arc, Condvar, Mutex,
    atomic::{AtomicUsize, Ordering},
};
use std::thread::ThreadId;

pub type Runtime = TxRxVTPR;

static NEXT_SESSION_ID: AtomicUsize = AtomicUsize::new(0);
static NEXT_BINDING_ORDER: AtomicUsize = AtomicUsize::new(0);

struct SessionState {
    id: usize,
    runtime: Runtime,
    pat_recorder: PatRecorder,
    rulesets: Mutex<HashMap<&'static str, RulesetRegistration>>,
    rulesets_cv: Condvar,
}

impl SessionState {
    fn new() -> Self {
        Self {
            id: NEXT_SESSION_ID.fetch_add(1, Ordering::Relaxed),
            runtime: Runtime::new(),
            pat_recorder: PatRecorder::new(),
            rulesets: Mutex::new(HashMap::new()),
            rulesets_cv: Condvar::new(),
        }
    }
}

#[derive(Clone, Copy)]
enum RulesetRegistration {
    Building { owner: ThreadId },
    Ready(RuleSetId),
}

#[derive(Clone)]
struct SessionBinding {
    state: Arc<SessionState>,
    order: usize,
}

impl SessionBinding {
    fn new(state: Arc<SessionState>) -> Self {
        Self {
            state,
            order: NEXT_BINDING_ORDER.fetch_add(1, Ordering::Relaxed),
        }
    }
}

tokio::task_local! {
    static CURRENT_SESSION_TASK: SessionBinding;
}

thread_local! {
    static CURRENT_SESSION_THREAD: RefCell<Option<SessionBinding>> = const { RefCell::new(None) };
}

fn current_state() -> Arc<SessionState> {
    let thread_binding = CURRENT_SESSION_THREAD.with(|slot| slot.borrow().clone());
    let task_binding = CURRENT_SESSION_TASK.try_with(Clone::clone).ok();

    match (thread_binding, task_binding) {
        (Some(thread), Some(task)) => {
            if thread.order > task.order {
                thread.state
            } else {
                task.state
            }
        }
        (Some(thread), None) => thread.state,
        (None, Some(task)) => task.state,
        (None, None) => panic!(
            "no pseudo-singleton session is active; enter via Session::run, Session::run_async, Session::spawn, or Session::spawn_async"
        ),
    }
}

#[derive(Clone)]
pub struct Session {
    state: Arc<SessionState>,
}

impl Session {
    pub fn raw(&self) -> usize {
        self.state.id
    }

    pub fn run<R>(&self, f: impl FnOnce() -> R) -> R {
        CURRENT_SESSION_THREAD.with(|slot| {
            let previous = slot.replace(Some(SessionBinding::new(Arc::clone(&self.state))));
            let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(f));
            slot.replace(previous);
            match result {
                Ok(value) => value,
                Err(payload) => std::panic::resume_unwind(payload),
            }
        })
    }

    pub async fn run_async<R>(&self, fut: impl Future<Output = R>) -> R {
        CURRENT_SESSION_TASK
            .scope(SessionBinding::new(Arc::clone(&self.state)), fut)
            .await
    }

    pub fn spawn<R: Send + 'static>(
        &self,
        f: impl FnOnce() -> R + Send + 'static,
    ) -> std::thread::JoinHandle<R> {
        let session = self.clone();
        std::thread::spawn(move || session.run(f))
    }

    pub fn spawn_async<R: Send + 'static>(
        &self,
        fut: impl Future<Output = R> + Send + 'static,
    ) -> tokio::task::JoinHandle<R> {
        let binding = SessionBinding::new(Arc::clone(&self.state));
        tokio::spawn(CURRENT_SESSION_TASK.scope(binding, fut))
    }
}

pub fn new_session() -> Session {
    Session {
        state: Arc::new(SessionState::new()),
    }
}

pub(super) fn current_session() -> Session {
    Session {
        state: current_state(),
    }
}

pub(super) fn get_or_register_ruleset(
    key: &'static str,
    build: impl FnOnce() -> RuleSetId,
) -> RuleSetId {
    let state = current_state();
    let current_thread = std::thread::current().id();

    loop {
        let mut rulesets = state.rulesets.lock().unwrap_or_else(|err| err.into_inner());
        match rulesets.get(key).copied() {
            Some(RulesetRegistration::Ready(ruleset)) => return ruleset,
            Some(RulesetRegistration::Building { owner }) => {
                if owner == current_thread {
                    drop(rulesets);
                    panic!("reentrant ruleset registration for key `{key}` is not supported");
                }
                let guard = state
                    .rulesets_cv
                    .wait(rulesets)
                    .unwrap_or_else(|err| err.into_inner());
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

                let mut rulesets = state.rulesets.lock().unwrap_or_else(|err| err.into_inner());
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

fn with_runtime<R>(f: impl FnOnce(&Runtime) -> R) -> R {
    let state = current_state();
    f(&state.runtime)
}

fn with_pat_recorder<R>(f: impl FnOnce(&PatRecorder) -> R) -> R {
    let state = current_state();
    f(&state.pat_recorder)
}

pub struct MyTxFacade;
static MY_TX_FACADE: MyTxFacade = MyTxFacade;

pub struct MyPatRecFacade;
static MY_PAT_REC_FACADE: MyPatRecFacade = MyPatRecFacade;

pub struct MyTx;

impl SingletonGetter for MyTx {
    type RetTy = MyTxFacade;

    fn sgl() -> &'static MyTxFacade {
        &MY_TX_FACADE
    }
}

impl eggplant::wrap::NonPatRecSgl for MyTx {
    fn egraph() -> std::sync::Arc<std::sync::Mutex<EGraph>> {
        with_runtime(|runtime| runtime.egraph.clone())
    }
}

pub struct MyPatRec;

impl SingletonGetter for MyPatRec {
    type RetTy = MyPatRecFacade;

    fn sgl() -> &'static MyPatRecFacade {
        &MY_PAT_REC_FACADE
    }
}

impl eggplant::wrap::WithPatRecSgl for MyTx {
    type PatRecSgl = MyPatRec;
}

impl eggplant::wrap::WithRxSgl for MyPatRec {
    type RxSgl = MyTx;
}

impl eggplant::wrap::NodeOwner for MyTxFacade {
    type OwnerSpecDataInNode<T: EgglogTy, V: eggplant::wrap::EgglogEnumVariantTy> = ();
}

impl eggplant::wrap::NodeDropper for MyTxFacade {}

impl eggplant::wrap::NodeSetter for MyTxFacade {
    fn on_set(&self, _node: &mut (impl EgglogNode + 'static)) {}
}

impl eggplant::wrap::VersionCtl for MyTxFacade {
    fn locate_latest(&self, node: eggplant::wrap::Sym) -> eggplant::wrap::Sym {
        with_runtime(|runtime| runtime.locate_latest(node))
    }

    fn locate_next(&self, node: eggplant::wrap::Sym) -> eggplant::wrap::Sym {
        with_runtime(|runtime| runtime.locate_next(node))
    }

    fn locate_prev(&self, node: eggplant::wrap::Sym) -> eggplant::wrap::Sym {
        with_runtime(|runtime| runtime.locate_prev(node))
    }

    fn set_latest(&self, node: &mut eggplant::wrap::Sym) {
        with_runtime(|runtime| runtime.set_latest(node))
    }

    fn set_next(&self, node: &mut eggplant::wrap::Sym) {
        with_runtime(|runtime| runtime.set_next(node))
    }

    fn set_prev(&self, node: &mut eggplant::wrap::Sym) {
        with_runtime(|runtime| runtime.set_prev(node))
    }
}

impl eggplant::wrap::Tx for MyTxFacade {
    fn send(&self, sended: eggplant::wrap::TxCommand) {
        with_runtime(|runtime| runtime.send(sended))
    }

    fn on_new(&self, node: &(impl EgglogNode + 'static)) {
        with_runtime(|runtime| runtime.on_new(node))
    }

    fn on_func_set<'a, F: eggplant::wrap::EgglogFunc>(
        &self,
        input: <F::Input as eggplant::wrap::EgglogFuncInputs>::Ref<'a>,
        output: <F::Output as eggplant::wrap::EgglogFuncOutput>::Ref<'a>,
    ) {
        with_runtime(|runtime| runtime.on_func_set::<F>(input, output))
    }

    fn on_union(&self, node1: &(impl EgglogNode + 'static), node2: &(impl EgglogNode + 'static)) {
        with_runtime(|runtime| runtime.on_union(node1, node2))
    }

    fn canonical_raw(&self, node1: &(impl EgglogNode + 'static)) -> eggplant::egglog::Value {
        with_runtime(|runtime| runtime.canonical_raw(node1))
    }
}

impl eggplant::wrap::TxCommit for MyTxFacade {
    fn on_stage<T: EgglogNode + ?Sized>(&self, node: &T) {
        with_runtime(|runtime| runtime.on_stage(node))
    }

    fn on_commit_op_hook<T: EgglogNode>(
        &self,
        node: &T,
        hook: Option<Box<dyn eggplant::wrap::RuleCtxHook>>,
    ) {
        with_runtime(|runtime| runtime.on_commit_op_hook(node, hook))
    }
}

impl eggplant::wrap::Rx for MyTxFacade {
    fn on_func_get<'a, F: eggplant::wrap::EgglogFunc>(
        &self,
        input: <F::Input as eggplant::wrap::EgglogFuncInputs>::Ref<'a>,
    ) -> F::Output {
        with_runtime(|runtime| runtime.on_func_get::<F>(input))
    }

    fn on_funcs_get<'a, 'b, F: eggplant::wrap::EgglogFunc>(
        &self,
        max_size: Option<usize>,
    ) -> Vec<(
        <F::Input as eggplant::wrap::EgglogFuncInputs>::Ref<'b>,
        <F::Output as eggplant::wrap::EgglogFuncOutput>::Ref<'b>,
    )> {
        with_runtime(|runtime| runtime.on_funcs_get::<F>(max_size))
    }

    fn on_pull_sym<T: EgglogTy>(&self, sym: eggplant::wrap::Sym) -> eggplant::wrap::SymLit {
        with_runtime(|runtime| runtime.on_pull_sym::<T>(sym))
    }

    fn on_pull_value<T: EgglogTy>(
        &self,
        value: eggplant::wrap::Value<T>,
    ) -> eggplant::wrap::SymLit {
        with_runtime(|runtime| runtime.on_pull_value(value))
    }
}

impl eggplant::wrap::rule::RuleRunner<MyPatRec> for MyTxFacade {
    fn add_rule<P: eggplant::wrap::PatVars<MyPatRec>>(
        &self,
        rule_name: &str,
        rule_set: eggplant::wrap::RuleSetId,
        pat: impl Fn() -> P,
        action: impl Fn(&eggplant::wrap::PRRuleCtx<MyPatRec>, &P::Valued)
        + Send
        + Sync
        + 'static
        + Clone,
        ctx_hook: Option<Box<dyn eggplant::wrap::RuleCtxHook>>,
    ) {
        with_runtime(|runtime| runtime.add_rule::<P>(rule_name, rule_set, pat, action, ctx_hook))
    }

    fn new_ruleset(&self, rule_set: &'static str) -> eggplant::wrap::RuleSetId {
        with_runtime(|runtime| {
            <Runtime as eggplant::wrap::rule::RuleRunner<MyPatRec>>::new_ruleset(runtime, rule_set)
        })
    }

    fn run_ruleset(
        &self,
        rule_set_id: eggplant::wrap::RuleSetId,
        run_config: eggplant::wrap::RunConfig,
    ) -> egglog_reports::RunReport {
        with_runtime(|runtime| {
            <Runtime as eggplant::wrap::rule::RuleRunner<MyPatRec>>::run_ruleset(
                runtime,
                rule_set_id,
                run_config,
            )
        })
    }

    fn value<T: EgglogNode>(&self, node: &T) -> eggplant::wrap::Value<T> {
        with_runtime(|runtime| {
            <Runtime as eggplant::wrap::rule::RuleRunner<MyPatRec>>::value(runtime, node)
        })
    }
}

impl eggplant::wrap::NodeOwner for MyPatRecFacade {
    type OwnerSpecDataInNode<T: EgglogTy, V: eggplant::wrap::EgglogEnumVariantTy> = u32;
}

impl eggplant::wrap::NodeDropper for MyPatRecFacade {
    fn on_drop(&self, dropped: &mut (impl EgglogNode + 'static)) {
        with_pat_recorder(|pat_recorder| pat_recorder.on_drop(dropped))
    }
}

impl eggplant::wrap::NodeSetter for MyPatRecFacade {
    fn on_set(&self, node: &mut (impl EgglogNode + 'static)) {
        with_pat_recorder(|pat_recorder| pat_recorder.on_set(node))
    }
}

impl eggplant::wrap::Tx for MyPatRecFacade {
    fn send(&self, sended: eggplant::wrap::TxCommand) {
        with_pat_recorder(|pat_recorder| pat_recorder.send(sended))
    }

    fn on_new(&self, node: &(impl EgglogNode + 'static)) {
        with_pat_recorder(|pat_recorder| pat_recorder.on_new(node))
    }

    fn on_func_set<'a, F: eggplant::wrap::EgglogFunc>(
        &self,
        input: <F::Input as eggplant::wrap::EgglogFuncInputs>::Ref<'a>,
        output: <F::Output as eggplant::wrap::EgglogFuncOutput>::Ref<'a>,
    ) {
        with_pat_recorder(|pat_recorder| pat_recorder.on_func_set::<F>(input, output))
    }

    fn on_union(&self, node1: &(impl EgglogNode + 'static), node2: &(impl EgglogNode + 'static)) {
        with_pat_recorder(|pat_recorder| pat_recorder.on_union(node1, node2))
    }

    fn canonical_raw(&self, node1: &(impl EgglogNode + 'static)) -> eggplant::egglog::Value {
        with_pat_recorder(|pat_recorder| pat_recorder.canonical_raw(node1))
    }
}

impl PatRec for MyPatRecFacade {
    type MetaTy = ();

    fn on_new_query_leaf(&self, node: &(impl EgglogNode + 'static)) {
        with_pat_recorder(|pat_recorder| pat_recorder.on_new_query_leaf(node))
    }

    fn on_new_constraint(&self, constraint: impl IntoConstraintFact) {
        with_pat_recorder(|pat_recorder| pat_recorder.on_new_constraint(constraint))
    }

    fn on_new_table_fact(
        &self,
        query_table: eggplant::wrap::TableName,
        vars: Vec<(eggplant::wrap::VarName, eggplant::wrap::SortName)>,
    ) {
        with_pat_recorder(|pat_recorder| pat_recorder.on_new_table_fact(query_table, vars))
    }

    fn on_new_relation_fact(
        &self,
        query_table: eggplant::wrap::TableName,
        vars: Vec<(eggplant::wrap::VarName, eggplant::wrap::SortName)>,
    ) {
        with_pat_recorder(|pat_recorder| pat_recorder.on_new_relation_fact(query_table, vars))
    }

    fn on_record_start(&self) {
        with_pat_recorder(|pat_recorder| pat_recorder.on_record_start())
    }

    fn on_record_end<T: PatRecSgl>(
        &self,
        pat_vars: &impl eggplant::wrap::PatVars<T>,
    ) -> eggplant::wrap::PatId {
        with_pat_recorder(|pat_recorder| pat_recorder.on_record_end(pat_vars))
    }

    fn pat2fact_builder(&self, pat_id: eggplant::wrap::PatId) -> FactsBuilder {
        with_pat_recorder(|pat_recorder| pat_recorder.pat2fact_builder(pat_id))
    }
}
