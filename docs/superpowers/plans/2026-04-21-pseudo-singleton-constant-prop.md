# Pseudo-Singleton Constant Prop Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a reusable example support module that keeps the `MyTx::...` proc-macro-facing interface while routing calls into multiple independent `TxRxVTPR` sessions, and demonstrate it with a constant-propagation example.

**Architecture:** Keep the proc-macro-generated node API unchanged by continuing to implement `SingletonGetter`, `TxSgl`, `RxSgl`, `WithPatRecSgl`, and related traits for a fake singleton type. Route `sgl()` through a thread-local current-session pointer into a small global session registry that owns multiple `TxRxVTPR` runtimes. Put the reusable routing code under `examples/support`, put the constant-prop DSL and helpers in a dedicated support file, and keep the top-level example focused on the reader-facing flow.

**Tech Stack:** Rust, existing `eggplant` proc macros, `TxRxVTPR`, `OnceLock`, `Mutex`, thread-local routing, example-backed integration test.

---

### Task 1: Add a failing integration test for multi-session isolation

**Files:**
- Create: `tests/pseudo_singleton_constant_prop.rs`
- Test: `tests/pseudo_singleton_constant_prop.rs`

- [ ] **Step 1: Write the failing test**

```rust
#[path = "../examples/support/pseudo_singleton_constant_prop.rs"]
mod pseudo_singleton_constant_prop;

#[test]
fn pseudo_singleton_constant_prop_keeps_sessions_isolated() {
    pseudo_singleton_constant_prop::two_isolated_sessions_keep_separate_egraphs();
}
```

- [ ] **Step 2: Run the test to verify it fails**

Run: `cargo test --test pseudo_singleton_constant_prop`
Expected: FAIL because `examples/support/pseudo_singleton_constant_prop.rs` does not exist yet.

- [ ] **Step 3: Commit the red test**

```bash
git add tests/pseudo_singleton_constant_prop.rs
git commit -m "test: add pseudo-singleton constant-prop isolation coverage"
```

### Task 2: Add the reusable pseudo-singleton router and constant-prop support

**Files:**
- Create: `examples/support/pseudo_singleton_runtime.rs`
- Create: `examples/support/pseudo_singleton_constant_prop.rs`
- Create: `examples/constant_prop_pseudo_singleton.rs`
- Modify: `tests/pseudo_singleton_constant_prop.rs`
- Test: `tests/pseudo_singleton_constant_prop.rs`

- [ ] **Step 1: Write the reusable router support**

```rust
pub type Runtime = eggplant::instances::tx_rx_vt_pr::TxRxVTPR;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct SessionId(usize);

pub fn new_session() -> SessionId { /* allocate Runtime::new() in a registry */ }
pub fn with_session<R>(session: SessionId, f: impl FnOnce() -> R) -> R { /* set current session */ }
pub fn current_session() -> SessionId { /* panic if unset */ }
pub fn current_runtime() -> &'static Runtime { /* return selected runtime */ }

pub struct MyTx {
    tx: Runtime,
}

impl eggplant::prelude::SingletonGetter for MyTx {
    type RetTy = Runtime;

    fn sgl() -> &'static Runtime {
        current_runtime()
    }
}

impl eggplant::wrap::NonPatRecSgl for MyTx {
    fn egraph() -> std::sync::Arc<std::sync::Mutex<eggplant::egglog::EGraph>> {
        Self::sgl().egraph.clone()
    }
}

eggplant::basic_patttern_recorder!(MyPatRec);
impl eggplant::wrap::WithPatRecSgl for MyTx {
    type PatRecSgl = MyPatRec;
}
impl eggplant::wrap::WithRxSgl for MyPatRec {
    type RxSgl = MyTx;
}
```

- [ ] **Step 2: Write the constant-prop support module on top of the router**

```rust
#[eggplant::dsl]
pub enum Expr {
    Const { num: i64 },
    Mul { l: Expr, r: Expr },
    Add { l: Expr, r: Expr },
}

pub fn register_constant_prop_rules() -> eggplant::prelude::RuleSetId { /* Add + Mul rules */ }
pub fn canonical_eq(lhs: &Expr<MyTx>, rhs_const: i64) -> bool { /* compare canonical values */ }
pub fn two_isolated_sessions_keep_separate_egraphs() {
    let left = new_session();
    let right = new_session();

    with_session(left, || { /* build 3*2 + 4, run rules, assert 10 */ });
    with_session(right, || { /* build 5*5 + 1, run rules, assert 26 */ });

    with_session(left, || { /* assert left did not become 26 */ });
    with_session(right, || { /* assert right did not become 10 */ });
}
```

- [ ] **Step 3: Write the top-level example that only demonstrates usage**

```rust
#[path = "support/pseudo_singleton_constant_prop.rs"]
mod pseudo_singleton_constant_prop;

fn main() {
    env_logger::init();
    pseudo_singleton_constant_prop::two_isolated_sessions_keep_separate_egraphs();
}
```

- [ ] **Step 4: Run the focused test to verify it passes**

Run: `cargo test --test pseudo_singleton_constant_prop`
Expected: PASS with one test green.

- [ ] **Step 5: Run the example to verify the reader-facing flow**

Run: `cargo run --example constant_prop_pseudo_singleton`
Expected: exits successfully after running two independent sessions through the same `MyTx` static interface.

- [ ] **Step 6: Commit the implementation**

```bash
git add \
  docs/superpowers/plans/2026-04-21-pseudo-singleton-constant-prop.md \
  examples/support/pseudo_singleton_runtime.rs \
  examples/support/pseudo_singleton_constant_prop.rs \
  examples/constant_prop_pseudo_singleton.rs \
  tests/pseudo_singleton_constant_prop.rs
git commit -m "feat: prototype pseudo-singleton constant-prop runtime"
```
