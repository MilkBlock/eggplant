# Pseudo-Singleton Review Checklist

> Scope:
> `examples/constant_prop_pseudo_singleton.rs`
> `examples/support/pseudo_singleton_runtime.rs`
> `examples/support/pseudo_singleton_constant_prop.rs`
> `tests/pseudo_singleton_constant_prop.rs`

This file records the issues raised during review, one by one, with an explicit checked status.

## Review Items

- [x] Issue 1: pattern recorder was globally shared across sessions.
  Problem:
  The first review found that `MyPatRec` still came from a true global singleton, so concurrent rule registration could mix pattern-recording state across sessions.
  Handling:
  `SessionState` now owns both the `TxRxVTPR` runtime and a per-session `PatRecorder`, and `MyPatRec::sgl()` routes through the active session.
  Evidence:
  `examples/support/pseudo_singleton_runtime.rs`
  Result:
  Fixed for this prototype.

- [x] Issue 2: rule registration was not reusable within one session.
  Problem:
  Repeated `register_constant_prop_rules()` calls in the same session used duplicate names and could panic.
  Handling:
  Registration is now cached per session, so repeated calls return the same `RuleSetId` instead of trying to add duplicate rulesets and rules.
  Evidence:
  `examples/support/pseudo_singleton_constant_prop.rs`
  `tests/pseudo_singleton_constant_prop.rs`
  Result:
  Fixed and covered by a focused regression test.

- [x] Issue 3: `Session::spawn` was not actually tested.
  Problem:
  An earlier threaded test still re-entered the session with `session.run(...)` inside the spawned closure, so it did not prove that `Session::spawn` itself installed the correct thread-local context.
  Handling:
  The threaded path now calls the active-session worker directly from inside `Session::spawn`, so the test depends on `spawn` providing the correct routing.
  Evidence:
  `examples/support/pseudo_singleton_runtime.rs`
  `examples/support/pseudo_singleton_constant_prop.rs`
  `tests/pseudo_singleton_constant_prop.rs`
  Result:
  Fixed and covered by a focused cross-thread test.

- [x] Issue 4: session routing is still thread-local, not task-local.
  Problem:
  The prototype still relies on thread-local active-session routing. Plain `std::thread::spawn` without re-entry and async runtimes that move work across threads remain unsupported by design.
  Handling:
  The runtime now supports explicit async entry points: `session.run_async(...)` and `session.spawn_async(...)`, in addition to the sync `session.run(...)` and `session.spawn(...)` APIs. Routing first checks the most recently-entered binding across task-local and thread-local scopes. This is verified for explicit wrapper usage, including `yield_now()` and mixed sync/async override paths. It does not claim ambient propagation into arbitrary async work that was not entered through these wrappers.
  Evidence:
  `examples/support/pseudo_singleton_runtime.rs`
  `examples/support/pseudo_singleton_constant_prop.rs`
  `tests/pseudo_singleton_constant_prop.rs`
  `examples/constant_prop_pseudo_singleton_async.rs`
  Result:
  Fixed for this prototype.

- [x] Issue 5: explicit `Box::leak` calls for runtimes and ruleset names.
  Problem:
  The first implementation used `Box::leak` for both session runtimes and generated names.
  Handling:
  The direct `Box::leak` usage was removed. Session state is now owned by `Arc<SessionState>`, and the constant-prop ruleset uses fixed static names plus per-session caching instead of unbounded generated-name storage.
  Evidence:
  `examples/support/pseudo_singleton_runtime.rs`
  `examples/support/pseudo_singleton_constant_prop.rs`
  Result:
  Fixed at the implementation level.

- [x] Issue 6: session-owned state still has no teardown path.
  Problem:
  Even without `Box::leak`, the registry still grows monotonically for the life of the process because sessions are never removed and interned names are kept as long as the session state lives.
  Handling:
  The support runtime now uses a static facade over reclaimable `Arc<SessionState>` ownership rather than a process-lifetime session registry. When session handles and in-flight task/thread context drop, the underlying state becomes reclaimable.
  Evidence:
  `examples/support/pseudo_singleton_runtime.rs`
  Result:
  Fixed for this prototype.

- [x] Issue 7: sync `Session::run(...)` inside an outer async session could still route to the outer async context.
  Problem:
  The re-review found that task-local context always won over thread-local context, so a sync `session.run(...)` entered from inside another session’s `run_async(...)` scope could still hit the wrong session.
  Handling:
  Session bindings now carry an entry order, and runtime lookup picks the most recently-entered binding across task-local and thread-local scopes.
  Evidence:
  `examples/support/pseudo_singleton_runtime.rs`
  `examples/support/pseudo_singleton_constant_prop.rs`
  `tests/pseudo_singleton_constant_prop.rs`
  Result:
  Fixed and covered by a mixed sync/async override test.

- [x] Issue 8: same-session concurrent rule registration could race.
  Problem:
  The re-review found that same-session callers could both miss the ruleset cache and both try to register the same ruleset concurrently.
  Handling:
  Rule registration is now guarded by a single per-session cache lock and uses atomic get-or-register behavior.
  Evidence:
  `examples/support/pseudo_singleton_runtime.rs`
  `examples/support/pseudo_singleton_constant_prop.rs`
  `tests/pseudo_singleton_constant_prop.rs`
  Result:
  Fixed and covered by a same-session concurrent registration test.

## Verification

- [x] `cargo test --test pseudo_singleton_constant_prop`
- [x] `cargo run --example constant_prop_pseudo_singleton`
- [x] `cargo run --example constant_prop_pseudo_singleton_async`

## Current Summary

- [x] Resolved in code: per-session recorder isolation
- [x] Resolved in code: same-session rule registration reuse
- [x] Resolved in code: direct `Session::spawn` coverage
- [x] Resolved in code: async task-local routing
- [x] Documented accurately: async support is explicit wrapper-based, not ambient task inheritance
- [x] Resolved in code: removal of explicit `Box::leak`
- [x] Resolved in code: reclaimable session-owned state without process-lifetime registry growth
- [x] Resolved in code: mixed sync/async session override ordering
- [x] Resolved in code: same-session concurrent registration safety
