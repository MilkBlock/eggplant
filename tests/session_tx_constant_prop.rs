#[path = "../examples/constant_prop_sessions.rs"]
#[allow(dead_code)]
mod constant_prop_sessions;

#[test]
fn tx_sessions_keep_egraphs_isolated() {
    constant_prop_sessions::two_isolated_sessions_keep_separate_egraphs_with_handles();
}

#[test]
fn tx_sessions_can_register_rules_twice_per_session() {
    constant_prop_sessions::registers_rules_twice_in_same_session();
}

#[test]
fn tx_sessions_support_explicit_cross_thread_reentry() {
    constant_prop_sessions::concurrent_sessions_can_register_rules_on_different_threads();
}

#[test]
fn tx_sessions_support_async_task_local_routing() {
    constant_prop_sessions::async_sessions_can_survive_yield_and_spawn();
}

#[test]
fn tx_sessions_sync_run_can_override_outer_async_session() {
    constant_prop_sessions::sync_run_can_override_outer_async_session();
}

#[test]
fn tx_sessions_same_session_concurrent_registration_is_safe() {
    constant_prop_sessions::same_session_concurrent_registration_is_safe();
}

#[test]
fn tx_sessions_nested_ruleset_registration_is_safe() {
    constant_prop_sessions::nested_ruleset_registration_is_safe();
}

#[test]
fn tx_sessions_same_key_nested_registration_fails_fast() {
    constant_prop_sessions::nested_same_key_registration_panics_instead_of_deadlocking();
}
