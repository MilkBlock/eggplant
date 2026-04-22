#[path = "../examples/support/pseudo_singleton_constant_prop.rs"]
#[allow(dead_code)]
mod pseudo_singleton_constant_prop;

#[test]
fn pseudo_singleton_constant_prop_keeps_sessions_isolated() {
    pseudo_singleton_constant_prop::two_isolated_sessions_keep_separate_egraphs_with_handles();
}

#[test]
fn pseudo_singleton_constant_prop_can_register_rules_twice_per_session() {
    pseudo_singleton_constant_prop::registers_rules_twice_in_same_session();
}

#[test]
fn pseudo_singleton_constant_prop_supports_explicit_cross_thread_reentry() {
    pseudo_singleton_constant_prop::concurrent_sessions_can_register_rules_on_different_threads();
}

#[test]
fn pseudo_singleton_constant_prop_supports_async_task_local_routing() {
    pseudo_singleton_constant_prop::async_sessions_can_survive_yield_and_spawn();
}

#[test]
fn pseudo_singleton_constant_prop_sync_run_can_override_outer_async_session() {
    pseudo_singleton_constant_prop::sync_run_can_override_outer_async_session();
}

#[test]
fn pseudo_singleton_constant_prop_same_session_concurrent_registration_is_safe() {
    pseudo_singleton_constant_prop::same_session_concurrent_registration_is_safe();
}

#[test]
fn pseudo_singleton_constant_prop_nested_ruleset_registration_is_safe() {
    pseudo_singleton_constant_prop::nested_ruleset_registration_is_safe();
}

#[test]
fn pseudo_singleton_constant_prop_same_key_nested_registration_fails_fast() {
    pseudo_singleton_constant_prop::nested_same_key_registration_panics_instead_of_deadlocking();
}
