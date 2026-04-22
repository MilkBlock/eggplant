#[path = "support/pseudo_singleton_constant_prop.rs"]
#[allow(dead_code)]
mod pseudo_singleton_constant_prop;

fn main() {
    env_logger::init();
    pseudo_singleton_constant_prop::async_sessions_can_survive_yield_and_spawn();
    pseudo_singleton_constant_prop::sync_run_can_override_outer_async_session();
    println!("constant_prop_pseudo_singleton_async passed");
}
