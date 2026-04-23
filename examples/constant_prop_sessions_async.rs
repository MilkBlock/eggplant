#[path = "constant_prop_sessions.rs"]
#[allow(dead_code)]
mod constant_prop_sessions;

fn main() {
    env_logger::init();
    constant_prop_sessions::async_sessions_can_survive_yield_and_spawn();
    constant_prop_sessions::sync_run_can_override_outer_async_session();
    println!("constant_prop_sessions_async passed");
}
