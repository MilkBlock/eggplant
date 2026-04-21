#[path = "support/pseudo_singleton_constant_prop.rs"]
mod pseudo_singleton_constant_prop;

fn main() {
    env_logger::init();
    pseudo_singleton_constant_prop::two_isolated_sessions_keep_separate_egraphs_with_handles();
    println!("constant_prop_pseudo_singleton passed");
}
