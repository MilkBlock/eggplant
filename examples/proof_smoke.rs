use egglog::EGraph;

fn main() {
    let mut egraph = EGraph::new_with_proofs();
    assert!(egraph.are_proofs_enabled(), "proofs should be enabled");

    let program = r#"
        (datatype Expr
          (Num i64))
        (let $x (Num 1))
        (prove (= $x $x))
    "#;

    egraph
        .parse_and_run_program(Some("proof_smoke".to_string()), program)
        .expect("proof smoke program should run");
    println!("proof smoke passed");
}
