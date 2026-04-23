use eggplant::{prelude::*, tx_rx_vt_pr};
use std::fs;
use std::path::PathBuf;
use std::time::{SystemTime, UNIX_EPOCH};

#[eggplant::dsl]
enum RenderExpr {
    #[eggplant::typst("{name}")]
    #[eggplant::precedence(100)]
    Var { name: String },
    #[eggplant::typst("{n}")]
    #[eggplant::precedence(100)]
    Const { n: i64 },
    #[eggplant::typst("{lhs} + {rhs}")]
    #[eggplant::precedence(10)]
    Add { lhs: RenderExpr, rhs: RenderExpr },
    #[eggplant::typst("{lhs} * {rhs}")]
    #[eggplant::precedence(20)]
    Mul { lhs: RenderExpr, rhs: RenderExpr },
}

tx_rx_vt_pr!(RenderTx, RenderPatRec);

fn unique_svg_path() -> PathBuf {
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_nanos();
    std::env::temp_dir().join(format!("eggplant_extract_render_{nanos}.svg"))
}

#[test]
fn extract_node_to_typst_with_backend_renders_using_typst_templates_and_precedence() {
    let _ = env_logger::builder().is_test(true).try_init();
    RenderTx::sgl().reset_for_bench();

    let x = Var::<RenderTx>::new("x".to_owned());
    x.commit();
    let y = Var::<RenderTx>::new("y".to_owned());
    y.commit();
    let z = Var::<RenderTx>::new("z".to_owned());
    z.commit();

    let add_xy = Add::<RenderTx>::new(&x, &y);
    add_xy.commit();
    let mul = Mul::<RenderTx>::new(&add_xy, &z);
    mul.commit();

    let (typst, _cost) = RenderTx::extract_node_to_typst_with_backend(
        &mul,
        ExtractBackend::<eggplant::egglog::extract::TreeAdditiveCostModel>::cost_model(
            eggplant::egglog::extract::TreeAdditiveCostModel::default(),
        ),
    )
    .expect("typst rendering should succeed");

    assert_eq!(typst, "(x + y) * z");
}

#[test]
fn extract_node_to_svg_with_backend_writes_svg_file() {
    let _ = env_logger::builder().is_test(true).try_init();
    RenderTx::sgl().reset_for_bench();

    if std::process::Command::new("typst")
        .arg("--version")
        .status()
        .is_err()
    {
        eprintln!("skipping svg render test because `typst` CLI is unavailable");
        return;
    }

    let one = Const::<RenderTx>::new(1);
    one.commit();
    let two = Const::<RenderTx>::new(2);
    two.commit();
    let add = Add::<RenderTx>::new(&one, &two);
    add.commit();

    let output_path = unique_svg_path();
    let cost = RenderTx::extract_node_to_svg_with_backend(
        &add,
        ExtractBackend::<eggplant::egglog::extract::TreeAdditiveCostModel>::cost_model(
            eggplant::egglog::extract::TreeAdditiveCostModel::default(),
        ),
        &output_path,
    )
    .expect("svg rendering should succeed");

    let svg = fs::read_to_string(&output_path).expect("svg file should be readable");
    assert!(svg.contains("<svg"));
    assert!(cost > 0);

    let _ = fs::remove_file(output_path);
}
