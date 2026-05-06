use eggplant::prelude::*;
use eggplant::tx_rx_vt_pr;
use eggplant::wrap::ActionSampleRecorder;
use std::path::PathBuf;

#[eggplant::dsl]
enum SampleExpr {
    TraceConst { n: i64 },
    TraceAdd { lhs: SampleExpr, rhs: SampleExpr },
}

#[eggplant::dsl]
enum SampleRoot {
    TraceRoot { node: SampleExpr },
}

tx_rx_vt_pr!(SampleTx, SamplePatRec);

fn main() {
    let output_path = std::env::args_os()
        .nth(1)
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from("sample_trace.json"));

    let root = TraceRoot::<SampleTx>::new(&TraceAdd::new(&TraceConst::new(2), &TraceConst::new(3)));
    root.commit();

    let ruleset = SampleTx::new_ruleset("sample_trace_json");
    let recorder = ActionSampleRecorder::default();
    let handle = recorder.clone();

    SampleTx::add_rule_with_hook(
        "sample_trace_json_rule",
        ruleset,
        || {
            let expr = SampleExpr::query_leaf();
            let _root = SampleRoot::query(&expr);
            #[eggplant::pat_vars]
            struct Pat {
                expr: SampleExpr,
            }
            Pat::new(expr)
        },
        |ctx, pat| {
            let one = ctx.insert_trace_const(1);
            let sum = ctx.insert_trace_add(pat.expr, one);
            ctx.union(pat.expr, sum);
        },
        Box::new(recorder),
    );

    SampleTx::run_ruleset(ruleset, RunConfig::Once);

    let trace = handle.trace();
    let json = serde_json::to_string_pretty(&trace).expect("trace json should serialize");
    std::fs::write(&output_path, json).expect("trace json should be writable");

    println!(
        "wrote sample trace json to {} (events={})",
        output_path.display(),
        trace.events.len()
    );
}
