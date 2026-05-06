use eggplant::{
    prelude::*,
    tx_rx_vt_pr,
    wrap::{ActionSampleEvent, ActionSampleRecorder},
};

#[eggplant::dsl]
enum TraceExpr {
    TraceConst { n: i64 },
    TraceAdd { lhs: TraceExpr, rhs: TraceExpr },
}

#[eggplant::dsl]
enum RootExpr {
    TraceRoot { node: TraceExpr },
}

tx_rx_vt_pr!(SampleTx, SamplePatRec);

fn describe_event(event: &ActionSampleEvent) -> String {
    match event {
        ActionSampleEvent::Insert {
            event_id,
            effect_id,
            table,
            key_debug,
        } => format!(
            "{event_id}: insert {table}({}) effect={effect_id:?}",
            key_debug.join(", ")
        ),
        ActionSampleEvent::Union {
            event_id,
            effect_id,
            lhs_debug,
            rhs_debug,
        } => format!("{event_id}: union {lhs_debug} == {rhs_debug} effect={effect_id:?}"),
        ActionSampleEvent::Subsume {
            event_id,
            effect_id,
            table,
            key_debug,
        } => format!(
            "{event_id}: subsume {table}({}) effect={effect_id:?}",
            key_debug.join(", ")
        ),
        ActionSampleEvent::Remove {
            event_id,
            effect_id,
            table,
            key_debug,
        } => format!(
            "{event_id}: remove {table}({}) effect={effect_id:?}",
            key_debug.join(", ")
        ),
        ActionSampleEvent::DynamicUnknown {
            event_id,
            effect_id,
            reason,
        } => format!("{event_id}: dynamic_unknown {reason} effect={effect_id:?}"),
    }
}

fn main() {
    env_logger::init();

    let root = TraceRoot::<SampleTx>::new(&TraceAdd::new(&TraceConst::new(2), &TraceConst::new(3)));
    root.commit();

    let ruleset = SampleTx::new_ruleset("sample_action_trace");
    let recorder = ActionSampleRecorder::default();
    let handle = recorder.clone();
    SampleTx::add_rule_with_hook(
        "sample_action_trace_rule",
        ruleset,
        || {
            let expr = TraceExpr::query_leaf();
            let _root = TraceRoot::query(&expr);
            #[eggplant::pat_vars]
            struct Pat {
                expr: TraceExpr,
            }
            Pat::new(expr)
        },
        |ctx, pat| {
            // Once the pattern matches, record the concrete effects produced by the action body.
            let one = ctx.insert_trace_const(1);
            let grown = ctx.insert_trace_add(pat.expr, one);
            ctx.union(pat.expr, grown);
        },
        Box::new(recorder),
    );

    let report = SampleTx::run_ruleset(ruleset, RunConfig::Once);
    let matches = report
        .num_matches_per_rule
        .get("@sample_action_trace_rule")
        .copied()
        .unwrap_or(0);

    let trace = handle.trace();
    let insertions = trace
        .events
        .iter()
        .filter(|event| matches!(event, ActionSampleEvent::Insert { .. }))
        .count();
    let unions = trace
        .events
        .iter()
        .filter(|event| matches!(event, ActionSampleEvent::Union { .. }))
        .count();

    println!("matched rule executions: {matches}");
    println!("captured inserts: {insertions}");
    println!("captured unions: {unions}");
    println!("captured action effects:");
    for event in &trace.events {
        println!("  {}", describe_event(event));
    }

    println!("\nraw trace json:");
    println!("{}", serde_json::to_string_pretty(&trace).unwrap());
}
