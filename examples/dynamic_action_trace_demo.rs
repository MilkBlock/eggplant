use eggplant::prelude::*;
use eggplant::tx_rx_vt_pr;
use eggplant::wrap::{ActionSampleEvent, ActionSampleRecorder};
use serde::Serialize;
use std::path::PathBuf;

#[eggplant::dsl]
enum DemoExpr {
    TraceConst { n: i64 },
    TraceAdd { lhs: DemoExpr, rhs: DemoExpr },
    TraceMul { lhs: DemoExpr, rhs: DemoExpr },
}

#[eggplant::dsl]
enum DemoRoot {
    TraceRoot { node: DemoExpr },
}

tx_rx_vt_pr!(DemoTx, DemoPatRec);

#[derive(Serialize)]
struct NormalizedTrace {
    version: u32,
    events: Vec<NormalizedEvent>,
}

#[derive(Serialize)]
#[serde(tag = "kind")]
enum NormalizedEvent {
    #[serde(rename = "insert")]
    Insert {
        id: String,
        effect_id: Option<String>,
        table: String,
        key_debug: Vec<String>,
        rendered_label: Option<String>,
    },
    #[serde(rename = "union")]
    Union {
        id: String,
        effect_id: Option<String>,
        lhs_debug: String,
        rhs_debug: String,
        rendered_label: Option<String>,
    },
    #[serde(rename = "dynamic-unknown")]
    DynamicUnknown {
        id: String,
        effect_id: Option<String>,
        reason: String,
    },
}

fn main() {
    let output_path = std::env::args_os()
        .nth(1)
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from("dynamic_action_trace_demo.json"));
    let branch = std::env::args().nth(2).unwrap_or_else(|| "mul".to_owned());
    let use_mul = branch != "add";

    let root = TraceRoot::<DemoTx>::new(&TraceAdd::new(&TraceConst::new(5), &TraceConst::new(6)));
    root.commit();

    let ruleset = DemoTx::new_ruleset("dynamic_action_trace_demo");
    let recorder = ActionSampleRecorder::default();
    let handle = recorder.clone();

    DemoTx::add_rule_with_hook(
        "dynamic_action_trace_demo_rule",
        ruleset,
        || {
            let l = DemoExpr::query_leaf();
            let r = DemoExpr::query_leaf();
            let expr = TraceAdd::query(&l, &r);
            let _root = DemoRoot::query(&expr);
            #[eggplant::pat_vars]
            struct Pat {
                l: DemoExpr,
                r: DemoExpr,
                expr: TraceAdd,
            }
            Pat::new(l, r, expr)
        },
        move |ctx, pat| {
            if use_mul {
                let two = ctx.insert_trace_const(2);
                let mul = ctx.insert_trace_mul(pat.r, two);
                ctx.union(pat.expr, mul);
            } else {
                let one = ctx.insert_trace_const(1);
                let add = ctx.insert_trace_add(pat.l, one);
                ctx.union(pat.expr, add);
            }
        },
        Box::new(recorder),
    );

    DemoTx::run_ruleset(ruleset, RunConfig::Once);

    let trace = normalize_trace(handle.trace(), use_mul);
    let json = serde_json::to_string_pretty(&trace).expect("trace json should serialize");
    std::fs::write(&output_path, json).expect("trace json should be writable");

    println!(
        "wrote normalized dynamic action trace to {} (branch={}, events={})",
        output_path.display(),
        if use_mul { "mul" } else { "add" },
        trace.events.len()
    );
}

fn normalize_trace(trace: eggplant::wrap::ActionSampleTrace, use_mul: bool) -> NormalizedTrace {
    let expected = expected_effects(use_mul);
    let events = trace
        .events
        .into_iter()
        .enumerate()
        .filter_map(|event| normalize_event(event, &expected))
        .collect();
    NormalizedTrace {
        version: trace.version,
        events,
    }
}

fn normalize_event(
    (index, event): (usize, ActionSampleEvent),
    expected: &[ExpectedEffect],
) -> Option<NormalizedEvent> {
    let slot = expected.get(index);
    match event {
        ActionSampleEvent::Insert {
            event_id,
            table,
            key_debug,
            ..
        } => {
            let effect_id = slot.map(|entry| entry.effect_id.clone());
            let rendered_label = slot.map(|entry| entry.rendered_label.clone());
            Some(NormalizedEvent::Insert {
                id: event_id,
                effect_id,
                table,
                key_debug,
                rendered_label,
            })
        }
        ActionSampleEvent::Union {
            event_id,
            lhs_debug,
            rhs_debug,
            ..
        } => {
            let effect_id = slot.map(|entry| entry.effect_id.clone());
            let rendered_label = slot.map(|entry| entry.rendered_label.clone());
            Some(NormalizedEvent::Union {
                id: event_id,
                effect_id,
                lhs_debug,
                rhs_debug,
                rendered_label,
            })
        }
        ActionSampleEvent::DynamicUnknown {
            event_id, reason, ..
        } => Some(NormalizedEvent::DynamicUnknown {
            id: event_id,
            effect_id: slot.map(|entry| entry.effect_id.clone()),
            reason,
        }),
        ActionSampleEvent::Subsume { .. } | ActionSampleEvent::Remove { .. } => None,
    }
}

struct ExpectedEffect {
    effect_id: String,
    rendered_label: String,
}

fn expected_effects(use_mul: bool) -> Vec<ExpectedEffect> {
    if use_mul {
        vec![
            ExpectedEffect {
                effect_id: effect_id_for("ctx.insert_trace_const(2)"),
                rendered_label: "sample:const(2)".to_owned(),
            },
            ExpectedEffect {
                effect_id: effect_id_for("ctx.insert_trace_mul(pat.r, two)"),
                rendered_label: "sample:mul(r, 2)".to_owned(),
            },
            ExpectedEffect {
                effect_id: effect_id_for("ctx.union(pat.expr, mul)"),
                rendered_label: "sample:union(expr, mul(r, 2))".to_owned(),
            },
        ]
    } else {
        vec![
            ExpectedEffect {
                effect_id: effect_id_for("ctx.insert_trace_const(1)"),
                rendered_label: "sample:const(1)".to_owned(),
            },
            ExpectedEffect {
                effect_id: effect_id_for("ctx.insert_trace_add(pat.l, one)"),
                rendered_label: "sample:add(l, 1)".to_owned(),
            },
            ExpectedEffect {
                effect_id: effect_id_for("ctx.union(pat.expr, add)"),
                rendered_label: "sample:union(expr, add(l, 1))".to_owned(),
            },
        ]
    }
}

fn effect_id_for(snippet: &str) -> String {
    const SOURCE: &str = include_str!("dynamic_action_trace_demo.rs");
    let start = SOURCE.find(snippet).expect("demo snippet should exist");
    let end = start + snippet.len();
    format!("effect@{start}:{end}")
}
