use eframe::CreationContext;
use egglog::{EGraph, RawEGraphNode, SerializeConfig, Value};
use eggplant_egui_graphs::{ENode, Graph, InnerPos, MaybeInner, ViewEdge, ViewNode};
use indexmap::IndexMap;
use petgraph::prelude::StableGraph;
use std::{
    collections::HashMap,
    sync::{Arc, Mutex},
};

#[cfg(feature = "events")]
use crate::event_filters::EventFilters;
use crate::*;

struct ValueWithCanno {
    value: Value,
    cano_value: Value,
}
struct ValueMeta {
    input: Value,
    cano_input: Value,
    inner_pos: InnerPos,
}

impl<T: EGraphViewerSgl> EGraphApp<T> {
    pub fn new(cc: &CreationContext<'_>) -> Self {
        let mut g = Graph::new(StableGraph::default());
        let tables = {
            let egraph = T::egraph().clone();
            let egraph = egraph.lock().unwrap();
            let tables = egraph.serialize_raw(SerializeConfig::default());
            let tables = tables
                .iter()
                .map(|(k, v)| {
                    (
                        k.clone(),
                        v.iter()
                            .map(|node| RawEGraphNode {
                                inputs: node
                                    .inputs
                                    .iter()
                                    .enumerate()
                                    .filter_map(|(i, value)| {
                                        let sort = &egraph
                                            .get_function(&k)
                                            .unwrap_or_else(|| panic!("can't find func {}", k))
                                            .schema()
                                            .input
                                            .get(i)
                                            .unwrap();
                                        if egraph.is_base_sort(sort) {
                                            None
                                        } else {
                                            let cano_value =
                                                egraph.get_canonical_value(*value, sort);
                                            println!(
                                                "canno value of {}{} is {}",
                                                k,
                                                value.rep(),
                                                cano_value.rep()
                                            );
                                            Some(ValueWithCanno {
                                                value: *value,
                                                cano_value,
                                            })
                                        }
                                    })
                                    .collect(),
                                output: ValueWithCanno {
                                    value: node.output,
                                    cano_value: egraph.get_canonical_value(
                                        node.output,
                                        &egraph
                                            .get_function(&k)
                                            .unwrap_or_else(|| panic!("can't find func {}", k))
                                            .schema()
                                            .output,
                                    ),
                                },
                                term: node.term,
                                subsumed: node.subsumed,
                                class_name: node.class_name.clone(),
                                node_name: node.node_name.clone(),
                            })
                            .collect::<Vec<RawEGraphNode<_, _>>>(),
                    )
                })
                .collect::<HashMap<String, Vec<_>>>();
            tables
        };
        let class2nodes: IndexMap<Value, Vec<(String, TblOffset)>> =
            tables
                .iter()
                .fold(IndexMap::default(), |mut acc, (func, rows)| {
                    rows.iter().enumerate().for_each(|(tbl_offset, row)| {
                        acc.entry(row.output.cano_value)
                            .or_default()
                            .push((func.clone(), tbl_offset))
                    });
                    acc
                });
        println!("{:?}", class2nodes);

        // add nodes
        let mut cano_value2node_idx = IndexMap::new();
        let mut value2cano_value_and_maybe_inner = IndexMap::new();
        class2nodes.iter().for_each(|(k, v)| {
            let mut cano_value = None;
            let node_idx = g.add_node({
                let ty_enode_list: Vec<(String, ENode)> = v
                    .iter()
                    .map(|(func, offset)| {
                        let rows = tables
                            .get(func)
                            .unwrap_or_else(|| panic!("func {} not found", func));
                        let row = rows
                            .get(*offset)
                            .unwrap_or_else(|| panic!("row {} in func {} not found", offset, func));
                        cano_value = Some(row.output.cano_value);
                        let enode = trans_raw_egraph_node(func.to_string(), row);
                        value2cano_value_and_maybe_inner.insert(
                            row.output.value.rep(),
                            (
                                row.output.cano_value.rep(),
                                MaybeInner::Inner {
                                    inner_pos: InnerPos {
                                        ty: func.clone(),
                                        cano_value: row.output.cano_value.rep(),
                                        value: row.output.value.rep(),
                                        operand_idx: 0,
                                    },
                                },
                            ),
                        );
                        (enode.func.to_string(), enode)
                    })
                    .collect();
                let mut enodes_of_one_eclass: IndexMap<String, Vec<ENode>> = IndexMap::default();
                for (ty, enode) in ty_enode_list {
                    enodes_of_one_eclass.entry(ty).or_default().push(enode);
                }
                let view_node = ViewNode::new(
                    Some(format!("c{:?}", k)),
                    enodes_of_one_eclass,
                    cano_value.expect("no node in this class").rep(),
                );
                println!("{:?}", view_node);
                view_node
            });
            cano_value2node_idx.insert(cano_value.unwrap().rep(), node_idx);
        });
        // add edges
        tables.iter().for_each(|(func, rows)| {
            rows.iter().for_each(|row| {
                let (start_cano, maybe_inner) = value2cano_value_and_maybe_inner
                    .get(&row.output.value.rep())
                    .unwrap();
                let start_node_idx = cano_value2node_idx.get(start_cano).unwrap();
                for input in &row.inputs {
                    let end = cano_value2node_idx.get(&input.cano_value.rep()).unwrap();
                    println!("insert edge {:?}", maybe_inner);
                    g.add_edge(
                        *start_node_idx,
                        *end,
                        ViewEdge {
                            identifier: None,
                            start_maybe_inner: maybe_inner.clone(),
                        },
                    );
                }
            });
        });
        println!("value2 cano_value {:?}", value2cano_value_and_maybe_inner);

        let settings_graph = settings::SettingsGraph::default();
        #[cfg(all(feature = "events", not(target_arch = "wasm32")))]
        let (event_publisher, event_consumer) = crate::unbounded();
        #[cfg(all(feature = "events", target_arch = "wasm32"))]
        let events_buf: Rc<RefCell<Vec<Event>>> = Rc::new(RefCell::new(Vec::new()));

        #[allow(unused_mut)]
        let mut app = Self {
            g: DemoGraph::Directed(g),
            settings_graph,
            settings_interaction: settings::SettingsInteraction::default(),
            settings_navigation: settings::SettingsNavigation::default(),
            settings_style: settings::SettingsStyle {
                labels_always: false,
                edge_deemphasis: true,
            },
            metrics: MetricsRecorder::new(),
            // Start with side panel hidden by default
            show_sidebar: false,
            #[cfg(not(feature = "events"))]
            copy_tip_until: None,
            #[cfg(feature = "events")]
            pan: [0.0, 0.0],
            #[cfg(feature = "events")]
            zoom: 1.0,
            #[cfg(feature = "events")]
            last_events: Vec::new(),
            #[cfg(all(feature = "events", not(target_arch = "wasm32")))]
            event_publisher,
            #[cfg(all(feature = "events", not(target_arch = "wasm32")))]
            event_consumer,
            #[cfg(all(feature = "events", target_arch = "wasm32"))]
            events_buf,
            #[cfg(feature = "events")]
            event_filters: EventFilters::default(),
            dark_mode: cc.egui_ctx.style().visuals.dark_mode,
            show_debug_overlay: true,
            show_keybindings_overlay: false,
            keybindings_just_opened: false,
            reset_requested: false,
            drag_hover_graph: false,
            status: StatusQueue::new(),
            // selected_layout: DemoLayout::FruchtermanReingold,
            selected_layout: DemoLayout::Force,
            typing_in_input: false,
            show_export_modal: false,
            export_include_layout: true,
            export_include_graph: true,
            export_include_positions: false,
            export_destination: ExportDestination::File,
            export_filename: crate::util::default_export_filename(),
            pending_layout: None,
            right_tab: RightTab::Playground,
            user_uploads: Vec::new(),
            #[cfg(target_arch = "wasm32")]
            web_upload_buf: Rc::new(RefCell::new(Vec::new())),
            fit_to_screen_once_pending: false,
            pan_to_graph_pending: false,
            _p: PhantomData,
        };

        // Web: if URL hash contains g=<example_name>, load that example graph automatically
        #[cfg(target_arch = "wasm32")]
        {
            if let Some(name) = crate::web_hash_get_param("g") {
                if let Some(data) = crate::web_lookup_example_asset(&name) {
                    app.load_graph_from_str(&name, data);
                }
            };
        }

        app
    }
}

fn trans_raw_egraph_node(
    func: String,
    row: &egglog::RawEGraphNode<ValueWithCanno, ValueWithCanno>,
) -> ENode {
    ENode {
        func: func,
        cano_value: row.output.cano_value.rep(),
        operands_num: row.inputs.len(),
        value: row.output.value.rep(),
    }
}
use egglog::NumericId;

pub fn view<T: EGraphViewerSgl>() -> Result<(), eframe::Error> {
    let native_options = eframe::NativeOptions::default();
    eframe::run_native(
        "eggplant_egui_graphs demo",
        native_options,
        Box::new(|cc| Ok::<Box<dyn eframe::App>, _>(Box::new(crate::EGraphApp::<T>::new(cc)))),
    )
}

pub trait EGraphViewerSgl {
    fn egraph() -> Arc<Mutex<EGraph>>;
    fn view() -> Result<(), eframe::Error>;
}

pub trait EGraphViewer {
    fn egraph(&self) -> Arc<Mutex<EGraph>>;
}
