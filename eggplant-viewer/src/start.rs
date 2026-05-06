use eframe::CreationContext;
use egglog::{EGraph, SerializeConfig, Value, sort::Sort};
use egglog_numeric_id::NumericId;
use eggplant_egui_graphs::{
    ENode, EventHandler, FuncOffset, Graph, InnerPos, MaybeInner, ViewEdge, ViewNode,
};
use indexmap::IndexMap;
use petgraph::prelude::StableGraph;

#[cfg(feature = "events")]
use crate::event_filters::EventFilters;
use crate::*;

type SortName = String;
struct ValueWithCano {
    #[allow(unused)]
    value: Value,
    cano_value: Value,
    sort: SortName,
    offset: usize,
}

struct SerializedRow {
    inputs_complex: Vec<ValueWithCano>,
    basics: Vec<u32>,
    output: ValueWithCano,
}
impl EGraphApp {
    pub fn new(
        cc: &CreationContext<'_>,
        layout: DemoLayout,
        egraph: &EGraph,
        event_handler: Box<dyn EventHandle>,
    ) -> Self {
        let mut g = Graph::new(StableGraph::default());
        let serialized = egraph.serialize(SerializeConfig {
            include_temporary_functions: true,
            ..SerializeConfig::default()
        });
        let serialized_graph = serialized.egraph;
        let mut tables: IndexMap<String, Vec<SerializedRow>> = IndexMap::new();
        for (node_id, node) in &serialized_graph.nodes {
            let func = node.op.clone();
            let function = egraph
                .get_function(&func)
                .unwrap_or_else(|| panic!("can't find func {}", func));
            let row_offset = tables.get(&func).map_or(0, Vec::len);
            let output_sort = function.schema().output.clone();
            let output_value = egraph.class_id_to_value(&node.eclass);
            let mut inputs_complex = Vec::new();
            let mut basics = Vec::new();
            for (i, child) in node.children.iter().enumerate() {
                let sort = function.schema().input.get(i).unwrap();
                let child_value = egraph.class_id_to_value(serialized_graph.nid_to_cid(child));
                if sort.value_type().is_some() {
                    basics.push(child_value.rep());
                } else {
                    inputs_complex.push(ValueWithCano {
                        value: child_value.clone(),
                        cano_value: child_value,
                        sort: sort.name().to_string(),
                        offset: row_offset,
                    });
                }
            }
            let _ = node_id;
            tables.entry(func).or_default().push(SerializedRow {
                inputs_complex,
                basics,
                output: ValueWithCano {
                    value: output_value.clone(),
                    cano_value: output_value,
                    sort: output_sort.name().to_string(),
                    offset: row_offset,
                },
            });
        }
        let class2nodes: IndexMap<u32, Vec<(String, TblOffset)>> =
            tables
                .iter()
                .fold(IndexMap::default(), |mut acc, (func, rows)| {
                    rows.iter().enumerate().for_each(|(tbl_offset, row)| {
                        acc.entry(row.output.cano_value.rep())
                            .or_default()
                            .push((func.clone(), tbl_offset))
                    });
                    acc
                });

        // add nodes
        let mut cano_value2node_idx = IndexMap::new();
        let mut sort_offset2cano_value_and_maybe_inner = IndexMap::new();
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
                        let enode = trans_serialized_row_node(func.to_string(), *offset, row);
                        sort_offset2cano_value_and_maybe_inner.insert(
                            FuncOffset::new(row.output.sort.clone(), *offset),
                            (
                                row.output.cano_value.rep(),
                                MaybeInner::Inner {
                                    inner_pos: InnerPos {
                                        cano_value: row.output.cano_value.rep(),
                                        id: FuncOffset::new(
                                            enode.func_offset.func.to_string(),
                                            *offset,
                                        ),
                                        operand_idx: 0,
                                    },
                                },
                            ),
                        );
                        (enode.func_offset.func.to_string(), enode)
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
                    EventHandler {
                        event_handle: event_handler.dyn_clone(),
                    },
                );
                println!("{:?}", view_node);
                view_node
            });
            cano_value2node_idx.insert(cano_value.unwrap().rep(), node_idx);
        });
        // add edges
        tables.iter().for_each(|(func, rows)| {
            rows.iter().enumerate().for_each(|(offset, row)| {
                let (start_cano, maybe_inner) = sort_offset2cano_value_and_maybe_inner
                    .get(&FuncOffset::new(func.clone(), offset))
                    .unwrap();
                let start_node_idx = cano_value2node_idx.get(start_cano).unwrap();
                println!("checking {}{}", func, offset);
                for (i, input) in row.inputs_complex.iter().enumerate() {
                    println!("has complex edge {}{}", input.sort, input.offset);
                    let end = cano_value2node_idx.get(&input.cano_value.rep()).unwrap();
                    // println!("insert edge {:?}", maybe_inner);
                    g.add_edge(
                        *start_node_idx,
                        *end,
                        ViewEdge {
                            identifier: None,
                            start_maybe_inner: match &maybe_inner {
                                MaybeInner::Itself => panic!("start can't be eclass"),
                                MaybeInner::Inner { inner_pos } => MaybeInner::Inner {
                                    inner_pos: InnerPos {
                                        operand_idx: i,
                                        ..inner_pos.clone()
                                    },
                                },
                            },
                        },
                    );
                }
            });
        });
        println!(
            "value2 cano_value {:?}",
            sort_offset2cano_value_and_maybe_inner
        );

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
                edge_router_kind: eggplant_egui_graphs::EdgeRouterKind::Straight,
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
            selected_layout: layout,
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
            event_handler,
            replan_routes_once_pending: false,
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

fn trans_serialized_row_node(func: String, offset: usize, row: &SerializedRow) -> ENode {
    ENode {
        func_offset: FuncOffset { func, offset },
        cano_value: row.output.cano_value.rep(),
        operands_num: row.inputs_complex.len(),
        basics: row.basics.clone(),
        display_label: None,
        dsl_metadata: None,
    }
}
