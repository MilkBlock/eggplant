use eframe::CreationContext;
use egglog::{EGraph, SerializeConfig, Value};
use eggplant_egui_graphs::{ViewEdge, Graph, ViewNode};
use petgraph::prelude::StableGraph;
use std::{
    collections::HashMap,
    sync::{Arc, Mutex},
};

#[cfg(feature = "events")]
use crate::event_filters::EventFilters;
use crate::*;

impl<T: EGraphViewerSgl> EGraphApp<T> {
    pub fn new(cc: &CreationContext<'_>) -> Self {
        let settings_graph = settings::SettingsGraph::default();
        let tables = T::egraph()
            .lock()
            .unwrap()
            .serialize_tracing_raw(SerializeConfig::default());
        let _class2nodes: HashMap<Value, Vec<TblOffset>> =
            tables
                .iter()
                .fold(HashMap::default(), |mut acc, (_func, rows)| {
                    rows.iter().enumerate().for_each(|(tbl_offset, row)| {
                        acc.entry(row.output).or_default().push(tbl_offset)
                    });
                    acc
                });

        let mut g = Graph::new(StableGraph::default());

        // Create 10 nodes
        let nodes: Vec<_> = (0..10).map(|i| g.add_node(ViewNode::default())).collect();

        // Create 20 edges to build a complex connection pattern
        let edges = vec![
            (0, 1),
            (0, 2),
            (1, 3),
            (2, 4),
            (3, 5),
            (4, 6),
            (5, 7),
            (6, 8),
            (7, 9),
            (8, 9),
            (0, 5),
            (1, 6),
            (2, 7),
            (3, 8),
            (4, 9),
            (0, 8),
            (1, 9),
            (2, 5),
            (3, 6),
            (7, 8),
        ];

        for (from_idx, to_idx) in edges {
            g.add_edge(nodes[from_idx], nodes[to_idx], ViewEdge::default());
        }

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
            }
        }

        app
    }
}

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
