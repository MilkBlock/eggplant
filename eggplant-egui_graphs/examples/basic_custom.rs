use eframe::{App, CreationContext, NativeOptions, run_native};
use eggplant_egui_graphs::{
    DefaultEdgeShape, DefaultGraphView, Graph, SettingsStyle, ViewEdge, ViewNode,
};
use egui::{Context, Pos2};
use petgraph::stable_graph::StableGraph;

pub struct BasicCustomApp {
    g: Graph<eggplant_egui_graphs::DefaultNodeShape, DefaultEdgeShape>,
}

impl BasicCustomApp {
    fn new(_: &CreationContext<'_>) -> Self {
        let mut g = Graph::new(StableGraph::default());

        let positions = vec![Pos2::new(0., 0.), Pos2::new(50., 0.), Pos2::new(0., 50.)];
        let mut idxs = Vec::with_capacity(positions.len());
        for position in positions {
            let idx =
                g.add_node_with_label_and_location(ViewNode::new(), position.to_string(), position);

            idxs.push(idx);
        }

        g.add_edge(idxs[0], idxs[1], ViewEdge::default());
        g.add_edge(idxs[1], idxs[2], ViewEdge::default());
        g.add_edge(idxs[2], idxs[0], ViewEdge::default());

        Self { g }
    }
}

impl App for BasicCustomApp {
    fn update(&mut self, ctx: &Context, _: &mut eframe::Frame) {
        egui::CentralPanel::default().show(ctx, |ui| {
            ui.add(
                &mut DefaultGraphView::new(&mut self.g)
                    .with_styles(&SettingsStyle::default().with_labels_always(true)),
            );
        });
    }
}

fn main() {
    let native_options = NativeOptions::default();
    run_native(
        "basic_custom",
        native_options,
        Box::new(|cc| Ok(Box::new(BasicCustomApp::new(cc)))),
    )
    .unwrap();
}
