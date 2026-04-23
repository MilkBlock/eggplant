use eframe::{App, CreationContext, NativeOptions, run_native};
use eggplant_egui_graphs::{
    DefaultEdgeShape, DefaultNodeShape, Graph, GraphView, LayoutHierarchical,
    LayoutHierarchicalState, ViewEdge, ViewNode, to_graph,
};
use egui::Context;
use petgraph::stable_graph::StableGraph;

pub struct BasicApp {
    g: Graph<DefaultNodeShape, DefaultEdgeShape>,
}

impl BasicApp {
    fn new(_: &CreationContext<'_>) -> Self {
        let g = generate_graph();
        Self { g: to_graph(&g) }
    }
}

impl App for BasicApp {
    fn update(&mut self, ctx: &Context, _: &mut eframe::Frame) {
        egui::CentralPanel::default().show(ctx, |ui| {
            ui.add(&mut GraphView::<
                _,
                _,
                LayoutHierarchicalState,
                LayoutHierarchical,
            >::new(&mut self.g));
        });
    }
}

fn generate_graph() -> StableGraph<ViewNode, ViewEdge> {
    let mut g = StableGraph::new();

    let a = g.add_node(ViewNode::default());
    let b = g.add_node(ViewNode::default());
    let c = g.add_node(ViewNode::default());

    g.add_edge(a, b, ViewEdge::default());
    g.add_edge(b, c, ViewEdge::default());
    g.add_edge(c, a, ViewEdge::default());

    g
}

fn main() {
    run_native(
        "basic",
        NativeOptions::default(),
        Box::new(|cc| Ok(Box::new(BasicApp::new(cc)))),
    )
    .unwrap();
}
