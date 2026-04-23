use eframe::{App, CreationContext, run_native};
use eggplant_egui_graphs::{
    DefaultEdgeShape, DefaultGraphView, DefaultNodeShape, Graph, generate_simple_digraph,
};
use egui::{Context, Window};

pub struct WindowApp {
    g: Graph<DefaultNodeShape, DefaultEdgeShape>,
}

impl WindowApp {
    fn new(_: &CreationContext<'_>) -> Self {
        let g = generate_simple_digraph();
        Self { g: Graph::from(&g) }
    }
}

impl App for WindowApp {
    fn update(&mut self, ctx: &Context, _: &mut eframe::Frame) {
        Window::new("windowed graph").show(ctx, |ui| {
            ui.add(&mut DefaultGraphView::new(&mut self.g));
        });
    }
}

fn main() {
    let native_options = eframe::NativeOptions::default();
    run_native(
        "window",
        native_options,
        Box::new(|cc| Ok(Box::new(WindowApp::new(cc)))),
    )
    .unwrap();
}
