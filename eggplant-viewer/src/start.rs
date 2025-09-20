use egglog::EGraph;
use std::sync::{Arc, Mutex};

pub fn view<T: EGraphViewerSgl>() -> Result<(), eframe::Error> {
    let native_options = eframe::NativeOptions::default();
    eframe::run_native(
        "egui_graphs demo",
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
