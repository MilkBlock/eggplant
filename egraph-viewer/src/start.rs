pub fn start() -> Result<(), eframe::Error> {
    let native_options = eframe::NativeOptions::default();
    eframe::run_native(
        "egui_graphs demo",
        native_options,
        Box::new(|cc| Ok::<Box<dyn eframe::App>, _>(Box::new(crate::EGraphApp::new(cc)))),
    )
}
