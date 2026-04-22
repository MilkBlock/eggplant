use edge::RainbowEdgeShape;
use eframe::{App, CreationContext, run_native};
use eggplant_egui_graphs::{DefaultNodeShape, Graph, GraphView, generate_simple_digraph};
use egui::Context;

pub struct RainbowEdgesApp {
    g: Graph<DefaultNodeShape, RainbowEdgeShape>,
}

impl RainbowEdgesApp {
    fn new(_: &CreationContext<'_>) -> Self {
        let g = generate_simple_digraph();
        Self { g: Graph::from(&g) }
    }
}

impl App for RainbowEdgesApp {
    fn update(&mut self, ctx: &Context, _: &mut eframe::Frame) {
        egui::CentralPanel::default().show(ctx, |ui| {
            ui.add(
                &mut GraphView::<DefaultNodeShape, RainbowEdgeShape>::new(&mut self.g)
                    .with_interactions(
                        &eggplant_egui_graphs::SettingsInteraction::default()
                            .with_dragging_enabled(true),
                    ),
            );
        });
    }
}

fn main() {
    let native_options = eframe::NativeOptions::default();
    run_native(
        "rainbow_edges",
        native_options,
        Box::new(|cc| Ok(Box::new(RainbowEdgesApp::new(cc)))),
    )
    .unwrap();
}

mod edge {
    use eggplant_egui_graphs::{
        DefaultEdgeShape, DisplayEdge, DisplayNode, DrawContext, EdgeProps, MaybeInner, Node,
    };
    use egui::{Color32, Pos2, Shape, Stroke, Vec2};
    use petgraph::Directed;

    const TIP_ANGLE: f32 = std::f32::consts::TAU / 30.;
    const TIP_SIZE: f32 = 15.;
    const COLORS: [Color32; 7] = [
        Color32::RED,
        Color32::from_rgb(255, 102, 0),
        Color32::YELLOW,
        Color32::GREEN,
        Color32::from_rgb(2, 216, 233),
        Color32::BLUE,
        Color32::from_rgb(91, 10, 145),
    ];

    #[derive(Clone)]
    pub struct RainbowEdgeShape {
        default_impl: DefaultEdgeShape,
    }

    impl From<EdgeProps> for RainbowEdgeShape {
        fn from(props: EdgeProps) -> Self {
            Self {
                default_impl: DefaultEdgeShape::from(props),
            }
        }
    }

    impl<Nd: DisplayNode<Directed>> DisplayEdge<Directed, Nd> for RainbowEdgeShape {
        fn shapes(
            &mut self,
            start: &Node<Directed, Nd>,
            start_maybe_inner: MaybeInner,
            end: &Node<Directed, Nd>,
            ctx: &DrawContext,
        ) -> Vec<egui::Shape> {
            let _ = start_maybe_inner;
            let mut res = vec![];
            let (start, end) = (start.location(), end.location());
            let (x_dist, y_dist) = (end.x - start.x, end.y - start.y);
            let (dx, dy) = (x_dist / COLORS.len() as f32, y_dist / COLORS.len() as f32);
            let d_vec = Vec2::new(dx, dy);

            let mut stroke = Stroke::default();
            let mut points_line;

            for (i, color) in COLORS.iter().enumerate() {
                stroke = Stroke::new(self.default_impl.width, *color);
                points_line = vec![
                    start + i as f32 * d_vec,
                    end - (COLORS.len() - i - 1) as f32 * d_vec,
                ];

                stroke.width = ctx.meta.canvas_to_screen_size(stroke.width);
                points_line = points_line
                    .iter()
                    .map(|p| ctx.meta.canvas_to_screen_pos(*p))
                    .collect();
                res.push(Shape::line_segment(
                    [points_line[0], points_line[1]],
                    stroke,
                ));
            }

            let tip_dir = (end - start).normalized();

            let arrow_tip_dir_1 = rotate_vector(tip_dir, TIP_ANGLE) * TIP_SIZE;
            let arrow_tip_dir_2 = rotate_vector(tip_dir, -TIP_ANGLE) * TIP_SIZE;

            let tip_start_1 = end - arrow_tip_dir_1;
            let tip_start_2 = end - arrow_tip_dir_2;

            let mut points_tip = vec![end, tip_start_1, tip_start_2];

            points_tip = points_tip
                .iter()
                .map(|p| ctx.meta.canvas_to_screen_pos(*p))
                .collect();

            res.push(Shape::convex_polygon(
                points_tip,
                stroke.color,
                Stroke::default(),
            ));

            res
        }

        fn update(&mut self, _: &eggplant_egui_graphs::EdgeProps) {}

        fn is_inside(
            &self,
            start: &Node<Directed, Nd>,
            end: &Node<Directed, Nd>,
            pos: Pos2,
        ) -> bool {
            self.default_impl.is_inside(start, end, pos)
        }
    }

    /// rotates vector by angle
    fn rotate_vector(vec: Vec2, angle: f32) -> Vec2 {
        let cos = angle.cos();
        let sin = angle.sin();
        Vec2::new(cos * vec.x - sin * vec.y, sin * vec.x + cos * vec.y)
    }
}
