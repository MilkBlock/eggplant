use eggplant_egui_graphs::{DisplayNode, NodeProps};
use egui::{Color32, FontFamily, FontId, Pos2, Rect, Shape, Stroke, Vec2, epaint::TextShape};
use petgraph::Directed;

#[derive(Clone, Debug)]
pub struct NodeShapeFlex {
    label: String,
    loc: Pos2,
    size_x: f32,
    size_y: f32,
}

impl From<NodeProps> for NodeShapeFlex {
    fn from(node_props: NodeProps) -> Self {
        Self {
            label: format!("{:?}", node_props.payload),
            loc: node_props.location(),
            size_x: 0.,
            size_y: 0.,
        }
    }
}

impl DisplayNode<Directed> for NodeShapeFlex {
    fn is_inside(&self, pos: Pos2) -> bool {
        let rect = Rect::from_center_size(self.loc, Vec2::new(self.size_x, self.size_y));

        rect.contains(pos)
    }

    fn closest_boundary_point(&self, dir: Vec2) -> Pos2 {
        find_intersection(self.loc, self.size_x / 2., self.size_y / 2., dir)
    }

    fn shapes(&mut self, ctx: &eggplant_egui_graphs::DrawContext) -> Vec<egui::Shape> {
        // find node center location on the screen coordinates
        let center = ctx.meta.canvas_to_screen_pos(self.loc);
        let color = ctx.ctx.style().visuals.text_color();

        // create label
        let galley = ctx.ctx.fonts(|f| {
            f.layout_no_wrap(
                // self.label.clone(),
                self.label.clone(),
                FontId::new(ctx.meta.canvas_to_screen_size(10.), FontFamily::Monospace),
                color,
            )
        });

        // we need to offset label by half its size to place it in the center of the rect
        let offset = Vec2::new(-galley.size().x / 2., -galley.size().y / 2.);

        // create the shape and add it to the layers
        let shape_label = TextShape::new(center + offset, galley, color);

        let rect = shape_label.visual_bounding_rect();
        let points = rect_to_points(rect);
        let shape_rect = Shape::convex_polygon(points, Color32::default(), Stroke::new(1., color));

        // update self size
        self.size_x = rect.size().x;
        self.size_y = rect.size().y;

        vec![shape_rect, shape_label.into()]
    }

    fn update(&mut self, state: &NodeProps) {
        // self.label.clone_from(&state.label);
        self.loc = state.location();
    }
}

fn find_intersection(center: Pos2, size_x: f32, size_y: f32, direction: Vec2) -> Pos2 {
    if (direction.x.abs() * size_y) > (direction.y.abs() * size_x) {
        // intersects left or right side
        let x = if direction.x > 0.0 {
            center.x + size_x / 2.0
        } else {
            center.x - size_x / 2.0
        };
        let y = center.y + direction.y / direction.x * (x - center.x);
        Pos2::new(x, y)
    } else {
        // intersects top or bottom side
        let y = if direction.y > 0.0 {
            center.y + size_y / 2.0
        } else {
            center.y - size_y / 2.0
        };
        let x = center.x + direction.x / direction.y * (y - center.y);
        Pos2::new(x, y)
    }
}

fn rect_to_points(rect: Rect) -> Vec<Pos2> {
    let top_left = rect.min;
    let bottom_right = rect.max;
    let top_right = Pos2::new(bottom_right.x, top_left.y);
    let bottom_left = Pos2::new(top_left.x, bottom_right.y);

    vec![top_left, top_right, bottom_right, bottom_left]
}

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

/// rotates vector by angle
fn rotate_vector(vec: Vec2, angle: f32) -> Vec2 {
    let cos = angle.cos();
    let sin = angle.sin();
    Vec2::new(cos * vec.x - sin * vec.y, sin * vec.x + cos * vec.y)
}
