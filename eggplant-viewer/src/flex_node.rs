use egui::{Color32, FontFamily, FontId, Pos2, Rect, Shape, Stroke, Vec2, epaint::TextShape};
use egui_graphs::{DisplayNode, NodeProps};
use itertools::Itertools;
use petgraph::{Directed, EdgeType, stable_graph::IndexType};
use std::iter;

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

    fn shapes(&mut self, ctx: &egui_graphs::DrawContext) -> Vec<egui::Shape> {
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

// use egui_graphs::egui::{Color32, Pos2, Shape, Stroke, Vec2};
use egui_graphs::{DefaultEdgeShape, DisplayEdge, DrawContext, EdgeProps, Node};

use crate::{EEdge, ElkNode};
// use petgraph::{EdgeType, stable_graph::IndexType};

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

#[derive(Clone, Debug)]
pub struct EEdgeShape {
    default_impl: DefaultEdgeShape,
    points: Vec<Pos2>,
}

impl From<EdgeProps> for EEdgeShape {
    fn from(props: EdgeProps) -> Self {
        Self {
            default_impl: DefaultEdgeShape::from(props),
            points: vec![],
        }
    }
}

impl<D: DisplayNode<Directed>> DisplayEdge<Directed, D> for EEdgeShape {
    fn shapes(
        &mut self,
        start: &Node<Directed, D>,
        end: &Node<Directed, D>,
        ctx: &DrawContext,
    ) -> Vec<egui::Shape> {
        let mut res = vec![];
        let (start, end) = (start.location(), end.location());
        let (x_dist, y_dist) = (end.x - start.x, end.y - start.y);
        let (dx, dy) = (x_dist / COLORS.len() as f32, y_dist / COLORS.len() as f32);
        let d_vec = Vec2::new(dx, dy);

        let mut stroke = Stroke::default();
        // let mut points_line;

        let start = Pos2::new(start.x, start.y);
        let start_iter = iter::once(&start);
        let end = Pos2::new(start.x, start.y);
        let end_iter = iter::once(&end);
        let start_inners_end = itertools::chain!(start_iter, self.points.iter(), end_iter);
        for line_chunk in start_inners_end.chunks(2).into_iter() {
            stroke = Stroke::new(self.default_impl.width, COLORS[6]);
            // points_line = vec![
            //     start + i as f32 * d_vec,
            //     end - (COLORS.len() - i - 1) as f32 * d_vec,
            // ];
            stroke.width = ctx.meta.canvas_to_screen_size(stroke.width);
            let mut line_chunk = line_chunk.map(|p| ctx.meta.canvas_to_screen_pos(*p));
            res.push(Shape::line_segment(
                [line_chunk.next().unwrap(), line_chunk.next().unwrap()],
                stroke,
            ));
        }
        let tip_dir = (end - *self.points.last().unwrap_or(&start)).normalized();

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

    fn update(&mut self, _: &egui_graphs::EdgeProps) {}

    fn is_inside(&self, start: &Node<Directed, D>, end: &Node<Directed, D>, pos: Pos2) -> bool {
        self.default_impl.is_inside(start, end, pos)
    }
}

/// rotates vector by angle
fn rotate_vector(vec: Vec2, angle: f32) -> Vec2 {
    let cos = angle.cos();
    let sin = angle.sin();
    Vec2::new(cos * vec.x - sin * vec.y, sin * vec.x + cos * vec.y)
}
