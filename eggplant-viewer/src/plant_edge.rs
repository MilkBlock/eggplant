use eggplant_egui_graphs::{
    DefaultEdgeShape, DisplayEdge, DisplayNode, DrawContext, EdgeProps, MaybeInner, Node,
};
use egui::{Color32, Pos2, Shape, Stroke, Vec2};
use petgraph::Directed;

const TIP_ANGLE: f32 = std::f32::consts::TAU / 20.;
const TIP_SIZE: f32 = 50.;
const EDGE_COLOR: Color32 = Color32::WHITE;

#[derive(Clone)]
pub struct PlantEdgeShape {
    default_impl: DefaultEdgeShape,
}

impl From<EdgeProps> for PlantEdgeShape {
    fn from(props: EdgeProps) -> Self {
        Self {
            default_impl: DefaultEdgeShape::from(props),
        }
    }
}

impl<Nd: DisplayNode<Directed>> DisplayEdge<Directed, Nd> for PlantEdgeShape {
    fn shapes(
        &mut self,
        start: &Node<Directed, Nd>,
        start_maybe_inner: MaybeInner,
        end: &Node<Directed, Nd>,
        ctx: &DrawContext,
    ) -> Vec<egui::Shape> {
        let mut res = vec![];
        let start = match start_maybe_inner {
            MaybeInner::Itself => start.location(),
            MaybeInner::Inner {
                ty,
                enode_id,
                operand_idx,
            } => {
                match start.payload().enodes.get(&ty) {
                    Some(type_specified_enodes) => {
                        let m = type_specified_enodes
                            .iter()
                            .find(|x| x.id == enode_id)
                            .unwrap_or_else(|| panic!("{:?} {} enode not found ", ty, enode_id));
                        println!("start: {:?} ", m)
                    }
                    None => {
                        panic!("type {} not found ", ty)
                    }
                }
                start.location() - Vec2::new(0., 10.0)
            }
        };
        let end = end.location();
        let (x_dist, y_dist) = (end.x - start.x, end.y - start.y);
        let (dx, dy) = (x_dist, y_dist);

        let mut points_line;

        let mut stroke = Stroke::new(self.default_impl.width, EDGE_COLOR);
        points_line = vec![start, end];

        stroke.width = ctx.meta.canvas_to_screen_size(stroke.width);
        points_line = points_line
            .iter()
            .map(|p| ctx.meta.canvas_to_screen_pos(*p))
            .collect();
        res.push(Shape::line_segment(
            [points_line[0], points_line[1]],
            stroke,
        ));

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

    fn is_inside(&self, start: &Node<Directed, Nd>, end: &Node<Directed, Nd>, pos: Pos2) -> bool {
        self.default_impl.is_inside(start, end, pos)
    }
}

/// rotates vector by angle
fn rotate_vector(vec: Vec2, angle: f32) -> Vec2 {
    let cos = angle.cos();
    let sin = angle.sin();
    Vec2::new(cos * vec.x - sin * vec.y, sin * vec.x + cos * vec.y)
}
