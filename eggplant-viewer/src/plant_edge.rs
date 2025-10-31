use eggplant_egui_graphs::{
    DefaultEdgeShape, DisplayEdge, DisplayNode, DrawContext, EdgeProps, InnerPos, MaybeInner, Node,
};
use egui::{Color32, Pos2, Shape, Stroke, Vec2, epaint::CircleShape};
use itertools::Itertools;
use petgraph::Directed;

const TIP_ANGLE: f32 = std::f32::consts::TAU / 20.;
const TIP_SIZE: f32 = 7.;
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
        let (start, end) = match start_maybe_inner {
            MaybeInner::Itself => (start.location(), end.location()),
            MaybeInner::Inner {
                inner_pos:
                    InnerPos {
                        cano_value,
                        id,
                        operand_idx,
                    },
            } => {
                let enodes = &start.payload().enodes;
                match enodes.get(&id.func) {
                    Some(type_specified_enodes) => {
                        // coordinate y = reduce all nodes number before + i
                        let (i, _enode) = type_specified_enodes
                            .iter()
                            .find_position(|x| x.func_offset == id)
                            .unwrap_or_else(|| {
                                panic!("{:?} {} enode not found ", id.func, cano_value)
                            });
                        let s = start.payload().enodes.get_index_of(&id.func).unwrap();
                        // println!("inner ty {}", ty);
                        let reduced_nodes_num = enodes.iter().take(s).fold(0.8, |m, (_k, v)| {
                            // println!("added {k} with len {}", v.len());
                            m + v.len() as f32 + 0.5
                        });
                        let y = reduced_nodes_num + i as f32;
                        // println!("y = {}", y);
                        if start.id() == end.id() {
                            let start_loc = start.location()
                                + Vec2::new(0., 6.0) * (y) as f32
                                + Vec2::new(6., 0.) * operand_idx as f32;
                            (start_loc.clone(), start_loc)
                        } else {
                            (
                                start.location()
                                    + Vec2::new(0., 6.0) * (y) as f32
                                    + Vec2::new(6., 0.) * operand_idx as f32,
                                end.location(),
                            )
                        }
                    }
                    None => {
                        panic!("type {} not found ", id.func)
                    }
                }
            }
        };
        let (x_dist, y_dist) = (end.x - start.x, end.y - start.y);
        let (_dx, _dy) = (x_dist, y_dist);

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
        // dot it so that we can see which enode as start
        res.push(Shape::Circle(CircleShape::filled(
            points_line[0],
            ctx.meta.canvas_to_screen_size(self.default_impl.width * 1.),
            Color32::GOLD,
        )));

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
