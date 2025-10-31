use eggplant_egui_graphs::{DisplayNode, NodeProps, ViewNode};
use egui::{Color32, FontFamily, FontId, Pos2, Rect, Shape, Stroke, Vec2, epaint::TextShape};
use petgraph::Directed;

#[derive(Clone, Debug)]
pub struct FlexNodeShape {
    payload: ViewNode,
    loc: Pos2,
    size_x: f32,
    size_y: f32,
}

impl From<NodeProps> for FlexNodeShape {
    fn from(node_props: NodeProps) -> Self {
        Self {
            loc: node_props.location(),
            size_x: 0.,
            size_y: 0.,
            payload: node_props.payload,
        }
    }
}

impl DisplayNode<Directed> for FlexNodeShape {
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
                "CLASS".to_string(),
                FontId::new(ctx.meta.canvas_to_screen_size(5.), FontFamily::Monospace),
                color,
            )
        });
        let painter = ctx.painter;
        // ctx.painter.text(pos, anchor, text, font_id, text_color)
        // let panel = egui::CentralPanel::default();
        // panel.show(&ctx.ctx, |ui| {
        //     for (func, enodes) in &self.payload.enodes {
        //         ui.label(format!("{}", func));
        //         for enode in enodes {
        //             ui.label(format!("{}{}", enode.ty, enode.id));
        //         }
        //     }
        // });
        // we need to offset label by half its size to place it in the center of the rect
        let offset = Vec2::new(-galley.size().x / 2., -galley.size().y / 2.);
        // create the shape and add it to the layers
        let shape_label = TextShape::new(center + offset, galley, color);
        let rect = shape_label.visual_bounding_rect();
        let mut points = rect_to_points(rect);
        let mut current_y = center.y;
        let mut enode_rect = vec![];
        for (func, enodes) in &self.payload.enodes {
            let _func_text = format!("{}", func);
            // painter.text(
            //     egui::pos2(center.x, current_y),
            //     egui::Align2::LEFT_TOP,
            //     func_text,
            //     FontId::new(ctx.meta.canvas_to_screen_size(40.), FontFamily::Monospace),
            //     COLORS[func.len() % 7], // 使用UI的文本颜色
            // );
            current_y += 5.;
            for enode in enodes {
                let enode_text = format!(
                    "{}{}{:?}",
                    enode.func_offset.func, enode.func_offset.offset, enode.basics
                );
                let rect = painter.text(
                    egui::pos2(center.x, current_y), // 缩进20像素
                    egui::Align2::LEFT_TOP,
                    enode_text,
                    FontId::new(ctx.meta.canvas_to_screen_size(5.), FontFamily::Monospace),
                    COLORS[func.len() % 7], // 使用UI的文本颜色
                );
                enode_rect.push(rect.clone());
                points.extend(rect_to_points(rect));
                current_y += rect.height();
            }

            // 在函数之间添加额外间距
            current_y += ctx.meta.canvas_to_screen_size(1.0);
        }

        let shape_rect = Shape::convex_polygon(points, Color32::default(), Stroke::new(1., color));
        // current_y = center.y;
        // for (func, enodes) in &self.payload.enodes {
        //     let _func_text = format!("{}", func);
        //     // painter.text(
        //     //     egui::pos2(center.x, current_y),
        //     //     egui::Align2::LEFT_TOP,
        //     //     func_text,
        //     //     FontId::new(ctx.meta.canvas_to_screen_size(40.), FontFamily::Monospace),
        //     //     COLORS[func.len() % 7], // 使用UI的文本颜色
        //     // );
        //     current_y += 5.;
        //     for enode in enodes {
        //         let enode_text = format!("{}{}", enode.func, enode.cano_value);
        //         let rect = painter.text(
        //             egui::pos2(center.x + 20., current_y),
        //             egui::Align2::LEFT_TOP,
        //             enode_text,
        //             FontId::new(ctx.meta.canvas_to_screen_size(40.), FontFamily::Monospace),
        //             COLORS[func.len() % 7],
        //         );
        //         enode_rect.push(rect.clone());
        //         current_y += 10.0;
        //     }

        //     // 在函数之间添加额外间距
        //     current_y += 8.0;
        // }

        // update self size
        self.size_x = rect.size().x;
        self.size_y = rect.size().y;

        vec![shape_rect, shape_label.into()] //.extend(enode_rect.iter().map(|rect| Shape::rect);
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
const COLORS: [Color32; 6] = [
    Color32::RED,
    Color32::from_rgb(255, 102, 0),
    Color32::YELLOW,
    Color32::GREEN,
    Color32::from_rgb(2, 216, 233),
    // Color32::BLUE,
    Color32::from_rgb(91, 10, 145),
];

/// rotates vector by angle
fn rotate_vector(vec: Vec2, angle: f32) -> Vec2 {
    let cos = angle.cos();
    let sin = angle.sin();
    Vec2::new(cos * vec.x - sin * vec.y, sin * vec.x + cos * vec.y)
}
