use egui::{Rect, Vec2};
use serde::{Deserialize, Serialize};

use super::core::ExtraForce;
use crate::{DisplayEdge, DisplayNode, Graph};
use petgraph::Directed;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CenterGravityParams {
    pub c: f32,
}
impl Default for CenterGravityParams {
    fn default() -> Self {
        Self { c: 0.3 }
    }
}

#[derive(Debug, Default)]
pub struct CenterGravity;

impl ExtraForce for CenterGravity {
    type Params = CenterGravityParams;

    fn apply<Nd: DisplayNode<Directed>, Ed: DisplayEdge<Directed, Nd>>(
        params: &Self::Params,
        g: &Graph<Nd, Ed>,
        indices: &[petgraph::stable_graph::NodeIndex],
        disp: &mut [Vec2],
        area: Rect,
        _k: f32,
    ) {
        if params.c == 0.0 {
            return;
        }
        let center = area.center();
        for (vec_pos, &idx) in indices.iter().enumerate() {
            let pos = g.g().node_weight(idx).unwrap().location();
            let delta = center - pos;
            disp[vec_pos] += delta * params.c;
        }
    }
}
