use crate::{Node, NodeProps, draw::drawer::DrawContext, elements::EdgeProps};
use egui::{Pos2, Shape, Vec2};
use petgraph::EdgeType;
use serde::{Deserialize, Serialize};

pub trait DisplayNode<Ty>: Clone + From<NodeProps>
where
    Ty: EdgeType,
{
    /// Returns the closest point on the shape boundary in the direction of dir.
    ///
    /// * `dir` - direction pointing from the shape center to the required boundary point.
    ///
    /// Could be used to snap the edge ends to the node.
    fn closest_boundary_point(&self, dir: Vec2) -> Pos2;

    /// Draws shapes of the node. If the node is interacted these shapes will be used for drawing on foreground layer, otherwise on background layer.
    /// Has mutable reference to itself for possibility to change internal state for the visualizations where this is important.
    ///
    /// * `ctx` - contains [`egui::Context`] and graph metadata.
    ///
    /// Use `ctx.meta` to properly scale and translate the shape.
    /// Use `ctx.painter` to have low level access to egui painting process.
    fn shapes(&mut self, ctx: &DrawContext) -> Vec<Shape>;

    /// Is called on every frame. Can be used for updating state of the implementation of [`DisplayNode`]
    fn update(&mut self, state: &NodeProps);

    /// Checks if the provided `pos` is inside the shape.
    ///
    /// * `pos` - position is in the canvas coordinates.
    ///
    /// Could be used to bind mouse events to the custom drawn nodes.
    fn is_inside(&self, pos: Pos2) -> bool;
}

// unique id for enodes
#[derive(Debug, Clone, Hash, PartialEq, Eq, Serialize, Deserialize)]
pub struct FuncOffset {
    pub func: String,
    pub offset: usize,
}
impl FuncOffset {
    pub fn new(id: String, offset: usize) -> Self {
        Self { func: id, offset }
    }
}
#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct InnerPos {
    pub cano_value: u32,
    pub id: FuncOffset, // id is (row number , value)
    pub operand_idx: usize,
}
#[derive(Debug, Serialize, Deserialize, Clone)]
pub enum MaybeInner {
    Itself,
    Inner { inner_pos: InnerPos },
}
pub trait DisplayEdge<Ty, D>: Clone + From<EdgeProps>
where
    Ty: EdgeType,
    D: DisplayNode<Ty>,
{
    /// Draws shapes of the edge. Uses [`DisplayNode`] implementation from node endpoints to get start and end coordinates using [`closest_boundary_point`](DisplayNode::closest_boundary_point).
    /// If the node is interacted these shapes will be used for drawing on foreground layer, otherwise on background layer.
    /// Has mutable reference to itself for possibility to change internal state for the visualizations where this is important.
    ///
    /// * `ctx` - contains [`egui::Context`] and graph metadata.
    /// * `start` and `end` - start and end points of the edge.
    ///
    /// Use `ctx.meta` to properly scale and translate the shape.
    /// Use `ctx.painter` to have low level access to egui painting process.
    fn shapes(
        &mut self,
        start: &Node<Ty, D>,
        start_mayber_inner: MaybeInner,
        end: &Node<Ty, D>,
        ctx: &DrawContext,
    ) -> Vec<Shape>;

    /// Is called on every frame. Can be used for updating state of the implementation of [`DisplayNode`]
    fn update(&mut self, state: &EdgeProps);

    /// Checks if the provided `pos` is inside the shape.
    ///
    /// * `start` - start node of the edge.
    /// * `end`   - end node of the edge.
    /// * `pos`   - position is in the canvas coordinates.
    ///
    /// Could be used to bind mouse events to the custom drawn nodes.
    fn is_inside(&self, start: &Node<Ty, D>, end: &Node<Ty, D>, pos: Pos2) -> bool;

    /// Provides optional extra bounds (in canvas coordinates) contributed by the edge beyond node bounds.
    /// Default implementation returns None (no extra bounds). Implementors can override to improve fit-to-screen.
    #[allow(unused_variables)]
    fn extra_bounds(&self, start: &Node<Ty, D>, end: &Node<Ty, D>) -> Option<(Pos2, Pos2)> {
        None
    }
}
