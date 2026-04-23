use crate::{DisplayEdge, DisplayNode, Graph};
use egui::Rect;
use petgraph::Directed;

use super::super::layout::LayoutState;

/// A pluggable force-directed algorithm interface decoupled from the UI boilerplate.
///
/// The algorithm operates on a Graph and a viewport Rect and advances the layout by one step.
pub trait ForceAlgorithm: Default {
    type State: LayoutState + Clone;

    /// Construct from a state value (typically deserialized each frame).
    fn from_state(state: Self::State) -> Self;

    /// Advance the simulation by one step using the given viewport rectangle if needed.
    fn step<Nd: DisplayNode<Directed>, Ed: DisplayEdge<Directed, Nd>>(
        &mut self,
        g: &mut Graph<Nd, Ed>,
        view: Rect,
    );

    /// Return current state to be stored by the layout system.
    fn state(&self) -> Self::State;
}
