use petgraph::Directed;

use crate::{DisplayEdge, DisplayNode, Graph, layouts::Layout};

use super::algorithm::ForceAlgorithm;

#[derive(Debug, Default)]
pub struct ForceDirected<A: ForceAlgorithm> {
    alg: A,
}

impl<A: ForceAlgorithm> Layout<A::State> for ForceDirected<A> {
    fn from_state(state: A::State) -> impl Layout<A::State> {
        Self {
            alg: A::from_state(state),
        }
    }

    fn next<Nd: DisplayNode<Directed>, Ed: DisplayEdge<Directed, Nd>>(
        &mut self,
        g: &mut Graph<Nd, Ed>,
        ui: &egui::Ui,
    ) {
        if g.node_count() == 0 {
            return;
        }

        self.alg.step(g, ui.ctx().screen_rect());
    }

    fn state(&self) -> A::State {
        self.alg.state()
    }
}
