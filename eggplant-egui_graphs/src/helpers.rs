use crate::elements::IndexTy;
use crate::view_types::{ViewEdge, ViewNode};
use crate::{DisplayEdge, DisplayNode, Edge, Graph, Node};
use egui::Vec2;
use petgraph::{
    Directed, EdgeType,
    stable_graph::{DefaultIx, NodeIndex, StableGraph},
    visit::IntoNodeReferences,
};
use std::collections::HashMap;

/// Helper function which transforms [`petgraph::stable_graph::StableGraph`] into the [`super::Graph`] required by the [`super::GraphView`] widget.
///
/// The function creates a new `StableGraph` where nodes and edges are represented by [`super::Node`] and [`super::Edge`] respectively.
/// New nodes and edges are created with [`default_node_transform`] and [`default_edge_transform`] functions.
/// If you want to define custom transformation procedures (e.g. to use custom label for nodes), use [`to_graph_custom`] instead.
///
/// # Example
/// ```
/// use petgraph::stable_graph::StableGraph;
/// use eggplant_egui_graphs::{to_graph, DefaultNodeShape, DefaultEdgeShape, Graph};
/// use egui::Pos2;
///
/// let mut g: StableGraph<&str, &str> = StableGraph::new();
/// let node1 = g.add_node("A");
/// let node2 = g.add_node("B");
/// g.add_edge(node1, node2, "edge1");
///
/// let result: Graph<_, _, _, _, DefaultNodeShape, DefaultEdgeShape> = to_graph(&g);
///
/// assert_eq!(result.g().node_count(), 2);
/// assert_eq!(result.g().edge_count(), 1);
///
/// let mut indxs = result.g().node_indices();
/// let result_node1 = indxs.next().unwrap();
/// let result_node2 = indxs.next().unwrap();
/// assert_eq!(*result.g().node_weight(result_node1).unwrap().payload(), "A");
/// assert_eq!(*result.g().node_weight(result_node2).unwrap().payload(), "B");
///
/// assert_eq!(*result.g().edge_weight(result.g().edge_indices().next().unwrap()).unwrap().payload(), "edge1");
///
/// assert_eq!(*result.g().node_weight(result_node1).unwrap().label().clone(), format!("node {}", result_node1.index()));
/// assert_eq!(*result.g().node_weight(result_node2).unwrap().label().clone(), format!("node {}", result_node2.index()));
/// ```
pub fn to_graph<Nd: DisplayNode<Directed>, Ed: DisplayEdge<Directed, Nd>>(
    g: &StableGraph<ViewNode, ViewEdge, Directed, IndexTy>,
) -> Graph<Nd, Ed> {
    transform(g, &mut default_node_transform, &mut default_edge_transform)
}

/// The same as [`to_graph`], but allows to define custom transformation procedures for nodes and edges.
pub fn to_graph_custom<Nd: DisplayNode<Directed>, Ed: DisplayEdge<Directed, Nd>>(
    g: &StableGraph<ViewNode, ViewEdge, Directed, IndexTy>,
    mut node_transform: impl FnMut(&mut Node<Directed, Nd>),
    mut edge_transform: impl FnMut(&mut Edge<Directed, Nd, Ed>),
) -> Graph<Nd, Ed> {
    transform(g, &mut node_transform, &mut edge_transform)
}

fn transform<Nd: DisplayNode<Directed>, Ed: DisplayEdge<Directed, Nd>>(
    input: &StableGraph<ViewNode, ViewEdge, Directed, DefaultIx>,
    node_transform: &mut impl FnMut(&mut Node<Directed, Nd>),
    edge_transform: &mut impl FnMut(&mut Edge<Directed, Nd, Ed>),
) -> Graph<Nd, Ed> {
    let g_stable = StableGraph::<Node<Directed, Nd>, Edge<Directed, Nd, Ed>, Directed>::default();

    let mut g = Graph::new(g_stable);

    let nidx_by_input_nidx = input
        .node_references()
        .map(|(input_n_idx, input_n)| {
            (
                input_n_idx,
                g.add_node_custom(input_n.clone(), &mut *node_transform),
            )
        })
        .collect::<HashMap<NodeIndex<DefaultIx>, NodeIndex<DefaultIx>>>();

    input.edge_indices().for_each(|input_e_idx| {
        let (input_source_n_idx, input_target_n_idx) = input.edge_endpoints(input_e_idx).unwrap();
        let input_e = input.edge_weight(input_e_idx).unwrap();

        let input_source_n = *nidx_by_input_nidx.get(&input_source_n_idx).unwrap();
        let input_target_n = *nidx_by_input_nidx.get(&input_target_n_idx).unwrap();

        g.add_edge_custom(
            input_source_n,
            input_target_n,
            input_e.clone(),
            &mut *edge_transform,
        );
    });

    g
}

/// Calculates the size of the node in the direction of the given vector
pub fn node_size<Ty: EdgeType, Nd: DisplayNode<Ty>>(node: &Node<Ty, Nd>, dir: Vec2) -> f32
where
    Nd: DisplayNode<Ty>,
{
    let connector_left = <Nd as DisplayNode<Ty>>::closest_boundary_point(node.display(), dir);
    let connector_right = <Nd as DisplayNode<Ty>>::closest_boundary_point(node.display(), -dir);

    ((connector_right.to_vec2() - connector_left.to_vec2()) / 2.).length()
}

/// Default edge transform function. Keeps original data and creates a new edge.
pub fn default_edge_transform<Nd: DisplayNode<Directed>, Ed: DisplayEdge<Directed, Nd>>(
    edge: &mut Edge<Directed, Nd, Ed>,
) {
    edge.set_label(format!("edge {}", edge.id().index()));
}

/// Default node transform function. Keeps original data and creates a new node with a random location and
/// label equal to the index of the node in the graph.
pub fn default_node_transform<Nd: DisplayNode<Directed>>(node: &mut Node<Directed, Nd>) {
    node.set_label(format!("node {}", node.id().index()));
}

/// Simple digraph for usage in examples and tests.
pub fn generate_simple_digraph() -> StableGraph<ViewNode, ViewEdge, Directed> {
    let mut g = StableGraph::new();

    let a = g.add_node(ViewNode::default());
    let b = g.add_node(ViewNode::default());
    let c = g.add_node(ViewNode::default());

    g.add_edge(a, b, ViewEdge::default());
    g.add_edge(b, c, ViewEdge::default());
    g.add_edge(c, a, ViewEdge::default());

    g
}

/// Simple ungraph for usage in examples and tests.
pub fn generate_simple_ungraph() -> StableGraph<ViewNode, ViewEdge, Directed> {
    let mut g = StableGraph::<_, _, Directed>::default();

    let a = g.add_node(ViewNode::default());
    let b = g.add_node(ViewNode::default());
    let c = g.add_node(ViewNode::default());

    g.add_edge(a, b, ViewEdge::default());
    g.add_edge(b, c, ViewEdge::default());
    g.add_edge(c, a, ViewEdge::default());

    g
}

#[cfg(test)]
mod tests {
    use crate::DefaultEdgeShape;
    use crate::DefaultNodeShape;

    use super::*;
    use petgraph::Directed;

    #[test]
    fn test_to_graph_directed() {
        let mut user_g: StableGraph<ViewNode, ViewEdge, Directed> = StableGraph::new();
        let mut node1 = ViewNode::default();
        node1.set_identifier("Node1".to_string());
        let mut node2 = ViewNode::default();
        node2.set_identifier("Node2".to_string());
        let n1 = user_g.add_node(node1);
        let n2 = user_g.add_node(node2);
        user_g.add_edge(n1, n2, ViewEdge::default());

        let input_g: Graph<DefaultNodeShape, DefaultEdgeShape> = to_graph(&user_g);

        assert_eq!(user_g.node_count(), input_g.g().node_count());
        assert_eq!(user_g.edge_count(), input_g.g().edge_count());
        assert_eq!(user_g.is_directed(), input_g.is_directed());

        for (user_idx, input_idx) in input_g.g().node_indices().zip(user_g.node_indices()) {
            let user_n = user_g.node_weight(user_idx).unwrap();
            let input_n = input_g.g().node_weight(input_idx).unwrap();

            assert_eq!(input_n.payload().identifier(), user_n.identifier());
            assert_eq!(*input_n.label(), format!("node {}", user_idx.index()));

            assert!(!input_n.selected().is_some());
            assert!(!input_n.dragged());
        }
    }

    #[test]
    fn test_to_graph_undirected() {
        let mut user_g: StableGraph<ViewNode, ViewEdge, Directed> = StableGraph::default();
        let mut node1 = ViewNode::default();
        node1.set_identifier("Node1".to_string());
        let mut node2 = ViewNode::default();
        node2.set_identifier("Node2".to_string());
        let n1 = user_g.add_node(node1);
        let n2 = user_g.add_node(node2);
        user_g.add_edge(n1, n2, ViewEdge::default());

        // For undirected graphs, we need to use to_graph_custom with proper type handling
        let input_g: Graph<DefaultNodeShape, DefaultEdgeShape> =
            to_graph_custom(&user_g, default_node_transform, default_edge_transform);

        assert_eq!(user_g.node_count(), input_g.g().node_count());
        assert_eq!(user_g.edge_count(), input_g.g().edge_count());
        assert_eq!(user_g.is_directed(), input_g.is_directed());

        for (user_idx, input_idx) in input_g.g().node_indices().zip(user_g.node_indices()) {
            let user_n = user_g.node_weight(user_idx).unwrap();
            let input_n = input_g.g().node_weight(input_idx).unwrap();

            assert_eq!(input_n.payload().identifier(), user_n.identifier());
            assert_eq!(*input_n.label(), format!("node {}", user_idx.index()));

            assert!(!input_n.selected().is_some());
            assert!(!input_n.dragged());
        }
    }
}
