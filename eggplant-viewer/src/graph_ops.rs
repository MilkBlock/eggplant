use crate::{DemoGraph, MAX_EDGE_COUNT};
use petgraph::stable_graph::NodeIndex;

pub struct GraphActions<'a> {
    pub g: &'a mut DemoGraph,
}

impl GraphActions<'_> {
    pub fn add_edge(&mut self, a: NodeIndex, b: NodeIndex) {
        let edge_cnt = match self.g {
            DemoGraph::Directed(g) => g.edge_count(),
            DemoGraph::Undirected(g) => g.edge_count(),
        };
        if edge_cnt >= MAX_EDGE_COUNT {
            return;
        }
        match self.g {
            DemoGraph::Directed(g) => {
                g.add_edge(a, b, crate::EEdge {});
            }
            DemoGraph::Undirected(g) => {
                g.add_edge(a, b, ());
            }
        }
    }
    pub fn remove_edge(&mut self, a: NodeIndex, b: NodeIndex) {
        let edge_id_opt = match self.g {
            DemoGraph::Directed(g) => g.edges_connecting(a, b).next().map(|(eid, _)| eid),
            DemoGraph::Undirected(g) => g.edges_connecting(a, b).next().map(|(eid, _)| eid),
        };
        if let Some(edge_id) = edge_id_opt {
            match self.g {
                DemoGraph::Directed(g) => {
                    g.remove_edge(edge_id);
                }
                DemoGraph::Undirected(g) => {
                    g.remove_edge(edge_id);
                }
            }
        }
    }
    pub fn remove_node_by_idx(&mut self, idx: NodeIndex) {
        match self.g {
            DemoGraph::Directed(g) => {
                g.remove_node(idx);
            }
            DemoGraph::Undirected(g) => {
                g.remove_node(idx);
            }
        }
    }
}
