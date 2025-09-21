use crate::{DemoGraph, MAX_EDGE_COUNT, MAX_NODE_COUNT};
use egui::Pos2;
use petgraph::stable_graph::{EdgeIndex, NodeIndex};
use rand::Rng;

pub struct GraphActions<'a> {
    pub g: &'a mut DemoGraph,
}

impl GraphActions<'_> {
    pub fn remove_edges(&mut self, n: u32) {
        for _ in 0..n {
            self.remove_random_edge();
        }
    }

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
    pub fn remove_random_edge(&mut self) {
        if let Some(eidx) = self.random_edge_idx() {
            let endpoints = match self.g {
                DemoGraph::Directed(g) => g.edge_endpoints(eidx),
                DemoGraph::Undirected(g) => g.edge_endpoints(eidx),
            };
            if let Some((a, b)) = endpoints {
                self.remove_edge(a, b);
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

    fn random_node_idx(&self) -> Option<NodeIndex> {
        let cnt = match &self.g {
            DemoGraph::Directed(g) => g.node_count(),
            DemoGraph::Undirected(g) => g.node_count(),
        };
        if cnt == 0 {
            return None;
        }
        let idx = rand::rng().random_range(0..cnt);
        match &self.g {
            DemoGraph::Directed(g) => g.g().node_indices().nth(idx),
            DemoGraph::Undirected(g) => g.g().node_indices().nth(idx),
        }
    }
    fn random_edge_idx(&self) -> Option<EdgeIndex> {
        let cnt = match &self.g {
            DemoGraph::Directed(g) => g.edge_count(),
            DemoGraph::Undirected(g) => g.edge_count(),
        };
        if cnt == 0 {
            return None;
        }
        let idx = rand::rng().random_range(0..cnt);
        match &self.g {
            DemoGraph::Directed(g) => g.g().edge_indices().nth(idx),
            DemoGraph::Undirected(g) => g.g().edge_indices().nth(idx),
        }
    }
}
