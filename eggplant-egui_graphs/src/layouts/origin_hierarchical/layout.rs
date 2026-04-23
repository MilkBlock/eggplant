use std::collections::{HashMap, HashSet, VecDeque};

use egui::Pos2;
use petgraph::{
    Directed,
    Direction::{Incoming, Outgoing},
    stable_graph::{DefaultIx, NodeIndex},
};
use serde::{Deserialize, Serialize};

use crate::{
    DisplayEdge, DisplayNode, Graph,
    layouts::{Layout, LayoutState},
};

/// Orientation of the hierarchical layout.
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, Default)]
pub enum Orientation {
    /// Levels grow downward (classic top-down tree). Rows are vertical steps.
    #[default]
    TopDown,
    /// Levels grow to the right. Rows are horizontal steps.
    LeftRight,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct State {
    /// Run only once unless reset via GraphView::reset_layout or by setting `triggered = false`.
    pub triggered: bool,
    /// Distance between levels (rows). Interpreted as Y step for TopDown and X step for LeftRight.
    pub row_dist: f32,
    /// Distance between siblings/columns. Interpreted as X step for TopDown and Y step for LeftRight.
    pub col_dist: f32,
    /// Center a parent above/beside the span of its children.
    pub center_parent: bool,
    /// Layout orientation.
    pub orientation: Orientation,
}

impl Default for State {
    fn default() -> Self {
        // This mirrors the earlier/simple behavior: same defaults as current hierarchical,
        // but placement uses a level-by-level (BFS) assignment with simple centering.
        Self {
            triggered: false,
            row_dist: 50.0,
            col_dist: 50.0,
            center_parent: false,
            orientation: Orientation::TopDown,
        }
    }
}

impl LayoutState for State {}

#[derive(Debug, Default)]
pub struct OriginHierarchical {
    state: State,
}

impl Layout<State> for OriginHierarchical {
    fn next<Nd: DisplayNode<Directed>, Ed: DisplayEdge<Directed, Nd>>(
        &mut self,
        g: &mut Graph<Nd, Ed>,
        _: &egui::Ui,
    ) {
        if self.state.triggered {
            return;
        }

        // 1) Level assignment via Kahn-like BFS (original simple approach)
        let mut indegree: HashMap<NodeIndex<DefaultIx>, usize> = HashMap::new();
        let mut level: HashMap<NodeIndex<DefaultIx>, usize> = HashMap::new();
        for n in g.g().node_indices() {
            let deg = g.g().neighbors_directed(n, Incoming).count();
            indegree.insert(n, deg);
            if deg == 0 {
                level.insert(n, 0);
            }
        }
        let mut q: VecDeque<NodeIndex<DefaultIx>> = VecDeque::new();
        for (&n, &deg) in indegree.iter() {
            if deg == 0 {
                q.push_back(n);
            }
        }

        while let Some(u) = q.pop_front() {
            let lu = *level.get(&u).unwrap_or(&0);
            for v in g.g().neighbors_directed(u, Outgoing) {
                let e = indegree.get_mut(&v).unwrap();
                *e = e.saturating_sub(1);
                let lv = level.entry(v).or_insert(0);
                *lv = (*lv).max(lu + 1);
                if *e == 0 {
                    q.push_back(v);
                }
            }
        }
        // Handle cycles: assign remaining nodes a best-effort level
        for n in g.g().node_indices() {
            level.entry(n).or_insert(0);
        }

        // 2) Group by levels, assign columns left-to-right per level
        let mut buckets: HashMap<usize, Vec<NodeIndex<DefaultIx>>> = HashMap::new();
        for (n, &lv) in level.iter() {
            buckets.entry(lv).or_default().push(*n);
        }
        for v in buckets.values_mut() {
            v.sort_by_key(|n| n.index());
        }

        // 3) Place nodes according to orientation
        for (lv, row) in buckets.iter() {
            for (i, n) in row.iter().enumerate() {
                let (x, y) = match self.state.orientation {
                    Orientation::TopDown => (
                        i as f32 * self.state.col_dist,
                        *lv as f32 * self.state.row_dist,
                    ),
                    Orientation::LeftRight => (
                        *lv as f32 * self.state.row_dist,
                        i as f32 * self.state.col_dist,
                    ),
                };
                let node = &mut g.g_mut()[*n];
                node.set_location(Pos2::new(x, y));
            }
        }

        self.state.triggered = true;
    }

    fn state(&self) -> State {
        self.state.clone()
    }
    fn from_state(state: State) -> impl Layout<State> {
        OriginHierarchical { state }
    }
}
