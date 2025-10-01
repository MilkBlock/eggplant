//! Demo-only import specs for layout and extras.
//! This keeps serde DTOs out of the public egui_graphs crate.

use crate::DagvizLayoutState;
use serde::{Deserialize, Serialize};

// Graph schema (reuse minimal form)
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct GraphSpecMinimal {
    #[serde(default)]
    pub nodes: Vec<i64>,
    #[serde(default)]
    pub edges: Vec<(i64, i64)>,
    #[serde(default)]
    pub directed: Option<bool>,
    #[serde(default)]
    pub positions: Option<Vec<(i64, f32, f32)>>,
}

// Layout and extras specs (only built-ins used in the demo)

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "camelCase")]
pub enum LayoutSpec {
    Hierarchical {
        #[serde(default)]
        row_dist: Option<f32>,
        #[serde(default)]
        col_dist: Option<f32>,
        #[serde(default)]
        center_parent: Option<bool>,
        #[serde(default)]
        orientation: Option<HierOrientationSpec>,
    },
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "camelCase")]
pub enum ExtrasSpec {
    CenterGravity {
        enabled: Option<bool>,
        c: Option<f32>,
    },
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum HierOrientationSpec {
    TopDown,
    LeftRight,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DemoImportSpec {
    #[serde(default)]
    pub version: Option<u32>,
    #[serde(default)]
    pub graph: Option<GraphSpecMinimal>,
    #[serde(default)]
    pub layout: Option<LayoutSpec>,
}

impl DemoImportSpec {
    pub fn try_parse(text: &str) -> Result<Self, String> {
        serde_json::from_str::<DemoImportSpec>(text)
            .map_err(|e| format!("invalid import spec json: {e}"))
    }
}

// Runtime mapping used by the demo to apply layout later (needs egui UI)
#[derive(Debug, Clone)]
pub enum PendingLayout {
    Hier(DagvizLayoutState),
}

impl LayoutSpec {
    pub fn to_pending(&self) -> PendingLayout {
        match self {
            LayoutSpec::Hierarchical {
                row_dist,
                col_dist,
                center_parent,
                orientation,
            } => {
                let mut st = DagvizLayoutState::default();
                if let Some(v) = row_dist {
                    st.row_dist = *v;
                }
                if let Some(v) = col_dist {
                    st.col_dist = *v;
                }
                if let Some(v) = center_parent {
                    st.center_parent = *v;
                }
                if let Some(o) = orientation {
                    st.orientation = match o {
                        HierOrientationSpec::TopDown => {
                            egui_graphs::LayoutHierarchicalOrientation::TopDown
                        }
                        HierOrientationSpec::LeftRight => {
                            egui_graphs::LayoutHierarchicalOrientation::LeftRight
                        }
                    };
                }
                // Trigger re-run once
                st.triggered = false;
                PendingLayout::Hier(st)
            }
        }
    }
}

// Export helpers (demo-only): read current UI layout state and build LayoutSpec
impl PendingLayout {
    pub fn from_ui_hier_state(ui: &mut egui::Ui) -> LayoutSpec {
        let st = egui_graphs::GraphView::<
            egui_graphs::DefaultNodeShape,
            egui_graphs::DefaultEdgeShape,
            DagvizLayoutState,
            crate::DagvizLayoutHierarchy,
        >::get_layout_state(ui);
        LayoutSpec::Hierarchical {
            row_dist: Some(st.row_dist),
            col_dist: Some(st.col_dist),
            center_parent: Some(st.center_parent),
            orientation: Some(match st.orientation {
                egui_graphs::LayoutHierarchicalOrientation::TopDown => HierOrientationSpec::TopDown,
                egui_graphs::LayoutHierarchicalOrientation::LeftRight => {
                    HierOrientationSpec::LeftRight
                }
            }),
        }
    }
}

#[allow(clippy::too_many_arguments)]
pub fn build_export_spec(
    ui: &mut egui::Ui,
    include_layout: bool,
    include_graph: bool,
    is_directed: bool,
    selected_layout: crate::DemoLayout,
    g_nodes: Vec<i64>,
    g_edges: Vec<(i64, i64)>,
    g_positions: Option<Vec<(i64, f32, f32)>>,
) -> DemoImportSpec {
    let layout = if include_layout {
        Some(match selected_layout {
            crate::DemoLayout::Hierarchical => PendingLayout::from_ui_hier_state(ui),
        })
    } else {
        None
    };
    let graph = if include_graph {
        Some(GraphSpecMinimal {
            nodes: g_nodes,
            edges: g_edges,
            directed: Some(is_directed),
            positions: g_positions,
        })
    } else {
        None
    };
    DemoImportSpec {
        version: Some(1),
        graph,
        layout,
    }
}
