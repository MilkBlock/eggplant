use crate::draw::MaybeInner;
use indexmap::IndexMap;

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct ViewNode {
    pub identifier: Option<String>,
    pub enodes: IndexMap<String, Vec<ENode>>,
    // pub contained_edges: Vec<ViewEdge>,
    // pub labels: Vec<ViewLabel>,
    // pub position: (f64, f64),
    // pub size: (f64, f64),
    // pub properties: HashMap<String, String>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct ENode {
    pub func: String,
    pub id: u32,
    pub operands_num: usize,
}

impl ViewNode {
    pub fn new(ident: Option<String>, enodes: IndexMap<String, Vec<ENode>>) -> Self {
        Self {
            identifier: ident,
            enodes,
            // contained_edges: Vec::new(),
            // labels: Vec::new(),
            // position: (0.0, 0.0),
            // size: (0.0, 0.0),
            // properties: HashMap::new(),
        }
    }

    pub fn set_identifier(&mut self, id: String) {
        self.identifier = Some(id);
    }

    pub fn identifier(&self) -> Option<&str> {
        self.identifier.as_deref()
    }

    // pub fn add_child(&mut self, child: ViewNode) {
    //     self.children.push(child);
    // }

    // pub fn children(&self) -> &[ViewNode] {
    //     &self.children
    // }

    // pub fn children_mut(&mut self) -> &mut [ViewNode] {
    //     &mut self.children
    // }

    // pub fn add_contained_edge(&mut self, edge: ViewEdge) {
    //     self.contained_edges.push(edge);
    // }

    // pub fn contained_edges(&self) -> &[ViewEdge] {
    //     &self.contained_edges
    // }

    // pub fn contained_edges_mut(&mut self) -> &mut [ViewEdge] {
    //     &mut self.contained_edges
    // }

    // pub fn add_label(&mut self, label: ViewLabel) {
    //     self.labels.push(label);
    // }

    // pub fn set_position(&mut self, x: f64, y: f64) {
    //     self.position = (x, y);
    // }

    // pub fn position(&self) -> (f64, f64) {
    //     self.position
    // }

    // pub fn set_size(&mut self, width: f64, height: f64) {
    //     self.size = (width, height);
    // }

    // pub fn size(&self) -> (f64, f64) {
    //     self.size
    // }
}

// /// ELK-compatible edge type for graph layout
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct ViewEdge {
    pub identifier: Option<String>,
    pub start_maybe_inner: MaybeInner,
    // pub sections: Vec<ElkEdgeSection>,
    // pub labels: Vec<ViewLabel>,
    // pub properties: HashMap<String, String>,
}

impl ViewEdge {
    pub fn new(maybe_inner: MaybeInner) -> Self {
        Self {
            identifier: None,
            start_maybe_inner: maybe_inner,
            // sections: Vec::new(),
            // labels: Vec::new(),
            // properties: HashMap::new(),
        }
    }

    //     pub fn set_identifier(&mut self, id: String) {
    //         self.identifier = Some(id);
    //     }

    //     pub fn identifier(&self) -> Option<&str> {
    //         self.identifier.as_deref()
    //     }

    //     pub fn add_section(&mut self, section: ElkEdgeSection) {
    //         self.sections.push(section);
    //     }

    //     pub fn sections(&self) -> &[ElkEdgeSection] {
    //         &self.sections
    //     }

    //     pub fn sections_mut(&mut self) -> &mut [ElkEdgeSection] {
    //         &mut self.sections
    //     }

    //     pub fn add_label(&mut self, label: ViewLabel) {
    //         self.labels.push(label);
    //     }
    // }

    // /// ELK edge section for complex edge routing
    // #[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
    // pub struct ElkEdgeSection {
    //     pub start_point: (f64, f64),
    //     pub end_point: (f64, f64),
    //     pub bend_points: Vec<(f64, f64)>,
}

// /// ELK label for nodes and edges
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct ViewLabel {
    // pub text: String,
    // pub position: (f64, f64),
}

impl ViewLabel {
    pub fn new() -> Self {
        Self {
            // text: String::new(),
            // position: (0.0, 0.0),
        }
    }

    pub fn set_text(&mut self, text: String) {
        // self.text = text;
    }
}

impl Default for ViewNode {
    fn default() -> Self {
        Self {
            identifier: None,
            enodes: Default::default(),
        }
    }
}

impl Default for ViewEdge {
    fn default() -> Self {
        Self::new(MaybeInner::Itself)
    }
}

impl Default for ViewLabel {
    fn default() -> Self {
        Self::new()
    }
}
