use std::collections::HashMap;

/// ELK-compatible node type for graph layout
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct ViewNode {
    pub identifier: Option<String>,
    pub children: Vec<ViewNode>,
    pub contained_edges: Vec<ElkEdge>,
    pub labels: Vec<ElkLabel>,
    pub position: (f64, f64),
    pub size: (f64, f64),
    pub properties: HashMap<String, String>,
}

impl ViewNode {
    pub fn new() -> Self {
        Self {
            identifier: None,
            children: Vec::new(),
            contained_edges: Vec::new(),
            labels: Vec::new(),
            position: (0.0, 0.0),
            size: (0.0, 0.0),
            properties: HashMap::new(),
        }
    }

    pub fn set_identifier(&mut self, id: String) {
        self.identifier = Some(id);
    }

    pub fn identifier(&self) -> Option<&str> {
        self.identifier.as_deref()
    }

    pub fn add_child(&mut self, child: ViewNode) {
        self.children.push(child);
    }

    pub fn children(&self) -> &[ViewNode] {
        &self.children
    }

    pub fn children_mut(&mut self) -> &mut [ViewNode] {
        &mut self.children
    }

    pub fn add_contained_edge(&mut self, edge: ElkEdge) {
        self.contained_edges.push(edge);
    }

    pub fn contained_edges(&self) -> &[ElkEdge] {
        &self.contained_edges
    }

    pub fn contained_edges_mut(&mut self) -> &mut [ElkEdge] {
        &mut self.contained_edges
    }

    pub fn add_label(&mut self, label: ElkLabel) {
        self.labels.push(label);
    }

    pub fn set_position(&mut self, x: f64, y: f64) {
        self.position = (x, y);
    }

    pub fn position(&self) -> (f64, f64) {
        self.position
    }

    pub fn set_size(&mut self, width: f64, height: f64) {
        self.size = (width, height);
    }

    pub fn size(&self) -> (f64, f64) {
        self.size
    }
}

/// ELK-compatible edge type for graph layout
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct ElkEdge {
    pub identifier: Option<String>,
    pub sections: Vec<ElkEdgeSection>,
    pub labels: Vec<ElkLabel>,
    pub properties: HashMap<String, String>,
}

impl ElkEdge {
    pub fn new() -> Self {
        Self {
            identifier: None,
            sections: Vec::new(),
            labels: Vec::new(),
            properties: HashMap::new(),
        }
    }

    pub fn set_identifier(&mut self, id: String) {
        self.identifier = Some(id);
    }

    pub fn identifier(&self) -> Option<&str> {
        self.identifier.as_deref()
    }

    pub fn add_section(&mut self, section: ElkEdgeSection) {
        self.sections.push(section);
    }

    pub fn sections(&self) -> &[ElkEdgeSection] {
        &self.sections
    }

    pub fn sections_mut(&mut self) -> &mut [ElkEdgeSection] {
        &mut self.sections
    }

    pub fn add_label(&mut self, label: ElkLabel) {
        self.labels.push(label);
    }
}

/// ELK edge section for complex edge routing
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct ElkEdgeSection {
    pub start_point: (f64, f64),
    pub end_point: (f64, f64),
    pub bend_points: Vec<(f64, f64)>,
}

/// ELK label for nodes and edges
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct ElkLabel {
    pub text: String,
    pub position: (f64, f64),
}

impl ElkLabel {
    pub fn new() -> Self {
        Self {
            text: String::new(),
            position: (0.0, 0.0),
        }
    }

    pub fn set_text(&mut self, text: String) {
        self.text = text;
    }
}

impl Default for ViewNode {
    fn default() -> Self {
        Self::new()
    }
}

impl Default for ElkEdge {
    fn default() -> Self {
        Self::new()
    }
}

impl Default for ElkLabel {
    fn default() -> Self {
        Self::new()
    }
}
