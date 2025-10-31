use crate::{FuncOffset, draw::MaybeInner};
use indexmap::IndexMap;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct ViewNode {
    pub identifier: Option<String>,
    pub enodes: IndexMap<String, Vec<ENode>>,
    pub cano_value: u32,
    // pub contained_edges: Vec<ViewEdge>,
    // pub labels: Vec<ViewLabel>,
    // pub position: (f64, f64),
    // pub size: (f64, f64),
    // pub properties: HashMap<String, String>,
    pub event_handle: EventHandler,
}
#[allow(unused)]
pub trait EventHandle: Send + Sync {
    fn on_drag(&self, cano_value: u32) {}
    fn on_hover(&self, cano_value: u32) {}
    fn on_selected(&self, cano_value: u32) {}
    fn dyn_clone(&self) -> Box<dyn EventHandle>;
}
pub struct EventHandler {
    pub event_handle: Box<dyn EventHandle>,
}
#[derive(Clone, Debug)]
pub struct EmptyH {}
impl EventHandle for EmptyH {
    fn dyn_clone(&self) -> Box<dyn EventHandle> {
        Box::new(self.clone())
    }
}
impl Default for EventHandler {
    fn default() -> Self {
        EventHandler {
            event_handle: EmptyH {}.dyn_clone(),
        }
    }
}
impl std::fmt::Debug for EventHandler {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("EventHandler").finish()
    }
}
impl Clone for EventHandler {
    fn clone(&self) -> Self {
        EventHandler {
            event_handle: self.event_handle.dyn_clone(),
        }
    }
}
impl Serialize for EventHandler {
    fn serialize<S>(&self, _serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        todo!()
    }
}
impl<'de> Deserialize<'de> for EventHandler {
    fn deserialize<D>(_deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        todo!()
    }
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct ENode {
    pub func_offset: FuncOffset,
    pub cano_value: u32,
    pub operands_num: usize,
    pub basics: Vec<u32>,
}

impl ViewNode {
    pub fn new(
        ident: Option<String>,
        enodes: IndexMap<String, Vec<ENode>>,
        cano_value: u32,
        event_handle: EventHandler,
    ) -> Self {
        Self {
            identifier: ident,
            enodes,
            cano_value: cano_value,
            event_handle,
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
}

impl Default for ViewNode {
    fn default() -> Self {
        Self {
            identifier: None,
            enodes: Default::default(),
            cano_value: 0,
            event_handle: Default::default(),
        }
    }
}

impl Default for ViewEdge {
    fn default() -> Self {
        Self::new(MaybeInner::Itself)
    }
}
