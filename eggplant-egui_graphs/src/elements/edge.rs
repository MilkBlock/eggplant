use std::marker::PhantomData;

use petgraph::{Directed, EdgeType, stable_graph::EdgeIndex};
use serde::{Deserialize, Serialize};

use crate::{
    DefaultEdgeShape, DefaultNodeShape, DisplayEdge, DisplayNode, ViewEdge, draw::MaybeInner,
};

/// Stores properties of an [Edge]
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct EdgeProps {
    pub payload: ViewEdge,
    pub order: usize,
    pub selected: bool,
    pub label: String,
}

/// Stores properties of an edge that can be changed. Used to apply changes to the graph.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Edge<
    Ty: EdgeType = Directed,
    Dn: DisplayNode<Ty> = DefaultNodeShape,
    D: DisplayEdge<Ty, Dn> = DefaultEdgeShape,
> {
    id: Option<EdgeIndex>,

    display: D,

    props: EdgeProps,
    _marker: PhantomData<(Ty, Dn)>,
}

impl<Ty: EdgeType, Dn: DisplayNode<Ty>, D: DisplayEdge<Ty, Dn>> Edge<Ty, Dn, D> {
    pub fn new(payload: ViewEdge) -> Self {
        let props = EdgeProps {
            payload,

            order: usize::default(),
            selected: bool::default(),
            label: String::default(),
        };

        let display = D::from(props.clone());
        Self {
            props,
            display,

            id: Option::default(),
            _marker: PhantomData,
        }
    }

    pub fn props(&self) -> &EdgeProps {
        &self.props
    }

    pub fn display(&self) -> &D {
        &self.display
    }

    pub fn display_mut(&mut self) -> &mut D {
        &mut self.display
    }

    #[allow(clippy::missing_panics_doc)] // TODO: Add panic message
    pub fn id(&self) -> EdgeIndex {
        self.id.unwrap()
    }

    pub(crate) fn set_id(&mut self, id: EdgeIndex) {
        self.id = Some(id);
    }

    pub fn order(&self) -> usize {
        self.props.order
    }

    pub(crate) fn set_order(&mut self, order: usize) {
        self.props.order = order;
    }

    pub fn payload(&self) -> &ViewEdge {
        &self.props.payload
    }

    pub fn payload_mut(&mut self) -> &mut ViewEdge {
        &mut self.props.payload
    }

    pub fn set_selected(&mut self, selected: bool) {
        self.props.selected = selected;
    }

    pub fn selected(&self) -> bool {
        self.props.selected
    }

    pub fn set_label(&mut self, label: String) {
        self.props.label = label;
    }

    pub fn label(&self) -> String {
        self.props.label.clone()
    }

    pub fn start_maybe_inner(&self) -> MaybeInner {
        self.props.payload.start_maybe_inner.clone()
    }
}
