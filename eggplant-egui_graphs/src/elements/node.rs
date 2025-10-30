use std::fmt::Debug;
use std::marker::PhantomData;

use egui::{Color32, Pos2};
use petgraph::{Directed, EdgeType, stable_graph::NodeIndex};
use serde::{Deserialize, Serialize};

use crate::{DefaultNodeShape, DisplayNode, ViewNode, draw::MaybeInner};

pub type IndexTy = u32;
/// Stores properties of a [Node]
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct NodeProps {
    pub payload: ViewNode,
    pub label: String,
    pub selected: Option<MaybeInner>,
    pub dragged: bool,
    pub hovered: bool,

    color: Option<Color32>,
    location: Pos2,

    // Hierarchical layout fields
    parent: Option<NodeIndex<IndexTy>>,
    children: Vec<NodeIndex<IndexTy>>,
    level: usize,
}

impl NodeProps {
    pub fn location(&self) -> Pos2 {
        self.location
    }

    pub fn color(&self) -> Option<Color32> {
        self.color
    }

    pub fn parent(&self) -> Option<NodeIndex> {
        self.parent
    }

    pub fn set_parent(&mut self, parent: Option<NodeIndex<IndexTy>>) {
        self.parent = parent;
    }

    pub fn children(&self) -> &[NodeIndex<IndexTy>] {
        &self.children
    }

    pub fn children_mut(&mut self) -> &mut Vec<NodeIndex<IndexTy>> {
        &mut self.children
    }

    pub fn add_child(&mut self, child: NodeIndex<IndexTy>) {
        self.children.push(child);
    }

    pub fn level(&self) -> usize {
        self.level
    }

    pub fn set_level(&mut self, level: usize) {
        self.level = level;
    }

    pub fn is_hierarchical(&self) -> bool {
        !self.children.is_empty()
    }
}

#[derive(Serialize, Deserialize)]
pub struct Node<Ty = Directed, D = DefaultNodeShape>
where
    Ty: EdgeType,
    D: DisplayNode<Ty>,
{
    id: Option<NodeIndex<IndexTy>>,

    props: NodeProps,
    display: D,

    _marker: PhantomData<Ty>,
}

#[allow(clippy::missing_fields_in_debug)] // TODO: add all fields or remove this and fix all warnings
impl<Ty, D> Debug for Node<Ty, D>
where
    Ty: EdgeType,
    D: DisplayNode<Ty>,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Node").field("id", &self.id).finish()
    }
}

impl<Ty, Nd> Clone for Node<Ty, Nd>
where
    Ty: EdgeType,
    Nd: DisplayNode<Ty>,
{
    fn clone(&self) -> Self {
        let idx = self.id().index();
        Self {
            id: Some(NodeIndex::new(idx)),
            props: self.props.clone(),
            display: self.display.clone(),
            _marker: PhantomData,
        }
    }
}

impl<Ty, Nd> Node<Ty, Nd>
where
    Ty: EdgeType,
    Nd: DisplayNode<Ty>,
{
    /// Creates a new node with default properties
    pub fn new(payload: ViewNode) -> Self {
        let props = NodeProps {
            payload,
            location: Pos2 {
                x: rand::random::<f32>() * 3.,
                y: rand::random::<f32>() * 3.,
            },
            color: Option::default(),
            label: String::default(),
            selected: None,
            dragged: bool::default(),
            hovered: bool::default(),
            parent: None,
            children: Vec::new(),
            level: 0,
        };

        Node::new_with_props(props)
    }

    /// Creates a new node with custom properties
    pub fn new_with_props(props: NodeProps) -> Self {
        let display = Nd::from(props.clone());
        Self {
            props,
            display,

            id: Option::default(),
            _marker: PhantomData,
        }
    }

    pub fn props(&self) -> &NodeProps {
        &self.props
    }

    pub fn display(&self) -> &Nd {
        &self.display
    }

    pub fn display_mut(&mut self) -> &mut Nd {
        &mut self.display
    }

    #[allow(clippy::missing_panics_doc)] // TODO: Add panic message
    pub fn id(&self) -> NodeIndex {
        self.id.unwrap()
    }

    pub(crate) fn set_id(&mut self, id: NodeIndex) {
        self.id = Some(id);
    }

    pub fn payload(&self) -> &ViewNode {
        &self.props.payload
    }

    pub fn payload_mut(&mut self) -> &mut ViewNode {
        &mut self.props.payload
    }

    pub fn color(&self) -> Option<Color32> {
        self.props.color()
    }

    pub fn set_color(&mut self, color: Color32) {
        self.props.color = Some(color);
    }

    pub fn location(&self) -> Pos2 {
        self.props.location()
    }

    pub fn set_location(&mut self, loc: Pos2) {
        self.props.location = loc;
    }

    pub fn parent(&self) -> Option<NodeIndex> {
        self.props.parent()
    }

    pub fn set_parent(&mut self, parent: Option<NodeIndex>) {
        self.props.set_parent(parent);
    }

    pub fn children(&self) -> &[NodeIndex] {
        self.props.children()
    }

    pub fn children_mut(&mut self) -> &mut Vec<NodeIndex> {
        self.props.children_mut()
    }

    pub fn add_child(&mut self, child: NodeIndex) {
        self.props.add_child(child);
    }

    pub fn level(&self) -> usize {
        self.props.level()
    }

    pub fn set_level(&mut self, level: usize) {
        self.props.set_level(level);
    }

    pub fn is_hierarchical(&self) -> bool {
        self.props.is_hierarchical()
    }

    pub fn selected(&self) -> Option<MaybeInner> {
        self.props.selected.clone()
    }

    pub fn set_selected(&mut self, selected: Option<MaybeInner>) {
        self.props.selected = selected;
    }

    pub fn dragged(&self) -> bool {
        self.props.dragged
    }

    pub fn set_dragged(&mut self, dragged: bool) {
        self.props.dragged = dragged;
    }

    pub fn hovered(&self) -> bool {
        self.props.hovered
    }

    pub fn set_hovered(&mut self, hovered: bool) {
        self.props.hovered = hovered;
    }

    pub fn label(&self) -> String {
        self.props.label.clone()
    }

    pub fn set_label(&mut self, label: String) {
        self.props.label = label;
    }
}
