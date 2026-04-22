use egui::{Rect, Vec2};
use petgraph::Directed;
use serde::{Deserialize, Serialize, de::DeserializeOwned};

use crate::{DisplayEdge, DisplayNode, Graph};

/// An additional force to be applied after the base forces.
/// Implementors are zero-sized marker types with the behavior in `apply`.
pub trait ExtraForce: std::fmt::Debug + Default + Send + Sync + 'static {
    type Params: Clone + Default + std::fmt::Debug + Send + Sync + 'static;

    /// Apply the extra force: accumulate into `disp` (same convention as base helpers).
    fn apply<Nd: DisplayNode<Directed>, Ed: DisplayEdge<Directed, Nd>>(
        params: &Self::Params,
        g: &Graph<Nd, Ed>,
        indices: &[petgraph::stable_graph::NodeIndex],
        disp: &mut [Vec2],
        area: Rect,
        k: f32,
    );
}

/// A configured instance of an extra force (on/off + parameters).
#[derive(Serialize, Deserialize)]
#[serde(bound(
    serialize = "E::Params: Serialize",
    deserialize = "E::Params: Deserialize<'de>"
))]
pub struct Extra<E: ExtraForce, const ENABLED_DEFAULT: bool> {
    pub enabled: bool,
    pub params: E::Params,
}

impl<E: ExtraForce, const ENABLED_DEFAULT: bool> Extra<E, ENABLED_DEFAULT> {
    pub fn new(params: E::Params) -> Self {
        Self {
            enabled: true,
            params,
        }
    }
}

impl<E: ExtraForce, const ENABLED_DEFAULT: bool> Default for Extra<E, ENABLED_DEFAULT> {
    fn default() -> Self {
        Self {
            enabled: ENABLED_DEFAULT,
            params: E::Params::default(),
        }
    }
}

impl<E: ExtraForce, const ENABLED_DEFAULT: bool> Clone for Extra<E, ENABLED_DEFAULT> {
    fn clone(&self) -> Self {
        Self {
            enabled: self.enabled,
            params: self.params.clone(),
        }
    }
}

impl<E: ExtraForce, const ENABLED_DEFAULT: bool> std::fmt::Debug for Extra<E, ENABLED_DEFAULT> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Extra")
            .field("enabled", &self.enabled)
            .field("params", &self.params)
            .finish()
    }
}

/// Trait to apply a heterogeneous tuple of extras.
pub trait ExtrasTuple:
    Serialize + DeserializeOwned + Clone + Default + std::fmt::Debug + Send + Sync + 'static
{
    fn apply_all<Nd: DisplayNode<Directed>, Ed: DisplayEdge<Directed, Nd>>(
        &self,
        g: &Graph<Nd, Ed>,
        indices: &[petgraph::stable_graph::NodeIndex],
        disp: &mut [Vec2],
        area: Rect,
        k: f32,
    );
}

impl ExtrasTuple for () {
    fn apply_all<Nd: DisplayNode<Directed>, Ed: DisplayEdge<Directed, Nd>>(
        &self,
        _g: &Graph<Nd, Ed>,
        _indices: &[petgraph::stable_graph::NodeIndex],
        _disp: &mut [Vec2],
        _area: Rect,
        _k: f32,
    ) {
    }
}

impl<Head, const B: bool, Tail> ExtrasTuple for (Extra<Head, B>, Tail)
where
    Head: ExtraForce,
    Head::Params: Serialize + DeserializeOwned,
    Tail: ExtrasTuple,
{
    fn apply_all<Nd: DisplayNode<Directed>, Ed: DisplayEdge<Directed, Nd>>(
        &self,
        g: &Graph<Nd, Ed>,
        indices: &[petgraph::stable_graph::NodeIndex],
        disp: &mut [Vec2],
        area: Rect,
        k: f32,
    ) {
        let (head, tail) = self;
        if head.enabled {
            Head::apply(&head.params, g, indices, disp, area, k);
        }
        tail.apply_all(g, indices, disp, area, k);
    }
}
