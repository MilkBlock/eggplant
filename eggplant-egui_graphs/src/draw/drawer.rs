use std::marker::PhantomData;

use egui::{Context, Painter, Shape};
use petgraph::Directed;
use std::collections::HashMap;
use std::vec::Vec;

use crate::{
    Graph, Metadata,
    layouts::{Layout, LayoutState},
    settings::SettingsStyle,
};

use super::{DisplayEdge, DisplayNode};

/// Contains all the data about current widget state which is needed for custom drawing functions.
pub struct DrawContext<'a> {
    pub ctx: &'a Context,
    pub painter: &'a Painter,
    pub style: &'a SettingsStyle,
    pub is_directed: bool,
    pub meta: &'a Metadata,
    /// Optional per-frame cache of preplanned polylines for oxdraw routing.
    pub routes: Option<&'a HashMap<u128, Vec<egui::Pos2>>>,
}

pub(crate) struct Drawer<'a, Nd, Ed, S, L>
where
    Nd: DisplayNode<Directed>,
    Ed: DisplayEdge<Directed, Nd>,
    S: LayoutState,
    L: Layout<S>,
{
    ctx: &'a DrawContext<'a>,
    g: &'a mut Graph<Nd, Ed>,
    delayed: Vec<Shape>,
    routes_screen: Option<std::collections::HashMap<u128, Vec<egui::Pos2>>>,

    _marker: PhantomData<(Nd, Ed, L, S)>,
}

impl<'a, Nd, Ed, S, L> Drawer<'a, Nd, Ed, S, L>
where
    Nd: DisplayNode<Directed>,
    Ed: DisplayEdge<Directed, Nd>,
    S: LayoutState,
    L: Layout<S>,
{
    pub fn new(g: &'a mut Graph<Nd, Ed>, ctx: &'a DrawContext<'a>) -> Self {
        Drawer {
            ctx,
            g,
            delayed: Vec::new(),
            routes_screen: None,
            _marker: PhantomData,
        }
    }

    /// Renders the graph for the current frame.
    ///
    /// Order matters:
    /// 1. `update_nodes` syncs each node's display object from its props so edge geometry
    ///    (which reads node display boundary points) uses fresh positions / sizes.
    /// 2. `draw_edges` builds edge shapes using the updated node display state.
    /// 3. `draw_nodes` paints nodes (non‑selected first) while deferring highlighted ones.
    /// 4. `draw_delayed` paints deferred (selected / dragged) shapes on top.
    pub(crate) fn draw(mut self) {
        self.update_nodes();
        self.draw_edges();
        self.draw_nodes();
        self.draw_delayed();
    }

    /// Synchronizes node display state with their current props without emitting shapes.
    ///
    /// This is a separate pass so edges can rely on `DisplayNode` geometry (e.g. boundary
    /// points) already being in sync when computing connector positions.
    fn update_nodes(&mut self) {
        self.g
            .g_mut()
            .node_indices()
            .collect::<Vec<_>>()
            .into_iter()
            .for_each(|idx| {
                if let Some(n) = self.g.node_mut(idx) {
                    let props = n.props().clone();
                    n.display_mut().update(&props);
                }
            });
    }

    fn draw_delayed(&mut self) {
        self.delayed.iter().for_each(|s| {
            self.ctx.painter.add(s.clone());
        });
    }

    fn draw_nodes(&mut self) {
        self.g
            .g_mut()
            .node_indices()
            .collect::<Vec<_>>()
            .into_iter()
            .for_each(|idx| {
                let n = self.g.node_mut(idx).unwrap();
                let shapes = <Nd as DisplayNode<Directed>>::shapes(n.display_mut(), self.ctx);

                if n.selected().is_some() || n.dragged() {
                    for s in shapes {
                        self.delayed.push(s);
                    }
                } else {
                    for s in shapes {
                        self.ctx.painter.add(s);
                    }
                }
            });
    }

    fn draw_edges(&mut self) {
        self.g
            .g_mut()
            .edge_indices()
            .collect::<Vec<_>>()
            .into_iter()
            .for_each(|idx| {
                let (idx_start, idx_end) = self.g.edge_endpoints(idx).unwrap();

                // FIXME: too costly to clone nodes for every edge
                let start = self.g.node(idx_start).cloned().unwrap();
                let end = self.g.node(idx_end).cloned().unwrap();

                let e = self.g.edge_mut(idx).unwrap();
                let start_maybe_inner = e.start_maybe_inner();
                let props = e.props().clone();

                let display = e.display_mut();
                display.update(&props);
                let shapes = <Ed as DisplayEdge<Directed, Nd>>::shapes(
                    display,
                    &start,
                    start_maybe_inner,
                    &end,
                    self.ctx,
                );

                if e.selected() {
                    for s in shapes {
                        self.delayed.push(s);
                    }
                } else {
                    for s in shapes {
                        self.ctx.painter.add(s);
                    }
                }
            });
    }
}

// Concrete type alias for the specialized graph
pub(crate) type ConcreteDrawer<'a, Nd, Ed, S, L> = Drawer<'a, Nd, Ed, S, L>;
