use std::collections::HashMap;
use std::cmp::Ordering;
use std::collections::{BinaryHeap, HashSet};

use egui::{Pos2, Vec2};
use petgraph::stable_graph::NodeIndex;
use petgraph::Directed;

use crate::draw::{DisplayNode, displays::MaybeInner};
use crate::elements::IndexTy;
use crate::graph::Graph;
use crate::settings::{EdgeRouterKind, SettingsStyle};

/// Preplan oxdraw-like polylines considering only edge-edge intersections;
/// Keep endpoints fixed; do not trim to node boundaries.
pub fn plan_oxdraw_class<Nd, Ed>(
    g: &Graph<Nd, Ed>,
    style: &SettingsStyle,
    _class_y_shift_canvas: f32,
) -> HashMap<u128, Vec<Pos2>>
where
    Nd: DisplayNode<Directed>,
    Ed: crate::draw::displays::DisplayEdge<Directed, Nd>,
{
    if style.edge_router_kind() != EdgeRouterKind::OxdrawClass {
        return HashMap::new();
    }

    // Safety guard: avoid O(E^2) blow-ups on very dense graphs.
    // If the edge count is too large, skip routing and let defaults render.
    const MAX_OXDRAW_EDGES: usize = 800;
    if g.edge_count() > MAX_OXDRAW_EDGES {
        return HashMap::new();
    }

    let mut routes: HashMap<u128, Vec<Pos2>> = HashMap::new();

    let mut edges: Vec<_> = g.edges_iter().collect();
    // Deterministic order avoids oscillation during same-frame planning
    edges.sort_by_key(|(eid, _)| eid.index());
    for (eidx, e) in edges.into_iter() {
        let (s_idx, t_idx) = g.edge_endpoints(eidx).unwrap();
        let s_node = g.node(s_idx).unwrap();
        let t_node = g.node(t_idx).unwrap();
        // Determine anchor endpoints consistent with PlantEdge (yellow enode port -> target node center)
        let (start, end) = compute_edge_anchors::<Nd>(s_node, t_node, e.start_maybe_inner());
        let v = end - start;
        if v.length_sq() <= f32::EPSILON { continue; }
        let dir = v.normalized();
        let normal = Vec2::new(-dir.y, dir.x);
        let span = v.length();
        let base_off = (span * 0.25).min(32.0);
        let base_stub = (span * 0.25).min(56.0);
        let order = e.order();

        let mut best: Option<Vec<Pos2>> = None;
        let mut best_metric = (u32::MAX, u32::MAX, u32::MAX);
        for sign in [1.0, -1.0] {
            for attempt in 0..3u32 {
                let off = base_off + attempt as f32 * 8.0;
                let stub = base_stub.min(span * 0.5 - 1.0).max(0.0);
                let mid = (start + end.to_vec2()) * 0.5 + normal * (off * sign);
                let first = start + dir * stub + normal * (off * sign);
                let second = end - dir * stub + normal * (off * sign);
                let mut pts = vec![start, first, mid, second, end];
                if !pts.iter().all(|p| p.x.is_finite() && p.y.is_finite()) { continue; }
                simplify(&mut pts);
                if !pts.iter().all(|p| p.x.is_finite() && p.y.is_finite()) { continue; }
                // Only penalize edge-edge crossings, not CLASS boxes
                let inter = count_route_intersections(&pts, &routes);
                let len = polyline_length(&pts) as u32;
                let metric = (0u32, inter as u32, len);
                if metric < best_metric {
                    best_metric = metric;
                    best = Some(pts);
                    if best_metric.1 == 0 { break; }
                }
            }
        }
        if let Some(r) = best {
            routes.insert(route_key(s_idx, t_idx, order), r);
        }
    }

    routes
}

/// Compute the start and end anchor points for a route, mirroring eggplant-viewer/src/plant_edge.rs logic.
fn compute_edge_anchors<Nd: DisplayNode<Directed>>(
    start: &crate::Node<Directed, Nd>,
    end: &crate::Node<Directed, Nd>,
    start_maybe_inner: MaybeInner,
) -> (Pos2, Pos2) {
    match start_maybe_inner {
        MaybeInner::Itself => (start.location(), end.location()),
        MaybeInner::Inner { inner_pos } => {
            // Mirror eggplant-viewer/src/plant_edge.rs exactly for yellow port anchor
            let enodes = &start.payload().enodes;
            if let Some(type_specified_enodes) = enodes.get(&inner_pos.id.func) {
                let i = type_specified_enodes
                    .iter()
                    .position(|x| x.func_offset == inner_pos.id)
                    .unwrap_or(0);
                let s = enodes.get_index_of(&inner_pos.id.func).unwrap_or(0);
                let reduced_nodes_num = enodes
                    .iter()
                    .take(s)
                    .fold(0.8f32, |m, (_k, v)| m + v.len() as f32 + 0.5);
                let y = reduced_nodes_num + i as f32;
                let start_loc = start.location()
                    + Vec2::new(0., 6.0) * y
                    + Vec2::new(6., 0.) * inner_pos.operand_idx as f32;
                if start.id() == end.id() {
                    (start_loc, start_loc)
                } else {
                    (start_loc, end.location())
                }
            } else {
                (start.location(), end.location())
            }
        }
    }
}

// ===========================
// Full orthogonal (grid + A*)
// ===========================

#[derive(Clone, Copy)]
pub struct GridParams {
    pub cell: f32,          // grid cell size in canvas units
    pub pad: f32,           // padding around graph bounds in canvas units
    pub margin: f32,        // inflate obstacles (canvas units)
    pub turn_penalty: f32,  // extra cost per 90° turn
    pub cross_penalty: f32, // extra cost for stepping through reserved edge cells
    pub max_w: usize,       // cap grid width in cells
    pub max_h: usize,       // cap grid height in cells
}

impl Default for GridParams {
    fn default() -> Self {
        GridParams {
            cell: 12.0,
            pad: 60.0,
            margin: 6.0,
            turn_penalty: 4.0,
            cross_penalty: 8.0,
            max_w: 400,
            max_h: 400,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
enum Dir { U, D, L, R }

impl Dir {
    fn delta(self) -> (i32, i32) {
        match self { Dir::U => (0,-1), Dir::D => (0,1), Dir::L => (-1,0), Dir::R => (1,0) }
    }
    fn all() -> [Dir;4] { [Dir::U, Dir::D, Dir::L, Dir::R] }
}

#[derive(Clone)]
struct Grid {
    origin: Pos2,
    cell: f32,
    w: usize,
    h: usize,
    blocked: Vec<u8>, // 0 free; 1 obstacle; 2 reserved edge (softer)
}

impl Grid {
    fn idx(&self, x: i32, y: i32) -> Option<usize> {
        if x<0 || y<0 { return None; }
        let (x,y) = (x as usize, y as usize);
        if x>=self.w || y>=self.h { None } else { Some(y*self.w + x) }
    }
    fn mark_rect(&mut self, min: Pos2, max: Pos2, val: u8, inflate: f32) {
        let inflate = inflate.max(0.0);
        let xmin = (min.x.min(max.x) - self.origin.x - inflate).floor() / self.cell;
        let ymin = (min.y.min(max.y) - self.origin.y - inflate).floor() / self.cell;
        let xmax = (max.x.max(min.x) - self.origin.x + inflate).ceil() / self.cell;
        let ymax = (max.y.max(min.y) - self.origin.y + inflate).ceil() / self.cell;
        let (xi0, yi0) = (xmin as i32, ymin as i32);
        let (xi1, yi1) = (xmax as i32, ymax as i32);
        for y in yi0..=yi1 { for x in xi0..=xi1 {
            if let Some(i) = self.idx(x,y) {
                self.blocked[i] = self.blocked[i].max(val);
            }
        }}
    }
    fn world_to_cell(&self, p: Pos2) -> (i32,i32) {
        let x = ((p.x - self.origin.x)/self.cell).round() as i32;
        let y = ((p.y - self.origin.y)/self.cell).round() as i32;
        (x,y)
    }
    fn cell_center(&self, x: i32, y: i32) -> Pos2 {
        Pos2::new(
            self.origin.x + (x as f32)*self.cell,
            self.origin.y + (y as f32)*self.cell,
        )
    }
}

#[derive(Clone, Copy, PartialEq)]
struct AStarNode { f: f32, g: f32, x: i32, y: i32, dir: Dir }

impl Eq for AStarNode {}
impl Ord for AStarNode { fn cmp(&self, other: &Self) -> Ordering { other.f.partial_cmp(&self.f).unwrap_or(Ordering::Equal) } }
impl PartialOrd for AStarNode { fn partial_cmp(&self, other:&Self)->Option<Ordering>{ Some(self.cmp(other)) } }

fn heuristic(ax:i32, ay:i32, bx:i32, by:i32) -> f32 { ((ax-bx).abs() + (ay-by).abs()) as f32 }

pub fn plan_oxdraw_full<Nd, Ed>(
    g: &Graph<Nd, Ed>,
    style: &SettingsStyle,
    class_y_shift_canvas: f32,
    params: GridParams,
) -> HashMap<u128, Vec<Pos2>>
where
    Nd: DisplayNode<Directed>,
    Ed: crate::draw::displays::DisplayEdge<Directed, Nd>,
{
    if style.edge_router_kind() != EdgeRouterKind::OxdrawFull {
        return HashMap::new();
    }

    // Build world bounds from nodes
    let mut min = Pos2::new(f32::INFINITY, f32::INFINITY);
    let mut max = Pos2::new(f32::NEG_INFINITY, f32::NEG_INFINITY);
    for (_idx, n) in g.nodes_iter() {
        let p = n.location();
        if p.x < min.x { min.x = p.x };
        if p.y < min.y { min.y = p.y };
        if p.x > max.x { max.x = p.x };
        if p.y > max.y { max.y = p.y };
    }
    if !min.x.is_finite() { return HashMap::new(); }
    min.x -= params.pad; min.y -= params.pad; max.x += params.pad; max.y += params.pad;
    let w = (((max.x-min.x)/params.cell).ceil() as usize).min(params.max_w).max(8);
    let h = (((max.y-min.y)/params.cell).ceil() as usize).min(params.max_h).max(8);
    let origin = Pos2::new(min.x, min.y);
    let mut grid = Grid { origin, cell: params.cell, w, h, blocked: vec![0; w*h] };

    // Obstacles: CLASS boxes shifted upward
    for (_idx, n) in g.nodes_iter() {
        let c0 = n.location();
        let c = Pos2::new(c0.x, c0.y - class_y_shift_canvas);
        let half_w = 20.0; let half_h = 6.0;
        let a = Pos2::new(c.x - half_w, c.y - half_h);
        let b = Pos2::new(c.x + half_w, c.y + half_h);
        grid.mark_rect(a, b, 1, params.margin);
    }

    // Plan per edge with A* (orthogonal). Use reserved cells for already routed edges.
    let mut routes: HashMap<u128, Vec<Pos2>> = HashMap::new();
    let mut edges: Vec<_> = g.edges_iter().collect();
    edges.sort_by_key(|(eid, _)| eid.index());

    for (eidx, e) in edges.into_iter() {
        let (s_idx, t_idx) = match g.edge_endpoints(eidx) { Some(v) => v, None => continue };
        let s_node = g.node(s_idx).unwrap();
        let t_node = g.node(t_idx).unwrap();
        // Use the same anchors as PlantEdge/plan_oxdraw_class
        let (start_p, end_p) = compute_edge_anchors::<Nd>(s_node, t_node, e.start_maybe_inner());
        let v = end_p - start_p;
        if !v.x.is_finite() || !v.y.is_finite() { continue; }
        let dir = if v.length_sq() <= f32::EPSILON { Dir::R } else {
            let dv = v.normalized();
            if dv.x.abs() >= dv.y.abs() { if dv.x>=0.0 { Dir::R } else { Dir::L } } else { if dv.y>=0.0 { Dir::D } else { Dir::U } }
        };
        let (sx, sy) = grid.world_to_cell(start_p);
        let (tx, ty) = grid.world_to_cell(end_p);

        // A* with direction state
        let mut open = BinaryHeap::new();
        let mut came_from: HashMap<(i32,i32,Dir),(i32,i32,Dir)> = HashMap::new();
        let mut g_score: HashMap<(i32,i32,Dir), f32> = HashMap::new();
        let start = (sx, sy, dir);
        let h0 = heuristic(sx, sy, tx, ty);
        open.push(AStarNode { f: h0, g: 0.0, x: sx, y: sy, dir });
        g_score.insert(start, 0.0);

        let mut found: Option<(i32,i32,Dir)> = None;
        let mut guard = 0usize;
        while let Some(cur) = open.pop() {
            guard += 1; if guard > (grid.w*grid.h*4).min(200_000) { break; }
            if cur.x == tx && cur.y == ty { found = Some((cur.x, cur.y, cur.dir)); break; }
            for ndir in Dir::all() {
                let (dx,dy) = ndir.delta();
                let nx = cur.x + dx; let ny = cur.y + dy;
                if let Some(i) = grid.idx(nx, ny) {
                    // Hard obstacle
                    if grid.blocked[i] == 1 { continue; }
                    let turn = if ndir as u8 == cur.dir as u8 { 0.0 } else { params.turn_penalty };
                    let cross = if grid.blocked[i] >= 2 { params.cross_penalty } else { 0.0 };
                    let tentative = cur.g + 1.0 + turn + cross;
                    let key = (nx, ny, ndir);
                    let old = g_score.get(&key).copied().unwrap_or(f32::INFINITY);
                    if tentative + 1e-6 < old {
                        g_score.insert(key, tentative);
                        came_from.insert(key, (cur.x, cur.y, cur.dir));
                        let f = tentative + heuristic(nx, ny, tx, ty);
                        open.push(AStarNode { f, g: tentative, x: nx, y: ny, dir: ndir });
                    }
                }
            }
        }

        let key_u128 = ((s_idx.index() as u128) << 64) ^ ((t_idx.index() as u128) << 32) ^ (e.order() as u128);
        if let Some(goal) = found {
            // Reconstruct
            let mut seq: Vec<(i32,i32)> = vec![(goal.0, goal.1)];
            let mut cur = goal;
            while let Some(prev) = came_from.get(&cur) { cur = *prev; seq.push((cur.0, cur.1)); if cur.0==sx && cur.1==sy { break; } }
            seq.reverse();
            // To canvas & simplify colinear
            let mut pts: Vec<Pos2> = seq.iter().map(|(x,y)| grid.cell_center(*x,*y)).collect();
            simplify(&mut pts);
            routes.insert(key_u128, pts.clone());
            // Reserve the path cells (soft) for next edges
            for (x,y) in seq { if let Some(i) = grid.idx(x,y) { grid.blocked[i] = grid.blocked[i].max(2); } }
        } else {
            // Fallback: if start/end非对齐，插入一个L型拐点，保证“有折线”。
            let aligned = (sx == tx) || (sy == ty);
            if !aligned {
                let cand1 = (sx, ty); let cand2 = (tx, sy);
                let ok1 = cand1.0>=0 && cand1.1>=0 && grid.idx(cand1.0, cand1.1).map(|i| grid.blocked[i] != 1).unwrap_or(false);
                let ok2 = cand2.0>=0 && cand2.1>=0 && grid.idx(cand2.0, cand2.1).map(|i| grid.blocked[i] != 1).unwrap_or(false);
                let mid = if ok1 { cand1 } else if ok2 { cand2 } else { (sx, sy) };
                let mut pts = vec![start_p, grid.cell_center(mid.0, mid.1), end_p];
                simplify(&mut pts);
                routes.insert(key_u128, pts);
            } else {
                // 仍然直线
                routes.insert(key_u128, vec![start_p, end_p]);
            }
        }
    }

    routes
}

fn route_key(start: petgraph::stable_graph::NodeIndex<IndexTy>, end: petgraph::stable_graph::NodeIndex<IndexTy>, order: usize) -> u128 {
    ((start.index() as u128) << 64) ^ ((end.index() as u128) << 32) ^ (order as u128)
}

fn simplify(points: &mut Vec<Pos2>) {
    if points.len() < 3 { return; }
    let mut out = Vec::with_capacity(points.len());
    out.push(points[0]);
    let mut i = 1usize;
    while i + 1 < points.len() {
        let a = *out.last().unwrap();
        let b = points[i];
        let c = points[i + 1];
        let ab = b - a;
        let bc = c - b;
        let cross = ab.x * bc.y - ab.y * bc.x;
        let colinear = cross.abs() < 1e-3;
        let within_x = b.x >= a.x.min(c.x) - 1e-3 && b.x <= a.x.max(c.x) + 1e-3;
        let within_y = b.y >= a.y.min(c.y) - 1e-3 && b.y <= a.y.max(c.y) + 1e-3;
        if colinear && within_x && within_y { i += 1; continue; }
        out.push(b);
        i += 1;
    }
    out.push(*points.last().unwrap());
    *points = out;
}

fn polyline_length(pts: &[Pos2]) -> f32 {
    let mut s = 0.0;
    for w in pts.windows(2) { s += (w[1] - w[0]).length(); }
    s
}

fn rect_intersects_segment(min: Pos2, max: Pos2, a: Pos2, b: Pos2) -> bool {
    let (xmin, ymin, xmax, ymax) = (
        min.x.min(max.x),
        min.y.min(max.y),
        min.x.max(max.x),
        min.y.max(max.y),
    );
    // Endpoint inside rectangle
    let inside = |p: Pos2| p.x >= xmin && p.x <= xmax && p.y >= ymin && p.y <= ymax;
    if inside(a) || inside(b) {
        return true;
    }
    // Check against 4 rectangle edges
    let tl = Pos2::new(xmin, ymin);
    let tr = Pos2::new(xmax, ymin);
    let br = Pos2::new(xmax, ymax);
    let bl = Pos2::new(xmin, ymax);
    segments_intersect(a, b, tl, tr)
        || segments_intersect(a, b, tr, br)
        || segments_intersect(a, b, br, bl)
        || segments_intersect(a, b, bl, tl)
}

// Note: CLASS collision check intentionally removed to match new requirement.

fn segments_intersect(a: Pos2, b: Pos2, c: Pos2, d: Pos2) -> bool {
    fn orient(a: Pos2, b: Pos2, c: Pos2) -> f32 { let ab = b - a; let ac = c - a; ab.x*ac.y - ab.y*ac.x }
    fn on_seg(a: Pos2, b: Pos2, p: Pos2) -> bool { p.x>=a.x.min(b.x)-1e-3 && p.x<=a.x.max(b.x)+1e-3 && p.y>=a.y.min(b.y)-1e-3 && p.y<=a.y.max(b.y)+1e-3 }
    let o1 = orient(a,b,c); let o2 = orient(a,b,d); let o3 = orient(c,d,a); let o4 = orient(c,d,b);
    if (o1>0.0 && o2<0.0 || o1<0.0 && o2>0.0) && (o3>0.0 && o4<0.0 || o3<0.0 && o4>0.0) { return true; }
    (o1.abs()<1e-3 && on_seg(a,b,c)) || (o2.abs()<1e-3 && on_seg(a,b,d)) || (o3.abs()<1e-3 && on_seg(c,d,a)) || (o4.abs()<1e-3 && on_seg(c,d,b))
}

fn count_route_intersections(route: &[Pos2], existing: &HashMap<u128, Vec<Pos2>>) -> usize {
    let mut n = 0usize;
    for other in existing.values() {
        for w1 in route.windows(2) {
            for w2 in other.windows(2) {
                if segments_intersect(w1[0], w1[1], w2[0], w2[1]) { n += 1; }
            }
        }
    }
    n
}
