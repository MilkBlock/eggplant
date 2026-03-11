use std::collections::HashMap;

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

// (A* grid router removed) – kept only geometric helpers used by both routers below.

pub fn plan_oxdraw_full<Nd, Ed>(
    g: &Graph<Nd, Ed>,
    style: &SettingsStyle,
    class_y_shift_canvas: f32,
    _params: (),
) -> HashMap<u128, Vec<Pos2>>
where
    Nd: DisplayNode<Directed>,
    Ed: crate::draw::displays::DisplayEdge<Directed, Nd>,
{
    if style.edge_router_kind() != EdgeRouterKind::OxdrawFull {
        return HashMap::new();
    }

    // -----------------------------
    // oxdraw-style heuristic router
    // -----------------------------

    // Temporary debug switch: ignore node volume (no node-collision checks, no endpoint trimming)
    const IGNORE_NODE_VOLUME: bool = true;

    // Heuristic constants mirrored from ~/Repos/oxdraw/src/lib.rs
    const EDGE_BIDIRECTIONAL_OFFSET: f32 = 28.0;
    const EDGE_BIDIRECTIONAL_STUB: f32 = 48.0;
    const EDGE_BIDIRECTIONAL_OFFSET_STEP: f32 = 12.0;
    const EDGE_BIDIRECTIONAL_STUB_STEP: f32 = 18.0;
    const EDGE_SINGLE_OFFSET: f32 = 32.0;
    const EDGE_SINGLE_STUB: f32 = 56.0;
    const EDGE_SINGLE_OFFSET_STEP: f32 = 14.0;
    const EDGE_SINGLE_STUB_STEP: f32 = 20.0;
    const EDGE_COLLISION_MARGIN: f32 = 6.0;
    const EDGE_COLLISION_MAX_ITER: usize = 6;

    // Approximate node rectangles (CLASS boxes) used for collision checks
    // to be consistent with earlier A* obstacles. Apply optional vertical shift.
    #[inline]
    fn node_rect(center: Pos2, class_y_shift_canvas: f32) -> (Pos2, Pos2) {
        let c = Pos2::new(center.x, center.y - class_y_shift_canvas);
        let half_w = 20.0;
        let half_h = 6.0;
        (Pos2::new(c.x - half_w, c.y - half_h), Pos2::new(c.x + half_w, c.y + half_h))
    }

    #[inline]
    fn inflate(min: Pos2, max: Pos2, m: f32) -> (Pos2, Pos2) {
        (
            Pos2::new(min.x - m, min.y - m),
            Pos2::new(max.x + m, max.y + m),
        )
    }

    // Build node bounds map once.
    let mut node_bounds: HashMap<NodeIndex<IndexTy>, (Pos2, Pos2)> = HashMap::new();
    if !IGNORE_NODE_VOLUME {
        for (idx, n) in g.nodes_iter() {
            node_bounds.insert(idx, node_rect(n.location(), class_y_shift_canvas));
        }
    }

    // Existing routes to consider for "edge-edge" intersection penalty
    let mut routes: HashMap<u128, Vec<Pos2>> = HashMap::new();

    // Deterministic edge order to reduce oscillation
    let mut edges: Vec<_> = g.edges_iter().collect();
    edges.sort_by_key(|(eid, _)| eid.index());

    // Group potential bidirectional pairs (min,max) -> Vec<(edge_idx,bool(is_forward))>
    let mut pairings: HashMap<(NodeIndex<IndexTy>, NodeIndex<IndexTy>), Vec<(petgraph::stable_graph::EdgeIndex<IndexTy>, bool)>> = HashMap::new();
    for (eidx, _e) in &edges {
        if let Some((s, t)) = g.edge_endpoints(*eidx) {
            let (a, b) = if s.index() <= t.index() { (s, t) } else { (t, s) };
            let is_forward = s.index() <= t.index();
            pairings.entry((a, b)).or_default().push((*eidx, is_forward));
        }
    }

    // Helper: evaluate candidate path and pick best by (node_collision, intersections, length)
    fn poly_len(pts: &[Pos2]) -> u32 { super::router::polyline_length(pts) as u32 }

    fn build_route(from: Pos2, mids: &[Pos2], to: Pos2) -> Vec<Pos2> {
        let mut v = Vec::with_capacity(mids.len() + 2);
        v.push(from);
        v.extend_from_slice(mids);
        v.push(to);
        v
    }

    fn route_collides_with_nodes(
        route: &[Pos2],
        node_bounds: &HashMap<NodeIndex<IndexTy>, (Pos2, Pos2)>,
        skip_a: NodeIndex<IndexTy>,
        skip_b: NodeIndex<IndexTy>,
    ) -> bool {
        if route.len() < 2 { return false; }
        if node_bounds.is_empty() { return false; }
        for w in route.windows(2) {
            let a = w[0];
            let b = w[1];
            for (nid, &(min, max)) in node_bounds {
                if *nid == skip_a || *nid == skip_b { continue; }
                let (min_i, max_i) = inflate(min, max, EDGE_COLLISION_MARGIN);
                if rect_intersects_segment(min_i, max_i, a, b) { return true; }
            }
        }
        false
    }

    fn evaluate_candidate(
        candidate_mids: Vec<Pos2>,
        from: Pos2,
        to: Pos2,
        node_bounds: &HashMap<NodeIndex<IndexTy>, (Pos2, Pos2)>,
        skip_a: NodeIndex<IndexTy>,
        skip_b: NodeIndex<IndexTy>,
        existing: &HashMap<u128, Vec<Pos2>>,
        current_best: &mut (u8, u32, u32),
        current_points: &mut Option<Vec<Pos2>>,
    ) -> bool {
        let mut r = build_route(from, &candidate_mids, to);
        simplify(&mut r);
        let node_col = route_collides_with_nodes(&r, node_bounds, skip_a, skip_b) as u8;
        let inter = count_route_intersections(&r, existing) as u32;
        let len = poly_len(&r);
        let metric = (node_col, inter, len);
        if metric < *current_best {
            *current_best = metric;
            *current_points = Some(r);
        }
        metric == (0, 0, len)
    }

    fn generate_bidir_points(from: Pos2, to: Pos2, offset: f32, stub: f32, normal_sign: f32) -> Vec<Pos2> {
        let v = to - from;
        let dist = v.length();
        if dist <= f32::EPSILON { return Vec::new(); }
        let t = v / dist;
        let n = Vec2::new(-t.y, t.x) * normal_sign;
        let off = n * offset;
        let stub_c = stub.min(dist / 2.0 - 1.0).max(0.0);
        if stub_c <= 0.0 {
            return vec![(from + to.to_vec2()) * 0.5 + off];
        }
        let stub_v = t * stub_c;
        let first = from + stub_v + off;
        let middle = (from + to.to_vec2()) * 0.5 + off;
        let second = to - stub_v + off;
        vec![first, middle, second]
    }

    fn generate_axis_detours(from: Pos2, to: Pos2) -> Vec<Vec<Pos2>> {
        // Clearance tuned to the CLASS box + margin
        let vertical_clearance = 12.0 + EDGE_COLLISION_MARGIN * 4.0;   // height(=12) + margin
        let horizontal_clearance = 40.0 + EDGE_COLLISION_MARGIN * 4.0; // width(=40) + margin

        let mut cands = Vec::new();
        let horizontal_span = (from.x - to.x).abs();
        let vertical_span = (from.y - to.y).abs();
        if horizontal_span > 20.0 {
            // above
            let y = from.y.min(to.y) - vertical_clearance;
            cands.push(vec![Pos2::new(from.x, y), Pos2::new(to.x, y)]);
            // below
            let y2 = from.y.max(to.y) + vertical_clearance;
            cands.push(vec![Pos2::new(from.x, y2), Pos2::new(to.x, y2)]);
        }
        if vertical_span > 12.0 {
            // left
            let x = from.x.min(to.x) - horizontal_clearance;
            cands.push(vec![Pos2::new(x, from.y), Pos2::new(x, to.y)]);
            // right
            let x2 = from.x.max(to.x) + horizontal_clearance;
            cands.push(vec![Pos2::new(x2, from.y), Pos2::new(x2, to.y)]);
        }
        cands
    }

    // Trim first/last points to node shape boundaries using DisplayNode geometry.
    fn trim_endpoints<Nd: DisplayNode<Directed>>(
        start_node: &crate::Node<Directed, Nd>,
        end_node: &crate::Node<Directed, Nd>,
        route: &mut Vec<Pos2>,
        start_is_inner: bool,
    ) {
        if route.len() < 2 { return; }
        // start
        let s_center = start_node.location();
        let s_next = route[1];
        let s_dir = s_next - s_center;
        if !start_is_inner {
            // Keep oxdraw-viewer's "yellow port" dot stable when the start is an inner anchor
            route[0] = start_node.display().closest_boundary_point(s_dir);
        }
        // end
        let e_center = end_node.location();
        let last_idx = route.len() - 1;
        let e_prev = route[last_idx - 1];
        let e_dir = e_prev - e_center;
        route[last_idx] = end_node.display().closest_boundary_point(e_dir);
    }

    // Main pass: try to resolve bidirectional pairs first (best visual symmetry),
    // then plan remaining edges individually.

    // Track which edges have been routed by their `EdgeIndex` so we don't duplicate work
    use std::collections::HashSet as StdHashSet;
    let mut routed_edges: StdHashSet<petgraph::stable_graph::EdgeIndex<IndexTy>> = StdHashSet::new();

    // 1) Handle pairs
    for ((_, _), entries) in pairings.iter() {
        let fwd: Vec<_> = entries.iter().copied().filter(|(_, f)| *f).collect();
        let bwd: Vec<_> = entries.iter().copied().filter(|(_, f)| !*f).collect();
        if fwd.is_empty() || bwd.is_empty() { continue; }
        // Use only the first pair for symmetry; others fall back to single-edge logic below
        let (f_eidx, _) = fwd[0];
        let (b_eidx, _) = bwd[0];
        if routed_edges.contains(&f_eidx) || routed_edges.contains(&b_eidx) { continue; }

        // Resolve pair with mirrored offsets
        let (sf, tf) = match g.edge_endpoints(f_eidx) { Some(v) => v, None => continue };
        let (sb, tb) = match g.edge_endpoints(b_eidx) { Some(v) => v, None => continue };
        let sn = g.node(sf).unwrap();
        let tn = g.node(tf).unwrap();
        let snb = g.node(sb).unwrap();
        let tnb = g.node(tb).unwrap();
        let ef = g.edge(f_eidx).unwrap();
        let eb = g.edge(b_eidx).unwrap();

        let (from, to) = compute_edge_anchors::<Nd>(sn, tn, ef.start_maybe_inner());
        let (from_b, to_b) = compute_edge_anchors::<Nd>(snb, tnb, eb.start_maybe_inner());

        // If anchors are degenerate or pairs aren't opposite, skip symmetric handling
        let v = to - from; if v.length_sq() <= f32::EPSILON { continue; }

        let distance = v.length();
        let base_offset = (distance * 0.25).min(EDGE_BIDIRECTIONAL_OFFSET);
        let base_stub = (distance * 0.25).min(EDGE_BIDIRECTIONAL_STUB);
        if base_offset <= 0.0 || base_stub <= 0.0 { continue; }

        let mut best_f: Option<Vec<Pos2>> = None;
        let mut best_b: Option<Vec<Pos2>> = None;
        let mut best_metric = (u8::MAX, u32::MAX, u32::MAX);

        for attempt in 0..=EDGE_COLLISION_MAX_ITER {
            let off = (base_offset + attempt as f32 * EDGE_BIDIRECTIONAL_OFFSET_STEP)
                .min((distance * 0.5) - EDGE_COLLISION_MARGIN)
                .max(base_offset);
            let stub = (base_stub + attempt as f32 * EDGE_BIDIRECTIONAL_STUB_STEP)
                .min((distance * 0.5) - EDGE_COLLISION_MARGIN)
                .max(base_stub);

            let f_mids = generate_bidir_points(from, to, off, stub, 1.0);
            let mut b_mids = generate_bidir_points(from_b, to_b, off, stub, -1.0);
            b_mids.reverse();

            let mut cur_best = best_metric;
            let mut cur_points: Option<Vec<Pos2>> = None;
            let done_f = evaluate_candidate(
                f_mids.clone(), from, to, &node_bounds, sf, tf, &routes, &mut cur_best, &mut cur_points,
            );
            let route_f = cur_points.clone();

            let done_b = evaluate_candidate(
                b_mids.clone(), from_b, to_b, &node_bounds, sb, tb, &routes, &mut cur_best, &mut cur_points,
            );
            if let Some(rf) = route_f { best_f = Some(rf); }
            if let Some(cb) = cur_points { best_b = Some(cb); }
            best_metric = cur_best;

            if done_f && done_b { break; }
        }

        // If still not perfect, try axis detours for each side independently
        if best_f.is_none() || best_metric.0 > 0 {
            for cand in generate_axis_detours(from, to) {
                let mut cur_best = best_metric;
                let mut cur_points: Option<Vec<Pos2>> = None;
                let done = evaluate_candidate(
                    cand.clone(), from, to, &node_bounds, sf, tf, &routes, &mut cur_best, &mut cur_points,
                );
                if let Some(r) = cur_points { best_f = Some(r); best_metric = cur_best; }
                if done { break; }
            }
        }
        if best_b.is_none() || best_metric.0 > 0 {
            for cand in generate_axis_detours(from_b, to_b) {
                let mut cur_best = best_metric;
                let mut cur_points: Option<Vec<Pos2>> = None;
                let done = evaluate_candidate(
                    cand.clone(), from_b, to_b, &node_bounds, sb, tb, &routes, &mut cur_best, &mut cur_points,
                );
                if let Some(r) = cur_points { best_b = Some(r); best_metric = cur_best; }
                if done { break; }
            }
        }

        // Fallback: straight lines
        let route_key_f = route_key(sf, tf, ef.order());
        let route_key_b = route_key(sb, tb, eb.order());
        let mut rf = best_f.unwrap_or_else(|| vec![from, to]);
        let mut rb = best_b.unwrap_or_else(|| vec![from_b, to_b]);
        let start_is_inner_f = matches!(ef.start_maybe_inner(), MaybeInner::Inner { .. });
        let start_is_inner_b = matches!(eb.start_maybe_inner(), MaybeInner::Inner { .. });
        if !IGNORE_NODE_VOLUME {
            trim_endpoints::<Nd>(sn, tn, &mut rf, start_is_inner_f);
            trim_endpoints::<Nd>(snb, tnb, &mut rb, start_is_inner_b);
        }
        routes.insert(route_key_f, rf);
        routes.insert(route_key_b, rb);

        routed_edges.insert(f_eidx);
        routed_edges.insert(b_eidx);
    }

    // 2) Plan the rest individually
    for (eidx, e) in edges.into_iter() {
        if routed_edges.contains(&eidx) { continue; }
        let (s_idx, t_idx) = match g.edge_endpoints(eidx) { Some(v) => v, None => continue };
        let s_node = g.node(s_idx).unwrap();
        let t_node = g.node(t_idx).unwrap();
        let (from, to) = compute_edge_anchors::<Nd>(s_node, t_node, e.start_maybe_inner());
        let v = to - from; if v.length_sq() <= f32::EPSILON { continue; }
        let distance = v.length();

        let mut best_metric = (u8::MAX, u32::MAX, u32::MAX);
        let mut best: Option<Vec<Pos2>> = None;

        // 先尝试直线（能直就直）
        {
            let mut cur_best = best_metric;
            let mut cur_points: Option<Vec<Pos2>> = None;
            let perfect = evaluate_candidate(
                Vec::new(), from, to, &node_bounds, s_idx, t_idx, &routes, &mut cur_best, &mut cur_points,
            );
            if let Some(r) = cur_points.clone() {
                best = Some(r);
                best_metric = cur_best;
            }
            // 若无碰撞且无交叉，直接采用直线
            if perfect {
                let mut r = best.unwrap();
                if !IGNORE_NODE_VOLUME {
                    let start_is_inner = matches!(e.start_maybe_inner(), MaybeInner::Inner { .. });
                    trim_endpoints::<Nd>(s_node, t_node, &mut r, start_is_inner);
                }
                routes.insert(route_key(s_idx, t_idx, e.order()), r);
                continue;
            }
        }

        // Try both sides with iterative offsets
        for &sign in &[1.0_f32, -1.0_f32] {
            let base_offset = (distance * 0.25).min(EDGE_SINGLE_OFFSET);
            let base_stub = (distance * 0.25).min(EDGE_SINGLE_STUB);
            if base_offset <= 0.0 || base_stub <= 0.0 { continue; }
            for attempt in 0..=EDGE_COLLISION_MAX_ITER {
                let off = (base_offset + attempt as f32 * EDGE_SINGLE_OFFSET_STEP)
                    .min((distance * 0.5) - EDGE_COLLISION_MARGIN)
                    .max(base_offset);
                let stub = (base_stub + attempt as f32 * EDGE_SINGLE_STUB_STEP)
                    .min((distance * 0.5) - EDGE_COLLISION_MARGIN)
                    .max(base_stub);
                let mids = generate_bidir_points(from, to, off, stub, sign);
                let mut cur_best = best_metric;
                let mut cur_points: Option<Vec<Pos2>> = None;
                let done = evaluate_candidate(
                    mids, from, to, &node_bounds, s_idx, t_idx, &routes, &mut cur_best, &mut cur_points,
                );
                if let Some(r) = cur_points { best = Some(r); best_metric = cur_best; }
                if done { break; }
            }
        }

        // Try axis detours if needed
        if best.is_none() || best_metric.0 > 0 {
            for cand in generate_axis_detours(from, to) {
                let mut cur_best = best_metric;
                let mut cur_points: Option<Vec<Pos2>> = None;
                let done = evaluate_candidate(
                    cand, from, to, &node_bounds, s_idx, t_idx, &routes, &mut cur_best, &mut cur_points,
                );
                if let Some(r) = cur_points { best = Some(r); best_metric = cur_best; }
                if done { break; }
            }
        }

        let mut r = best.unwrap_or_else(|| vec![from, to]);
        if !IGNORE_NODE_VOLUME {
            let start_is_inner = matches!(e.start_maybe_inner(), MaybeInner::Inner { .. });
            trim_endpoints::<Nd>(s_node, t_node, &mut r, start_is_inner);
        }
        routes.insert(route_key(s_idx, t_idx, e.order()), r);
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
