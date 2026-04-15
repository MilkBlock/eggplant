use crate::{
    butler_portugal::{Tensor, canonicalize},
    butler_portugal::canonicalization::{BSGS, Permutation},
    butler_portugal::schreier_sims::{is_member, schreier_sims},
    prelude::SlotMeta,
    wrap::{EgglogNode, PatRec, PatRecSgl, SlotVarID, Sym, Syms},
};
use dashmap::DashMap;
use derive_more::{Deref, DerefMut};
use egglog::{EGraph, Value, util::IndexMap};
use indexmap::IndexSet;
use std::sync::{
    Arc, OnceLock,
    atomic::{AtomicUsize, Ordering},
};

pub type FuncName = &'static str;
pub type SortName = &'static str;

pub type SEClassID = usize;
pub type SENodeID = usize;
pub type SlottedEClassId = SEClassID;
pub type SlottedENodeId = SENodeID;
#[derive(Debug, Clone)]
pub struct SENode {
    seclass_id: SEClassID,
    senode_id: SENodeID,
    ty_name: FuncName,
    repr: Vec<Vec<usize>>,
    renaming: SlotMeta,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SlottedShapeKey {
    ty_name: FuncName,
    de_bruijn: Vec<Vec<usize>>,
}

impl SlottedShapeKey {
    pub fn new(ty_name: FuncName, de_bruijn: Vec<Vec<usize>>) -> Self {
        Self {
            ty_name,
            de_bruijn,
        }
    }

    fn from_meta(ty_name: FuncName, meta: &SlotMeta) -> Self {
        Self::new(ty_name, meta.get_current_layer_de_bruijn())
    }

    pub fn ty_name(&self) -> FuncName {
        self.ty_name
    }

    pub fn de_bruijn(&self) -> &[Vec<usize>] {
        &self.de_bruijn
    }
}

#[derive(Debug, Clone)]
pub struct ShapeWitness {
    senode_id: SENodeID,
    renaming: SlotMeta,
}

impl ShapeWitness {
    pub fn senode_id(&self) -> SlottedENodeId {
        self.senode_id
    }

    pub fn renaming(&self) -> &SlotMeta {
        &self.renaming
    }
}

#[derive(Debug, Clone)]
pub struct ShapeEntry {
    witnesses: Vec<ShapeWitness>,
}

impl ShapeEntry {
    fn singleton(senode_id: SENodeID, renaming: SlotMeta) -> Self {
        Self {
            witnesses: vec![ShapeWitness { senode_id, renaming }],
        }
    }

    pub fn witnesses(&self) -> &[ShapeWitness] {
        &self.witnesses
    }

    pub fn senode_id(&self) -> SlottedENodeId {
        self.witnesses
            .first()
            .map(|w| w.senode_id)
            .expect("shape entry should have at least one witness")
    }

    fn add_witness(&mut self, senode_id: SENodeID, renaming: SlotMeta) {
        let exists = self
            .witnesses
            .iter()
            .any(|w| w.senode_id == senode_id && w.renaming == renaming);
        if !exists {
            self.witnesses.push(ShapeWitness { senode_id, renaming });
        }
    }
}

pub type SlottedShapeEntry = ShapeEntry;

#[derive(Debug, Clone)]
pub struct SEClass {
    seclass_id: SEClassID,
    slots: IndexSet<SlotVarID>,
    senode_ids: Vec<SENodeID>,
    shapes: IndexMap<SlottedShapeKey, ShapeEntry>,
    group: SlottedSymmetryGroup,
}

impl SEClass {
    pub fn eclass_id(&self) -> SlottedEClassId {
        self.seclass_id
    }

    pub fn slots(&self) -> &IndexSet<SlotVarID> {
        &self.slots
    }

    pub fn senode_ids(&self) -> &[SENodeID] {
        &self.senode_ids
    }

    pub fn shapes(&self) -> &IndexMap<SlottedShapeKey, ShapeEntry> {
        &self.shapes
    }

    pub fn group(&self) -> &SlottedSymmetryGroup {
        &self.group
    }

    fn canonical_shape_key(&self, ty_name: FuncName, shape: &[Vec<usize>]) -> SlottedShapeKey {
        SlottedShapeKey::new(ty_name, self.group.canonicalize_shape(shape))
    }

    fn rebuild_shape_index(&mut self, senodes: &IndexMap<SENodeID, SENode>) -> Vec<SENodeID> {
        let mut rebuilt = IndexMap::default();
        let mut compacted_ids = Vec::new();
        let mut removed_ids = Vec::new();
        for senode_id in &self.senode_ids {
            let senode = senodes
                .get(senode_id)
                .unwrap_or_else(|| panic!("senode_id {} missing while rebuilding shape index", senode_id));
            let key = self.canonical_shape_key(senode.ty_name, &senode.repr);
            let is_new_shape = !rebuilt.contains_key(&key);
            rebuilt
                .entry(key)
                .or_insert_with(|| ShapeEntry::singleton(*senode_id, senode.renaming.clone()))
                .add_witness(*senode_id, senode.renaming.clone());
            if cfg!(feature = "slotted-debug-keep-duplicate-senodes") || is_new_shape {
                compacted_ids.push(*senode_id);
            } else {
                removed_ids.push(*senode_id);
            }
        }
        self.senode_ids = compacted_ids;
        self.shapes = rebuilt;
        removed_ids
    }

    fn find_matching_shape(&self, ty_name: FuncName, shape: &[Vec<usize>]) -> Option<SENodeID> {
        let key = self.canonical_shape_key(ty_name, shape);
        self.shapes.get(&key).map(|entry| entry.senode_id())
    }
}

pub type SlottedEClass = SEClass;

#[derive(Debug, Clone)]
pub struct SEClassesWithCanoValue {
    // one erased egraph may contains several slotted eclass
    senodes: IndexMap<SENodeID, SENode>,
    // maintain a mapping from seclass 2 senodes and a mapping from senode
    seclass2senodes: IndexMap<SEClassID, SEClass>,
    ty2senodes: IndexMap<FuncName, Vec<SENodeID>>,
    cano_value: Value,
}

impl SEClassesWithCanoValue {
    fn new(cano_value: Value) -> Self {
        Self {
            cano_value,
            senodes: {
                let mut senodes = IndexMap::default();
                senodes
            },
            seclass2senodes: {
                let mut seclass2senodes: IndexMap<usize, SEClass> = IndexMap::default();
                seclass2senodes
            },
            ty2senodes: {
                let mut ty2senodes: IndexMap<FuncName, Vec<usize>> = IndexMap::default();
                ty2senodes
            },
        }
    }

    fn merge_from(&mut self, other: SEClassesWithCanoValue) {
        for (senode_id, senode) in other.senodes {
            self.senodes.insert(senode_id, senode);
        }
        for (seclass_id, seclass) in other.seclass2senodes {
            self.seclass2senodes.insert(seclass_id, seclass);
        }
        for (ty_name, mut senode_ids) in other.ty2senodes {
            let existing = self.ty2senodes.entry(ty_name).or_default();
            for senode_id in senode_ids.drain(..) {
                if !existing.contains(&senode_id) {
                    existing.push(senode_id);
                }
            }
        }
    }

    fn prune_senodes(&mut self, removed_ids: &[SENodeID]) {
        if removed_ids.is_empty() {
            return;
        }
        let removed: std::collections::HashSet<_> = removed_ids.iter().copied().collect();
        for senode_id in removed_ids {
            self.senodes.swap_remove(senode_id);
        }
        for senode_ids in self.ty2senodes.values_mut() {
            senode_ids.retain(|id| !removed.contains(id));
        }
    }

    fn snapshot(&self) -> SlottedBucket {
        SlottedBucket {
            canonical_value: self.cano_value,
            eclasses: self.seclass2senodes.values().cloned().collect(),
            senode_count: self.senodes.len(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct SlottedBucket {
    canonical_value: Value,
    eclasses: Vec<SEClass>,
    senode_count: usize,
}

impl SlottedBucket {
    pub fn canonical_value(&self) -> Value {
        self.canonical_value
    }

    pub fn eclasses(&self) -> &[SEClass] {
        &self.eclasses
    }

    pub fn senode_count(&self) -> usize {
        self.senode_count
    }

    pub fn seclass_count(&self) -> usize {
        self.eclasses.len()
    }
}

#[derive(Clone, Debug)]
pub struct SlottedSymmetryGroup {
    representative: Vec<Vec<usize>>,
    generators: Vec<Permutation>,
    bsgs: Option<BSGS>,
}
impl SlottedSymmetryGroup {
    fn new(shape: Vec<Vec<usize>>) -> Self {
        Self {
            representative: shape,
            generators: Vec::new(),
            bsgs: None,
        }
    }

    pub fn representative(&self) -> &[Vec<usize>] {
        &self.representative
    }

    pub fn generators(&self) -> &[Permutation] {
        &self.generators
    }

    pub fn canonicalize_shape(&self, shape: &[Vec<usize>]) -> Vec<Vec<usize>> {
        if self.generators.is_empty() {
            return shape.to_vec();
        }
        let mut best = shape.to_vec();
        let degree = self.slot_count().max(
            shape.iter()
                .flat_map(|part| part.iter().copied())
                .max()
                .map(|x| x + 1)
                .unwrap_or(0),
        );
        let bsgs = self
            .bsgs
            .clone()
            .unwrap_or_else(|| schreier_sims(&self.generators, degree));
        for perm in enumerate_group_elements(&bsgs, degree) {
            let candidate = apply_permutation_to_shape(shape, &perm);
            if candidate < best {
                best = candidate;
            }
        }
        best
    }

    fn rebuild_bsgs(&mut self) {
        let degree = self.slot_count();
        self.bsgs = Some(schreier_sims(&self.generators, degree));
    }

    fn slot_count(&self) -> usize {
        self.representative
            .iter()
            .flat_map(|part| part.iter().copied())
            .max()
            .map(|x| x + 1)
            .unwrap_or(0)
    }

    fn contains(&self, target: &[Vec<usize>]) -> bool {
        self.canonicalize_shape(target) == self.canonicalize_shape(&self.representative)
    }

    fn add_generator_for(&mut self, target: &[Vec<usize>]) {
        let Some(perm) = permutation_between(&self.representative, target) else {
            return;
        };
        if perm.iter().enumerate().all(|(idx, mapped)| idx == *mapped) {
            return;
        }
        if !self.generators.contains(&perm) {
            self.generators.push(perm);
            self.rebuild_bsgs();
        }
    }

    fn merge_from(&mut self, other: &SlottedSymmetryGroup) {
        self.add_generator_for(&other.representative);
        for generator in &other.generators {
            if !self.generators.contains(generator) {
                self.generators.push(generator.clone());
            }
        }
        self.rebuild_bsgs();
    }
}

fn apply_permutation_to_shape(shape: &[Vec<usize>], perm: &[usize]) -> Vec<Vec<usize>> {
    shape
        .iter()
        .map(|part| {
            part.iter()
                .map(|slot| perm.get(*slot).copied().unwrap_or(*slot))
                .collect()
        })
        .collect()
}

fn enumerate_group_elements(bsgs: &BSGS, degree: usize) -> Vec<Permutation> {
    use std::collections::HashSet;

    fn enumerate_recursive(
        generators: &[Permutation],
        current: &[usize],
        results: &mut Vec<Permutation>,
        visited: &mut HashSet<Vec<usize>>,
    ) {
        if !visited.insert(current.to_owned()) {
            return;
        }
        results.push(current.to_owned());
        for generator in generators {
            let next = crate::butler_portugal::schreier_sims::compose_permutations(current, generator);
            enumerate_recursive(generators, &next, results, visited);
        }
    }

    let mut results = Vec::new();
    let mut visited = HashSet::new();
    let identity: Permutation = (0..degree).collect();
    enumerate_recursive(&bsgs.generators, &identity, &mut results, &mut visited);
    results
}

#[derive(Debug)]
pub struct SlottedCtx {
    // slotted id info
    next_seclass_id: AtomicUsize,
    next_senode_id: AtomicUsize,

    cano_value2seclasses: DashMap<Value, SEClassesWithCanoValue>,

    pending_ops: crossbeam::queue::SegQueue<SlotPendingOps>,
}
impl Default for SlottedCtx {
    fn default() -> Self {
        Self::new()
    }
}

pub(crate) fn shared_slotted_ctx() -> Arc<SlottedCtx> {
    static SHARED: OnceLock<Arc<SlottedCtx>> = OnceLock::new();
    Arc::clone(SHARED.get_or_init(|| Arc::new(SlottedCtx::new())))
}
impl SlottedCtx {
    pub fn new() -> Self {
        Self {
            next_seclass_id: AtomicUsize::new(0),
            next_senode_id: AtomicUsize::new(0),
            cano_value2seclasses: Default::default(),
            pending_ops: Default::default(),
        }
    }
    // pending insert
    // pub fn insert<PR: SlottedPatRecSgl>(&mut self, cano_value: Value, meta: Vec<SlotMeta<PR>>) {}
    pub fn push_pending(&self, pended: SlotPendingOps) {
        self.pending_ops.push(pended);
    }

    pub fn clear(&self) {
        self.cano_value2seclasses.clear();
    }

    pub fn bucket_count(&self) -> usize {
        self.cano_value2seclasses.len()
    }

    pub fn bucket(&self, cano_value: Value) -> Option<SlottedBucket> {
        self.cano_value2seclasses
            .get(&cano_value)
            .map(|entry| entry.snapshot())
    }

    pub fn buckets(&self) -> Vec<SlottedBucket> {
        self.cano_value2seclasses
            .iter()
            .map(|entry| entry.snapshot())
            .collect()
    }

    pub fn flush_pending(&self, egraph: &EGraph) {
        log::debug!("flush_pending");
        while let Some(pended) = self.pending_ops.pop() {
            log::debug!("processing {:?}", pended);
            match pended {
                SlotPendingOps::Insert { inputs, output } => {
                    let output_cano_value = egraph.get_canonical_value(
                        output.2,
                        egraph
                            .get_sort_by_name(output.0)
                            .unwrap_or_else(|| panic!("missing sort `{}` for slotted insert", output.0)),
                    );
                    // find seclasses
                    let mut seclasses = self
                        .cano_value2seclasses
                        .entry(output_cano_value)
                        .or_insert(SEClassesWithCanoValue::new(output_cano_value));

                    // then find the satisfied enode or create new
                    if let Some(enode_id) = find_satisfied_enode(&seclasses, &inputs, &output) {
                        let target_shape = output.3.get_current_layer_de_bruijn();
                        let target_ty = output.1;
                        let seclass_id = seclasses
                            .senodes
                            .get(&enode_id)
                            .map(|node| node.seclass_id)
                            .unwrap_or_else(|| panic!("matching senode {enode_id} missing"));
                        let senodes_snapshot = seclasses.senodes.clone();
                        let (removed_ids, canonical_key) =
                            if let Some(target) = seclasses.seclass2senodes.get_mut(&seclass_id) {
                                target.group.add_generator_for(&target_shape);
                                let removed_ids = target.rebuild_shape_index(&senodes_snapshot);
                                let canonical_key =
                                    target.canonical_shape_key(target_ty, &target_shape);
                                (removed_ids, canonical_key)
                            } else {
                                (Vec::new(), SlottedShapeKey::new(target_ty, target_shape.clone()))
                            };
                        seclasses.prune_senodes(&removed_ids);
                        if let Some(target) = seclasses.seclass2senodes.get_mut(&seclass_id) {
                            if let Some(entry) = target.shapes.get_mut(&canonical_key) {
                                entry.add_witness(enode_id, output.3.clone());
                            }
                        }
                    } else {
                        // create new
                        let senode_id = self.next_senode_id.fetch_add(1, Ordering::SeqCst);
                        let seclass_id = self.next_seclass_id.fetch_add(1, Ordering::SeqCst);
                        let shape_key = SlottedShapeKey::from_meta(output.1, &output.3);
                        let class_slots = output.3.var_id_set.iter().cloned().collect();
                        seclasses.senodes.insert(
                            senode_id,
                            SENode {
                                seclass_id,
                                senode_id,
                                ty_name: output.1,
                                repr: output.3.get_current_layer_de_bruijn(),
                                renaming: output.3.clone(),
                            },
                        );
                        seclasses
                            .seclass2senodes
                            .entry(seclass_id)
                            .insert_entry(SEClass {
                                seclass_id,
                                slots: class_slots,
                                senode_ids: vec![senode_id],
                                shapes: IndexMap::from_iter([(
                                    shape_key,
                                    ShapeEntry::singleton(senode_id, output.3.clone()),
                                )]),
                                group: SlottedSymmetryGroup::new(
                                    output.3.get_current_layer_de_bruijn(),
                                ),
                            });
                        seclasses.ty2senodes.entry(output.1).or_default().push(senode_id);
                    }

                    log::debug!("inputs:{:?} output:{:?}", inputs, output);
                }
                SlotPendingOps::Union(a, b) => {
                    log::debug!("union :{:?} {:?}", a, b);
                    let cano_a = egraph.get_canonical_value(
                        a.2,
                        egraph
                            .get_sort_by_name(a.0)
                            .unwrap_or_else(|| panic!("missing sort `{}` for slotted union lhs", a.0)),
                    );
                    let cano_b = egraph.get_canonical_value(
                        b.2,
                        egraph
                            .get_sort_by_name(b.0)
                            .unwrap_or_else(|| panic!("missing sort `{}` for slotted union rhs", b.0)),
                    );

                    let mut seclasses = self
                        .cano_value2seclasses
                        .remove(&cano_a)
                        .map(|(_, bucket)| bucket)
                        .unwrap_or_else(|| SEClassesWithCanoValue::new(cano_a));
                    for key in [a.2, b.2, cano_b] {
                        if key == cano_a {
                            continue;
                        }
                        if let Some((_, other)) = self.cano_value2seclasses.remove(&key) {
                            seclasses.merge_from(other);
                        }
                    }

                    let senode_a = find_senode_for_output(&seclasses, &a);
                    let senode_b = find_senode_for_output(&seclasses, &b);
                    let (Some(senode_a), Some(senode_b)) = (senode_a, senode_b) else {
                        self.cano_value2seclasses.insert(cano_a, seclasses);
                        continue;
                    };
                    let seclass_a = seclasses
                        .senodes
                        .get(&senode_a)
                        .map(|x| x.seclass_id)
                        .unwrap();
                    let seclass_b = seclasses
                        .senodes
                        .get(&senode_b)
                        .map(|x| x.seclass_id)
                        .unwrap();
                    if seclass_a == seclass_b {
                        let senodes_snapshot = seclasses.senodes.clone();
                        if let Some(target) = seclasses.seclass2senodes.get_mut(&seclass_a) {
                            target.group.add_generator_for(&a.3.get_current_layer_de_bruijn());
                            target.group.add_generator_for(&b.3.get_current_layer_de_bruijn());
                            let removed_ids = target.rebuild_shape_index(&senodes_snapshot);
                            seclasses.prune_senodes(&removed_ids);
                        }
                        self.cano_value2seclasses.insert(cano_a, seclasses);
                        continue;
                    }
                    let merged = seclasses
                        .seclass2senodes
                        .remove(&seclass_b)
                        .unwrap_or_else(|| panic!("missing source seclass {}", seclass_b));
                    let merged_ids = merged.senode_ids;
                    let senodes_snapshot = seclasses.senodes.clone();
                    for senode_id in &merged_ids {
                        if let Some(senode) = seclasses.senodes.get_mut(senode_id) {
                            senode.seclass_id = seclass_a;
                        }
                    }
                    let target = seclasses
                        .seclass2senodes
                        .get_mut(&seclass_a)
                        .unwrap_or_else(|| panic!("missing target seclass {}", seclass_a));
                    let merged_slots = merged.slots;
                    let merged_group = merged.group;
                    target.slots = target
                        .slots
                        .intersection(&merged_slots)
                        .cloned()
                        .collect();
                    target.group.merge_from(&merged_group);
                    for senode_id in merged_ids {
                        if !target.senode_ids.contains(&senode_id) {
                            target.senode_ids.push(senode_id);
                        }
                    }
                    let removed_ids = target.rebuild_shape_index(&senodes_snapshot);
                    seclasses.prune_senodes(&removed_ids);
                    self.cano_value2seclasses.insert(cano_a, seclasses);
                }
            }
        }
    }
    pub fn add_pending_ops() {}

    pub fn seclass_count(&self, cano_value: Value) -> usize {
        self.cano_value2seclasses
            .get(&cano_value)
            .map(|entry| entry.seclass2senodes.len())
            .unwrap_or(0)
    }

    pub fn senode_count(&self, cano_value: Value) -> usize {
        self.cano_value2seclasses
            .get(&cano_value)
            .map(|entry| entry.senodes.len())
            .unwrap_or(0)
    }

    pub fn eclasses(&self, cano_value: Value) -> Vec<SEClass> {
        self.bucket(cano_value)
            .map(|bucket| bucket.eclasses)
            .unwrap_or_default()
    }
}

fn find_satisfied_enode(
    seclasses: &dashmap::mapref::one::RefMut<'_, Value, SEClassesWithCanoValue>,
    _inputs: &[FuncValueMetaInner],
    (_output_sort, output_func, output_cano_val, output_meta): &FuncValueMetaInner,
) -> Option<SENodeID> {
    let _ = output_cano_val;
    let target_shape = output_meta.get_current_layer_de_bruijn();
    let senode_ids = seclasses.ty2senodes.get(output_func)?;
    let mut visited_classes = std::collections::HashSet::new();
    for enode_id in senode_ids.iter() {
        let senode = seclasses
            .senodes
            .get(enode_id)
            .unwrap_or_else(|| panic!("senode_id {} missing from ty index", enode_id));
        if !visited_classes.insert(senode.seclass_id) {
            continue;
        }
        let seclass = seclasses
            .seclass2senodes
            .get(&senode.seclass_id)
            .unwrap_or_else(|| panic!("seclass {} missing from seclass index", senode.seclass_id));
        if let Some(senode_id) = seclass.find_matching_shape(*output_func, &target_shape) {
            return Some(senode_id);
        };
    }
    None
}

fn find_senode_for_output(
    seclasses: &SEClassesWithCanoValue,
    (_output_sort, output_func, _output_val, output_meta): &FuncValueMetaInner,
) -> Option<SENodeID> {
    let senode_ids = seclasses.ty2senodes.get(output_func)?;
    let target = output_meta.get_current_layer_de_bruijn();
    let mut visited_classes = std::collections::HashSet::new();
    for enode_id in senode_ids.iter() {
        let senode = seclasses
            .senodes
            .get(enode_id)
            .unwrap_or_else(|| panic!("senode_id {} missing from ty index", enode_id));
        if !visited_classes.insert(senode.seclass_id) {
            continue;
        }
        let seclass = seclasses
            .seclass2senodes
            .get(&senode.seclass_id)
            .unwrap_or_else(|| panic!("seclass {} missing from seclass index", senode.seclass_id));
        if let Some(senode_id) = seclass.find_matching_shape(*output_func, &target) {
            return Some(senode_id);
        }
    }
    None
}

fn permutation_between(base: &[Vec<usize>], target: &[Vec<usize>]) -> Option<Permutation> {
    let base_flat: Vec<usize> = base.iter().flat_map(|part| part.iter().copied()).collect();
    let target_flat: Vec<usize> = target.iter().flat_map(|part| part.iter().copied()).collect();
    if base_flat.len() != target_flat.len() {
        return None;
    }

    let degree = base_flat
        .iter()
        .chain(target_flat.iter())
        .copied()
        .max()
        .map(|x| x + 1)
        .unwrap_or(0);
    let mut perm: Vec<Option<usize>> = vec![None; degree];
    let mut used_targets = vec![false; degree];

    for (src, dst) in base_flat.into_iter().zip(target_flat.into_iter()) {
        match perm[src] {
            Some(existing) if existing != dst => return None,
            Some(_) => {}
            None => {
                if used_targets[dst] {
                    return None;
                }
                perm[src] = Some(dst);
                used_targets[dst] = true;
            }
        }
    }

    let mut next_unused = 0usize;
    let mut out = vec![0; degree];
    for i in 0..degree {
        if let Some(mapped) = perm[i] {
            out[i] = mapped;
        } else {
            while next_unused < degree && used_targets[next_unused] {
                next_unused += 1;
            }
            if next_unused >= degree {
                return None;
            }
            out[i] = next_unused;
            used_targets[next_unused] = true;
        }
    }
    Some(out)
}

#[cfg(test)]
mod symmetry_tests {
    use super::*;
    use indexmap::IndexSet;

    #[test]
    fn generators_enable_orbit_membership() {
        let rep = vec![vec![0], vec![1]];
        let swapped = vec![vec![1], vec![0]];
        let repeated = vec![vec![0], vec![0]];

        let mut group = SlottedSymmetryGroup::new(rep.clone());
        assert!(group.contains(&rep));
        assert!(!group.contains(&swapped));
        assert_eq!(group.canonicalize_shape(&rep), rep);

        group.add_generator_for(&swapped);
        assert!(group.contains(&swapped));
        assert_eq!(group.canonicalize_shape(&swapped), rep);
        assert!(!group.contains(&repeated));
    }

    #[cfg(not(feature = "slotted-debug-keep-duplicate-senodes"))]
    #[test]
    fn rebuild_shape_index_compacts_duplicate_senodes_by_default() {
        let senodes = IndexMap::from_iter([
            (
                0,
                SENode {
                    seclass_id: 0,
                    senode_id: 0,
                    ty_name: "Add",
                    repr: vec![vec![0], vec![1]],
                    renaming: SlotMeta::default(),
                },
            ),
            (
                1,
                SENode {
                    seclass_id: 0,
                    senode_id: 1,
                    ty_name: "Add",
                    repr: vec![vec![0], vec![1]],
                    renaming: SlotMeta::default(),
                },
            ),
        ]);
        let mut eclass = SEClass {
            seclass_id: 0,
            slots: IndexSet::default(),
            senode_ids: vec![0, 1],
            shapes: IndexMap::default(),
            group: SlottedSymmetryGroup::new(vec![vec![0], vec![1]]),
        };

        let removed = eclass.rebuild_shape_index(&senodes);

        assert_eq!(removed, vec![1]);
        assert_eq!(eclass.senode_ids, vec![0]);
        assert_eq!(eclass.shapes.len(), 1);
        assert_eq!(eclass.shapes.values().next().unwrap().witnesses().len(), 2);
    }

    #[cfg(feature = "slotted-debug-keep-duplicate-senodes")]
    #[test]
    fn rebuild_shape_index_keeps_duplicate_senodes_with_debug_feature() {
        let senodes = IndexMap::from_iter([
            (
                0,
                SENode {
                    seclass_id: 0,
                    senode_id: 0,
                    ty_name: "Add",
                    repr: vec![vec![0], vec![1]],
                    renaming: SlotMeta::default(),
                },
            ),
            (
                1,
                SENode {
                    seclass_id: 0,
                    senode_id: 1,
                    ty_name: "Add",
                    repr: vec![vec![0], vec![1]],
                    renaming: SlotMeta::default(),
                },
            ),
        ]);
        let mut eclass = SEClass {
            seclass_id: 0,
            slots: IndexSet::default(),
            senode_ids: vec![0, 1],
            shapes: IndexMap::default(),
            group: SlottedSymmetryGroup::new(vec![vec![0], vec![1]]),
        };

        let removed = eclass.rebuild_shape_index(&senodes);

        assert!(removed.is_empty());
        assert_eq!(eclass.senode_ids, vec![0, 1]);
        assert_eq!(eclass.shapes.len(), 1);
        assert_eq!(eclass.shapes.values().next().unwrap().witnesses().len(), 2);
    }
}

pub type _FuncValueMeta = (SortName, FuncName, egglog::Value, SlotMeta);
pub type FuncValueMeta = (SortName, FuncName, egglog::Value, SlotMeta);
pub type FuncValueMetaInner = (SortName, FuncName, egglog::Value, SlotMeta);
#[derive(Clone, Debug)]
pub enum SlotPendingOps {
    Insert {
        inputs: Vec<FuncValueMetaInner>,
        output: FuncValueMetaInner,
    },
    Union(FuncValueMetaInner, FuncValueMetaInner),
}

#[derive(DerefMut, Deref)]
pub struct SlotWorkAreaNode {
    pub next: Option<Sym>,
    pub prev: Option<Sym>,
    pub preds: Syms,
    #[deref]
    #[deref_mut]
    pub egglog: Box<dyn EgglogNode>,
    pub pulled_by: Option<egglog::Value>,
}

impl SlotWorkAreaNode {
    pub fn new(node: Box<dyn EgglogNode>) -> Self {
        Self {
            preds: Syms::default(),
            egglog: node,
            next: None,
            prev: None,
            pulled_by: None,
        }
    }
    pub fn new_pulled(node: Box<dyn EgglogNode>, pulled_by: egglog::Value) -> Self {
        Self {
            preds: Syms::default(),
            egglog: node,
            next: None,
            prev: None,
            pulled_by: Some(pulled_by),
        }
    }
    pub fn succs_mut(&mut self) -> impl Iterator<Item = &mut Sym> {
        self.egglog.succs_mut().into_iter()
    }
    pub fn preds_mut(&mut self) -> impl Iterator<Item = &mut Sym> {
        self.preds.iter_mut()
    }
    pub fn preds(&self) -> impl Iterator<Item = &Sym> {
        self.preds.iter()
    }
}

impl std::fmt::Debug for SlotWorkAreaNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} {} | {} | pulled_by {:?}",
            self.variant_name().unwrap_or(self.ty_name()),
            self.cur_sym(),
            self.to_egglog_string().unwrap_or(
                self.egglog
                    .basic_field_types()
                    .iter()
                    .map(|s| s.to_string())
                    .collect::<Vec<_>>()
                    .join(" ")
            ),
            self.pulled_by
        )
    }
}

impl Clone for SlotWorkAreaNode {
    fn clone(&self) -> Self {
        Self {
            next: self.next.clone(),
            preds: self.preds.clone(),
            egglog: self.egglog.clone_dyn(),
            prev: None,
            pulled_by: self.pulled_by,
        }
    }
}
