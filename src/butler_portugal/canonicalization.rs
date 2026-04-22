//! Butler-Portugal tensor canonicalization algorithm
//!
//! This module implements the core Butler-Portugal algorithm for bringing
//! tensors into canonical form by systematically applying symmetry operations.
//!
//! The algorithm is based on the double coset approach where a tensor with
//! slot symmetries S and dummy symmetries D is canonicalized by finding
//! the minimal representative in the double coset D*g*S.

use crate::butler_portugal::index::DeBrus;

use super::error::Result;
use super::index::TensorIndex;
use super::schreier_sims::schreier_sims;
use super::symmetry::Symmetry;
use super::tensor::Tensor;

/// Represents a permutation in array form
pub type Permutation = Vec<usize>;

/// Represents a base and strong generating set (BSGS)
#[derive(Debug, Clone)]
pub struct BSGS {
    pub base: Vec<usize>,
    pub generators: Vec<Permutation>,
}

impl Default for BSGS {
    fn default() -> Self {
        Self::new()
    }
}

impl BSGS {
    pub fn new() -> Self {
        Self {
            base: Vec::new(),
            generators: Vec::new(),
        }
    }

    pub fn identity(size: usize) -> Self {
        Self {
            base: Vec::new(),
            generators: vec![(0..size).collect()],
        }
    }
}

/// Canonicalizes a tensor using the Butler-Portugal algorithm
///
/// The Butler-Portugal algorithm works by:
/// 1. Identifying all possible index permutations respecting symmetries
/// 2. Finding the lexicographically minimal form
/// 3. Returning the canonical tensor with appropriate coefficient
///
/// # Arguments
/// * `tensor` - The tensor to canonicalize
///
/// # Returns
/// * `Ok(Tensor)` - The canonicalized tensor
/// * `Err(ButlerPortugalError)` - If canonicalization fails
///
/// # Example
/// ```rust
/// use butler_portugal::{canonicalize, Symmetry, Tensor, TensorIndex};
///
/// let mut tensor = Tensor::new(
///     "R",
///     vec![
///         TensorIndex::new("a", 0),
///         TensorIndex::new("b", 1),
///         TensorIndex::new("c", 2),
///         TensorIndex::new("d", 3),
///     ],
/// );
///
/// // Riemann tensor symmetries
/// tensor.add_symmetry(Symmetry::antisymmetric(vec![0, 1]));
/// tensor.add_symmetry(Symmetry::antisymmetric(vec![2, 3]));
///
/// let canonical = canonicalize(&tensor);
/// ```
pub fn canonicalize(tensor: &Tensor) -> Result<Tensor> {
    // Handle trivial cases
    if tensor.is_zero() {
        let mut zero_tensor = tensor.clone();
        zero_tensor.set_coefficient(0);
        return Ok(zero_tensor);
    }

    if tensor.rank() <= 1 {
        return Ok(tensor.clone());
    }

    // Check for zero tensor due to symmetry constraints
    for symmetry in tensor.symmetries() {
        if symmetry.makes_tensor_zero(tensor.indices()) {
            let mut zero_tensor = tensor.clone();
            zero_tensor.set_coefficient(0);
            return Ok(zero_tensor);
        }
    }

    // Generate all valid permutations considering symmetries
    let valid_permutations = generate_valid_permutations(tensor);

    if valid_permutations.is_empty() {
        return Ok(tensor.clone());
    }

    // Find lexicographically minimal tensor form
    let mut best_tensor = None;
    let mut best_canonical_key = None;

    for perm in valid_permutations {
        let candidate = tensor.permute(&perm)?;

        if candidate.is_zero() {
            continue;
        }

        let canonical_key = tensor_canonical_key(&candidate);

        if let Some(ref best_key) = best_canonical_key {
            if canonical_key < *best_key {
                best_canonical_key = Some(canonical_key);
                best_tensor = Some(candidate);
            }
        } else {
            best_canonical_key = Some(canonical_key);
            best_tensor = Some(candidate);
        }
    }

    if let Some(tensor) = best_tensor {
        Ok(tensor)
    } else {
        // All permutations resulted in zero
        let mut zero_tensor = tensor.clone();
        zero_tensor.set_coefficient(0);
        Ok(zero_tensor)
    }
}

/// Generates all valid permutations respecting symmetries using Schreier-Sims BSGS
fn generate_valid_permutations(tensor: &Tensor) -> Vec<Permutation> {
    let n = tensor.rank();
    let generators = tensor_symmetry_generators(tensor);
    let bsgs = schreier_sims(&generators, n);
    enumerate_group(&bsgs, n)
}

/// Enumerate all group elements from a BSGS by recursively applying all strong generators to the identity permutation, using a HashSet to avoid duplicates. This efficiently generates the full permutation group defined by the base and strong generating set, and is much faster than brute-force BFS for most practical tensor symmetry groups.
fn enumerate_group(bsgs: &BSGS, degree: usize) -> Vec<Permutation> {
    // If there is no base, just return the identity
    if bsgs.base.is_empty() {
        return vec![(0..degree).collect()];
    }

    // Recursive helper to build up group elements
    fn enumerate_recursive(
        generators: &[Permutation],
        current: &[usize],
        results: &mut Vec<Permutation>,
        visited: &mut std::collections::HashSet<Vec<usize>>,
    ) {
        if !visited.insert(current.to_owned()) {
            return;
        }
        results.push(current.to_owned());
        for ge in generators {
            let next = super::schreier_sims::compose_permutations(current, ge);
            enumerate_recursive(generators, &next, results, visited);
        }
    }

    let mut results = Vec::new();
    let mut visited = std::collections::HashSet::new();
    let identity: Permutation = (0..degree).collect();
    enumerate_recursive(&bsgs.generators, &identity, &mut results, &mut visited);
    results
}

/// Creates a canonical key for tensor comparison
fn tensor_canonical_key(tensor: &Tensor) -> DeBrus {
    let mut key = DeBrus::new();

    // Add index names in order with their variance
    for index in tensor.indices() {
        key.push(index.de_bru());
    }

    key
}

/// Converts a symmetry to permutation generators
fn symmetry_to_generators(symmetry: &Symmetry, size: usize) -> Vec<Permutation> {
    match symmetry {
        Symmetry::Symmetric { indices } => {
            let mut generators = Vec::new();
            // For symmetric group, generate adjacent transpositions
            for i in 0..indices.len().saturating_sub(1) {
                let mut perm: Vec<usize> = (0..size).collect();
                if indices[i] < size && indices[i + 1] < size {
                    perm.swap(indices[i], indices[i + 1]);
                }
                generators.push(perm);
            }
            generators
        }
        Symmetry::Antisymmetric { indices } => {
            let mut generators = Vec::new();
            // For antisymmetric group, generate adjacent transpositions
            for i in 0..indices.len().saturating_sub(1) {
                let mut perm: Vec<usize> = (0..size).collect();
                if indices[i] < size && indices[i + 1] < size {
                    perm.swap(indices[i], indices[i + 1]);
                }
                generators.push(perm);
            }
            generators
        }
        Symmetry::SymmetricPairs { pairs } => {
            let mut generators = Vec::new();

            // Generate swaps within each pair
            for &(i, j) in pairs {
                if i < size && j < size {
                    let mut perm: Vec<usize> = (0..size).collect();
                    perm.swap(i, j);
                    generators.push(perm);
                }
            }

            // Generate pair exchanges between consecutive pairs
            // For Riemann tensor: (0,1) ↔ (2,3) gives permutation [2, 3, 0, 1]
            for pair_idx in 0..pairs.len().saturating_sub(1) {
                let (i1, j1) = pairs[pair_idx];
                let (i2, j2) = pairs[pair_idx + 1];

                if i1 < size && j1 < size && i2 < size && j2 < size {
                    let mut perm: Vec<usize> = (0..size).collect();
                    perm[i1] = i2;
                    perm[j1] = j2;
                    perm[i2] = i1;
                    perm[j2] = j1;
                    generators.push(perm);
                }
            }

            generators
        }
        Symmetry::Cyclic { indices } => {
            if indices.len() > 1 {
                let mut perm: Vec<usize> = (0..size).collect();
                // Create cyclic permutation
                if indices.iter().all(|&i| i < size) {
                    let first = indices[0];
                    for i in 0..indices.len() - 1 {
                        perm[indices[i]] = indices[i + 1];
                    }
                    perm[indices[indices.len() - 1]] = first;
                }
                vec![perm]
            } else {
                vec![(0..size).collect()]
            }
        }
        Symmetry::Custom {
            valid_permutations,
            signs: _,
        } => valid_permutations.clone(),
    }
}

/// Checks if a permutation is the identity
#[allow(dead_code)]
fn is_identity(perm: &[usize]) -> bool {
    perm.iter().enumerate().all(|(i, &val)| i == val)
}

/// Canonicalization method options
pub enum CanonicalizationMethod {
    SchreierSims,
    YoungSymmetrizer,
}

/// Advanced canonicalization with optimization for specific tensor types
/// Optionally, project onto a Young tableau if provided (advanced feature)
/// and optionally use Young symmetrizer-based canonicalization.
pub fn canonicalize_with_optimizations(
    tensor: &Tensor,
    tableau: Option<&super::young_tableaux::StandardTableau>,
    method: &CanonicalizationMethod,
) -> Result<Tensor> {
    match method {
        CanonicalizationMethod::SchreierSims => {
            let mut result = if is_riemann_like(tensor) {
                canonicalize_riemann_tensor(tensor)
            } else if is_symmetric_tensor(tensor) {
                canonicalize_symmetric_tensor(tensor)
            } else if is_antisymmetric_tensor(tensor) {
                canonicalize_antisymmetric_tensor(tensor)
            } else {
                canonicalize(tensor)
            }?;
            if let Some(tab) = tableau {
                result = result.project_with_tableau(tab)?;
            }
            Ok(result)
        }
        CanonicalizationMethod::YoungSymmetrizer => {
            if let Some(tab) = tableau {
                // First canonicalize the tensor to ensure it's in the correct form
                // before applying the Young symmetrizer projection
                let canonicalized = canonicalize(tensor)?;
                canonicalized.project_with_tableau(tab)
            } else {
                Err(super::ButlerPortugalError::InvalidPermutation(
                    "YoungSymmetrizer method requires a tableau".to_string(),
                ))
            }
        }
    }
}

/// Checks if tensor has Riemann-like symmetries
fn is_riemann_like(tensor: &Tensor) -> bool {
    if tensor.rank() != 4 {
        return false;
    }

    let symmetries = tensor.symmetries();
    let has_first_antisym = symmetries.iter().any(|s| s.is_antisymmetric_pair(0, 1));
    let has_second_antisym = symmetries.iter().any(|s| s.is_antisymmetric_pair(2, 3));

    has_first_antisym && has_second_antisym
}

/// Optimized canonicalization for Riemann tensors
fn canonicalize_riemann_tensor(tensor: &Tensor) -> Result<Tensor> {
    // For Riemann tensors, use the general algorithm with full symmetries
    canonicalize(tensor)
}

/// Checks if tensor is purely symmetric
fn is_symmetric_tensor(tensor: &Tensor) -> bool {
    tensor.symmetries().iter().all(|s| s.is_symmetric())
}

/// Optimized canonicalization for symmetric tensors
fn canonicalize_symmetric_tensor(tensor: &Tensor) -> Result<Tensor> {
    let mut indices_with_positions: Vec<(usize, &TensorIndex)> =
        tensor.indices().iter().enumerate().collect();

    indices_with_positions.sort_by(|a, b| a.1.canonical_cmp(b.1));

    let permutation: Vec<usize> = indices_with_positions.iter().map(|(pos, _)| *pos).collect();
    tensor.permute(&permutation)
}

/// Checks if tensor is purely antisymmetric
fn is_antisymmetric_tensor(tensor: &Tensor) -> bool {
    tensor.symmetries().iter().all(|s| s.is_antisymmetric())
}

/// Optimized canonicalization for antisymmetric tensors
fn canonicalize_antisymmetric_tensor(tensor: &Tensor) -> Result<Tensor> {
    let mut indices_with_positions: Vec<(usize, &TensorIndex)> =
        tensor.indices().iter().enumerate().collect();

    indices_with_positions.sort_by(|a, b| a.1.canonical_cmp(b.1));

    let permutation: Vec<usize> = indices_with_positions.iter().map(|(pos, _)| *pos).collect();
    tensor.permute(&permutation)
}

/// Converts all tensor symmetries into a flat list of permutation generators
fn tensor_symmetry_generators(tensor: &Tensor) -> Vec<Permutation> {
    let n = tensor.rank();
    let mut gens = Vec::new();
    for sym in tensor.symmetries() {
        gens.extend(symmetry_to_generators(sym, n));
    }
    gens
}

#[cfg(test)]
mod tests {
    use crate::butler_portugal::tensor::DeBru;

    use super::Symmetry;
    use super::*;

    #[test]
    fn test_trivial_canonicalization() {
        let tensor = Tensor::new(vec![TensorIndex::new(DeBru::new(3), 0)]);
        let result = match canonicalize(&tensor) {
            Ok(val) => val,
            Err(e) => panic!("canonicalize failed: {e}"),
        };
        assert_eq!(result, tensor);
    }

    #[test]
    fn test_symmetric_tensor_canonicalization() {
        let mut tensor = Tensor::new(vec![
            TensorIndex::new(DeBru::new(1), 0),
            TensorIndex::new(DeBru::new(2), 1),
        ]);

        tensor.add_symmetry(Symmetry::symmetric(vec![0, 1]));

        let result = match canonicalize(&tensor) {
            Ok(val) => val,
            Err(e) => panic!("canonicalize failed: {e}"),
        };
        assert_eq!(result.indices()[0].de_bru(), DeBru::new(1));
        assert_eq!(result.indices()[1].de_bru(), DeBru::new(2));
    }

    #[test]
    fn test_identity_permutation() {
        let perm = vec![0, 1, 2, 3];
        assert!(is_identity(&perm));

        let non_identity = vec![1, 0, 2, 3];
        assert!(!is_identity(&non_identity));
    }
}
