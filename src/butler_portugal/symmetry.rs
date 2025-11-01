//! Tensor symmetry properties and operations
//!
//! This module defines various types of tensor symmetries and provides
//! methods for checking permutation validity and calculating sign changes.

use super::index::TensorIndex;
use std::collections::HashSet;

/// Represents different types of tensor symmetries
#[derive(Debug, Clone, PartialEq)]
pub enum Symmetry {
    /// Symmetric in a group of indices
    Symmetric { indices: Vec<usize> },
    /// Antisymmetric in a group of indices  
    Antisymmetric { indices: Vec<usize> },
    /// Symmetric exchange between pairs of indices
    SymmetricPairs { pairs: Vec<(usize, usize)> },
    /// Cyclic symmetry in a group of indices
    Cyclic { indices: Vec<usize> },
    /// Custom symmetry with explicit permutation rules
    Custom {
        valid_permutations: Vec<Vec<usize>>,
        signs: Vec<i32>,
    },
}

impl Symmetry {
    /// Creates a symmetric group
    ///
    /// # Arguments
    /// * `indices` - Vector of indices that are symmetric under exchange
    ///
    /// # Example
    /// ```rust
    /// use butler_portugal::Symmetry;
    ///
    /// let sym = Symmetry::symmetric(vec![0, 1, 2]); // T_abc = T_bac = T_cab = ...
    /// ```
    pub fn symmetric(indices: Vec<usize>) -> Self {
        Self::Symmetric { indices }
    }

    /// Creates an antisymmetric group
    ///
    /// # Arguments
    /// * `indices` - Vector of indices that are antisymmetric under exchange
    ///
    /// # Example
    /// ```rust
    /// use butler_portugal::Symmetry;
    ///
    /// let asym = Symmetry::antisymmetric(vec![0, 1]); // T_ab = -T_ba
    /// ```
    pub fn antisymmetric(indices: Vec<usize>) -> Self {
        Self::Antisymmetric { indices }
    }

    /// Creates symmetric pair exchange
    ///
    /// # Arguments
    /// * `pairs` - Vector of index pairs that can be exchanged
    ///
    /// # Example
    /// ```rust
    /// use butler_portugal::Symmetry;
    ///
    /// // Riemann tensor: R_abcd = R_cdab
    /// let sym = Symmetry::symmetric_pairs(vec![(0, 1), (2, 3)]);
    /// ```
    pub fn symmetric_pairs(pairs: Vec<(usize, usize)>) -> Self {
        Self::SymmetricPairs { pairs }
    }

    /// Creates cyclic symmetry
    ///
    /// # Arguments
    /// * `indices` - Vector of indices with cyclic symmetry
    ///
    /// # Example
    /// ```rust
    /// use butler_portugal::Symmetry;
    ///
    /// let cyc = Symmetry::cyclic(vec![0, 1, 2]); // T_abc = T_bca = T_cab
    /// ```
    pub fn cyclic(indices: Vec<usize>) -> Self {
        Self::Cyclic { indices }
    }

    /// Creates custom symmetry with explicit rules
    ///
    /// # Arguments
    /// * `valid_permutations` - Vector of allowed permutations
    /// * `signs` - Corresponding signs for each permutation
    pub fn custom(valid_permutations: Vec<Vec<usize>>, signs: Vec<i32>) -> Self {
        Self::Custom {
            valid_permutations,
            signs,
        }
    }

    /// Returns the sign change when swapping two specific indices
    ///
    /// # Arguments
    /// * `i` - First index position
    /// * `j` - Second index position
    ///
    /// # Returns
    /// * `1` if the swap preserves sign or indices are not covered by this symmetry
    /// * `-1` if the swap changes sign
    /// * `0` if the swap makes the tensor zero
    pub fn sign_change_for_swap(&self, i: usize, j: usize) -> i32 {
        if i == j {
            return 1;
        }

        match self {
            Self::Symmetric { indices: _ } => {
                // For symmetric tensors, swaps always preserve sign
                1
            }
            Self::Antisymmetric { indices } => {
                if indices.contains(&i) && indices.contains(&j) {
                    -1 // Antisymmetric: sign change
                } else {
                    1 // Not covered by this symmetry
                }
            }
            Self::SymmetricPairs { pairs } => {
                // Check if this swap is between paired indices
                for &(a, b) in pairs {
                    if (i == a && j == b) || (i == b && j == a) {
                        return 1; // Symmetric pair exchange
                    }
                }
                1 // Not a pair exchange
            }
            Self::Cyclic { indices } => {
                if indices.contains(&i) && indices.contains(&j) {
                    // For cyclic symmetry, adjacent swaps have sign +1
                    // Non-adjacent swaps need to be computed based on cycle structure
                    let Some(pos_i) = indices.iter().position(|&x| x == i) else {
                        return 1;
                    };
                    let Some(pos_j) = indices.iter().position(|&x| x == j) else {
                        return 1;
                    };
                    let distance = (pos_i as i32 - pos_j as i32).abs();
                    if distance == 1 || distance == indices.len() as i32 - 1 {
                        1 // Adjacent in cycle
                    } else {
                        // For general cyclic permutations, compute based on parity
                        if (pos_i as i32 - pos_j as i32).abs() % 2 == 1 {
                            -1
                        } else {
                            1
                        }
                    }
                } else {
                    1
                }
            }
            Self::Custom {
                valid_permutations,
                signs,
            } => {
                // Create the swap permutation and check if it's valid
                let mut perm = (0..std::cmp::max(i, j) + 1).collect::<Vec<_>>();
                perm.swap(i, j);

                if let Some(pos) = valid_permutations.iter().position(|p| *p == perm) {
                    signs[pos]
                } else {
                    0 // Invalid permutation
                }
            }
        }
    }

    /// Returns the sign of a complete permutation
    ///
    /// # Arguments
    /// * `permutation` - The permutation to check
    pub fn permutation_sign(&self, permutation: &[usize]) -> i32 {
        match self {
            // merge symmetric and symmetric pairs into one case
            Self::Symmetric { indices: _ } | Self::SymmetricPairs { pairs: _ } => {
                // Symmetric groups always have sign +1
                1
            }
            Self::Antisymmetric { indices } => {
                // Calculate sign based on parity of permutation within the antisymmetric group
                self.antisymmetric_permutation_sign(permutation, indices)
            }
            Self::Cyclic { indices } => {
                // Calculate sign for cyclic permutation
                self.cyclic_permutation_sign(permutation, indices)
            }
            Self::Custom {
                valid_permutations,
                signs,
            } => {
                if let Some(pos) = valid_permutations.iter().position(|p| *p == permutation) {
                    signs[pos]
                } else {
                    0 // Invalid permutation
                }
            }
        }
    }

    /// Calculates sign for antisymmetric permutation
    fn antisymmetric_permutation_sign(&self, permutation: &[usize], indices: &[usize]) -> i32 {
        // Extract the sub-permutation for the antisymmetric indices
        let mut sub_perm = Vec::new();
        let mut index_map = std::collections::HashMap::new();

        for (new_pos, &orig_pos) in indices.iter().enumerate() {
            index_map.insert(orig_pos, new_pos);
        }

        for &perm_val in permutation {
            if let Some(&mapped) = index_map.get(&perm_val) {
                sub_perm.push(mapped);
            }
        }

        // Calculate parity of the sub-permutation
        permutation_parity(&sub_perm)
    }

    /// Calculates sign for cyclic permutation
    fn cyclic_permutation_sign(&self, permutation: &[usize], indices: &[usize]) -> i32 {
        // For cyclic symmetry, only cyclic permutations of the specified indices are allowed
        let n = indices.len();
        if n <= 1 {
            return 1;
        }

        // Extract the sub-permutation: for each cyclic index, where does it go?
        let mut sub_perm = vec![0; n];
        for (i, &idx_pos) in indices.iter().enumerate() {
            if idx_pos >= permutation.len() {
                return 0; // Invalid
            }
            let target = permutation[idx_pos];

            // Find where this target appears in the cyclic indices
            if let Some(target_pos) = indices.iter().position(|&x| x == target) {
                sub_perm[i] = target_pos;
            } else {
                return 0; // Not a permutation within the cyclic group
            }
        }

        // Check if it's a valid cyclic permutation and return sign +1
        // Cyclic permutations in our implementation always have sign +1
        if is_cyclic_permutation(&sub_perm) {
            1
        } else {
            0 // Invalid
        }
    }

    /// Checks if a permutation is valid under this symmetry
    pub fn is_valid_permutation(&self, permutation: &[usize]) -> bool {
        self.permutation_sign(permutation) != 0
    }

    /// Checks if swapping two indices makes the tensor zero
    pub fn makes_tensor_zero(&self, indices: &[TensorIndex]) -> bool {
        match self {
            Self::Antisymmetric {
                indices: sym_indices,
            } => {
                // Check if any two indices in the antisymmetric group are equal
                let names: Vec<&str> = sym_indices
                    .iter()
                    .filter_map(|&i| indices.get(i))
                    .map(|idx| idx.name())
                    .collect();

                let unique_names: HashSet<&str> = names.iter().cloned().collect();
                names.len() != unique_names.len()
            }
            _ => false,
        }
    }

    /// Returns true if this is a symmetric symmetry
    pub fn is_symmetric(&self) -> bool {
        matches!(self, Self::Symmetric { .. } | Self::SymmetricPairs { .. })
    }

    /// Returns true if this is an antisymmetric symmetry
    pub fn is_antisymmetric(&self) -> bool {
        matches!(self, Self::Antisymmetric { .. })
    }

    /// Checks if this symmetry involves an antisymmetric pair of specific indices
    pub fn is_antisymmetric_pair(&self, i: usize, j: usize) -> bool {
        match self {
            Self::Antisymmetric { indices } => indices.contains(&i) && indices.contains(&j),
            _ => false,
        }
    }
}

/// Calculates the parity (sign) of a permutation
/// Returns 1 for even permutations, -1 for odd permutations
fn permutation_parity(permutation: &[usize]) -> i32 {
    let n = permutation.len();
    let mut visited = vec![false; n];
    let mut sign = 1;

    for i in 0..n {
        if visited[i] {
            continue;
        }

        let mut cycle_length = 0;
        let mut current = i;

        while !visited[current] {
            visited[current] = true;
            current = permutation[current];
            cycle_length += 1;
        }

        if cycle_length % 2 == 0 {
            sign *= -1;
        }
    }

    sign
}

/// Checks if a permutation is cyclic
fn is_cyclic_permutation(permutation: &[usize]) -> bool {
    let n = permutation.len();
    if n <= 1 {
        return true;
    }

    let mut visited = vec![false; n];
    let mut cycle_count = 0;

    for i in 0..n {
        if visited[i] {
            continue;
        }

        cycle_count += 1;
        if cycle_count > 1 {
            return false; // More than one cycle
        }

        let mut current = i;
        while !visited[current] {
            visited[current] = true;
            current = permutation[current];
        }
    }

    cycle_count == 1
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_symmetric_sign_change() {
        let sym = Symmetry::symmetric(vec![0, 1]);
        assert_eq!(sym.sign_change_for_swap(0, 1), 1);
        assert_eq!(sym.sign_change_for_swap(0, 2), 1); // Not in group
    }

    #[test]
    fn test_antisymmetric_sign_change() {
        let asym = Symmetry::antisymmetric(vec![0, 1]);
        assert_eq!(asym.sign_change_for_swap(0, 1), -1);
        assert_eq!(asym.sign_change_for_swap(0, 2), 1); // Not in group
    }

    #[test]
    fn test_symmetric_pairs() {
        let sym = Symmetry::symmetric_pairs(vec![(0, 1), (2, 3)]);
        assert_eq!(sym.sign_change_for_swap(0, 1), 1);
        assert_eq!(sym.sign_change_for_swap(2, 3), 1);
        assert_eq!(sym.sign_change_for_swap(0, 2), 1); // Not a pair
    }

    #[test]
    fn test_permutation_parity() {
        assert_eq!(permutation_parity(&[0, 1, 2]), 1); // Identity
        assert_eq!(permutation_parity(&[1, 0, 2]), -1); // Single swap
        assert_eq!(permutation_parity(&[2, 1, 0]), -1); // Single 2-cycle (0↔2)
    }

    #[test]
    fn test_antisymmetric_makes_zero() {
        let asym = Symmetry::antisymmetric(vec![0, 1]);
        let indices = vec![
            TensorIndex::new("a", 0),
            TensorIndex::new("a", 1), // Same name - should be zero
        ];

        assert!(asym.makes_tensor_zero(&indices));
    }

    #[test]
    fn test_cyclic_permutation_check() {
        assert!(is_cyclic_permutation(&[1, 2, 0])); // 0->1->2->0
        assert!(is_cyclic_permutation(&[0])); // Single element
        assert!(!is_cyclic_permutation(&[1, 0, 2])); // Two cycles: 0<->1, 2->2
    }
}
