//! Young tableaux and related combinatorics for tensor canonicalization

use itertools::Itertools;
use std::fmt;

/// A Young diagram shape, represented as a vector of row lengths (partition)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Shape(pub Vec<usize>);

impl Shape {
    /// Returns the number of rows
    pub fn rows(&self) -> usize {
        self.0.len()
    }
    /// Returns the number of boxes (size)
    pub fn size(&self) -> usize {
        self.0.iter().sum()
    }
    /// Returns the number of columns
    pub fn cols(&self) -> usize {
        self.0.iter().max().copied().unwrap_or(0)
    }
}

/// A standard Young tableau: filling of a shape with 1..n, increasing in rows and columns
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StandardTableau {
    pub shape: Shape,
    pub entries: Vec<Vec<usize>>, // rows of entries
}

impl StandardTableau {
    /// Construct from a shape and row-wise entries
    pub fn new(shape: Shape, entries: Vec<Vec<usize>>) -> Option<Self> {
        // Check shape matches
        if shape.0.len() != entries.len()
            || shape.0.iter().zip(&entries).any(|(&l, row)| l != row.len())
        {
            return None;
        }
        // Check standardness: strictly increasing rows and columns, entries are 1..n
        let n = shape.size();
        let mut seen = vec![false; n + 1];
        for (i, row) in entries.iter().enumerate() {
            for (j, &val) in row.iter().enumerate() {
                if val == 0 || val > n || seen[val] {
                    return None;
                }
                seen[val] = true;
                // Row check
                if j > 0 && row[j - 1] >= val {
                    return None;
                }
                // Column check
                if i > 0 && j < entries[i - 1].len() && entries[i - 1][j] >= val {
                    return None;
                }
            }
        }
        Some(Self { shape, entries })
    }

    /// Returns the shape
    pub fn shape(&self) -> &Shape {
        &self.shape
    }

    /// Returns the size (number of boxes)
    pub fn size(&self) -> usize {
        self.shape.size()
    }

    /// Returns the row reading word
    pub fn row_reading_word(&self) -> Vec<usize> {
        self.entries
            .iter()
            .flat_map(|row| row.iter().copied())
            .collect()
    }

    /// Returns the column reading word
    pub fn column_reading_word(&self) -> Vec<usize> {
        let mut word = Vec::with_capacity(self.size());
        let cols = self.shape.cols();
        for j in 0..cols {
            for row in &self.entries {
                if j < row.len() {
                    word.push(row[j]);
                }
            }
        }
        word
    }
}

impl fmt::Display for StandardTableau {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for row in &self.entries {
            for &val in row {
                write!(f, "{val:2} ")?;
            }
            writeln!(f)?;
        }
        Ok(())
    }
}

/// A semistandard Young tableau: entries weakly increase in rows, strictly in columns
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SemistandardTableau {
    pub shape: Shape,
    pub entries: Vec<Vec<usize>>, // rows of entries
}

impl SemistandardTableau {
    /// Construct from a shape and row-wise entries
    pub fn new(shape: Shape, entries: Vec<Vec<usize>>) -> Option<Self> {
        if shape.0.len() != entries.len()
            || shape.0.iter().zip(&entries).any(|(&l, row)| l != row.len())
        {
            return None;
        }
        // Check semistandardness: weakly increasing rows, strictly increasing columns
        for (i, row) in entries.iter().enumerate() {
            for (j, &val) in row.iter().enumerate() {
                // Row check
                if j > 0 && row[j - 1] > val {
                    return None;
                }
                // Column check
                if i > 0 && j < entries[i - 1].len() && entries[i - 1][j] >= val {
                    return None;
                }
            }
        }
        Some(Self { shape, entries })
    }

    /// Returns the shape
    pub fn shape(&self) -> &Shape {
        &self.shape
    }
}

/// Robinson-Schensted (RSK) insertion for a word (returns (P, Q) tableaux)
pub fn rsk(word: &[usize]) -> (SemistandardTableau, StandardTableau) {
    // Simple RSK implementation (not optimized)
    let mut p_rows: Vec<Vec<usize>> = Vec::new();
    let mut q_rows: Vec<Vec<usize>> = Vec::new();
    let mut next_label = 1;
    for &x in word {
        let mut i = 0;
        let mut to_insert = x;
        loop {
            if i == p_rows.len() {
                p_rows.push(vec![to_insert]);
                q_rows.push(vec![next_label]);
                break;
            }
            let row = &mut p_rows[i];
            let pos = row.iter().position(|&y| y > to_insert);
            if let Some(j) = pos {
                std::mem::swap(&mut row[j], &mut to_insert);
                i += 1;
            } else {
                row.push(to_insert);
                q_rows[i].push(next_label);
                break;
            }
        }
        next_label += 1;
    }
    let shape = Shape(p_rows.iter().map(|r| r.len()).collect());
    let p = SemistandardTableau {
        shape: shape.clone(),
        entries: p_rows,
    };
    let q = StandardTableau {
        shape,
        entries: q_rows,
    };
    (p, q)
}

/// Given a standard tableau, return the set of (permutation, sign) pairs for the Young symmetrizer
/// (row symmetrizer then column antisymmetrizer). This can be used to project tensors onto irreducible representations.
///
/// The returned vector contains (permutation, sign) pairs, where sign is +1 or -1.
///
/// # Arguments
/// * `tableau` - The standard tableau specifying the symmetry type
/// * `degree` - The number of slots/indices (should match tableau size)
///
/// # Returns
/// A vector of (permutation, sign) pairs representing the Young symmetrizer action
pub fn young_symmetrizer_permutations(
    tableau: &StandardTableau,
    degree: usize,
) -> Vec<(Vec<usize>, i32)> {
    // Row symmetrizer: sum over all permutations within each row (symmetrize rows)
    let mut row_group = vec![(0..degree).collect::<Vec<_>>()];
    for row in &tableau.entries {
        let mut new_group = Vec::new();
        for perm in row.clone().into_iter().permutations(row.len()).unique() {
            let mut p = (0..degree).collect::<Vec<_>>();
            for (i, &slot) in perm.iter().enumerate() {
                if i < row.len() && slot > 0 && (slot - 1) < row.len() {
                    p[row[i] - 1] = row[slot - 1] - 1;
                }
            }
            for g in &row_group {
                let composed = compose_permutations(g, &p);
                new_group.push(composed);
            }
        }
        row_group = new_group;
    }
    // Column antisymmetrizer: sum over all permutations within each column (antisymmetrize columns)
    let cols = tableau.shape.cols();
    let mut col_group = vec![((0..degree).collect::<Vec<usize>>(), 1)];
    for j in 0..cols {
        // Collect the column indices
        let mut col_indices = Vec::new();
        for row in tableau.entries.iter() {
            if j < row.len() {
                col_indices.push(row[j]);
            }
        }
        if col_indices.len() <= 1 {
            continue;
        }
        let mut new_group = Vec::new();
        for perm in col_indices
            .clone()
            .into_iter()
            .permutations(col_indices.len())
            .unique()
        {
            if perm.len() != col_indices.len() {
                continue; // Defensive: skip malformed permutations
            }
            let mut p = (0..degree).collect::<Vec<_>>();
            let perm_vec: Vec<usize> = perm.iter().map(|&x| x - 1).collect();
            let sign = permutation_parity_usize(&perm_vec);
            for (i, &slot) in perm.iter().enumerate() {
                if i >= col_indices.len() {
                    continue;
                } // Defensive: skip out-of-bounds
                p[col_indices[i] - 1] = slot - 1;
            }
            for (g, s) in &col_group {
                let composed = compose_permutations(g, &p);
                new_group.push((composed, s * sign));
            }
        }
        col_group = new_group;
    }
    // Combine row symmetrizer and column antisymmetrizer
    let mut result = Vec::new();
    for g in row_group {
        for (h, sign) in &col_group {
            let composed = compose_permutations(&g, h);
            result.push((composed, *sign));
        }
    }
    result
}

/// Helper: parity of a permutation (usize version)
pub fn permutation_parity_usize(perm: &[usize]) -> i32 {
    let n = perm.len();
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
            current = perm[current];
            cycle_length += 1;
        }
        if cycle_length % 2 == 0 && cycle_length > 0 {
            sign *= -1;
        }
    }
    sign
}

/// Compose two permutations (usize version)
pub fn compose_permutations(p1: &[usize], p2: &[usize]) -> Vec<usize> {
    p1.iter().map(|&i| p2[i]).collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_standard_tableau_valid() {
        let shape = Shape(vec![3, 2]);
        let entries = vec![vec![1, 2, 4], vec![3, 5]];
        let t = StandardTableau::new(shape, entries);
        assert!(t.is_some());
    }

    #[test]
    fn test_standard_tableau_invalid() {
        let shape = Shape(vec![2, 2]);
        let entries = vec![vec![1, 2], vec![2, 3]]; // duplicate 2
        let t = StandardTableau::new(shape, entries);
        assert!(t.is_none());
    }

    #[test]
    fn test_semistandard_tableau_valid() {
        let shape = Shape(vec![2, 2]);
        let entries = vec![vec![1, 2], vec![2, 3]];
        let t = SemistandardTableau::new(shape, entries);
        assert!(t.is_some());
    }

    #[test]
    fn test_semistandard_tableau_invalid() {
        let shape = Shape(vec![2, 2]);
        let entries = vec![vec![2, 1], vec![2, 3]]; // not weakly increasing
        let t = SemistandardTableau::new(shape, entries);
        assert!(t.is_none());
    }

    #[test]
    fn test_rsk() {
        let word = vec![3, 1, 2, 1];
        let (p, q) = rsk(&word);
        assert_eq!(p.shape.size(), 4);
        assert_eq!(q.shape.size(), 4);
    }
}
