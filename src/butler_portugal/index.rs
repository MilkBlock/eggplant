//! Tensor index representation and manipulation
//!
//! This module provides the `TensorIndex` struct for representing
//! individual tensor indices with names and positions.

use std::fmt;

use crate::butler_portugal::tensor::DeBru;

/// Represents a single tensor index
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TensorIndex {
    /// The name/label of the index (e.g., "mu", "nu", "a", "b")
    de_bruijn: DeBru,
    /// The position of the index in the tensor
    position: usize,
    /// Whether the index is contravariant (true) or covariant (false)
    contravariant: bool,
}

impl TensorIndex {
    /// Creates a new tensor index
    ///
    /// # Arguments
    /// * `name` - The name of the index
    /// * `position` - The position in the tensor
    ///
    /// # Example
    /// ```rust
    /// use butler_portugal::TensorIndex;
    ///
    /// let index = TensorIndex::new("mu", 0);
    /// ```
    pub fn new(de_bruijn: DeBru, position: usize) -> Self {
        Self {
            de_bruijn,
            position,
            contravariant: false, // Default to covariant
        }
    }

    /// Creates a new covariant tensor index
    ///
    /// # Arguments
    /// * `name` - The name of the index
    /// * `position` - The position in the tensor
    pub fn covariant(de_bruijn: DeBru, position: usize) -> Self {
        Self {
            de_bruijn,
            position,
            contravariant: false,
        }
    }

    /// Returns the name of the index
    pub fn de_bru(&self) -> DeBru {
        self.de_bruijn
    }

    /// Returns the position of the index
    pub fn position(&self) -> usize {
        self.position
    }

    /// Sets the position of the index
    pub fn set_position(&mut self, position: usize) {
        self.position = position;
    }

    /// Returns true if the index is contravariant
    pub fn is_contravariant(&self) -> bool {
        self.contravariant
    }

    /// Returns true if the index is covariant
    pub fn is_covariant(&self) -> bool {
        !self.contravariant
    }

    /// Sets the variance of the index
    pub fn set_contravariant(&mut self, contravariant: bool) {
        self.contravariant = contravariant;
    }

    /// Creates a copy with a new name
    pub fn with_name(&self, de_bruijn: DeBru) -> Self {
        Self {
            de_bruijn,
            position: self.position,
            contravariant: self.contravariant,
        }
    }

    /// Creates a copy with a new position
    pub fn with_position(&self, position: usize) -> Self {
        Self {
            de_bruijn: self.de_bruijn.clone(),
            position,
            contravariant: self.contravariant,
        }
    }

    /// Checks if two indices can be contracted (same name, different variance)
    pub fn can_contract_with(&self, other: &TensorIndex) -> bool {
        self.de_bruijn == other.de_bruijn && self.contravariant != other.contravariant
    }

    /// Compares indices for canonical ordering
    /// Orders by: name (alphabetically), then by variance (covariant first), then by position
    pub fn canonical_cmp(&self, other: &TensorIndex) -> std::cmp::Ordering {
        use std::cmp::Ordering;

        match self.de_bruijn.cmp(&other.de_bruijn) {
            Ordering::Equal => match self.contravariant.cmp(&other.contravariant) {
                Ordering::Equal => self.position.cmp(&other.position),
                other => other,
            },
            other => other,
        }
    }
}

impl fmt::Display for TensorIndex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.contravariant {
            write!(f, "^{}", self.de_bruijn)
        } else {
            write!(f, "_{}", self.de_bruijn)
        }
    }
}

impl PartialOrd for TensorIndex {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(std::cmp::Ord::cmp(self, other))
    }
}

impl Ord for TensorIndex {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.canonical_cmp(other)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_index_creation() {
        let index = TensorIndex::new(DeBru::new(1), 0);
        assert_eq!(index.de_bru(), DeBru::new(1));
        assert_eq!(index.position(), 0);
        assert!(index.is_covariant());
        assert!(!index.is_contravariant());
    }

    // #[test]
    // fn test_canonical_ordering() {
    //     let index1 = TensorIndex::covariant("a", 0);
    //     // let index2 = TensorIndex::contravariant("a", 1);
    //     let index3 = TensorIndex::covariant("b", 2);

    //     assert!(index1 < index2); // covariant comes before contravariant
    //     assert!(index1 < index3); // "a" comes before "b"
    //     assert!(index2 < index3); // "a" comes before "b"
    // }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd)]
pub struct DeBrus {
    de_bruijns: Vec<DeBru>,
}
impl DeBrus {
    pub fn new() -> Self {
        Self { de_bruijns: vec![] }
    }
    pub fn push(&mut self, de_bru: DeBru) {
        self.de_bruijns.push(de_bru);
    }
}
