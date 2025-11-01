//! # Butler-Portugal Tensor Canonicalization Library
//!
//! This library implements the Butler-Portugal algorithm for tensor canonicalization
//! in theoretical physics applications. The algorithm systematically applies symmetry
//! operations to bring tensors into canonical form.
//!
//! ## Example
//! ```rust
//! use butler_portugal::{canonicalize, Symmetry, Tensor, TensorIndex};
//!
//! // Create a tensor with some indices
//! let mut tensor = Tensor::new(
//!     "R",
//!     vec![
//!         TensorIndex::new("a", 0),
//!         TensorIndex::new("b", 1),
//!         TensorIndex::new("c", 2),
//!         TensorIndex::new("d", 3),
//!     ],
//! );
//!
//! // Add symmetry properties (Riemann tensor symmetries)
//! tensor.add_symmetry(Symmetry::antisymmetric(vec![0, 1]));
//! tensor.add_symmetry(Symmetry::antisymmetric(vec![2, 3]));
//! tensor.add_symmetry(Symmetry::symmetric_pairs(vec![(0, 1), (2, 3)]));
//!
//! // Canonicalize the tensor
//! let canonical_tensor = canonicalize(&tensor)?;
//! # Ok::<(), butler_portugal::ButlerPortugalError>(())
//! ```

pub mod canonicalization;
pub mod error;
pub mod index;
pub mod schreier_sims;
pub mod symmetry;
pub mod tensor;
pub mod young_tableaux;

pub use canonicalization::canonicalize;
pub use error::{ButlerPortugalError, Result};
pub use index::TensorIndex;
pub use tensor::Tensor;

#[cfg(test)]
mod tests {
    use crate::butler_portugal::symmetry::Symmetry;

    use super::*;

    #[test]
    fn test_basic_tensor_creation() {
        let tensor = Tensor::new(
            "T",
            vec![TensorIndex::new("i", 0), TensorIndex::new("j", 1)],
        );

        assert_eq!(tensor.name(), "T");
        assert_eq!(tensor.indices().len(), 2);
    }

    #[test]
    fn test_symmetry_application() {
        let mut tensor = Tensor::new(
            "S",
            vec![TensorIndex::new("a", 0), TensorIndex::new("b", 1)],
        );

        tensor.add_symmetry(Symmetry::symmetric(vec![0, 1]));
        assert_eq!(tensor.symmetries().len(), 1);
    }
}
