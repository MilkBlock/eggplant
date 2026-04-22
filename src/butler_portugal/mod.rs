#![allow(dead_code)]

//! # Butler-Portugal Tensor Canonicalization Library
//!
//! This library implements the Butler-Portugal algorithm for tensor canonicalization
//! in theoretical physics applications. The algorithm systematically applies symmetry
//! operations to bring tensors into canonical form.
//!
//! ## Example
//! ```rust
//! use eggplant::butler_portugal::{canonicalize, DeBru, Symmetry, Tensor, TensorIndex};
//!
//! // Create a tensor with some indices
//! let mut tensor = Tensor::new(vec![
//!         TensorIndex::new(DeBru::new(1), 0),
//!         TensorIndex::new(DeBru::new(2), 1),
//!         TensorIndex::new(DeBru::new(3), 2),
//!         TensorIndex::new(DeBru::new(4), 3),
//! ]);
//!
//! // Add symmetry properties (Riemann tensor symmetries)
//! tensor.add_symmetry(Symmetry::antisymmetric(vec![0, 1]));
//! tensor.add_symmetry(Symmetry::antisymmetric(vec![2, 3]));
//! tensor.add_symmetry(Symmetry::symmetric_pairs(vec![(0, 1), (2, 3)]));
//!
//! // Canonicalize the tensor
//! let canonical_tensor = canonicalize(&tensor)?;
//! # Ok::<(), eggplant::butler_portugal::ButlerPortugalError>(())
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
#[allow(unused_imports)]
pub use index::DeBrus;
pub use index::TensorIndex;
pub use symmetry::Symmetry;
pub use tensor::DeBru;
pub use tensor::Tensor;
