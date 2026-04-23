//! Error types for the Butler-Portugal library
//!
//! This module defines error types that can occur during tensor
//! canonicalization and related operations.

use std::error::Error;
use std::fmt;

/// Result type for Butler-Portugal operations
pub type Result<T> = std::result::Result<T, ButlerPortugalError>;

/// Errors that can occur during tensor canonicalization
#[derive(Debug, Clone, PartialEq)]
pub enum ButlerPortugalError {
    /// Invalid permutation provided
    InvalidPermutation(String),

    /// Invalid tensor structure
    InvalidTensor(String),

    /// Invalid symmetry specification
    InvalidSymmetry(String),

    /// Index out of bounds
    IndexOutOfBounds { index: usize, max: usize },

    /// Incompatible tensor operations
    IncompatibleTensors(String),

    /// Mathematical error (division by zero, etc.)
    MathematicalError(String),

    /// Generic computation error
    ComputationError(String),
}

impl fmt::Display for ButlerPortugalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ButlerPortugalError::InvalidPermutation(msg) => {
                write!(f, "Invalid permutation: {msg}")
            }
            ButlerPortugalError::InvalidTensor(msg) => {
                write!(f, "Invalid tensor: {msg}")
            }
            ButlerPortugalError::InvalidSymmetry(msg) => {
                write!(f, "Invalid symmetry: {msg}")
            }
            ButlerPortugalError::IndexOutOfBounds { index, max } => {
                write!(f, "Index {index} out of bounds (max: {max})")
            }
            ButlerPortugalError::IncompatibleTensors(msg) => {
                write!(f, "Incompatible tensors: {msg}")
            }
            ButlerPortugalError::MathematicalError(msg) => {
                write!(f, "Mathematical error: {msg}")
            }
            ButlerPortugalError::ComputationError(msg) => {
                write!(f, "Computation error: {msg}")
            }
        }
    }
}

impl Error for ButlerPortugalError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        None
    }
}

impl From<std::num::ParseIntError> for ButlerPortugalError {
    fn from(err: std::num::ParseIntError) -> Self {
        ButlerPortugalError::ComputationError(format!("Parse error: {err}"))
    }
}

impl From<std::string::FromUtf8Error> for ButlerPortugalError {
    fn from(err: std::string::FromUtf8Error) -> Self {
        ButlerPortugalError::ComputationError(format!("UTF-8 error: {err}"))
    }
}

/// Helper macro for creating errors with formatted messages
#[macro_export]
macro_rules! bp_error {
    ($variant:ident, $fmt:expr) => {
        ButlerPortugalError::$variant($fmt.to_string())
    };
    ($variant:ident, $fmt:expr, $($arg:tt)*) => {
        ButlerPortugalError::$variant(format!($fmt, $($arg)*))
    };
}

/// Helper macro for creating Results with formatted error messages
#[macro_export]
macro_rules! bp_bail {
    ($variant:ident, $fmt:expr) => {
        return Err($crate::bp_error!($variant, $fmt))
    };
    ($variant:ident, $fmt:expr, $($arg:tt)*) => {
        return Err($crate::bp_error!($variant, $fmt, $($arg)*))
    };
}

/// Helper function for validating index bounds
pub fn validate_index_bounds(index: usize, max: usize) -> Result<()> {
    if index >= max {
        Err(ButlerPortugalError::IndexOutOfBounds { index, max })
    } else {
        Ok(())
    }
}

/// Helper function for validating permutation
pub fn validate_permutation(permutation: &[usize], expected_size: usize) -> Result<()> {
    if permutation.len() != expected_size {
        return Err(ButlerPortugalError::InvalidPermutation(format!(
            "Expected permutation of size {}, got {}",
            expected_size,
            permutation.len()
        )));
    }

    let mut seen = vec![false; expected_size];
    for &p in permutation {
        if p >= expected_size {
            return Err(ButlerPortugalError::InvalidPermutation(format!(
                "Permutation element {} out of bounds (max: {})",
                p,
                expected_size - 1
            )));
        }
        if seen[p] {
            return Err(ButlerPortugalError::InvalidPermutation(format!(
                "Duplicate element {p} in permutation"
            )));
        }
        seen[p] = true;
    }

    Ok(())
}

/// Helper function for validating tensor indices
pub fn validate_tensor_indices(indices: &[super::TensorIndex]) -> Result<()> {
    if indices.is_empty() {
        return Err(ButlerPortugalError::InvalidTensor(
            "Tensor must have at least one index".to_string(),
        ));
    }

    // Check for position consistency
    for (i, index) in indices.iter().enumerate() {
        if index.position() != i {
            return Err(ButlerPortugalError::InvalidTensor(format!(
                "Index at position {} has incorrect position value {}",
                i,
                index.position()
            )));
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_error_display() {
        let err = ButlerPortugalError::InvalidPermutation("test message".to_string());
        let display = format!("{err}");
        assert!(display.contains("Invalid permutation"));
        assert!(display.contains("test message"));
    }

    #[test]
    fn test_index_bounds_validation() {
        assert!(validate_index_bounds(0, 5).is_ok());
        assert!(validate_index_bounds(4, 5).is_ok());
        assert!(validate_index_bounds(5, 5).is_err());
        assert!(validate_index_bounds(10, 5).is_err());
    }

    #[test]
    fn test_permutation_validation() {
        assert!(validate_permutation(&[0, 1, 2], 3).is_ok());
        assert!(validate_permutation(&[2, 1, 0], 3).is_ok());
        assert!(validate_permutation(&[0, 1], 3).is_err()); // Wrong size
        assert!(validate_permutation(&[0, 1, 3], 3).is_err()); // Out of bounds
        assert!(validate_permutation(&[0, 1, 1], 3).is_err()); // Duplicate
    }

    #[test]
    fn test_bp_error_macro() {
        let err = bp_error!(InvalidTensor, "test");
        assert_eq!(err, ButlerPortugalError::InvalidTensor("test".to_string()));

        let err = bp_error!(InvalidTensor, "test {}", 42);
        assert_eq!(
            err,
            ButlerPortugalError::InvalidTensor("test 42".to_string())
        );
    }
}
