//! Transpiler module that converts egglog DSL to Rust code

use eggplant_transpiler::ast::parse::Parser;
use eggplant_transpiler::eggplant::{
    CodeGenOptions, EggplantCodeGenerator, TRANSPILER_FALLBACK_PANIC_PREFIX,
    convert_to_eggplant_with_source_and_program,
};

/// Transpiler that converts egglog DSL to Rust code
pub struct Transpiler {
    options: CodeGenOptions,
}

impl Transpiler {
    /// Create a new transpiler with default options
    #[allow(unused)]
    pub fn new() -> Self {
        Self {
            options: CodeGenOptions::default(),
        }
    }

    /// Create a new transpiler with custom options
    pub fn with_options(options: CodeGenOptions) -> Self {
        Self { options }
    }

    /// Transpile egglog DSL to Rust code
    pub fn transpile(&self, dsl_code: &str) -> String {
        // Use the actual transpiler functions from eggplant_transpiler
        let mut parser = Parser::default();
        let mut codegen = EggplantCodeGenerator::with_options(self.options.clone());

        // Parse the DSL program
        let commands = parser
            .get_program_from_string(None, dsl_code)
            .unwrap_or_else(|err| {
                panic!(
                    "{}: macro transpiler parse failure: {}",
                    TRANSPILER_FALLBACK_PANIC_PREFIX, err
                );
            });

        // Convert to eggplant commands
        let eggplant_commands = convert_to_eggplant_with_source_and_program(
            &commands,
            Some("transpiled.egg".to_string()),
        );

        // Generate Rust code. Unsupported egglog must fail during transpilation, not
        // return Rust that falls back at runtime.
        let rust = codegen.generate_rust(&eggplant_commands);
        if let Some(fallback_line) = rust
            .lines()
            .find(|line| line.contains(TRANSPILER_FALLBACK_PANIC_PREFIX))
        {
            panic!(
                "{}: macro transpiler generated fallback code: {}",
                TRANSPILER_FALLBACK_PANIC_PREFIX,
                fallback_line.trim()
            );
        }
        rust
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_transpile_datatype() {
        let transpiler = Transpiler::new();
        let dsl = "(datatype Math (MNum i64) (MAdd Math Math))";
        let rust = transpiler.transpile(dsl);
        // The actual transpiler should generate valid Rust code
        assert!(!rust.is_empty());
    }

    #[test]
    fn test_transpile_rewrite_fails_fast_on_unsupported_fallback() {
        let transpiler = Transpiler::new();
        let dsl = "(rewrite (MAdd (MNum ?a) (MNum ?b)) (MNum (+ ?a ?b)))";
        let panic = std::panic::catch_unwind(|| transpiler.transpile(dsl)).unwrap_err();
        let message = if let Some(message) = panic.downcast_ref::<&str>() {
            (*message).to_string()
        } else if let Some(message) = panic.downcast_ref::<String>() {
            message.clone()
        } else {
            "panic without string payload".to_string()
        };
        assert!(message.contains(TRANSPILER_FALLBACK_PANIC_PREFIX));
    }
}
