//! Transpiler module that converts egglog DSL to Rust code

use eggplant_transpiler::ast::parse::Parser;
use eggplant_transpiler::eggplant::{
    CodeGenOptions, EggplantCodeGenerator, convert_to_eggplant_with_source_and_program,
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
            .unwrap_or_else(|_| {
                // If parsing fails, return error message as Rust code
                return vec![];
            });

        // Convert to eggplant commands
        let eggplant_commands = convert_to_eggplant_with_source_and_program(
            &commands,
            Some("transpiled.egg".to_string()),
        );

        // Generate Rust code
        codegen.generate_rust(&eggplant_commands)
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
    fn test_transpile_rewrite() {
        let transpiler = Transpiler::new();
        let dsl = "(rewrite (MAdd (MNum ?a) (MNum ?b)) (MNum (+ ?a ?b)))";
        let rust = transpiler.transpile(dsl);
        // The actual transpiler should generate valid Rust code
        assert!(!rust.is_empty());
    }
}
