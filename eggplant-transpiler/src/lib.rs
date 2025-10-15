pub mod ast;
pub mod eggplant;
pub use eggplant::*;

pub use ast::*;

#[cfg(test)]
mod tests {
    use super::ast::parse::Parser;

    #[test]
    fn test_basic_parsing() {
        let program = r#"
            (datatype Math)
            (constructor Num (i64) Math)
            (let x (Num 42))
        "#;
        let mut parser = Parser::default();
        let commands = parser.get_program_from_string(None, program).unwrap();
        assert_eq!(commands.len(), 3);
    }
}
