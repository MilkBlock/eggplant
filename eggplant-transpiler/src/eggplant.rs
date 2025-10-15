//! Eggplant AST and code generator
//!
//! Eggplant is a simplified version of egglog with focus on educational examples

use crate::ast::*;
use heck::ToSnakeCase;
use std::collections::HashMap;

/// Eggplant DSL type definition
#[derive(Debug, Clone, PartialEq)]
pub struct DslType {
    pub name: String,
    pub variants: Vec<DslVariant>,
}

/// Eggplant DSL variant definition
#[derive(Debug, Clone, PartialEq)]
pub struct DslVariant {
    pub name: String,
    pub fields: Vec<DslField>,
    pub source_file: Option<String>,
    pub source_line: Option<usize>,
}

/// Eggplant DSL field definition
#[derive(Debug, Clone, PartialEq)]
pub struct DslField {
    pub name: String,
    pub field_type: String,
}

/// Eggplant pattern variable definition
#[derive(Debug, Clone, PartialEq)]
pub struct PatternVars {
    pub name: String,
    pub variables: Vec<PatternVariable>,
}

/// Eggplant pattern variable
#[derive(Debug, Clone, PartialEq)]
pub struct PatternVariable {
    pub name: String,
    pub var_type: String,
}

/// Eggplant rule definition
#[derive(Debug, Clone)]
pub struct EggplantRule {
    pub name: String,
    pub pattern_query: String,
    pub action: String,
    pub ruleset: String,
    pub src_expr: Command,
}

/// Eggplant-specific AST nodes
#[derive(Debug, Clone)]
pub enum EggplantCommand {
    /// Define a DSL type with #[eggplant::dsl]
    DslType(DslType),
    /// Define pattern variables with #[eggplant::pat_vars]
    PatternVars(PatternVars),
    /// Define a rule with add_rule
    Rule(EggplantRule),
    /// Create a ruleset
    Ruleset(String),
    /// Run ruleset
    RunRuleset(String, String),
    /// Commit operation
    Commit(String),
    /// Pull operation
    Pull(String),
    /// Transaction definition
    Transaction(String),
    /// Pattern recorder definition
    PatternRecorder(String),
    /// Test assertion
    Assert { expr: Expr, expected: Expr },
    /// Variable assignment
    Let { var: String, expr: Expr },
    /// Print statement
    Print { expr: Expr },
}

/// Eggplant command with source file information
#[derive(Debug, Clone)]
pub struct EggplantCommandWithSource {
    pub command: EggplantCommand,
    pub source_file: Option<String>,
    pub source_line: Option<usize>,
}

/// Code generation options
#[derive(Debug, Clone, Default)]
pub struct CodeGenOptions {
    /// Whether to omit the main function wrapper
    pub omit_main: bool,
    /// Whether to omit use statements (prelude, etc.)
    pub omit_use_statements: bool,
    /// Whether to omit ruleset definitions
    pub omit_ruleset_definitions: bool,
    /// Whether to omit run_ruleset calls
    pub omit_run_ruleset_calls: bool,
    /// Whether to omit global singleton definitions like tx_rx_vt_pr!
    pub omit_global_singleton_definitions: bool,
    /// Whether to omit datatype definitions (#[eggplant::dsl])
    pub omit_datatype: bool,
    pub omit_head_annotation: bool,
}

/// Eggplant code generator
pub struct EggplantCodeGenerator {
    output: String,
    indent_level: usize,
    options: CodeGenOptions,
}

impl EggplantCodeGenerator {
    pub fn new() -> Self {
        Self {
            output: String::new(),
            indent_level: 0,
            options: CodeGenOptions::default(),
        }
    }

    /// Create a new code generator with custom options
    pub fn with_options(options: CodeGenOptions) -> Self {
        Self {
            output: String::new(),
            indent_level: 0,
            options,
        }
    }

    /// Generate Rust code with eggplant macros
    pub fn generate_rust(&mut self, commands: &[EggplantCommandWithSource]) -> String {
        self.output.clear();

        if !self.options.omit_head_annotation {
            self.add_line("// Generated Eggplant Rust Code");
            self.add_line("// Source files referenced in comments below");
        }

        // Add use statements unless omitted
        if !self.options.omit_use_statements {
            if !self.options.omit_global_singleton_definitions {
                self.add_line("use eggplant::{{prelude::*, tx_rx_vt_pr}};");
            } else {
                self.add_line("use eggplant::prelude::*;");
            }
            self.add_line("use log::info;");
            self.add_line("");
        }

        // Generate type definitions (outside main)
        for cmd_with_source in commands {
            match &cmd_with_source.command {
                EggplantCommand::DslType(_) => {
                    if !self.options.omit_datatype {
                        self.generate_rust_command_with_source(cmd_with_source);
                    }
                }
                _ => {}
            }
        }

        // Collect ruleset definitions
        let mut ruleset_definitions = Vec::new();
        let mut other_commands = Vec::new();

        for cmd_with_source in commands {
            match &cmd_with_source.command {
                EggplantCommand::Ruleset(_) => {
                    if !self.options.omit_ruleset_definitions {
                        ruleset_definitions.push(cmd_with_source);
                    }
                }
                EggplantCommand::DslType(_) => {
                    // Skip type definitions (already generated above)
                }
                _ => {
                    other_commands.push(cmd_with_source);
                }
            }
        }

        // Add main function unless omitted
        if !self.options.omit_main {
            self.add_line("fn main() {");
            self.indent();
            self.add_line("env_logger::init();");

            // Generate ruleset definitions at the beginning of main
            for cmd_with_source in ruleset_definitions {
                self.generate_rust_command_with_source(cmd_with_source);
            }

            // Generate PatternVars and other runtime commands inside main
            for cmd_with_source in other_commands {
                self.generate_rust_command_with_source(cmd_with_source);
            }

            self.add_line("info!(\"Eggplant program executed successfully!\");");
            self.dedent();
            self.add_line("}");
        } else {
            // Generate ruleset definitions and other commands without main wrapper
            for cmd_with_source in ruleset_definitions {
                self.generate_rust_command_with_source(cmd_with_source);
            }

            // Generate PatternVars and other runtime commands
            for cmd_with_source in other_commands {
                self.generate_rust_command_with_source(cmd_with_source);
            }
        }

        self.output.clone()
    }

    fn generate_rust_command_with_source(&mut self, cmd_with_source: &EggplantCommandWithSource) {
        // Add source file comment if available
        if let (Some(file), Some(line)) =
            (&cmd_with_source.source_file, cmd_with_source.source_line)
        {
            if !self.options.omit_head_annotation {
                self.add_line(&format!("// Source: {}:{}", file, line));
            }
        }

        match &cmd_with_source.command {
            EggplantCommand::DslType(dsl_type) => {
                self.add_line(&format!(
                    "// Datatype '{}' defined with variants:",
                    dsl_type.name
                ));
                for variant in &dsl_type.variants {
                    if !self.options.omit_head_annotation {
                        if let (Some(file), Some(line)) =
                            (&variant.source_file, variant.source_line)
                        {
                            self.add_line(&format!(
                                "//   - {}: variant (defined at {}:{})",
                                variant.name, file, line
                            ));
                        } else {
                            self.add_line(&format!("//   - {}: variant", variant.name));
                        }
                    }
                }
                self.add_line(&format!("#[eggplant::dsl]"));
                self.add_line(&format!("enum {} {{", dsl_type.name));
                self.indent();
                for variant in &dsl_type.variants {
                    if variant.fields.is_empty() {
                        self.add_line(&format!("{} {{}},", variant.name));
                    } else {
                        self.add_line(&format!("{} {{ ", variant.name));
                        for field in &variant.fields {
                            self.add_line(&format!("    {}: {},", field.name, field.field_type));
                        }
                        self.add_line("},");
                    }
                }
                self.dedent();
                self.add_line("}");
                self.add_line("");
            }
            EggplantCommand::PatternVars(pattern_vars) => {
                // Display original egglog statement if available
                if !self.options.omit_head_annotation {
                    self.add_line(&format!("// Pattern variables for rule matching"));
                    let var_names: Vec<String> = pattern_vars
                        .variables
                        .iter()
                        .map(|v| v.name.clone())
                        .collect();
                    self.add_line(&format!("// Variables: {}", var_names.join(", ")));
                }
                self.add_line(&format!("#[eggplant::pat_vars]"));
                self.add_line(&format!("struct {} {{", pattern_vars.name));
                self.indent();
                for var in &pattern_vars.variables {
                    self.add_line(&format!("{}: {},", var.name, var.var_type));
                }
                self.dedent();
                self.add_line("}");
                self.add_line("");
            }
            EggplantCommand::Rule(rule) => {
                if !self.options.omit_head_annotation {
                    self.add_line(&format!("// Rule: {}", rule.name));
                    self.add_line(&format!("// {}", rule.src_expr));
                }
                self.add_line(&format!("MyTx::add_rule("));
                self.indent();
                self.add_line(&format!("\"{}\",", rule.name));
                self.add_line(&format!("{},", rule.ruleset));
                self.add_line(&format!("|| {{"));
                self.indent();
                self.add_line(&format!("{}", rule.pattern_query));
                self.dedent();
                self.add_line("},");
                self.add_line(&format!("|ctx, pat| {{"));
                self.indent();
                self.add_line(&format!("{}", rule.action));
                self.dedent();
                self.add_line("},");
                self.dedent();
                self.add_line(");");
                self.add_line("");
            }
            EggplantCommand::Ruleset(name) => {
                self.add_line(&format!("let {} = MyTx::new_ruleset(\"{}\");", name, name));
            }
            EggplantCommand::Transaction(name) => {
                if !self.options.omit_global_singleton_definitions {
                    self.add_line(&format!("tx_rx_vt_pr!({}, MyPatRec);", name));
                    self.add_line("");
                }
            }
            EggplantCommand::PatternRecorder(name) => {
                self.add_line(&format!("// Pattern recorder: {}", name));
            }
            EggplantCommand::Commit(expr) => {
                self.add_line(&format!("{}.commit();", expr));
            }
            EggplantCommand::Pull(expr) => {
                self.add_line(&format!("{}.pull();", expr));
            }
            EggplantCommand::RunRuleset(ruleset, config) => {
                if !self.options.omit_run_ruleset_calls {
                    self.add_line(&format!(
                        "{}::run_ruleset({}, RunConfig::{});",
                        ruleset, "default_ruleset", config
                    ));
                }
            }
            EggplantCommand::Assert { expr, expected } => {
                self.add_line(&format!(
                    "// Assert: {} == {}",
                    self.expr_to_string(expr),
                    self.expr_to_string(expected)
                ));
            }
            EggplantCommand::Let { var, expr } => {
                let expr_str = self.expr_to_string(expr);
                // println!("DEBUG: Let command: {} = {}", var, expr_str);
                self.add_line(&format!(
                    "let {}:{}<MyTx> = {};",
                    var,
                    match &expr {
                        GenericExpr::Call(_span, head, _generic_exprs) => head,
                        _ => {
                            panic!("ty unable to infer")
                        }
                    },
                    expr_str
                ));
                self.add_line(&format!("{}.commit();", var));
            }
            EggplantCommand::Print { expr } => {
                self.add_line(&format!(
                    "info!(\"{{:?}}\", {});",
                    self.expr_to_string(expr)
                ));
            }
        }
    }

    fn expr_to_string(&self, expr: &Expr) -> String {
        match expr {
            Expr::Lit(_, lit) => match lit {
                Literal::Int(i) => i.to_string(),
                Literal::Float(f) => f.0.to_string(),
                Literal::String(s) => format!("\"{}\"", s),
                Literal::Bool(b) => b.to_string(),
                Literal::Unit => "()".to_string(),
            },
            Expr::Var(_, var) => normalize_identifier(var),
            Expr::Call(_, func, args) => {
                let arg_str = args
                    .iter()
                    .map(|a| self.expr_with_reference(a))
                    .collect::<Vec<_>>()
                    .join(", ");
                let result = format!("{}::new({})", normalize_identifier(func), arg_str);
                // println!("DEBUG: Call expression: {} -> {}", func, result);
                result
            }
        }
    }

    fn expr_with_reference(&self, expr: &Expr) -> String {
        let expr_str = self.expr_to_string(expr);
        // Check if this is a basic type (literal) or custom type (variable/call)
        match expr {
            Expr::Lit(_, _) => expr_str, // Basic types don't need references
            Expr::Var(_, _) => {
                // Check if variable type is a basic type or custom type
                let var_type = infer_type_from_expr(expr);
                if self.is_basic_type(&var_type) {
                    expr_str // Basic types don't need references
                } else {
                    format!("&{}", expr_str) // Custom types need references
                }
            }
            Expr::Call(_, func, _) => {
                // Check if function return type is a basic type or custom type
                // For constructor calls like g_::new, we need to check the return type
                // Since g_ returns G (custom type), it should use references
                if self.is_basic_type(func) {
                    expr_str // Basic types don't need references
                } else {
                    format!("&{}", expr_str) // Custom types need references
                }
            }
        }
    }

    /// Check if a type is a basic type (i64, f64, bool, String, etc.)
    fn is_basic_type(&self, type_name: &str) -> bool {
        let basic_types = ["i64", "f64", "bool", "String", "()"];
        let rst = basic_types.contains(&type_name) || type_name.starts_with('&');
        rst
    }

    fn add_line(&mut self, line: &str) {
        let indent = "    ".repeat(self.indent_level);
        self.output.push_str(&format!("{}{}\n", indent, line));
    }

    fn indent(&mut self) {
        self.indent_level += 1;
    }

    fn dedent(&mut self) {
        if self.indent_level > 0 {
            self.indent_level -= 1;
        }
    }
}

/// Convert egglog commands to eggplant commands with source information
pub fn convert_to_eggplant_with_source(
    commands: &[Command],
    source_file: Option<String>,
) -> Vec<EggplantCommandWithSource> {
    convert_to_eggplant_with_source_and_program(commands, source_file)
}

/// Convert egglog commands to eggplant commands with source information and original program
pub fn convert_to_eggplant_with_source_and_program(
    commands: &[Command],
    source_file: Option<String>,
) -> Vec<EggplantCommandWithSource> {
    let mut eggplant_commands = Vec::new();
    let mut dsl_types: HashMap<String, DslType> = HashMap::new();

    // Extract original statements if program is provided

    // Add transaction definition
    eggplant_commands.push(EggplantCommandWithSource {
        command: EggplantCommand::Transaction("MyTx".to_string()),
        source_file: source_file.clone(),
        source_line: Some(1),
    });

    // First pass: collect all constructors for each datatype/sort with source info
    let mut datatype_constructors: HashMap<String, Vec<DslVariant>> = HashMap::new();
    let mut constructor_line_numbers: HashMap<String, usize> = HashMap::new();
    let mut constructor_counter = 1;

    for command in commands {
        constructor_counter += 1;
        if let Command::Constructor { name, schema, .. } = command {
            // Extract the datatype/sort name from the schema output
            let datatype_name = &schema.output;

            let dsl_variant = DslVariant {
                name: normalize_identifier(name),
                fields: schema
                    .input
                    .iter()
                    .enumerate()
                    .map(|(i, field_type)| DslField {
                        name: format!("arg{}", i),
                        field_type: normalize_identifier(field_type),
                    })
                    .collect(),
                source_file: source_file.clone(),
                source_line: Some(constructor_counter),
            };

            constructor_line_numbers.insert(name.clone(), constructor_counter);

            datatype_constructors
                .entry(normalize_identifier(datatype_name))
                .or_insert_with(Vec::new)
                .push(dsl_variant);
        }
    }

    for command in commands {
        match command {
            Command::Datatype {
                name,
                span,
                variants,
            } => {
                // Use the collected constructors for this datatype
                let mut dsl_variants = datatype_constructors
                    .get(name)
                    .cloned()
                    .unwrap_or_else(Vec::new);

                // Use the datatype command inner variants for this datatype
                for (counter, variant) in variants.iter().enumerate() {
                    let fields: Vec<DslField> = if !variant.field_names.is_empty() {
                        // Use provided field names from args_name
                        variant
                            .types
                            .iter()
                            .zip(variant.field_names.iter())
                            .map(|(ty, field_name)| DslField {
                                name: field_name.clone(),
                                field_type: ty.clone(),
                            })
                            .collect()
                    } else {
                        // Generate default field names
                        variant
                            .types
                            .iter()
                            .enumerate()
                            .map(|(i, ty)| DslField {
                                name: format!("arg_{}_{}{}", ty, counter, i),
                                field_type: ty.clone(),
                            })
                            .collect()
                    };

                    dsl_variants.push(DslVariant {
                        name: variant.name.clone(),
                        fields,
                        source_file: variant.span.file.clone(),
                        source_line: Some(variant.span.line),
                    });
                }

                let dsl_type = DslType {
                    name: normalize_identifier(name),
                    variants: dsl_variants.clone(),
                };
                dsl_types.insert(normalize_identifier(name), dsl_type.clone());

                eggplant_commands.push(EggplantCommandWithSource {
                    command: EggplantCommand::DslType(dsl_type),
                    source_file: source_file.clone(),
                    source_line: Some(span.line),
                });
            }
            Command::Function {
                name, schema, span, ..
            } => {
                // Create pattern variables for function - these are the matched nodes
                let pattern_vars = PatternVars {
                    name: format!("{}Pat", normalize_identifier(name)),
                    variables: schema
                        .input
                        .iter()
                        .enumerate()
                        .map(|(i, typ)| PatternVariable {
                            name: format!("arg{}", i),
                            var_type: normalize_identifier(typ),
                        })
                        .collect(),
                };

                eggplant_commands.push(EggplantCommandWithSource {
                    command: EggplantCommand::PatternVars(pattern_vars),
                    source_file: source_file.clone(),
                    source_line: Some(span.line),
                });

                // Create rule for function
                let rule = EggplantRule {
                    name: format!("{}_rule", normalize_identifier(name)),
                    pattern_query: format!(
                        "// TODO: implement pattern query for {}",
                        normalize_identifier(name)
                    ),
                    action: format!("// TODO: implement {} action", normalize_identifier(name)),
                    ruleset: "default_ruleset".to_string(),
                    src_expr: command.clone(),
                };

                eggplant_commands.push(EggplantCommandWithSource {
                    command: EggplantCommand::Rule(rule),
                    source_file: source_file.clone(),
                    source_line: Some(span.line),
                });
            }
            Command::Rule { name, rule, .. } => {
                println!("panic rule {:?}", rule);
                // For rules, we need to analyze the pattern to determine what nodes are matched
                // For now, create a simple pattern with generic nodes
                let unique_name = if name == "default" {
                    format!("rule_{}", rule.span.line)
                } else {
                    normalize_identifier(name)
                };
                let pattern_vars = PatternVars {
                    name: format!("{}Pat", unique_name),
                    variables: vec![
                        PatternVariable {
                            name: "l".to_string(),
                            var_type: "Expr".to_string(),
                        },
                        PatternVariable {
                            name: "r".to_string(),
                            var_type: "Expr".to_string(),
                        },
                        PatternVariable {
                            name: "p".to_string(),
                            var_type: normalize_identifier(name),
                        },
                    ],
                };

                eggplant_commands.push(EggplantCommandWithSource {
                    command: EggplantCommand::PatternVars(pattern_vars),
                    source_file: source_file.clone(),
                    source_line: Some(rule.span.line),
                });

                // Create rule with proper pattern query
                let pattern_query = format!(
                    "let l = Expr::query_leaf();\nlet r = Expr::query_leaf();\nlet p = {}::query(&l, &r);\n{}Pat::new(l, r, p)",
                    name, unique_name
                );

                let action = format!(
                    "// TODO: implement action for {}\nlet result = ctx.insert_expr(/* calculation */);\nctx.union(p, result);",
                    unique_name
                );

                let eggplant_rule = EggplantRule {
                    name: unique_name.clone(),
                    pattern_query,
                    action,
                    ruleset: "default_ruleset".to_string(),
                    src_expr: command.clone(),
                };

                eggplant_commands.push(EggplantCommandWithSource {
                    command: EggplantCommand::Rule(eggplant_rule),
                    source_file: source_file.clone(),
                    source_line: Some(rule.span.line),
                });
            }
            Command::Rewrite(_name, rewrite, _, rule_name) => {
                // Use provided name or generate one based on line number
                let unique_name = rule_name
                    .clone()
                    .unwrap_or_else(|| format!("rule_{}_{}", rewrite.span.line, rewrite.span.col));
                let (pattern_vars, pattern_query, context, root_node) =
                    analyze_rewrite_pattern_with_conditions(
                        &rewrite.lhs,
                        &unique_name,
                        &dsl_types,
                        &rewrite.conditions,
                    );

                // Create pattern variables
                eggplant_commands.push(EggplantCommandWithSource {
                    command: EggplantCommand::PatternVars(pattern_vars),
                    source_file: source_file.clone(),
                    source_line: Some(rewrite.span.line),
                });

                // Create rule with pattern query and action
                let action = generate_rewrite_action_with_context(
                    &rewrite.rhs,
                    &unique_name,
                    &context,
                    &dsl_types,
                    &root_node,
                );

                let rule = EggplantRule {
                    name: unique_name.clone(),
                    pattern_query,
                    action,
                    ruleset: "default_ruleset".to_string(),
                    src_expr: command.clone(),
                };

                eggplant_commands.push(EggplantCommandWithSource {
                    command: EggplantCommand::Rule(rule),
                    source_file: source_file.clone(),
                    source_line: Some(rewrite.span.line),
                });
            }
            Command::Check(span, facts) => {
                for fact in facts {
                    if let Fact::Op(_, e1, e2) = fact {
                        eggplant_commands.push(EggplantCommandWithSource {
                            command: EggplantCommand::Assert {
                                expr: e1.clone(),
                                expected: e2.clone(),
                            },
                            source_file: source_file.clone(),
                            source_line: Some(span.line),
                        });
                    }
                }
            }
            Command::Constructor { .. } => {
                // Constructor commands are already handled above
                // Skip duplicate processing
            }
            Command::Action(action) => {
                if let Action::Let(span, var, expr) = action {
                    eggplant_commands.push(EggplantCommandWithSource {
                        command: EggplantCommand::Let {
                            var: normalize_identifier(var),
                            expr: expr.clone(),
                        },
                        source_file: source_file.clone(),
                        source_line: Some(span.line),
                    });
                }
            }
            Command::Push(_) => {
                eggplant_commands.push(EggplantCommandWithSource {
                    command: EggplantCommand::Commit("current_expr".to_string()),
                    source_file: source_file.clone(),
                    source_line: Some(1), // Default line for Push command
                });
            }
            Command::Pop(span, _) => {
                eggplant_commands.push(EggplantCommandWithSource {
                    command: EggplantCommand::Pull("current_expr".to_string()),
                    source_file: source_file.clone(),
                    source_line: Some(span.line),
                });
            }
            Command::Sort(span, name, _) => {
                // Sort command defines a basic type
                // Use the collected constructors for this sort
                let dsl_variants = datatype_constructors
                    .get(name)
                    .cloned()
                    .unwrap_or_else(Vec::new);

                let dsl_type = DslType {
                    name: normalize_identifier(name),
                    variants: dsl_variants.clone(),
                };
                dsl_types.insert(normalize_identifier(name), dsl_type.clone());

                eggplant_commands.push(EggplantCommandWithSource {
                    command: EggplantCommand::DslType(dsl_type),
                    source_file: source_file.clone(),
                    source_line: Some(span.line),
                });
            }
            _ => {
                // Skip unsupported commands for now
            }
        }
    }

    // Add ruleset and run commands
    eggplant_commands.push(EggplantCommandWithSource {
        command: EggplantCommand::Ruleset("default_ruleset".to_string()),
        source_file: source_file.clone(),
        source_line: Some(1), // Default line number
    });
    eggplant_commands.push(EggplantCommandWithSource {
        command: EggplantCommand::RunRuleset("MyTx".to_string(), "Sat".to_string()),
        source_file: source_file.clone(),
        source_line: Some(1), // Default line number
    });

    eggplant_commands
}

/// Normalize identifier by replacing invalid characters with underscores
fn normalize_identifier(identifier: &str) -> String {
    identifier
        .chars()
        .map(|c| {
            if c.is_alphanumeric() || c == '_' {
                c
            } else {
                '_'
            }
        })
        .collect()
}

/// Check if a type is a basic type (i64, f64, bool, String, etc.)
fn is_basic_type(type_name: &str) -> bool {
    let basic_types = [
        "i64",
        "f64",
        "bool",
        "String",
        "&'static str",
        "BigRational",
        "()",
    ];
    basic_types.contains(&type_name)
}

/// Infer type from expression context with better variable type inference
fn infer_type_from_expr(expr: &Expr) -> String {
    match expr {
        Expr::Call(_, func_name, _) => normalize_identifier(func_name),
        Expr::Var(_, name) => {
            // For pattern variables starting with ?, try to infer type from context
            if name.starts_with('?') {
                // Default to String for pattern variables that might be string types
                // This is a temporary solution until we have better type inference
                "String".to_string()
            } else {
                // For other variables, default to String
                "String".to_string()
            }
        }
        Expr::Lit(_, lit) => match lit {
            Literal::Int(_) => "i64".to_string(),
            Literal::Float(_) => "f64".to_string(),
            Literal::String(_) => "String".to_string(),
            Literal::Bool(_) => "bool".to_string(),
            Literal::Unit => "()".to_string(),
        },
    }
}

/// Infer variable type from constructor context using DSL type information
fn infer_variable_type_from_constructor(
    constructor_name: &str,
    arg_index: usize,
    dsl_types: &HashMap<String, DslType>,
) -> String {
    // Look for the constructor in DSL types
    for (dsl_type_name, dsl_type) in dsl_types {
        // Find the variant that matches the constructor name
        if let Some(variant) = dsl_type
            .variants
            .iter()
            .find(|v| v.name == constructor_name)
        {
            // Check if we have a field at this argument index
            if arg_index < variant.fields.len() {
                // println!("{} infer to be {}", constructor_name, dsl_type_name);
                return variant.fields[arg_index].field_type.clone();
            } else {
                // println!("{} infer to be {}", constructor_name, dsl_type_name);
                // If no field at this index, return the DSL type name
                return dsl_type_name.clone();
            }
        }
    }

    // Fallback: if constructor not found in DSL types, use the constructor name as type
    // println!("WARNING: {} infer to be itself", constructor_name);
    constructor_name.to_string()
}

/// Generate better pattern query with type inference, variable context, and conditions
fn generate_pattern_query_with_context_and_conditions(
    lhs: &Expr,
    rule_name: &str,
    dsl_types: &HashMap<String, DslType>,
    conditions: &[Fact],
) -> (
    PatternVars,
    String,
    HashMap<String, (String, String, usize)>,
    String, // root node name
) {
    let mut variables = Vec::new();
    let mut pattern_query_parts = Vec::new();
    let mut node_counter = 0;
    let mut variable_constructors = HashMap::new();
    let mut all_nodes = Vec::new();

    // Extract variables with better type inference and build the query tree
    let (root_node, is_root) = extract_variables_with_types_and_context(
        lhs,
        &mut variables,
        &mut pattern_query_parts,
        &mut node_counter,
        dsl_types,
        &mut variable_constructors,
        &mut all_nodes,
        true, // This is the root expression
    );

    // Process conditions to extract literal values and operators for basic type conditions
    let mut condition_info = HashMap::new();
    let mut condition_expressions = Vec::new();

    for condition in conditions {
        if let Fact::Op(span, e1, e2) = condition {
            // Extract operator from span file field (temporary hack)
            let operator = if let Some(ref file) = span.file {
                if file.starts_with("operator:") {
                    file.trim_start_matches("operator:").to_string()
                } else {
                    "=".to_string() // Default to "=" if no operator info
                }
            } else {
                "=".to_string() // Default to "=" if no operator info
            };

            // Handle simple case: (Var, Lit) or (Lit, Var)
            if let (Expr::Var(_, var_name), Expr::Lit(_, lit)) = (e1, e2) {
                let normalized_var_name = normalize_identifier(var_name);
                let literal_value = match lit {
                    Literal::Int(i) => i.to_string(),
                    Literal::Float(f) => f.0.to_string(),
                    Literal::String(s) => format!("\"{}\"", s),
                    Literal::Bool(b) => b.to_string(),
                    Literal::Unit => "()".to_string(),
                };

                condition_info.insert(normalized_var_name, (operator, literal_value));
            } else if let (Expr::Lit(_, lit), Expr::Var(_, var_name)) = (e1, e2) {
                let normalized_var_name = normalize_identifier(var_name);
                let literal_value = match lit {
                    Literal::Int(i) => i.to_string(),
                    Literal::Float(f) => f.0.to_string(),
                    Literal::String(s) => format!("\"{}\"", s),
                    Literal::Bool(b) => b.to_string(),
                    Literal::Unit => "()".to_string(),
                };

                condition_info.insert(normalized_var_name, (operator, literal_value));
            } else {
                // Handle complex expressions - store the entire condition for later processing
                condition_expressions.push((operator, e1.clone(), e2.clone()));
            }
        }
    }

    // Create pattern variables struct - exclude basic types and include only complex types
    let mut pattern_vars_variables: Vec<PatternVariable> = variables
        .iter()
        .filter(|v| !is_basic_type(&v.var_type))
        .cloned()
        .collect();

    // Add constructor nodes to pattern variables only if they are complex types
    for (_var_name, (constructor_name, node_name, _)) in &variable_constructors {
        if !pattern_vars_variables.iter().any(|v| v.name == *node_name) {
            // Only add if this constructor is NOT a basic type
            let is_complex_type = !is_basic_type(constructor_name);

            if is_complex_type {
                pattern_vars_variables.push(PatternVariable {
                    name: node_name.clone(),
                    var_type: constructor_name.clone(),
                });
            }
        }
    }

    // For rewrite rules, add the root node to PatternVars for union operation
    if is_root && !pattern_vars_variables.iter().any(|v| v.name == root_node) {
        // Infer the type of the root node
        let root_node_type = infer_type_from_expr(lhs);
        pattern_vars_variables.push(PatternVariable {
            name: root_node.clone(),
            var_type: root_node_type,
        });
    }

    // Generate improved pattern query with conditions using handle and assert pattern
    let pattern_query = if pattern_query_parts.is_empty() {
        format!("// TODO: implement pattern query for {}", rule_name)
    } else {
        let mut assert_conditions = Vec::new();

        // Process conditions to generate handle queries and assert conditions
        // println!(
        //     "DEBUG: Processing conditions, variable_constructors: {:?}",
        //     variable_constructors
        // );
        println!("DEBUG: Condition info: {:?}", condition_info);
        let mut condition_vars = Vec::new();
        for (var_name, (constructor_name, node_name, arg_index)) in &variable_constructors {
            if let Some((operator, literal_value)) = condition_info.get(var_name) {
                // println!(
                //     "DEBUG: Found condition for variable {}: {} {}",
                //     var_name, operator, literal_value
                // );
                // Check if this constructor has basic type fields at this argument position
                let has_basic_type_field = dsl_types
                    .values()
                    .flat_map(|dsl_type| &dsl_type.variants)
                    .find(|variant| variant.name == *constructor_name)
                    .map_or(false, |variant| {
                        if *arg_index < variant.fields.len() {
                            is_basic_type(&variant.fields[*arg_index].field_type)
                        } else {
                            false
                        }
                    });

                if has_basic_type_field {
                    let field_name = get_field_name_for_variable_in_constructor(
                        constructor_name,
                        *arg_index,
                        dsl_types,
                    );

                    // Generate handle call
                    let handle_call = format!("{}.handle_{}()", node_name, field_name);

                    // Generate condition variable name (e.g., "a_b_eq" for variable "a" and "b")
                    let condition_var_name = format!("{}_{}_cond", var_name, literal_value);

                    // Generate condition expression
                    let condition_expr = match operator.as_str() {
                        "=" => format!("{}.eq(&{})", handle_call, literal_value),
                        "<" => format!("{}.lt(&{})", handle_call, literal_value),
                        "<=" => format!("{}.le(&{})", handle_call, literal_value),
                        ">" => format!("{}.gt(&{})", handle_call, literal_value),
                        ">=" => format!("{}.ge(&{})", handle_call, literal_value),
                        "!=" => format!("{}.ne(&{})", handle_call, literal_value),
                        _ => format!("{}.UNKNOWN(&{})", handle_call, literal_value), // default to eq
                    };

                    // Generate condition variable assignment
                    let condition_var =
                        format!("let {} = {{ {} }};", condition_var_name, condition_expr);
                    condition_vars.push(condition_var);
                    assert_conditions.push(condition_var_name);
                }
            }
        }

        // Process complex condition expressions
        for (operator, e1, e2) in &condition_expressions {
            // println!(
            //     "DEBUG: Processing complex condition: {} {:?} {:?}",
            //     operator, e1, e2
            // );

            // For complex expressions, we need to generate variables for both sides
            let mut temp_query_parts = Vec::new();
            let left_var = generate_expression_variable(
                e1,
                dsl_types,
                &mut temp_query_parts,
                &mut pattern_vars_variables,
                &mut node_counter,
                &mut variable_constructors,
            );
            let right_var = generate_expression_variable(
                e2,
                dsl_types,
                &mut temp_query_parts,
                &mut pattern_vars_variables,
                &mut node_counter,
                &mut variable_constructors,
            );

            // println!(
            //     "DEBUG: Generated variables: left_var={}, right_var={}",
            //     left_var, right_var
            // );
            // println!(
            //     "DEBUG: temp_query_parts after generation: {:?}",
            //     temp_query_parts
            // );

            // Generate condition expression using handle() method
            // For variables, look up their corresponding constructor nodes and generate field handle calls
            let left_handle = if let Some((constructor_name, node_name, arg_index)) =
                variable_constructors.get(&left_var)
            {
                // This is a variable with a constructor - generate field handle call
                let field_name = get_field_name_for_variable_in_constructor(
                    constructor_name,
                    *arg_index,
                    dsl_types,
                );
                format!("{}.handle_{}()", node_name, field_name)
            } else {
                // This is a literal or other expression - use generic handle()
                format!("{}.handle()", left_var)
            };

            let right_handle = if let Some((constructor_name, node_name, arg_index)) =
                variable_constructors.get(&right_var)
            {
                // This is a variable with a constructor - generate field handle call
                let field_name = get_field_name_for_variable_in_constructor(
                    constructor_name,
                    *arg_index,
                    dsl_types,
                );
                format!("{}.handle_{}()", node_name, field_name)
            } else {
                // This is a literal or other expression - use generic handle()
                format!("{}.handle()", right_var)
            };

            let condition_expr = match operator.as_str() {
                "=" => format!("{}.eq(&{})", left_handle, right_handle),
                "<" => format!("{}.lt(&{})", left_handle, right_handle),
                "<=" => format!("{}.le(&{})", left_handle, right_handle),
                ">" => format!("{}.gt(&{})", left_handle, right_handle),
                ">=" => format!("{}.ge(&{})", left_handle, right_handle),
                "!=" => format!("{}.ne(&{})", left_handle, right_handle),
                _ => format!("{}.eq(&{})", left_handle, right_handle), // default to eq
            };

            // Generate condition variable with node definitions inside the braces
            let condition_var_name = format!("cond_{}_{}", left_var, right_var);
            let condition_var = if temp_query_parts.is_empty() {
                format!("let {} = {{ {} }};", condition_var_name, condition_expr)
            } else {
                format!(
                    "let {} = {{ {} {} }};",
                    condition_var_name,
                    temp_query_parts.join(" "),
                    condition_expr
                )
            };
            condition_vars.push(condition_var);
            assert_conditions.push(condition_var_name);
        }

        // Create the final query AFTER all condition processing is complete
        let mut query = pattern_query_parts.join("\n");

        // Add condition variables before the pattern struct creation
        if !condition_vars.is_empty() {
            query.push_str("\n");
            query.push_str(&condition_vars.join("\n"));
        }

        query.push_str(&format!("\n{}Pat::new(", rule_name));

        // Add only the variables that are actually in PatternVars to the struct creation
        let pattern_var_refs: Vec<String> = pattern_vars_variables
            .iter()
            .map(|var| var.name.clone())
            .collect();
        query.push_str(&pattern_var_refs.join(", "));
        query.push_str(")\n");

        // Add assert conditions if any
        for condition in assert_conditions {
            query.push_str(format!(".assert({})\n", condition).as_str());
        }

        query
    };

    let pattern_vars = PatternVars {
        name: format!("{}Pat", rule_name),
        variables: pattern_vars_variables,
    };

    (
        pattern_vars,
        pattern_query,
        variable_constructors,
        root_node,
    )
}

/// Extract variables with type inference and build query pattern tree with variable context
/// Returns (node_name, is_root)
fn extract_variables_with_types_and_context(
    expr: &Expr,
    variables: &mut Vec<PatternVariable>,
    pattern_query_parts: &mut Vec<String>,
    node_counter: &mut usize,
    dsl_types: &HashMap<String, DslType>,
    variable_constructors: &mut HashMap<String, (String, String, usize)>,
    all_nodes: &mut Vec<String>,
    is_root: bool,
) -> (String, bool) {
    match expr {
        Expr::Var(_, var_name) => {
            // Variable reference - add to pattern variables with inferred type
            let var_type = infer_type_from_expr(expr);

            // Only add complex type variables to pattern variables
            // Basic type variables will be accessed through constructor instance fields
            let normalized_var_name = normalize_identifier(var_name);
            if !is_basic_type(&var_type) && !variables.iter().any(|v| v.name == normalized_var_name)
            {
                variables.push(PatternVariable {
                    name: normalized_var_name.clone(),
                    var_type: var_type.clone(),
                });
                // For complex types, add query
                let query_method = if is_leaf_constructor(&var_type, dsl_types) {
                    "query"
                } else {
                    "query"
                };
                pattern_query_parts.push(format!(
                    "let {} = {}::{}();",
                    normalized_var_name, var_type, query_method
                ));
                // Add variable node to all nodes
                all_nodes.push(normalized_var_name.clone());
            }
            (normalized_var_name, is_root)
        }
        Expr::Call(_, func_name, args) => {
            // Function call - recursively extract variables from arguments and build the tree
            let mut complex_arg_nodes = Vec::new();
            let mut basic_conditions = Vec::new();

            // Generate unique node name for this constructor call (shared for all arguments)
            *node_counter += 1;
            let constructor_node_name = format!(
                "{}_node{}",
                normalize_identifier(&func_name.to_snake_case()),
                node_counter
            );

            for (index, arg) in args.iter().enumerate() {
                // For constructor calls, infer variable types from context
                let (arg_node, _) = if let Expr::Var(_, var_name) = arg {
                    // This variable appears in a constructor call - infer its type
                    let inferred_type =
                        infer_variable_type_from_constructor(func_name, index, dsl_types);

                    // Record the constructor context for this variable - use shared constructor node
                    let normalized_var_name = normalize_identifier(var_name);
                    variable_constructors.insert(
                        normalized_var_name.clone(),
                        (func_name.clone(), constructor_node_name.clone(), index),
                    );

                    // Only add complex type variables to pattern variables
                    // Basic type variables will be accessed through constructor instance fields
                    if !is_basic_type(&inferred_type)
                        && !variables.iter().any(|v| v.name == normalized_var_name)
                    {
                        variables.push(PatternVariable {
                            name: normalized_var_name.clone(),
                            var_type: inferred_type.clone(),
                        });
                        // For complex types, add query
                        let query_method = if is_leaf_constructor(&inferred_type, dsl_types) {
                            format!(r#"query_leaf"#)
                        } else {
                            format!(r#"query_leaf"#)
                        };
                        pattern_query_parts.push(format!(
                            "let {} = {}::{}();",
                            normalized_var_name, inferred_type, query_method
                        ));
                    }
                    (normalized_var_name, false)
                } else {
                    // For non-variable arguments, use the normal recursive extraction
                    extract_variables_with_types_and_context(
                        arg,
                        variables,
                        pattern_query_parts,
                        node_counter,
                        dsl_types,
                        variable_constructors,
                        all_nodes,
                        false,
                    )
                };

                // Check if this argument is a basic type (literal or basic type variable)
                let arg_type = infer_variable_type_from_constructor(func_name, index, dsl_types);
                if is_basic_type(&arg_type) {
                    // For basic types, create StrippedCondition instead of query parameter
                    // But for string variables, don't create constraints in query - they can be accessed in action
                    if let Expr::Var(_, var_name) = arg {
                        if arg_type == "String" {
                            // String variables don't need constraints in query - they can be accessed via pat.xxx.field_name
                            // Just add them to pattern variables if not already present
                            let normalized_var_name = normalize_identifier(var_name);
                            if !variables.iter().any(|v| v.name == normalized_var_name) {
                                variables.push(PatternVariable {
                                    name: normalized_var_name.clone(),
                                    var_type: "String".to_string(),
                                });
                            }
                        } else {
                            // For other basic types (i64, f64, bool), create constraints
                            let field_name = get_field_name_for_variable_in_constructor(
                                func_name, index, dsl_types,
                            );
                            basic_conditions.push(format!(
                                "pat.{}.{} == {}",
                                constructor_node_name, field_name, arg_node
                            ));
                        }
                    } else {
                        // For literals, just use the value directly
                        basic_conditions
                            .push(format!("pat.{} == {}", constructor_node_name, arg_node));
                    }
                } else {
                    // For complex types, add to argument nodes only if this is not a leaf constructor
                    if !is_leaf_constructor(func_name, dsl_types) {
                        // println!("push {}:{}", arg_node, arg_type);
                        complex_arg_nodes.push(arg_node);
                    }
                }
            }

            // Special case: if this is a constructor call with only variable arguments,
            // we still need to create query nodes for the constructor to represent the hyperedge
            // but we can simplify the return value
            let all_args_are_variables = args.iter().all(|arg| matches!(arg, Expr::Var(_, _)));
            if all_args_are_variables && complex_arg_nodes.len() == args.len() {
                // For constructor calls like (Const b), we still need the constructor query
                // but we can use the first variable node as the return value for pattern matching
                if !complex_arg_nodes.is_empty() {
                    // We still create the constructor query node, but return the variable for pattern
                    // This ensures all constructor queries are generated
                }
            }

            // For leaf constructors (only basic type arguments), use the shared constructor node
            // For internal constructors (with complex type arguments), create a new query node
            let node_name = if is_leaf_constructor(func_name, dsl_types) {
                // Use the shared constructor node for leaf constructors
                constructor_node_name
            } else {
                // Generate unique node name for internal constructors
                // *node_counter += 1;
                // format!(
                //     "{}_node{}",
                //     normalize_identifier(&func_name.to_snake_case()),
                //     node_counter
                // )
                constructor_node_name
            };

            // Add this node to the list of all nodes
            all_nodes.push(node_name.clone());

            // Build argument references - only for complex types
            let arg_refs: Vec<String> = complex_arg_nodes
                .iter()
                .map(|node_name| {
                    // Check if this is a literal (starts with digit or quote)
                    if node_name
                        .chars()
                        .next()
                        .map_or(false, |c| c.is_digit(10) || c == '"')
                    {
                        node_name.clone()
                    } else {
                        println!(" recog as complex {:?}", node_name);
                        format!("&{}", node_name)
                    }
                })
                .collect();

            // Build condition queries for basic type arguments
            let mut condition_queries = Vec::new();
            for condition in &basic_conditions {
                // Parse condition like "pat.m_num_node2.arg_i64_00 == a"
                // Convert to query condition like ".arg_i64_00(&10000)"
                if condition.contains(" == ") {
                    let parts: Vec<&str> = condition.split(" == ").collect();
                    if parts.len() == 2 {
                        let field_access = parts[0].trim();
                        let value = parts[1].trim();

                        // Extract field name from field access (e.g., "pat.m_num_node2.arg_i64_00" -> "arg_i64_00")
                        if let Some(field_name) = field_access.split('.').last() {
                            // For conditions, we need to use the literal value from the condition
                            // The value should be the literal (e.g., 10000) not the variable name
                            // Since we're processing conditions, the value should already be the literal
                            condition_queries.push(format!(".{}(&{})", field_name, value));
                        }
                    }
                }
            }

            // Add the query for this function call (the hyperedge) - only complex args
            // Use query_leaf() for leaf nodes (no arguments), query() for internal nodes
            if is_leaf_constructor(func_name, dsl_types) {
                // Leaf nodes use query_leaf() without arguments
                // Only add the query if we haven't already added it for this constructor
                if !pattern_query_parts
                    .iter()
                    .any(|part| part.contains(&node_name))
                {
                    // For leaf nodes, always generate simple query without condition queries
                    // Conditions are handled in the assert phase using handle patterns
                    let full_query = format!(
                        "let {} = {}::query();",
                        node_name,
                        normalize_identifier(func_name)
                    );
                    pattern_query_parts.push(full_query);
                }
            } else {
                // Internal nodes use query() with arguments
                let base_query = format!(
                    "let {} = {}::query({})",
                    node_name,
                    normalize_identifier(func_name),
                    arg_refs.join(", ")
                );
                let full_query = if !condition_queries.is_empty() {
                    format!("{}{};", base_query, condition_queries.join(""))
                } else {
                    format!("{};", base_query)
                };
                pattern_query_parts.push(full_query);
            }

            (node_name, is_root)
        }
        Expr::Lit(_, lit) => {
            // Literal - no variables to extract, return literal value
            (
                match lit {
                    Literal::Int(i) => i.to_string(),
                    Literal::Float(f) => f.0.to_string(),
                    Literal::String(s) => format!("\"{}\"", s),
                    Literal::Bool(b) => b.to_string(),
                    Literal::Unit => "()".to_string(),
                },
                is_root,
            )
        }
    }
}

/// Convert egglog commands to eggplant commands (backward compatibility)
pub fn convert_to_eggplant(commands: &[Command]) -> Vec<EggplantCommand> {
    convert_to_eggplant_with_source(commands, None)
        .into_iter()
        .map(|cmd_with_source| cmd_with_source.command)
        .collect()
}

/// Variable context that maps variables to their constructor and field information
#[derive(Debug, Clone)]
struct VariableContext {
    variables: Vec<PatternVariable>,
    variable_constructors: HashMap<String, (String, String, usize)>, // var_name -> (constructor_name, node_name, arg_index)
}

/// Analyze a rewrite pattern with conditions to extract pattern variables and generate pattern query
fn analyze_rewrite_pattern_with_conditions(
    lhs: &Expr,
    rule_name: &str,
    dsl_types: &HashMap<String, DslType>,
    conditions: &[Fact],
) -> (PatternVars, String, VariableContext, String) {
    let (pattern_vars, pattern_query, variable_constructors, root_node) =
        generate_pattern_query_with_context_and_conditions(lhs, rule_name, dsl_types, conditions);
    let context = VariableContext {
        variables: pattern_vars.variables.clone(),
        variable_constructors,
    };
    (pattern_vars, pattern_query, context, root_node)
}

/// Generate rewrite action from the right-hand side pattern with variable context
fn generate_rewrite_action_with_context(
    rhs: &Expr,
    _rule_name: &str,
    context: &VariableContext,
    dsl_types: &HashMap<String, DslType>,
    root_node_name: &str,
) -> String {
    let result_expr = generate_insert_expr(rhs, context, dsl_types);

    format!(
        "let result = {};\nctx.union(pat.{}, result);",
        result_expr, root_node_name
    )
}

/// Generate insert expression for RHS nodes with proper function names and parameter ordering
fn generate_insert_expr(
    expr: &Expr,
    context: &VariableContext,
    dsl_types: &HashMap<String, DslType>,
) -> String {
    match expr {
        Expr::Var(_, var_name) => {
            // Simple variable reference
            // Check if this variable is a basic type accessed through constructor
            let normalized_var_name = normalize_identifier(var_name);
            if let Some((constructor_name, node_name, arg_index)) =
                context.variable_constructors.get(&normalized_var_name)
            {
                // Check if this constructor has basic type fields at this argument position
                let has_basic_type_field = dsl_types
                    .values()
                    .flat_map(|dsl_type| &dsl_type.variants)
                    .find(|variant| variant.name == *constructor_name)
                    .map_or(false, |variant| {
                        if *arg_index < variant.fields.len() {
                            is_basic_type(&variant.fields[*arg_index].field_type)
                        } else {
                            false
                        }
                    });

                if has_basic_type_field {
                    // Basic type variable accessed through constructor field
                    let field_name = get_field_name_for_variable_in_constructor(
                        constructor_name,
                        *arg_index,
                        dsl_types,
                    );
                    format!("ctx.devalue(pat.{}.{})", node_name, field_name)
                } else {
                    // TODO insert function might be different when insert container
                    // Complex type variable - use the variable directly
                    if let Some(_var_info) = context
                        .variables
                        .iter()
                        .find(|v| v.name == normalized_var_name)
                    {
                        // let insert_function =
                        //     format!("insert_{}", var_info.var_type.to_snake_case());
                        // format!("ctx.{}(pat.{})", insert_function, normalized_var_name)
                        format!("pat.{}", normalized_var_name)
                    } else {
                        // Fallback for complex type variables
                        format!("pat.{}", normalized_var_name)
                    }
                }
            } else {
                // TODO insert function might be different when insert container
                // Complex type variable - we need to access its fields
                // Find the variable type and generate appropriate insert function
                if let Some(_var_info) = context
                    .variables
                    .iter()
                    .find(|v| v.name == normalized_var_name)
                {
                    format!("pat.{}", normalized_var_name)
                } else {
                    // Fallback for complex type variables
                    format!("pat.{}", normalized_var_name)
                }
            }
        }
        Expr::Call(_, func_name, args) => {
            // Check if this is a basic operation (+, -, *, /) that needs ctx.devalue()
            if is_basic_operation(func_name) {
                // For basic operations, we need to extract values using ctx.devalue()
                let operation_args: Vec<String> = args
                    .iter()
                    .map(|arg| {
                        let expr = generate_insert_expr(arg, context, dsl_types);
                        // For basic operations, we need to extract the actual values, not insert them
                        if expr.starts_with("ctx.devalue(") {
                            // Already using devalue, just use as is
                            expr
                        } else if expr.starts_with("ctx.insert_") {
                            // For complex type inserts, we need to use ctx.devalue()
                            format!("ctx.devalue({})", expr)
                        } else {
                            expr
                        }
                    })
                    .collect();

                // For basic operations, compute the value
                format!(
                    "({} {} {})",
                    operation_args[0], func_name, operation_args[1]
                )
            } else {
                // Complex type constructor call - generate proper insert function
                // Get the variant information to ensure correct parameter ordering
                let _variant_info = find_variant_info(func_name, dsl_types);

                let arg_exprs: Vec<String> = args
                    .iter()
                    .map(|arg| generate_insert_expr(arg, context, dsl_types))
                    .collect();

                // Generate the correct insert function name based on variant or primitve matching
                match func_name.as_str() {
                    "max" => {
                        let max_prim_fn = format!("std::cmp::max");
                        format!("{}({})", max_prim_fn, arg_exprs.join(", "))
                    }
                    "min" => {
                        let min_prim_fn = format!("std::cmp::min");
                        format!("{}({})", min_prim_fn, arg_exprs.join(", "))
                    }
                    "&" => {
                        let bitand_prim_fn = format!("std::ops::BitAnd::bitand");
                        format!("{}({})", bitand_prim_fn, arg_exprs.join(", "))
                    }
                    _ => {
                        let insert_fn = format!("insert_{}", func_name.to_snake_case());
                        format!("ctx.{}({})", insert_fn, arg_exprs.join(", "))
                    }
                }
            }
        }
        Expr::Lit(_, lit) => {
            // Literal value - use directly for basic literals
            match lit {
                Literal::Int(i) => i.to_string(),
                Literal::Float(f) => f.0.to_string(),
                Literal::String(s) => format!("\"{}\"", s),
                Literal::Bool(b) => b.to_string(),
                Literal::Unit => "()".to_string(),
            }
        }
    }
}

/// Check if a function name represents a basic operation
fn is_basic_operation(func_name: &str) -> bool {
    let basic_operations = ["+", "-", "*", "/", "<", ">", "<=", ">=", "==", "!="];
    basic_operations.contains(&func_name)
}

/// Check if a constructor is a leaf node (has only basic type arguments)
fn is_leaf_constructor(func_name: &str, dsl_types: &HashMap<String, DslType>) -> bool {
    // Look for the constructor in DSL types
    for (_, dsl_type) in dsl_types {
        if let Some(variant) = dsl_type.variants.iter().find(|v| v.name == func_name) {
            // Check if all fields are basic types
            return variant
                .fields
                .iter()
                .all(|field| is_basic_type(&field.field_type));
        }
    }
    false
}

/// Get the field name for a variable in a specific constructor
fn get_field_name_for_variable_in_constructor(
    constructor_name: &str,
    arg_index: usize,
    dsl_types: &HashMap<String, DslType>,
) -> String {
    // Look for the constructor in DSL types
    for (_, dsl_type) in dsl_types {
        if let Some(variant) = dsl_type
            .variants
            .iter()
            .find(|v| v.name == constructor_name)
        {
            // Check if we have a field at this argument index
            if arg_index < variant.fields.len() {
                return variant.fields[arg_index].name.clone();
            }
        }
    }

    // Fallback: use generic field name
    format!("arg{}", arg_index)
}

/// Find variant information for a constructor
fn find_variant_info<'a>(
    constructor_name: &str,
    dsl_types: &'a HashMap<String, DslType>,
) -> Option<&'a DslVariant> {
    for (_, dsl_type) in dsl_types {
        if let Some(variant) = dsl_type
            .variants
            .iter()
            .find(|v| v.name == constructor_name)
        {
            return Some(variant);
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::parse::Parser;

    #[test]
    fn test_eggplant_conversion() {
        let program = r#"
            (datatype Math)
            (constructor Num (i64) Math)
            (let x (Num 42))
        "#;

        let mut parser = Parser::default();
        let commands = parser.get_program_from_string(None, program).unwrap();
        let eggplant_commands = convert_to_eggplant(&commands);

        assert!(eggplant_commands.len() >= 3); // Transaction, DslType, Let, Ruleset, RunRuleset

        // Check for DslType
        let has_dsl_type = eggplant_commands.iter().any(|cmd| {
            if let EggplantCommand::DslType(dsl_type) = cmd {
                dsl_type.name == "Math"
            } else {
                false
            }
        });
        assert!(has_dsl_type);
    }
}

/// Generate a variable for an expression in condition context
fn generate_expression_variable(
    expr: &Expr,
    dsl_types: &HashMap<String, DslType>,
    pattern_query_parts: &mut Vec<String>,
    pattern_vars_variables: &mut Vec<PatternVariable>,
    node_counter: &mut usize,
    variable_constructors: &mut HashMap<String, (String, String, usize)>,
) -> String {
    match expr {
        Expr::Var(_, var_name) => {
            // For variables, use the existing variable name
            normalize_identifier(var_name)
        }
        Expr::Lit(_, lit) => {
            // For literals, generate a unique variable name
            let var_name = match lit {
                Literal::Int(i) => format!("lit_{}", i),
                Literal::Float(f) => format!("lit_{}", f.0),
                Literal::String(s) => format!("lit_{}", s.replace('\"', "")),
                Literal::Bool(b) => format!("lit_{}", b),
                Literal::Unit => "lit_unit".to_string(),
            };

            // For literals, we don't add them to pattern variables since they are basic types
            // Only complex types should be added to pattern variables

            var_name
        }
        Expr::Call(_, func_name, args) => {
            // Check if this is a known constructor in DslTypes or a basic type
            let is_known_constructor =
                dsl_types.contains_key(func_name) || is_basic_type(func_name);

            if !is_known_constructor {
                // This is an unknown action/function call - generate function call format
                let node_name = format!("node_{}", node_counter);
                *node_counter += 1;

                // Extract variables or literal values for arguments
                let arg_values: Vec<String> = args
                    .iter()
                    .map(|arg| match arg {
                        Expr::Lit(_, lit) => {
                            // For literals, use the literal value directly
                            match lit {
                                Literal::Int(i) => i.to_string(),
                                Literal::Float(f) => f.0.to_string(),
                                Literal::String(s) => format!("\"{}\"", s),
                                Literal::Bool(b) => b.to_string(),
                                Literal::Unit => "()".to_string(),
                            }
                        }
                        Expr::Var(_, var_name) => {
                            // For variables, check if they have constructor context
                            let normalized_var_name = normalize_identifier(var_name);
                            if let Some((constructor_name, node_name, arg_index)) =
                                variable_constructors.get(&normalized_var_name)
                            {
                                // This variable has constructor context - generate handle call
                                let field_name = get_field_name_for_variable_in_constructor(
                                    constructor_name,
                                    *arg_index,
                                    dsl_types,
                                );
                                format!("{}.handle_{}()", node_name, field_name)
                            } else {
                                // No constructor context - use the variable name
                                normalized_var_name
                            }
                        }
                        _ => {
                            // For other expressions, generate variables
                            generate_expression_variable(
                                arg,
                                dsl_types,
                                pattern_query_parts,
                                pattern_vars_variables,
                                node_counter,
                                variable_constructors,
                            )
                        }
                    })
                    .collect();

                // Generate function call query
                let query = if arg_values.is_empty() {
                    format!("let {} = {}::query_leaf();", node_name, func_name)
                } else {
                    // Generate function call with arguments
                    // For operators like %, use a valid function name
                    let valid_func_name = format!("TODO_{}", func_name);
                    let mut query_parts =
                        vec![format!("let {} = {}::query()", node_name, valid_func_name)];

                    for (i, arg_value) in arg_values.iter().enumerate() {
                        query_parts.push(format!(".arg_{:02}(&{})", i, arg_value));
                    }

                    query_parts.push(";".to_string());
                    query_parts.join("")
                };

                pattern_query_parts.push(query);

                // For unknown actions, assume return type is i64 (most common for arithmetic operations)
                let return_type = "i64".to_string();

                // Add to pattern variables only if it's a complex type
                if !is_basic_type(&return_type)
                    && !pattern_vars_variables.iter().any(|v| v.name == node_name)
                {
                    pattern_vars_variables.push(PatternVariable {
                        name: node_name.clone(),
                        var_type: return_type,
                    });
                }

                node_name
            } else {
                // This is a known constructor - use the existing logic
                let node_name = format!("node_{}", node_counter);
                *node_counter += 1;

                // Extract variables or literal values for arguments
                let arg_values: Vec<String> = args
                    .iter()
                    .map(|arg| match arg {
                        Expr::Lit(_, lit) => {
                            // For literals, use the literal value directly
                            match lit {
                                Literal::Int(i) => i.to_string(),
                                Literal::Float(f) => f.0.to_string(),
                                Literal::String(s) => format!("\"{}\"", s),
                                Literal::Bool(b) => b.to_string(),
                                Literal::Unit => "()".to_string(),
                            }
                        }
                        _ => {
                            // For other expressions, generate variables
                            generate_expression_variable(
                                arg,
                                dsl_types,
                                pattern_query_parts,
                                pattern_vars_variables,
                                node_counter,
                                variable_constructors,
                            )
                        }
                    })
                    .collect();

                // Generate query for this constructor
                let query = if arg_values.is_empty() {
                    format!("let {} = {}::query_leaf();", node_name, func_name)
                } else {
                    // For constructor calls with literal arguments, use method chaining
                    // e.g., MNum::query().num(&1) instead of MNum::query(&1)
                    let constructor_name = func_name.to_string();
                    let mut query_parts =
                        vec![format!("let {} = {}::query()", node_name, constructor_name)];

                    // Get the constructor variant to determine field names
                    if let Some(dsl_type) = dsl_types.get(&constructor_name) {
                        for (i, arg_value) in arg_values.iter().enumerate() {
                            if let Some(variant) = dsl_type
                                .variants
                                .iter()
                                .find(|v| v.name == constructor_name)
                            {
                                if i < variant.fields.len() {
                                    let field_name =
                                        format!("arg_{}_{:02}", variant.fields[i].field_type, i);
                                    query_parts.push(format!(".{}(&{})", field_name, arg_value));
                                } else {
                                    // Fallback if we don't have enough type info
                                    query_parts.push(format!(".arg_{:02}(&{})", i, arg_value));
                                }
                            } else {
                                // Fallback if variant not found
                                query_parts.push(format!(".arg_{:02}(&{})", i, arg_value));
                            }
                        }
                    } else {
                        // Fallback if type not found
                        for (i, arg_value) in arg_values.iter().enumerate() {
                            query_parts.push(format!(".arg_{:02}(&{})", i, arg_value));
                        }
                    }

                    query_parts.push(";".to_string());
                    query_parts.join("")
                };

                pattern_query_parts.push(query);

                // Infer the correct return type for this constructor
                let return_type = infer_variable_type_from_constructor(func_name, 0, dsl_types);

                // Add to pattern variables only if it's a complex type
                if !is_basic_type(&return_type)
                    && !pattern_vars_variables.iter().any(|v| v.name == node_name)
                {
                    pattern_vars_variables.push(PatternVariable {
                        name: node_name.clone(),
                        var_type: return_type,
                    });
                }

                node_name
            }
        }
    }
}
