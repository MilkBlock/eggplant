//! Eggplant AST and code generator
//!
//! Eggplant is a simplified version of egglog with focus on educational examples

use crate::ast::*;
use heck::ToSnakeCase;
use std::collections::{HashMap, HashSet};

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

/// Eggplant relation type definition
#[derive(Debug, Clone, PartialEq)]
pub struct RelationType {
    pub name: String,
    pub fields: Vec<DslField>,
}

/// Eggplant function type definition
#[derive(Debug, Clone, PartialEq)]
pub struct FunctionType {
    pub name: String,
    pub fields: Vec<DslField>,
    pub output_type: String,
    pub merge_expr: Option<String>,
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
    /// Define a relation type with #[eggplant::relation]
    RelationType(RelationType),
    /// Define a function type with #[eggplant::func]
    FunctionType(FunctionType),
    /// Define pattern variables with #[eggplant::pat_vars]
    PatternVars(PatternVars),
    /// Define a rule with add_rule
    Rule(EggplantRule),
    /// Create a ruleset
    Ruleset(String),
    /// Run ruleset
    RunRuleset(String, String),
    /// Bridge run :until through the underlying egglog interpreter
    RunUntil {
        ruleset: String,
        limit: Option<usize>,
        until: Fact,
    },
    /// Bridge run-schedule through the underlying egglog interpreter
    RunSchedule { schedules: Vec<Schedule> },
    /// Commit operation
    Commit(String),
    /// Pull operation
    Pull(String),
    /// Extract the current best term/value for an expression
    Extract { expr: Expr, variants: Option<usize> },
    /// Transaction definition
    Transaction(String),
    /// Pattern recorder definition
    PatternRecorder(String),
    /// Test assertion
    Assert { expr: Expr, expected: Expr },
    /// Variable assignment
    Let { var: String, expr: Expr },
    /// Relation seed insert
    RelationInsert { relation: String, args: Vec<Expr> },
    /// Print statement
    Print { expr: Expr },
    /// Bridge print-size through the underlying egglog interpreter
    PrintSize { target: Option<String> },
    /// Bridge print-function through the underlying egglog interpreter
    PrintFunction {
        name: String,
        size: Option<usize>,
        file: Option<String>,
        mode: Option<String>,
    },
    /// Bridge include through the underlying egglog interpreter
    Include { file: String },
    /// Bridge an arbitrary egglog command through the underlying interpreter
    RawEgglogCommand { command: String },
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
    binding_types: HashMap<String, String>,
}

impl EggplantCodeGenerator {
    pub fn new() -> Self {
        Self {
            output: String::new(),
            indent_level: 0,
            options: CodeGenOptions::default(),
            binding_types: HashMap::new(),
        }
    }

    /// Create a new code generator with custom options
    pub fn with_options(options: CodeGenOptions) -> Self {
        Self {
            output: String::new(),
            indent_level: 0,
            options,
            binding_types: HashMap::new(),
        }
    }

    /// Generate Rust code with eggplant macros
    pub fn generate_rust(&mut self, commands: &[EggplantCommandWithSource]) -> String {
        self.output.clear();
        self.binding_types.clear();

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
                EggplantCommand::FunctionType(_) => {
                    self.generate_rust_command_with_source(cmd_with_source);
                }
                EggplantCommand::RelationType(_) => {
                    self.generate_rust_command_with_source(cmd_with_source);
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
                EggplantCommand::FunctionType(_) => {
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
            EggplantCommand::RelationType(relation_type) => {
                if !self.options.omit_head_annotation {
                    self.add_line(&format!("// Relation '{}'", relation_type.name));
                }
                self.add_line("#[eggplant::relation]");
                self.add_line(&format!("struct {} {{", relation_type.name));
                self.indent();
                for field in &relation_type.fields {
                    self.add_line(&format!("{}: {},", field.name, field.field_type));
                }
                self.dedent();
                self.add_line("}");
                self.add_line("");
            }
            EggplantCommand::FunctionType(function_type) => {
                if !self.options.omit_head_annotation {
                    self.add_line(&format!("// Function '{}'", function_type.name));
                }
                let merge_attr = if let Some(merge_expr) = &function_type.merge_expr {
                    format!("merge = {:?}", merge_expr)
                } else {
                    "no_merge".to_string()
                };
                self.add_line(&format!(
                    "#[eggplant::func(output = {}, {})]",
                    function_type.output_type, merge_attr
                ));
                self.add_line(&format!("struct {} {{", function_type.name));
                self.indent();
                for field in &function_type.fields {
                    self.add_line(&format!("{}: {},", field.name, field.field_type));
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
            EggplantCommand::Extract { expr, variants } => {
                if let Some(variants) = variants {
                    self.add_line(&format!("// extract requested {} variant(s)", variants));
                }
                self.add_line(&format!("{}.pull();", self.expr_to_string(expr)));
            }
            EggplantCommand::RunRuleset(ruleset, config) => {
                if !self.options.omit_run_ruleset_calls {
                    self.add_line(&format!(
                        "MyTx::run_ruleset({}, RunConfig::{});",
                        ruleset, config
                    ));
                }
            }
            EggplantCommand::RunUntil {
                ruleset,
                limit,
                until,
            } => {
                let mut command = format!("(run {}", ruleset);
                if let Some(limit) = limit {
                    command.push_str(&format!(" {}", limit));
                }
                command.push_str(&format!(
                    " :until {})",
                    fact_to_egglog_command_string(until)
                ));
                self.add_line("let outputs = {");
                self.indent();
                self.add_line("let mut egraph = MyTx::sgl().egraph.lock().unwrap();");
                self.add_line(&format!(
                    "egraph.parse_and_run_program(None, {:?}).unwrap()",
                    command
                ));
                self.dedent();
                self.add_line("};");
                self.add_line("for output in outputs {");
                self.indent();
                self.add_line("print!(\"{}\", output);");
                self.dedent();
                self.add_line("}");
            }
            EggplantCommand::RunSchedule { schedules } => {
                if schedules.iter().any(schedule_has_until) {
                    let schedule_program = schedules
                        .iter()
                        .map(ToString::to_string)
                        .collect::<Vec<_>>()
                        .join(" ");
                    let command = format!("(run-schedule {schedule_program})");
                    self.add_line("let outputs = {");
                    self.indent();
                    self.add_line("let mut egraph = MyTx::sgl().egraph.lock().unwrap();");
                    self.add_line(&format!(
                        "egraph.parse_and_run_program(None, {:?}).unwrap()",
                        command
                    ));
                    self.dedent();
                    self.add_line("};");
                    self.add_line("for output in outputs {");
                    self.indent();
                    self.add_line("print!(\"{}\", output);");
                    self.dedent();
                    self.add_line("}");
                } else {
                    self.add_line(&format!(
                        "let schedule = {};",
                        schedule_items_to_rust_expr(schedules)
                    ));
                    self.add_line("let _report = MyTx::run_schedule(schedule);");
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
                let var_type = self.infer_expr_type(expr);
                self.binding_types.insert(var.clone(), var_type.clone());
                if self.is_basic_type(&var_type) {
                    self.add_line(&format!("let {var}:{var_type} = {expr_str};"));
                } else {
                    self.add_line(&format!("let {var}:{var_type}<MyTx> = {expr_str};"));
                    self.add_line(&format!("{var}.commit();"));
                }
            }
            EggplantCommand::RelationInsert { relation, args } => {
                let arg_str = args
                    .iter()
                    .map(|arg| self.expr_with_reference(arg))
                    .collect::<Vec<_>>()
                    .join(", ");
                self.add_line(&format!("{relation}::<MyTx>::insert({arg_str});"));
            }
            EggplantCommand::Print { expr } => {
                self.add_line(&format!(
                    "info!(\"{{:?}}\", {});",
                    self.expr_to_string(expr)
                ));
            }
            EggplantCommand::PrintSize { target } => {
                let mut command = "(print-size".to_string();
                if let Some(target) = target {
                    command.push(' ');
                    command.push_str(&normalize_identifier(target));
                }
                command.push(')');

                self.add_line("let outputs = {");
                self.indent();
                self.add_line("let mut egraph = MyTx::sgl().egraph.lock().unwrap();");
                self.add_line(&format!(
                    "egraph.parse_and_run_program(None, {:?}).unwrap()",
                    command
                ));
                self.dedent();
                self.add_line("};");
                self.add_line("for output in outputs {");
                self.indent();
                self.add_line("print!(\"{}\", output);");
                self.dedent();
                self.add_line("}");
            }
            EggplantCommand::PrintFunction {
                name,
                size,
                file,
                mode,
            } => {
                let mut command = format!("(print-function {}", normalize_identifier(name));
                if let Some(size) = size {
                    command.push_str(&format!(" {}", size));
                }
                if let Some(file) = file {
                    command.push_str(&format!(" :file {:?}", file));
                }
                if let Some(mode) = mode {
                    command.push_str(&format!(" :mode {}", mode));
                }
                command.push(')');

                self.add_line("let outputs = {");
                self.indent();
                self.add_line("let mut egraph = MyTx::sgl().egraph.lock().unwrap();");
                self.add_line(&format!(
                    "egraph.parse_and_run_program(None, {:?}).unwrap()",
                    command
                ));
                self.dedent();
                self.add_line("};");
                self.add_line("for output in outputs {");
                self.indent();
                self.add_line("print!(\"{}\", output);");
                self.dedent();
                self.add_line("}");
            }
            EggplantCommand::Include { file } => {
                let command = format!("(include {:?})", file);
                self.add_line("let outputs = {");
                self.indent();
                self.add_line("let mut egraph = MyTx::sgl().egraph.lock().unwrap();");
                self.add_line(&format!(
                    "egraph.parse_and_run_program(None, {:?}).unwrap()",
                    command
                ));
                self.dedent();
                self.add_line("};");
                self.add_line("for output in outputs {");
                self.indent();
                self.add_line("print!(\"{}\", output);");
                self.dedent();
                self.add_line("}");
            }
            EggplantCommand::RawEgglogCommand { command } => {
                self.add_line("let outputs = {");
                self.indent();
                self.add_line("let mut egraph = MyTx::sgl().egraph.lock().unwrap();");
                self.add_line(&format!(
                    "egraph.parse_and_run_program(None, {:?}).unwrap()",
                    command
                ));
                self.dedent();
                self.add_line("};");
                self.add_line("for output in outputs {");
                self.indent();
                self.add_line("print!(\"{}\", output);");
                self.dedent();
                self.add_line("}");
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
            Expr::Var(_, var) => {
                let var_type = self
                    .binding_types
                    .get(&normalize_identifier(var))
                    .cloned()
                    .unwrap_or_else(|| infer_type_from_expr(expr));
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

    fn infer_expr_type(&self, expr: &Expr) -> String {
        match expr {
            Expr::Var(_, var) => self
                .binding_types
                .get(&normalize_identifier(var))
                .cloned()
                .unwrap_or_else(|| infer_type_from_expr(expr)),
            _ => infer_type_from_expr(expr),
        }
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
    let mut sort_schemas: HashMap<String, SortSchema> = HashMap::new();
    let mut relation_types: HashMap<String, RelationType> = HashMap::new();
    let mut function_types: HashMap<String, FunctionType> = HashMap::new();
    let mut global_bindings: HashMap<String, Expr> = HashMap::new();

    // Extract original statements if program is provided

    // Add transaction definition
    eggplant_commands.push(EggplantCommandWithSource {
        command: EggplantCommand::Transaction("MyTx".to_string()),
        source_file: source_file.clone(),
        source_line: Some(1),
    });

    // First pass: collect all constructors for each datatype/sort with source info
    let mut datatype_constructors: HashMap<String, Vec<DslVariant>> = HashMap::new();
    let mut declared_datatypes: HashSet<String> = HashSet::new();
    let mut constructor_line_numbers: HashMap<String, usize> = HashMap::new();
    let mut constructor_counter = 1;

    for command in commands {
        constructor_counter += 1;
        match command {
            Command::Datatype { name, .. } => {
                declared_datatypes.insert(normalize_identifier(name));
            }
            Command::Sort(_, name, schema) => {
                if let Some((sort_kind, args)) = schema {
                    sort_schemas.insert(
                        normalize_identifier(name),
                        SortSchema {
                            name: normalize_identifier(sort_kind),
                            args: args.iter().map(expr_type_name).collect(),
                        },
                    );
                }
            }
            Command::Constructor { name, schema, .. } => {
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
            Command::Relation { name, inputs, .. } => {
                relation_types.insert(
                    name.clone(),
                    RelationType {
                        name: normalize_identifier(name),
                        fields: inputs
                            .iter()
                            .enumerate()
                            .map(|(i, field_type)| DslField {
                                name: format!("arg{}", i),
                                field_type: normalize_identifier(field_type),
                            })
                            .collect(),
                    },
                );
            }
            Command::Function {
                name,
                schema,
                merge,
                ..
            } => {
                function_types.insert(
                    name.clone(),
                    FunctionType {
                        name: normalize_identifier(name),
                        fields: schema
                            .input
                            .iter()
                            .enumerate()
                            .map(|(i, typ)| DslField {
                                name: format!("arg{}", i),
                                field_type: normalize_identifier(typ),
                            })
                            .collect(),
                        output_type: normalize_identifier(&schema.output),
                        merge_expr: merge.as_ref().map(|expr| expr.to_string()),
                    },
                );
            }
            Command::Action(Action::Let(_, var, expr)) => {
                global_bindings.insert(normalize_identifier(var), expr.clone());
            }
            _ => {}
        }
    }

    synthesize_implicit_list_helpers(
        commands,
        &datatype_constructors,
        &mut eggplant_commands,
        &mut dsl_types,
        &mut relation_types,
        &mut function_types,
        &source_file,
    );
    synthesize_constructor_only_sorts(
        &datatype_constructors,
        &declared_datatypes,
        &mut eggplant_commands,
        &mut dsl_types,
        &source_file,
    );

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
            Command::Function { name, span, .. } => {
                let function_type = function_types.get(name).cloned().unwrap_or(FunctionType {
                    name: normalize_identifier(name),
                    fields: Vec::new(),
                    output_type: "()".to_string(),
                    merge_expr: None,
                });
                eggplant_commands.push(EggplantCommandWithSource {
                    command: EggplantCommand::FunctionType(function_type),
                    source_file: source_file.clone(),
                    source_line: Some(span.line),
                });
            }
            Command::Relation { span, name, inputs } => {
                let relation_type =
                    relation_types
                        .get(name)
                        .cloned()
                        .unwrap_or_else(|| RelationType {
                            name: normalize_identifier(name),
                            fields: inputs
                                .iter()
                                .enumerate()
                                .map(|(i, field_type)| DslField {
                                    name: format!("arg{}", i),
                                    field_type: normalize_identifier(field_type),
                                })
                                .collect(),
                        });

                eggplant_commands.push(EggplantCommandWithSource {
                    command: EggplantCommand::RelationType(relation_type),
                    source_file: source_file.clone(),
                    source_line: Some(span.line),
                });
            }
            Command::Rule {
                name,
                ruleset,
                rule,
            } => {
                let unique_name = if name == "default" {
                    format!("rule_{}", rule.span.line)
                } else {
                    normalize_identifier(name)
                };

                if let Some((pattern_vars, pattern_query, rule_context)) = analyze_rule_pattern(
                    &unique_name,
                    rule,
                    &dsl_types,
                    &sort_schemas,
                    &relation_types,
                    &function_types,
                    &global_bindings,
                ) {
                    if let Some(action) = generate_rule_action_with_context(
                        &rule.head,
                        &rule_context,
                        &dsl_types,
                        &relation_types,
                    ) {
                        eggplant_commands.push(EggplantCommandWithSource {
                            command: EggplantCommand::PatternVars(pattern_vars),
                            source_file: source_file.clone(),
                            source_line: Some(rule.span.line),
                        });

                        let eggplant_rule = EggplantRule {
                            name: unique_name.clone(),
                            pattern_query,
                            action,
                            ruleset: normalize_ruleset_name(ruleset),
                            src_expr: command.clone(),
                        };

                        eggplant_commands.push(EggplantCommandWithSource {
                            command: EggplantCommand::Rule(eggplant_rule),
                            source_file: source_file.clone(),
                            source_line: Some(rule.span.line),
                        });
                        continue;
                    }
                }

                eggplant_commands.push(EggplantCommandWithSource {
                    command: EggplantCommand::RawEgglogCommand {
                        command: rule_to_egglog_command_string(rule, ruleset, name),
                    },
                    source_file: source_file.clone(),
                    source_line: Some(rule.span.line),
                });
            }
            Command::Rewrite(ruleset, rewrite, _, rule_name) => {
                // Use provided name or generate one based on line number
                let unique_name = rule_name
                    .clone()
                    .unwrap_or_else(|| format!("rule_{}_{}", rewrite.span.line, rewrite.span.col));

                if matches!(rewrite.lhs, Expr::Var(_, _)) && !rewrite.conditions.is_empty() {
                    let synthetic_rule = Rule {
                        span: rewrite.span.clone(),
                        head: vec![Action::Union(
                            rewrite.span.clone(),
                            rewrite.lhs.clone(),
                            rewrite.rhs.clone(),
                        )],
                        body: rewrite.conditions.clone(),
                    };

                    if let Some((pattern_vars, pattern_query, rule_context)) = analyze_rule_pattern(
                        &unique_name,
                        &synthetic_rule,
                        &dsl_types,
                        &sort_schemas,
                        &relation_types,
                        &function_types,
                        &global_bindings,
                    ) {
                        if let Some(action) = generate_rule_action_with_context(
                            &synthetic_rule.head,
                            &rule_context,
                            &dsl_types,
                            &relation_types,
                        ) {
                            eggplant_commands.push(EggplantCommandWithSource {
                                command: EggplantCommand::PatternVars(pattern_vars),
                                source_file: source_file.clone(),
                                source_line: Some(rewrite.span.line),
                            });

                            eggplant_commands.push(EggplantCommandWithSource {
                                command: EggplantCommand::Rule(EggplantRule {
                                    name: unique_name.clone(),
                                    pattern_query,
                                    action,
                                    ruleset: normalize_ruleset_name(ruleset),
                                    src_expr: command.clone(),
                                }),
                                source_file: source_file.clone(),
                                source_line: Some(rewrite.span.line),
                            });
                            continue;
                        }
                    }
                }

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
                    ruleset: normalize_ruleset_name(ruleset),
                    src_expr: command.clone(),
                };

                eggplant_commands.push(EggplantCommandWithSource {
                    command: EggplantCommand::Rule(rule),
                    source_file: source_file.clone(),
                    source_line: Some(rewrite.span.line),
                });
            }
            Command::BiRewrite(_, rewrite) => {
                eggplant_commands.push(EggplantCommandWithSource {
                    command: EggplantCommand::RawEgglogCommand {
                        command: command.to_string(),
                    },
                    source_file: source_file.clone(),
                    source_line: Some(rewrite.span.line),
                });
            }
            Command::Check(span, facts) => {
                if facts.iter().all(|fact| matches!(fact, Fact::Op(_, _, _))) {
                    for fact in facts {
                        let Fact::Op(_, e1, e2) = fact else {
                            unreachable!();
                        };
                        eggplant_commands.push(EggplantCommandWithSource {
                            command: EggplantCommand::Assert {
                                expr: e1.clone(),
                                expected: e2.clone(),
                            },
                            source_file: source_file.clone(),
                            source_line: Some(span.line),
                        });
                    }
                } else {
                    eggplant_commands.push(EggplantCommandWithSource {
                        command: EggplantCommand::RawEgglogCommand {
                            command: command.to_string(),
                        },
                        source_file: source_file.clone(),
                        source_line: Some(span.line),
                    });
                }
            }
            Command::Constructor { .. } => {
                // Constructor commands are already handled above
                // Skip duplicate processing
            }
            Command::Action(action) => match action {
                Action::Let(span, var, expr) => {
                    eggplant_commands.push(EggplantCommandWithSource {
                        command: EggplantCommand::Let {
                            var: normalize_identifier(var),
                            expr: expr.clone(),
                        },
                        source_file: source_file.clone(),
                        source_line: Some(span.line),
                    });
                }
                Action::Expr(span, Expr::Call(_, op, args)) if op == "print-size" => {
                    let target = args.first().and_then(|arg| match arg {
                        Expr::Var(_, name) => Some(normalize_identifier(name)),
                        _ => None,
                    });
                    eggplant_commands.push(EggplantCommandWithSource {
                        command: EggplantCommand::PrintSize { target },
                        source_file: source_file.clone(),
                        source_line: Some(span.line),
                    });
                }
                Action::Expr(span, Expr::Call(_, relation, args))
                    if relation_types.contains_key(relation) =>
                {
                    eggplant_commands.push(EggplantCommandWithSource {
                        command: EggplantCommand::RelationInsert {
                            relation: normalize_identifier(relation),
                            args: args.clone(),
                        },
                        source_file: source_file.clone(),
                        source_line: Some(span.line),
                    });
                }
                _ => {
                    eggplant_commands.push(EggplantCommandWithSource {
                        command: EggplantCommand::RawEgglogCommand {
                            command: command.to_string(),
                        },
                        source_file: source_file.clone(),
                        source_line: Some(action.span().line),
                    });
                }
            },
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
            Command::AddRuleset(span, name) => {
                eggplant_commands.push(EggplantCommandWithSource {
                    command: EggplantCommand::Ruleset(normalize_ruleset_name(name)),
                    source_file: source_file.clone(),
                    source_line: Some(span.line),
                });
            }
            Command::Run {
                span,
                ruleset,
                limit,
                until,
            } => {
                eggplant_commands.push(EggplantCommandWithSource {
                    command: match until {
                        Some(until) => EggplantCommand::RunUntil {
                            ruleset: normalize_ruleset_name(
                                ruleset.as_deref().unwrap_or("default"),
                            ),
                            limit: *limit,
                            until: until.clone(),
                        },
                        None => EggplantCommand::RunRuleset(
                            normalize_ruleset_name(ruleset.as_deref().unwrap_or("default")),
                            match limit {
                                Some(limit) => format!("Times({limit})"),
                                None => "Sat".to_string(),
                            },
                        ),
                    },
                    source_file: source_file.clone(),
                    source_line: Some(span.line),
                });
            }
            Command::RunSchedule { span, schedules } => {
                eggplant_commands.push(EggplantCommandWithSource {
                    command: EggplantCommand::RunSchedule {
                        schedules: schedules.clone(),
                    },
                    source_file: source_file.clone(),
                    source_line: Some(span.line),
                });
            }
            Command::Extract {
                span,
                expr,
                variants,
            } => {
                eggplant_commands.push(EggplantCommandWithSource {
                    command: EggplantCommand::Extract {
                        expr: expr.clone(),
                        variants: *variants,
                    },
                    source_file: source_file.clone(),
                    source_line: Some(span.line),
                });
            }
            Command::PrintFunction(span, name, size, file, mode) => {
                eggplant_commands.push(EggplantCommandWithSource {
                    command: EggplantCommand::PrintFunction {
                        name: normalize_identifier(name),
                        size: *size,
                        file: file.clone(),
                        mode: mode.clone(),
                    },
                    source_file: source_file.clone(),
                    source_line: Some(span.line),
                });
            }
            Command::Include(span, file) => {
                eggplant_commands.push(EggplantCommandWithSource {
                    command: EggplantCommand::Include {
                        file: resolve_include_path(file, source_file.as_deref()),
                    },
                    source_file: source_file.clone(),
                    source_line: Some(span.line),
                });
            }
            Command::Fail(span, _inner) => {
                eggplant_commands.push(EggplantCommandWithSource {
                    command: EggplantCommand::RawEgglogCommand {
                        command: command.to_string(),
                    },
                    source_file: source_file.clone(),
                    source_line: Some(span.line),
                });
            }
            _ => {
                // Skip unsupported commands for now
            }
        }
    }

    // Ensure the default ruleset always exists for unlabeled rules.
    eggplant_commands.push(EggplantCommandWithSource {
        command: EggplantCommand::Ruleset(default_ruleset_name()),
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

fn default_ruleset_name() -> String {
    "default_ruleset".to_string()
}

fn normalize_ruleset_name(name: &str) -> String {
    if name == "default" {
        default_ruleset_name()
    } else {
        normalize_identifier(name)
    }
}

fn same_normalized_identifier(left: &str, right: &str) -> bool {
    normalize_identifier(left) == normalize_identifier(right)
}

fn schedule_has_until(schedule: &Schedule) -> bool {
    match schedule {
        Schedule::Run { until, .. } => until.is_some(),
        Schedule::Named(_) => false,
        Schedule::Seq(items) | Schedule::Saturate(items) => items.iter().any(schedule_has_until),
        Schedule::Repeat(_, inner) => schedule_has_until(inner),
    }
}

fn schedule_items_to_rust_expr(schedules: &[Schedule]) -> String {
    match schedules {
        [] => "RunSchedule::builder().build()".to_string(),
        [schedule] => schedule_to_rust_expr(schedule),
        _ => {
            let mut expr = "RunSchedule::builder()".to_string();
            for schedule in schedules {
                expr.push_str(&format!(".then({})", schedule_to_rust_expr(schedule)));
            }
            expr.push_str(".build()");
            expr
        }
    }
}

fn schedule_to_rust_expr(schedule: &Schedule) -> String {
    match schedule {
        Schedule::Run {
            ruleset,
            limit,
            until: None,
        } => {
            let ruleset = schedule_ruleset_expr(ruleset.as_deref());
            let run = format!("RunSchedule::builder().run({ruleset}).build()");
            match limit {
                Some(limit) => {
                    format!(
                        "RunSchedule::builder().repeat({limit}, |schedule| schedule.then({run})).build()"
                    )
                }
                None => run,
            }
        }
        Schedule::Run { .. } => {
            unreachable!("run-schedule with :until must use raw egglog bridge")
        }
        Schedule::Named(name) => {
            format!(
                "RunSchedule::builder().run({}).build()",
                normalize_ruleset_name(name)
            )
        }
        Schedule::Seq(items) => schedule_items_to_rust_expr(items),
        Schedule::Saturate(items) => match items.as_slice() {
            [single] => {
                if let Some(ruleset) = direct_schedule_ruleset_expr(single) {
                    format!("RunSchedule::builder().saturate({ruleset}).build()")
                } else {
                    format!(
                        "RunSchedule::builder().saturate_schedule({}).build()",
                        schedule_to_rust_expr(single)
                    )
                }
            }
            _ => format!(
                "RunSchedule::builder().saturate_schedule({}).build()",
                schedule_items_to_rust_expr(items)
            ),
        },
        Schedule::Repeat(times, inner) => {
            format!(
                "RunSchedule::builder().repeat({times}, |schedule| schedule.then({})).build()",
                schedule_to_rust_expr(inner)
            )
        }
    }
}

fn direct_schedule_ruleset_expr(schedule: &Schedule) -> Option<String> {
    match schedule {
        Schedule::Named(name) => Some(normalize_ruleset_name(name)),
        Schedule::Run {
            ruleset,
            limit: None,
            until: None,
        } => Some(schedule_ruleset_expr(ruleset.as_deref())),
        _ => None,
    }
}

fn schedule_ruleset_expr(ruleset: Option<&str>) -> String {
    normalize_ruleset_name(ruleset.unwrap_or("default"))
}

fn expr_type_name(expr: &Expr) -> String {
    match expr {
        Expr::Var(_, name) => normalize_identifier(name),
        Expr::Call(_, name, args) => {
            if args.is_empty() {
                normalize_identifier(name)
            } else {
                format!(
                    "{}<{}>",
                    normalize_identifier(name),
                    args.iter()
                        .map(expr_type_name)
                        .collect::<Vec<_>>()
                        .join("_")
                )
            }
        }
        Expr::Lit(_, lit) => literal_type_name(lit),
    }
}

fn expr_to_egglog_command_string(expr: &Expr) -> String {
    match expr {
        Expr::Lit(_, lit) => lit.to_string(),
        Expr::Var(_, var) => normalize_identifier(var),
        Expr::Call(_, op, children) => {
            let op = normalize_identifier(op);
            if children.is_empty() {
                format!("({op})")
            } else {
                let child_str = children
                    .iter()
                    .map(expr_to_egglog_command_string)
                    .collect::<Vec<_>>()
                    .join(" ");
                format!("({op} {child_str})")
            }
        }
    }
}

fn fact_to_egglog_command_string(fact: &Fact) -> String {
    match fact {
        Fact::Op(span, left, right) => {
            let operator = if let Some(file) = &span.file {
                if file.starts_with("operator:") {
                    file.trim_start_matches("operator:")
                } else {
                    "="
                }
            } else {
                "="
            };
            format!(
                "({} {} {})",
                operator,
                expr_to_egglog_command_string(left),
                expr_to_egglog_command_string(right)
            )
        }
        Fact::Fact(expr) => expr_to_egglog_command_string(expr),
    }
}

fn action_to_egglog_command_string(action: &Action) -> String {
    match action {
        Action::Let(_, var, expr) => format!(
            "(let {} {})",
            normalize_identifier(var),
            expr_to_egglog_command_string(expr)
        ),
        Action::Set(_, head, args, value) => {
            let head = normalize_identifier(head);
            let args = args
                .iter()
                .map(expr_to_egglog_command_string)
                .collect::<Vec<_>>()
                .join(" ");
            if args.is_empty() {
                format!("(set ({head}) {})", expr_to_egglog_command_string(value))
            } else {
                format!(
                    "(set ({head} {args}) {})",
                    expr_to_egglog_command_string(value)
                )
            }
        }
        Action::Union(_, left, right) => format!(
            "(union {} {})",
            expr_to_egglog_command_string(left),
            expr_to_egglog_command_string(right)
        ),
        Action::Delete(_, expr) => {
            format!("(delete {})", expr_to_egglog_command_string(expr))
        }
        Action::Expr(_, expr) => expr_to_egglog_command_string(expr),
    }
}

fn rule_to_egglog_command_string(rule: &Rule, ruleset: &str, name: &str) -> String {
    let body = rule
        .body
        .iter()
        .map(fact_to_egglog_command_string)
        .collect::<Vec<_>>()
        .join(" ");
    let head = rule
        .head
        .iter()
        .map(action_to_egglog_command_string)
        .collect::<Vec<_>>()
        .join(" ");
    let ruleset = if ruleset.is_empty() {
        String::new()
    } else {
        format!(" :ruleset {}", normalize_ruleset_name(ruleset))
    };
    let name = if name.is_empty() || name == "default" {
        String::new()
    } else {
        format!(" :name {:?}", name)
    };
    format!("(rule ({body}) ({head}){ruleset}{name})")
}

fn resolve_include_path(file: &str, source_file: Option<&str>) -> String {
    let include_path = std::path::Path::new(file);
    if include_path.is_absolute() {
        return include_path.display().to_string();
    }

    if let Some(source_file) = source_file {
        let source_path = std::path::Path::new(source_file);
        if let Some(found) = source_path
            .ancestors()
            .filter(|ancestor| ancestor.is_dir())
            .map(|ancestor| ancestor.join(include_path))
            .find(|candidate| candidate.exists())
        {
            return found.display().to_string();
        }
    }

    include_path.display().to_string()
}

#[derive(Debug, Clone)]
enum RuleVarBinding {
    Complex {
        pat_name: String,
    },
    BaseVar {
        pat_name: String,
        ty: String,
    },
    BaseField {
        owner_pat_name: String,
        field_name: String,
        ty: String,
    },
}

#[derive(Debug, Clone)]
struct SortSchema {
    name: String,
    args: Vec<String>,
}

#[derive(Debug, Clone)]
struct RuleActionContext {
    bindings: HashMap<String, RuleVarBinding>,
    global_bindings: HashMap<String, Expr>,
    function_types: HashMap<String, FunctionType>,
}

#[derive(Clone)]
struct RulePatternState<'a> {
    pattern_vars: Vec<PatternVariable>,
    pattern_query_lines: Vec<String>,
    assert_exprs: Vec<String>,
    bindings: HashMap<String, RuleVarBinding>,
    relation_fact_counts: HashMap<String, usize>,
    temp_counter: usize,
    dsl_types: &'a HashMap<String, DslType>,
    sort_schemas: &'a HashMap<String, SortSchema>,
    relation_types: &'a HashMap<String, RelationType>,
    function_types: &'a HashMap<String, FunctionType>,
    global_bindings: &'a HashMap<String, Expr>,
}

fn analyze_rule_pattern(
    rule_name: &str,
    rule: &Rule,
    dsl_types: &HashMap<String, DslType>,
    sort_schemas: &HashMap<String, SortSchema>,
    relation_types: &HashMap<String, RelationType>,
    function_types: &HashMap<String, FunctionType>,
    global_bindings: &HashMap<String, Expr>,
) -> Option<(PatternVars, String, RuleActionContext)> {
    let mut state = RulePatternState {
        pattern_vars: Vec::new(),
        pattern_query_lines: Vec::new(),
        assert_exprs: Vec::new(),
        bindings: HashMap::new(),
        relation_fact_counts: HashMap::new(),
        temp_counter: 0,
        dsl_types,
        sort_schemas,
        relation_types,
        function_types,
        global_bindings,
    };

    let mut pending_facts: Vec<&Fact> = rule.body.iter().collect();
    while !pending_facts.is_empty() {
        let mut next_pending = Vec::new();
        let mut made_progress = false;

        for fact in pending_facts {
            let mut attempt = state.clone();
            if lower_rule_fact(fact, &mut attempt).is_some() {
                state = attempt;
                made_progress = true;
            } else {
                next_pending.push(fact);
            }
        }

        if !made_progress {
            return None;
        }

        pending_facts = next_pending;
    }

    if state.pattern_query_lines.is_empty() && !rule.body.is_empty() {
        return None;
    }

    let mut pattern_query = state.pattern_query_lines.join("\n");
    pattern_query.push('\n');
    pattern_query.push_str(&format!("{}Pat::new(", rule_name));
    pattern_query.push_str(
        &state
            .pattern_vars
            .iter()
            .map(|var| var.name.clone())
            .collect::<Vec<_>>()
            .join(", "),
    );
    pattern_query.push(')');

    for assert_expr in state.assert_exprs {
        pattern_query.push_str(&format!("\n.assert({assert_expr})"));
    }

    Some((
        PatternVars {
            name: format!("{}Pat", rule_name),
            variables: state.pattern_vars,
        },
        pattern_query,
        RuleActionContext {
            bindings: state.bindings,
            global_bindings: global_bindings.clone(),
            function_types: function_types.clone(),
        },
    ))
}

fn lower_rule_fact(fact: &Fact, state: &mut RulePatternState<'_>) -> Option<()> {
    match fact {
        Fact::Fact(expr) => match expr {
            Expr::Call(_, name, args) if state.relation_types.contains_key(name) => {
                lower_relation_fact(expr, state)
            }
            Expr::Call(_, name, args) if state.function_types.contains_key(name) => {
                lower_function_fact(name, args, None, state).map(|_| ())
            }
            Expr::Call(_, name, args)
                if find_variant_output_type(name, state.dsl_types).is_some() =>
            {
                lower_constructor_fact(name, args, None, state).map(|_| ())
            }
            _ => {
                let constraint = render_rule_fact_constraint_expr(expr, state)?;
                state.assert_exprs.push(constraint);
                Some(())
            }
        },
        Fact::Op(span, lhs, rhs) => {
            if operator_from_span(span) == "=" {
                if let Expr::Var(_, var_name) = lhs {
                    return lower_rule_binding_fact(var_name, rhs, state);
                }
                if let Expr::Var(_, var_name) = rhs {
                    return lower_rule_binding_fact(var_name, lhs, state);
                }
            }

            let operator = operator_from_span(span);
            let left = lower_constraint_operand(lhs, state)?;
            let right = lower_constraint_operand(rhs, state)?;
            let method = operator_method_name(operator)?;
            state
                .assert_exprs
                .push(format!("{left}.{method}(&{right})"));
            Some(())
        }
    }
}

fn lower_rule_binding_fact(
    var_name: &str,
    expr: &Expr,
    state: &mut RulePatternState<'_>,
) -> Option<()> {
    let normalized_var_name = normalize_identifier(var_name);
    let existing_binding = state.bindings.get(&normalized_var_name).cloned();

    match expr {
        Expr::Call(_, func_name, args) if state.relation_types.contains_key(func_name) => {
            if let Some(existing_binding) = existing_binding {
                let binding = lower_relation_expr(expr, None, state)?;
                let existing_handle = binding_handle_expr(&existing_binding);
                let binding_handle = binding_handle_expr(&binding);
                if existing_handle != binding_handle {
                    state
                        .assert_exprs
                        .push(format!("{existing_handle}.eq(&{binding_handle})"));
                }
                Some(())
            } else {
                None
            }
        }
        Expr::Call(_, func_name, args) if state.function_types.contains_key(func_name) => {
            if let Some(existing_binding) = existing_binding {
                let binding = lower_function_fact(func_name, args, None, state)?;
                let existing_handle = binding_handle_expr(&existing_binding);
                let binding_handle = binding_handle_expr(&binding);
                if existing_handle != binding_handle {
                    state
                        .assert_exprs
                        .push(format!("{existing_handle}.eq(&{binding_handle})"));
                }
                Some(())
            } else {
                lower_function_fact(func_name, args, Some(&normalized_var_name), state).map(|_| ())
            }
        }
        Expr::Call(_, func_name, args)
            if find_variant_output_type(func_name, state.dsl_types).is_some() =>
        {
            if let Some(existing_binding) = existing_binding {
                let binding = lower_constructor_fact(func_name, args, None, state)?;
                let existing_handle = binding_handle_expr(&existing_binding);
                let binding_handle = binding_handle_expr(&binding);
                if existing_handle != binding_handle {
                    state
                        .assert_exprs
                        .push(format!("{existing_handle}.eq(&{binding_handle})"));
                }
                Some(())
            } else {
                lower_constructor_fact(func_name, args, Some(&normalized_var_name), state)
                    .map(|_| ())
            }
        }
        Expr::Var(_, other_var) => {
            let other_normalized_var_name = normalize_identifier(other_var);
            let current_binding = existing_binding;
            let other_binding = state.bindings.get(&other_normalized_var_name).cloned();

            match (current_binding, other_binding) {
                (Some(current_binding), Some(other_binding)) => {
                    let current_handle = binding_handle_expr(&current_binding);
                    let other_handle = binding_handle_expr(&other_binding);
                    if current_handle != other_handle {
                        state
                            .assert_exprs
                            .push(format!("{current_handle}.eq(&{other_handle})"));
                    }
                    Some(())
                }
                (Some(current_binding), None) => {
                    state
                        .bindings
                        .insert(other_normalized_var_name, current_binding);
                    Some(())
                }
                (None, Some(other_binding)) => {
                    state.bindings.insert(normalized_var_name, other_binding);
                    Some(())
                }
                (None, None) => state
                    .global_bindings
                    .get(&other_normalized_var_name)
                    .cloned()
                    .and_then(|global_expr| lower_rule_binding_fact(var_name, &global_expr, state)),
            }
        }
        Expr::Lit(_, lit) => {
            let binding_handle = if let Some(binding) = existing_binding {
                binding_handle_expr(&binding)
            } else {
                let binding_name = ensure_base_binding(
                    &normalized_var_name,
                    &literal_type_name(lit),
                    true,
                    state,
                )?;
                format!("{binding_name}.handle()")
            };
            state
                .assert_exprs
                .push(format!("{binding_handle}.eq(&{})", literal_to_rust(lit)));
            Some(())
        }
        Expr::Call(_, op, _) if is_basic_operation(op) => {
            let binding_handle = if let Some(binding) = existing_binding {
                binding_handle_expr(&binding)
            } else {
                let inferred_type =
                    infer_rule_expr_type(expr, state).unwrap_or_else(|| "i64".to_string());
                let binding_name =
                    ensure_base_binding(&normalized_var_name, &inferred_type, true, state)?;
                format!("{binding_name}.handle()")
            };
            let inferred_type = infer_rule_expr_type(expr, state);
            let rendered =
                render_rule_base_handle_expr_with_hint(expr, inferred_type.as_deref(), state)?;
            state
                .assert_exprs
                .push(format!("{binding_handle}.eq(&{rendered})"));
            Some(())
        }
        Expr::Call(_, _, _) => {
            let binding_handle = if let Some(binding) = existing_binding {
                binding_handle_expr(&binding)
            } else {
                let inferred_type =
                    infer_rule_expr_type(expr, state).unwrap_or_else(|| "i64".to_string());
                let binding_name =
                    ensure_base_binding(&normalized_var_name, &inferred_type, true, state)?;
                format!("{binding_name}.handle()")
            };
            let inferred_type = infer_rule_expr_type(expr, state);
            let rendered =
                render_rule_base_handle_expr_with_hint(expr, inferred_type.as_deref(), state)?;
            state
                .assert_exprs
                .push(format!("{binding_handle}.eq(&{rendered})"));
            Some(())
        }
    }
}

fn lower_relation_fact(expr: &Expr, state: &mut RulePatternState<'_>) -> Option<()> {
    lower_relation_expr(expr, None, state).map(|_| ())
}

fn lower_relation_expr(
    expr: &Expr,
    preferred_name: Option<&str>,
    state: &mut RulePatternState<'_>,
) -> Option<RuleVarBinding> {
    let Expr::Call(_, relation_name, args) = expr else {
        return None;
    };
    let relation_type = state.relation_types.get(relation_name)?.clone();
    let relation_ident = normalize_identifier(relation_name);
    let row_name = if let Some(preferred_name) = preferred_name {
        let preferred_name = normalize_identifier(preferred_name);
        ensure_rule_pattern_var(&mut state.pattern_vars, &preferred_name, &relation_ident);
        preferred_name
    } else {
        let relation_fact_index = state
            .relation_fact_counts
            .entry(relation_ident.clone())
            .and_modify(|count| *count += 1)
            .or_insert(1);
        format!(
            "{}_fact_{}",
            relation_ident.to_snake_case(),
            relation_fact_index
        )
    };

    let mut explicit_query_args = Vec::new();
    for (field, arg) in relation_type.fields.iter().zip(args.iter()) {
        if is_rule_base_like_type(&field.field_type, state.dsl_types) {
            let current_handle = format!("{row_name}.handle_{}()", field.name);
            match arg {
                Expr::Var(_, var_name) => {
                    let normalized_var_name = normalize_identifier(var_name);
                    if let Some(binding) = state.bindings.get(&normalized_var_name) {
                        state.assert_exprs.push(format!(
                            "{current_handle}.eq(&{})",
                            binding_handle_expr(binding)
                        ));
                    } else if let Some(global_expr) =
                        state.global_bindings.get(&normalized_var_name).cloned()
                    {
                        let rendered = render_rule_base_handle_expr_with_hint(
                            &global_expr,
                            Some(&field.field_type),
                            state,
                        )?;
                        state
                            .assert_exprs
                            .push(format!("{current_handle}.eq(&{rendered})"));
                    } else {
                        state.bindings.insert(
                            normalized_var_name,
                            RuleVarBinding::BaseField {
                                owner_pat_name: row_name.clone(),
                                field_name: field.name.clone(),
                                ty: field.field_type.clone(),
                            },
                        );
                    }
                }
                _ => {
                    let rendered = render_rule_base_handle_expr_with_hint(
                        arg,
                        Some(&field.field_type),
                        state,
                    )?;
                    state
                        .assert_exprs
                        .push(format!("{current_handle}.eq(&{rendered})"));
                }
            }
        } else {
            let complex_pat_name =
                ensure_complex_binding_from_expr(arg, &field.field_type, None, true, state)?;
            explicit_query_args.push(format!("&{complex_pat_name}"));
        }
    }

    if relation_type
        .fields
        .iter()
        .any(|field| !is_rule_base_like_type(&field.field_type, state.dsl_types))
    {
        state.pattern_query_lines.push(format!(
            "let {row_name} = {relation_ident}::query({});",
            explicit_query_args.join(", ")
        ));
    } else {
        state
            .pattern_query_lines
            .push(format!("let {row_name} = {relation_ident}::query();"));
    }

    Some(RuleVarBinding::Complex { pat_name: row_name })
}

fn ensure_complex_binding_from_expr(
    expr: &Expr,
    field_type: &str,
    preferred_name: Option<&str>,
    include_in_pattern_vars: bool,
    state: &mut RulePatternState<'_>,
) -> Option<String> {
    match expr {
        Expr::Var(_, var_name) => {
            let normalized_var_name = normalize_identifier(var_name);
            if let Some(binding) = state.bindings.get(&normalized_var_name) {
                match binding {
                    RuleVarBinding::Complex { pat_name } => Some(pat_name.clone()),
                    RuleVarBinding::BaseVar { .. } | RuleVarBinding::BaseField { .. } => None,
                }
            } else if let Some(global_expr) =
                state.global_bindings.get(&normalized_var_name).cloned()
            {
                ensure_complex_binding_from_expr(
                    &global_expr,
                    field_type,
                    preferred_name,
                    include_in_pattern_vars,
                    state,
                )
            } else {
                let normalized_type = normalize_identifier(field_type);
                state.pattern_query_lines.push(format!(
                    "let {normalized_var_name} = {normalized_type}::query_leaf();"
                ));
                if include_in_pattern_vars {
                    ensure_rule_pattern_var(
                        &mut state.pattern_vars,
                        &normalized_var_name,
                        &normalized_type,
                    );
                }
                state.bindings.insert(
                    normalized_var_name.clone(),
                    RuleVarBinding::Complex {
                        pat_name: normalized_var_name.clone(),
                    },
                );
                Some(normalized_var_name)
            }
        }
        Expr::Call(_, func_name, args) if state.function_types.contains_key(func_name) => {
            lower_function_fact(func_name, args, preferred_name, state).and_then(|binding| {
                match binding {
                    RuleVarBinding::Complex { pat_name } => {
                        if include_in_pattern_vars && preferred_name.is_none() {
                            let output_type =
                                state.function_types.get(func_name)?.output_type.clone();
                            ensure_rule_pattern_var(
                                &mut state.pattern_vars,
                                &pat_name,
                                &output_type,
                            );
                        }
                        Some(pat_name)
                    }
                    RuleVarBinding::BaseVar { .. } | RuleVarBinding::BaseField { .. } => None,
                }
            })
        }
        Expr::Call(_, func_name, args)
            if find_variant_output_type(func_name, state.dsl_types).is_some() =>
        {
            lower_constructor_fact(func_name, args, preferred_name, state).map(|binding| {
                match binding {
                    RuleVarBinding::Complex { pat_name } => pat_name,
                    RuleVarBinding::BaseVar { .. } | RuleVarBinding::BaseField { .. } => {
                        unreachable!()
                    }
                }
            })
        }
        Expr::Call(_, func_name, _) => {
            let pat_name = preferred_name
                .map(normalize_identifier)
                .unwrap_or_else(|| state.fresh_temp_name(func_name));
            let normalized_type = normalize_identifier(field_type);
            state
                .pattern_query_lines
                .push(format!("let {pat_name} = {normalized_type}::query_leaf();"));
            if include_in_pattern_vars {
                ensure_rule_pattern_var(&mut state.pattern_vars, &pat_name, &normalized_type);
            }
            let rendered = render_rule_handle_expr_with_expected_type(expr, field_type, state)?;
            state
                .assert_exprs
                .push(format!("{pat_name}.handle().eq(&{rendered})"));
            if let Some(preferred_name) = preferred_name {
                state.bindings.insert(
                    normalize_identifier(preferred_name),
                    RuleVarBinding::Complex {
                        pat_name: pat_name.clone(),
                    },
                );
            }
            Some(pat_name)
        }
        _ => None,
    }
}

fn ensure_rule_pattern_var(pattern_vars: &mut Vec<PatternVariable>, name: &str, var_type: &str) {
    if !pattern_vars.iter().any(|var| var.name == name) {
        pattern_vars.push(PatternVariable {
            name: name.to_string(),
            var_type: var_type.to_string(),
        });
    }
}

fn operator_from_span(span: &Span) -> &str {
    span.file
        .as_deref()
        .and_then(|file| file.strip_prefix("operator:"))
        .unwrap_or("=")
}

fn operator_method_name(operator: &str) -> Option<&'static str> {
    match operator {
        "=" => Some("eq"),
        "!=" => Some("ne"),
        "<" => Some("lt"),
        "<=" => Some("le"),
        ">" => Some("gt"),
        ">=" => Some("ge"),
        _ => None,
    }
}

fn lower_constraint_operand(expr: &Expr, state: &mut RulePatternState<'_>) -> Option<String> {
    match expr {
        Expr::Var(_, var_name) => {
            let normalized_var_name = normalize_identifier(var_name);
            if let Some(binding) = state.bindings.get(&normalized_var_name) {
                Some(binding_handle_expr(binding))
            } else if let Some(global_expr) =
                state.global_bindings.get(&normalized_var_name).cloned()
            {
                lower_constraint_operand(&global_expr, state)
            } else {
                None
            }
        }
        Expr::Lit(_, lit) => Some(literal_to_rust(lit)),
        Expr::Call(_, relation_name, _) if state.relation_types.contains_key(relation_name) => {
            let binding = lower_relation_expr(expr, None, state)?;
            Some(binding_handle_expr(&binding))
        }
        Expr::Call(_, _, _) => render_rule_base_handle_expr(expr, state),
    }
}

fn binding_handle_expr(binding: &RuleVarBinding) -> String {
    match binding {
        RuleVarBinding::Complex { pat_name } => format!("{pat_name}.handle()"),
        RuleVarBinding::BaseVar { pat_name, .. } => format!("{pat_name}.handle()"),
        RuleVarBinding::BaseField {
            owner_pat_name,
            field_name,
            ..
        } => format!("{owner_pat_name}.handle_{field_name}()"),
    }
}

fn binding_value_expr(binding: &RuleVarBinding) -> String {
    match binding {
        RuleVarBinding::Complex { pat_name } => format!("pat.{pat_name}"),
        RuleVarBinding::BaseVar { pat_name, .. } => format!("ctx.devalue(pat.{pat_name})"),
        RuleVarBinding::BaseField {
            owner_pat_name,
            field_name,
            ..
        } => format!("ctx.devalue(pat.{owner_pat_name}.{field_name})"),
    }
}

fn generate_rule_action_with_context(
    actions: &[Action],
    context: &RuleActionContext,
    dsl_types: &HashMap<String, DslType>,
    relation_types: &HashMap<String, RelationType>,
) -> Option<String> {
    let mut lines = Vec::new();
    let mut local_vars = HashMap::new();

    for action in actions {
        match action {
            Action::Let(_, var, expr) => {
                let local_name = normalize_identifier(var);
                let value = generate_rule_action_expr(
                    expr,
                    context,
                    dsl_types,
                    relation_types,
                    &local_vars,
                )?;
                lines.push(format!("let {local_name} = {value};"));
                local_vars.insert(local_name.clone(), local_name);
            }
            Action::Set(_, head, args, value) => {
                if relation_types.contains_key(head) {
                    return None;
                }
                let mut arg_exprs = args
                    .iter()
                    .map(|arg| {
                        generate_rule_action_expr(
                            arg,
                            context,
                            dsl_types,
                            relation_types,
                            &local_vars,
                        )
                    })
                    .collect::<Option<Vec<_>>>()?;
                arg_exprs.push(generate_rule_action_expr(
                    value,
                    context,
                    dsl_types,
                    relation_types,
                    &local_vars,
                )?);
                lines.push(format!(
                    "ctx.set_{}({});",
                    normalize_identifier(head).to_snake_case(),
                    arg_exprs.join(", ")
                ));
            }
            Action::Union(_, lhs, rhs) => {
                let left = generate_rule_action_expr(
                    lhs,
                    context,
                    dsl_types,
                    relation_types,
                    &local_vars,
                )?;
                let right = generate_rule_action_expr(
                    rhs,
                    context,
                    dsl_types,
                    relation_types,
                    &local_vars,
                )?;
                lines.push(format!("ctx.union({left}, {right});"));
            }
            Action::Delete(_, Expr::Call(_, relation, args))
                if relation_types.contains_key(relation) =>
            {
                let arg_exprs = args
                    .iter()
                    .map(|arg| {
                        generate_rule_action_expr(
                            arg,
                            context,
                            dsl_types,
                            relation_types,
                            &local_vars,
                        )
                    })
                    .collect::<Option<Vec<_>>>()?;
                lines.push(format!(
                    "ctx.remove_{}({});",
                    normalize_identifier(relation).to_snake_case(),
                    arg_exprs.join(", ")
                ));
            }
            Action::Delete(_, Expr::Call(_, function, args))
                if context.function_types.contains_key(function) =>
            {
                let arg_exprs = args
                    .iter()
                    .map(|arg| {
                        generate_rule_action_expr(
                            arg,
                            context,
                            dsl_types,
                            relation_types,
                            &local_vars,
                        )
                    })
                    .collect::<Option<Vec<_>>>()?;
                let key_exprs = arg_exprs
                    .iter()
                    .map(|arg| format!("{arg}.to_value(ctx).erase()"))
                    .collect::<Vec<_>>();
                lines.push(format!("let key = [{}];", key_exprs.join(", ")));
                lines.push(format!(
                    "ctx.remove(\"{}\", &key);",
                    normalize_identifier(function)
                ));
            }
            Action::Delete(_, Expr::Call(_, constructor, args))
                if find_variant_output_type(constructor, dsl_types).is_some() =>
            {
                let arg_exprs = args
                    .iter()
                    .map(|arg| {
                        generate_rule_action_expr(
                            arg,
                            context,
                            dsl_types,
                            relation_types,
                            &local_vars,
                        )
                    })
                    .collect::<Option<Vec<_>>>()?;
                lines.push(format!(
                    "ctx.remove_{}({});",
                    normalize_identifier(constructor).to_snake_case(),
                    arg_exprs.join(", ")
                ));
            }
            Action::Expr(_, Expr::Call(_, builtin, args)) if builtin == "subsume" => {
                if args.len() != 1 {
                    return None;
                }
                let Expr::Call(_, constructor, constructor_args) = &args[0] else {
                    return None;
                };
                if find_variant_output_type(constructor, dsl_types).is_none() {
                    return None;
                }
                let arg_exprs = constructor_args
                    .iter()
                    .map(|arg| {
                        generate_rule_action_expr(
                            arg,
                            context,
                            dsl_types,
                            relation_types,
                            &local_vars,
                        )
                    })
                    .collect::<Option<Vec<_>>>()?;
                lines.push(format!(
                    "ctx.subsume_{}({});",
                    normalize_identifier(constructor).to_snake_case(),
                    arg_exprs.join(", ")
                ));
            }
            Action::Expr(_, Expr::Call(_, relation, args))
                if relation_types.contains_key(relation) =>
            {
                let arg_exprs = args
                    .iter()
                    .map(|arg| {
                        generate_rule_action_expr(
                            arg,
                            context,
                            dsl_types,
                            relation_types,
                            &local_vars,
                        )
                    })
                    .collect::<Option<Vec<_>>>()?;
                lines.push(format!(
                    "ctx.insert_{}({});",
                    normalize_identifier(relation).to_snake_case(),
                    arg_exprs.join(", ")
                ));
            }
            Action::Expr(_, Expr::Call(_, constructor, args))
                if find_variant_output_type(constructor, dsl_types).is_some() =>
            {
                let arg_exprs = args
                    .iter()
                    .map(|arg| {
                        generate_rule_action_expr(
                            arg,
                            context,
                            dsl_types,
                            relation_types,
                            &local_vars,
                        )
                    })
                    .collect::<Option<Vec<_>>>()?;
                lines.push(format!(
                    "let _ = ctx.insert_{}({});",
                    normalize_identifier(constructor).to_snake_case(),
                    arg_exprs.join(", ")
                ));
            }
            Action::Expr(_, Expr::Call(_, builtin, args)) if builtin == "panic" => {
                if args.len() != 1 {
                    return None;
                }
                let message = generate_rule_action_expr(
                    &args[0],
                    context,
                    dsl_types,
                    relation_types,
                    &local_vars,
                )?;
                lines.push(format!("panic!(\"{{}}\", {message});"));
            }
            Action::Expr(_, expr) => {
                let value = generate_rule_action_expr(
                    expr,
                    context,
                    dsl_types,
                    relation_types,
                    &local_vars,
                )?;
                lines.push(format!("let _ = {value};"));
            }
            _ => return None,
        }
    }

    Some(lines.join("\n"))
}

fn generate_rule_action_expr(
    expr: &Expr,
    context: &RuleActionContext,
    dsl_types: &HashMap<String, DslType>,
    relation_types: &HashMap<String, RelationType>,
    local_vars: &HashMap<String, String>,
) -> Option<String> {
    match expr {
        Expr::Var(_, var_name) => {
            let normalized_var_name = normalize_identifier(var_name);
            if let Some(local_var) = local_vars.get(&normalized_var_name) {
                Some(local_var.clone())
            } else if let Some(global_expr) = context.global_bindings.get(&normalized_var_name) {
                generate_rule_action_expr(
                    global_expr,
                    context,
                    dsl_types,
                    relation_types,
                    local_vars,
                )
            } else {
                context
                    .bindings
                    .get(&normalized_var_name)
                    .map(binding_value_expr)
            }
        }
        Expr::Lit(_, lit) => Some(action_literal_to_rust(lit)),
        Expr::Call(_, func_name, args) => {
            if relation_types.contains_key(func_name) {
                return None;
            }

            if let Some(function_type) = context.function_types.get(func_name) {
                let arg_exprs = args
                    .iter()
                    .map(|arg| {
                        generate_rule_action_expr(
                            arg,
                            context,
                            dsl_types,
                            relation_types,
                            local_vars,
                        )
                    })
                    .collect::<Option<Vec<_>>>()?;
                let call_prefix = if is_basic_type(&function_type.output_type) {
                    "read"
                } else {
                    "insert"
                };
                let func_ident = normalize_identifier(func_name).to_snake_case();
                Some(format!(
                    "ctx.{call_prefix}_{func_ident}({})",
                    arg_exprs.join(", ")
                ))
            } else if is_basic_operation(func_name) {
                let operation_args = args
                    .iter()
                    .map(|arg| {
                        generate_rule_action_expr(
                            arg,
                            context,
                            dsl_types,
                            relation_types,
                            local_vars,
                        )
                    })
                    .collect::<Option<Vec<_>>>()?;
                render_basic_operation_expr(func_name, &operation_args)
            } else if find_variant_output_type(func_name, dsl_types).is_some() {
                let arg_exprs = args
                    .iter()
                    .map(|arg| {
                        generate_rule_action_expr(
                            arg,
                            context,
                            dsl_types,
                            relation_types,
                            local_vars,
                        )
                    })
                    .collect::<Option<Vec<_>>>()?;

                Some(format!(
                    "ctx.insert_{}({})",
                    normalize_identifier(func_name).to_snake_case(),
                    arg_exprs.join(", ")
                ))
            } else {
                let arg_exprs = args
                    .iter()
                    .map(|arg| {
                        generate_rule_action_expr(
                            arg,
                            context,
                            dsl_types,
                            relation_types,
                            local_vars,
                        )
                    })
                    .collect::<Option<Vec<_>>>()?;
                render_rule_runtime_call_expr(func_name, &arg_exprs)
            }
        }
    }
}

fn render_basic_operation_expr(func_name: &str, arg_exprs: &[String]) -> Option<String> {
    match func_name {
        "+" | "-" | "*" | "/" | "%" => {
            let (first, rest) = arg_exprs.split_first()?;
            Some(rest.iter().fold(first.clone(), |acc, arg| {
                format!("({acc} {func_name} {arg})")
            }))
        }
        "<" | ">" | "<=" | ">=" | "==" | "!=" if arg_exprs.len() == 2 => {
            Some(format!("({} {} {})", arg_exprs[0], func_name, arg_exprs[1]))
        }
        _ => None,
    }
}

fn literal_to_rust(lit: &Literal) -> String {
    match lit {
        Literal::Int(i) => i.to_string(),
        Literal::Float(f) => f.0.to_string(),
        Literal::String(s) => format!("\"{}\"", s),
        Literal::Bool(b) => b.to_string(),
        Literal::Unit => "()".to_string(),
    }
}

fn literal_type_name(lit: &Literal) -> String {
    match lit {
        Literal::Int(_) => "i64".to_string(),
        Literal::Float(_) => "f64".to_string(),
        Literal::String(_) => "String".to_string(),
        Literal::Bool(_) => "bool".to_string(),
        Literal::Unit => "()".to_string(),
    }
}

fn typed_literal_to_rust(lit: &Literal) -> String {
    match lit {
        Literal::Int(i) => format!("{i}_i64"),
        Literal::Float(f) => format!("{}_f64", f.0),
        Literal::String(s) => format!("\"{}\".to_owned()", s),
        Literal::Bool(b) => b.to_string(),
        Literal::Unit => "()".to_string(),
    }
}

fn literal_handle_expr(lit: &Literal) -> String {
    format!("(&({})).as_handle()", typed_literal_to_rust(lit))
}

fn action_literal_to_rust(lit: &Literal) -> String {
    match lit {
        Literal::String(_) | Literal::Float(_) => typed_literal_to_rust(lit),
        _ => literal_to_rust(lit),
    }
}

fn rule_base_hint<'a>(hint_type: Option<&'a str>, state: &RulePatternState<'_>) -> Option<&'a str> {
    hint_type.filter(|ty| is_rule_base_like_type(ty, state.dsl_types))
}

fn render_rule_base_handle_expr(expr: &Expr, state: &mut RulePatternState<'_>) -> Option<String> {
    render_rule_base_handle_expr_with_hint(expr, None, state)
}

fn render_rule_base_handle_expr_with_hint(
    expr: &Expr,
    hint_type: Option<&str>,
    state: &mut RulePatternState<'_>,
) -> Option<String> {
    match expr {
        Expr::Var(_, var_name) => {
            let normalized_var_name = normalize_identifier(var_name);
            if let Some(binding) = state.bindings.get(&normalized_var_name) {
                Some(binding_handle_expr(binding))
            } else if let Some(global_expr) =
                state.global_bindings.get(&normalized_var_name).cloned()
            {
                render_rule_base_handle_expr_with_hint(&global_expr, hint_type, state)
            } else {
                let hint_type = rule_base_hint(hint_type, state)?;
                let pat_name = ensure_base_binding(&normalized_var_name, hint_type, true, state)?;
                Some(format!("{pat_name}.handle()"))
            }
        }
        Expr::Lit(_, lit) => Some(literal_handle_expr(lit)),
        Expr::Call(_, func_name, args) if state.function_types.contains_key(func_name) => {
            let binding = lower_function_fact(func_name, args, None, state)?;
            Some(binding_handle_expr(&binding))
        }
        Expr::Call(_, func_name, args)
            if find_variant_output_type(func_name, state.dsl_types).is_some() =>
        {
            let binding = lower_constructor_fact(func_name, args, None, state)?;
            Some(binding_handle_expr(&binding))
        }
        Expr::Call(_, func_name, args) => {
            render_rule_primitive_handle_expr_with_hint(func_name, args, hint_type, state)
        }
    }
}

fn render_rule_handle_expr_with_expected_type(
    expr: &Expr,
    expected_type: &str,
    state: &mut RulePatternState<'_>,
) -> Option<String> {
    match expr {
        Expr::Call(_, func_name, args)
            if !state.relation_types.contains_key(func_name)
                && !state.function_types.contains_key(func_name)
                && find_variant_output_type(func_name, state.dsl_types).is_none() =>
        {
            let constraint_type = constraint_type_name(expected_type);
            let hint = rule_base_hint(Some(expected_type), state).map(str::to_string);
            let arg_exprs = args
                .iter()
                .map(|arg| {
                    render_rule_base_handle_expr_with_hint(arg, hint.as_deref(), state)
                        .map(|rendered| format!("{rendered}.into_handle_ty()"))
                })
                .collect::<Option<Vec<_>>>()?;
            Some(format!(
                "prim_call::<{constraint_type}>(\"{func_name}\", vec![{}])",
                arg_exprs.join(", ")
            ))
        }
        _ => render_rule_base_handle_expr(expr, state),
    }
}

fn render_rule_fact_constraint_expr(
    expr: &Expr,
    state: &mut RulePatternState<'_>,
) -> Option<String> {
    let Expr::Call(_, func_name, args) = expr else {
        return None;
    };
    let arg_types = infer_primitive_fact_arg_types(func_name, args, state);
    let arg_exprs = args
        .iter()
        .enumerate()
        .map(|(index, arg)| {
            render_rule_fact_arg_handle_expr(
                arg,
                arg_types
                    .as_ref()
                    .and_then(|types| types.get(index))
                    .and_then(|ty| ty.as_deref()),
                state,
            )
            .map(|rendered| format!("{rendered}.into_handle_ty()"))
        })
        .collect::<Option<Vec<_>>>()?;
    Some(format!(
        "prim_fact(\"{func_name}\", vec![{}])",
        arg_exprs.join(", ")
    ))
}

fn render_rule_fact_arg_handle_expr(
    expr: &Expr,
    expected_type: Option<&str>,
    state: &mut RulePatternState<'_>,
) -> Option<String> {
    if let Some(expected_type) = expected_type {
        if is_rule_base_like_type(expected_type, state.dsl_types) {
            return render_rule_base_handle_expr_with_hint(expr, Some(expected_type), state);
        }

        let pat_name = ensure_complex_binding_from_expr(expr, expected_type, None, true, state)?;
        return Some(format!("{pat_name}.handle()"));
    }

    render_rule_base_handle_expr(expr, state)
}

fn render_rule_primitive_handle_expr_with_hint(
    func_name: &str,
    args: &[Expr],
    hint_type: Option<&str>,
    state: &mut RulePatternState<'_>,
) -> Option<String> {
    let output_type = hint_type
        .map(normalize_identifier)
        .or_else(|| infer_primitive_output_type(func_name, args, state))
        .unwrap_or_else(|| "i64".to_string());
    let constraint_type = constraint_type_name(&output_type);
    let arg_hints = primitive_call_arg_hints(func_name, &output_type, args, state);
    let arg_exprs = args
        .iter()
        .enumerate()
        .map(|(index, arg)| {
            render_rule_base_handle_expr_with_hint(
                arg,
                arg_hints
                    .as_ref()
                    .and_then(|hints| hints.get(index))
                    .and_then(|hint| hint.as_deref()),
                state,
            )
            .map(|rendered| format!("{rendered}.into_handle_ty()"))
        })
        .collect::<Option<Vec<_>>>()?;
    Some(format!(
        "prim_call::<{constraint_type}>(\"{func_name}\", vec![{}])",
        arg_exprs.join(", ")
    ))
}

fn ensure_base_binding(
    preferred_name: &str,
    field_type: &str,
    include_in_pattern_vars: bool,
    state: &mut RulePatternState<'_>,
) -> Option<String> {
    let normalized_name = normalize_identifier(preferred_name);
    if let Some(binding) = state.bindings.get(&normalized_name) {
        return match binding {
            RuleVarBinding::BaseVar { pat_name, .. } => Some(pat_name.clone()),
            RuleVarBinding::Complex { .. } | RuleVarBinding::BaseField { .. } => None,
        };
    }

    let normalized_type = normalize_identifier(field_type);
    state.pattern_query_lines.push(format!(
        "let {normalized_name} = BaseVar::<{normalized_type}, MyPatRec>::query_named(\"{normalized_name}\");"
    ));
    if include_in_pattern_vars {
        ensure_rule_pattern_var(&mut state.pattern_vars, &normalized_name, &normalized_type);
    }
    state.bindings.insert(
        normalized_name.clone(),
        RuleVarBinding::BaseVar {
            pat_name: normalized_name.clone(),
            ty: normalized_type.clone(),
        },
    );
    Some(normalized_name)
}

fn lower_function_base_arg(
    expr: &Expr,
    field_type: &str,
    hint: &str,
    state: &mut RulePatternState<'_>,
) -> Option<String> {
    match expr {
        Expr::Var(_, var_name) => {
            let normalized_var_name = normalize_identifier(var_name);
            if let Some(binding) = state.bindings.get(&normalized_var_name).cloned() {
                match binding {
                    RuleVarBinding::BaseVar { pat_name, .. } => Some(pat_name.clone()),
                    RuleVarBinding::Complex { .. } | RuleVarBinding::BaseField { .. } => {
                        let alias_name = state.fresh_temp_name(hint);
                        let alias_type = normalize_identifier(field_type);
                        state.pattern_query_lines.push(format!(
                            "let {alias_name} = BaseVar::<{alias_type}, MyPatRec>::query_named(\"{alias_name}\");"
                        ));
                        state.assert_exprs.push(format!(
                            "{alias_name}.handle().eq(&{})",
                            binding_handle_expr(&binding)
                        ));
                        Some(alias_name)
                    }
                }
            } else if let Some(global_expr) =
                state.global_bindings.get(&normalized_var_name).cloned()
            {
                let alias_name = state.fresh_temp_name(hint);
                let alias_type = normalize_identifier(field_type);
                state.pattern_query_lines.push(format!(
                    "let {alias_name} = BaseVar::<{alias_type}, MyPatRec>::query_named(\"{alias_name}\");"
                ));
                let rendered =
                    render_rule_base_handle_expr_with_hint(&global_expr, Some(field_type), state)?;
                state
                    .assert_exprs
                    .push(format!("{alias_name}.handle().eq(&{rendered})"));
                Some(alias_name)
            } else {
                ensure_base_binding(&normalized_var_name, field_type, true, state)
            }
        }
        _ => {
            let alias_name = state.fresh_temp_name(hint);
            let alias_type = normalize_identifier(field_type);
            state.pattern_query_lines.push(format!(
                "let {alias_name} = BaseVar::<{alias_type}, MyPatRec>::query_named(\"{alias_name}\");"
            ));
            let rendered = render_rule_base_handle_expr_with_hint(expr, Some(field_type), state)?;
            state
                .assert_exprs
                .push(format!("{alias_name}.handle().eq(&{rendered})"));
            Some(alias_name)
        }
    }
}

fn lower_function_fact(
    function_name: &str,
    args: &[Expr],
    preferred_name: Option<&str>,
    state: &mut RulePatternState<'_>,
) -> Option<RuleVarBinding> {
    let function_type = state.function_types.get(function_name)?.clone();
    let output_name = preferred_name.map(normalize_identifier).unwrap_or_else(|| {
        state.fresh_temp_name(&format!(
            "{}_out",
            normalize_identifier(function_name).to_snake_case()
        ))
    });
    let include_in_pattern_vars = preferred_name.is_some();

    let mut query_args = Vec::new();
    for (index, (field, arg)) in function_type.fields.iter().zip(args.iter()).enumerate() {
        if is_rule_base_like_type(&field.field_type, state.dsl_types) {
            let base_arg = lower_function_base_arg(
                arg,
                &field.field_type,
                &format!(
                    "{}_arg{}",
                    normalize_identifier(function_name).to_snake_case(),
                    index
                ),
                state,
            )?;
            query_args.push(format!("&{base_arg}"));
        } else {
            let complex_arg =
                ensure_complex_binding_from_expr(arg, &field.field_type, None, true, state)?;
            query_args.push(format!("&{complex_arg}"));
        }
    }

    state.pattern_query_lines.push(format!(
        "let {output_name} = {}::query({});",
        normalize_identifier(function_name),
        query_args.join(", ")
    ));

    if include_in_pattern_vars {
        ensure_rule_pattern_var(
            &mut state.pattern_vars,
            &output_name,
            &function_type.output_type,
        );
    }

    let binding = if is_basic_type(&function_type.output_type) {
        RuleVarBinding::BaseVar {
            pat_name: output_name.clone(),
            ty: function_type.output_type.clone(),
        }
    } else {
        RuleVarBinding::Complex {
            pat_name: output_name.clone(),
        }
    };

    if let Some(preferred_name) = preferred_name {
        state
            .bindings
            .insert(normalize_identifier(preferred_name), binding.clone());
    }

    Some(binding)
}

fn lower_constructor_fact(
    constructor_name: &str,
    args: &[Expr],
    preferred_name: Option<&str>,
    state: &mut RulePatternState<'_>,
) -> Option<RuleVarBinding> {
    let variant = find_variant_info(constructor_name, state.dsl_types)?.clone();
    let output_type = find_variant_output_type(constructor_name, state.dsl_types)?;
    let node_name = preferred_name.map(normalize_identifier).unwrap_or_else(|| {
        state.fresh_temp_name(&format!(
            "{}_node",
            normalize_identifier(constructor_name).to_snake_case()
        ))
    });
    let include_in_pattern_vars = preferred_name.is_some();

    let mut complex_args = Vec::new();
    for (field, arg) in variant.fields.iter().zip(args.iter()) {
        if is_rule_base_like_type(&field.field_type, state.dsl_types) {
            if let Expr::Var(_, var_name) = arg {
                let normalized_var_name = normalize_identifier(var_name);
                if let Some(binding) = state.bindings.get(&normalized_var_name) {
                    state.assert_exprs.push(format!(
                        "{node_name}.handle_{}().eq(&{})",
                        field.name,
                        binding_handle_expr(binding)
                    ));
                } else if let Some(global_expr) =
                    state.global_bindings.get(&normalized_var_name).cloned()
                {
                    let rendered = render_rule_base_handle_expr_with_hint(
                        &global_expr,
                        Some(&field.field_type),
                        state,
                    )?;
                    state.assert_exprs.push(format!(
                        "{node_name}.handle_{}().eq(&{rendered})",
                        field.name
                    ));
                } else {
                    state.bindings.insert(
                        normalized_var_name,
                        RuleVarBinding::BaseField {
                            owner_pat_name: node_name.clone(),
                            field_name: field.name.clone(),
                            ty: field.field_type.clone(),
                        },
                    );
                }
            } else {
                let rendered =
                    render_rule_base_handle_expr_with_hint(arg, Some(&field.field_type), state)?;
                state.assert_exprs.push(format!(
                    "{node_name}.handle_{}().eq(&{rendered})",
                    field.name
                ));
            }
        } else {
            let complex_arg =
                ensure_complex_binding_from_expr(arg, &field.field_type, None, true, state)?;
            complex_args.push(format!("&{complex_arg}"));
        }
    }

    if complex_args.is_empty() {
        state.pattern_query_lines.push(format!(
            "let {node_name} = {}::query();",
            normalize_identifier(constructor_name)
        ));
    } else {
        state.pattern_query_lines.push(format!(
            "let {node_name} = {}::query({});",
            normalize_identifier(constructor_name),
            complex_args.join(", ")
        ));
    }

    if include_in_pattern_vars {
        ensure_rule_pattern_var(&mut state.pattern_vars, &node_name, &output_type);
    }

    let binding = RuleVarBinding::Complex {
        pat_name: node_name.clone(),
    };

    if let Some(preferred_name) = preferred_name {
        state
            .bindings
            .insert(normalize_identifier(preferred_name), binding.clone());
    }

    Some(binding)
}

fn find_variant_output_type(
    constructor_name: &str,
    dsl_types: &HashMap<String, DslType>,
) -> Option<String> {
    for (dsl_type_name, dsl_type) in dsl_types {
        if dsl_type
            .variants
            .iter()
            .any(|variant| same_normalized_identifier(&variant.name, constructor_name))
        {
            return Some(dsl_type_name.clone());
        }
    }
    None
}

impl RulePatternState<'_> {
    fn fresh_temp_name(&mut self, prefix: &str) -> String {
        self.temp_counter += 1;
        format!("{}_{}", normalize_identifier(prefix), self.temp_counter)
    }
}

/// Check if a type is a basic type (i64, f64, bool, String, etc.)
fn is_basic_type(type_name: &str) -> bool {
    let basic_types = [
        "i64",
        "f64",
        "bool",
        "String",
        "&'static str",
        "Rational",
        "BigRat",
        "BigInt",
        "BigRational",
        "Q",
        "Z",
        "()",
    ];
    basic_types.contains(&type_name)
}

fn is_rule_base_like_type(type_name: &str, dsl_types: &HashMap<String, DslType>) -> bool {
    if is_basic_type(type_name) {
        return true;
    }

    dsl_types
        .get(&normalize_identifier(type_name))
        .is_some_and(|dsl_type| dsl_type.variants.is_empty())
}

fn constraint_type_name(type_name: &str) -> String {
    match type_name {
        "Rational" => "egglog::sort::Q".to_string(),
        "BigRat" | "BigRational" | "Q" => "egglog::sort::Q".to_string(),
        "BigInt" | "Z" => "egglog::sort::Z".to_string(),
        other => normalize_identifier(other),
    }
}

fn binding_type_name(binding: &RuleVarBinding) -> Option<&str> {
    match binding {
        RuleVarBinding::Complex { .. } => None,
        RuleVarBinding::BaseVar { ty, .. } | RuleVarBinding::BaseField { ty, .. } => {
            Some(ty.as_str())
        }
    }
}

fn infer_rule_expr_type(expr: &Expr, state: &RulePatternState<'_>) -> Option<String> {
    match expr {
        Expr::Var(_, var_name) => {
            let normalized_var_name = normalize_identifier(var_name);
            if let Some(binding) = state.bindings.get(&normalized_var_name) {
                binding_type_name(binding).map(str::to_string)
            } else if let Some(global_expr) = state.global_bindings.get(&normalized_var_name) {
                infer_rule_expr_type(global_expr, state)
            } else {
                None
            }
        }
        Expr::Lit(_, lit) => Some(literal_type_name(lit)),
        Expr::Call(_, func_name, args) if state.function_types.contains_key(func_name) => {
            Some(state.function_types.get(func_name)?.output_type.clone())
        }
        Expr::Call(_, func_name, _) if state.relation_types.contains_key(func_name) => {
            Some(normalize_identifier(func_name))
        }
        Expr::Call(_, func_name, _)
            if find_variant_output_type(func_name, state.dsl_types).is_some() =>
        {
            find_variant_output_type(func_name, state.dsl_types)
        }
        Expr::Call(_, func_name, args) => infer_primitive_output_type(func_name, args, state),
    }
}

fn infer_numeric_operation_type(
    args: &[Expr],
    state: &RulePatternState<'_>,
    fallback: &str,
) -> String {
    let left = args
        .first()
        .and_then(|arg| infer_rule_expr_type(arg, state));
    let right = args.get(1).and_then(|arg| infer_rule_expr_type(arg, state));
    if matches!(left.as_deref(), Some("f64")) || matches!(right.as_deref(), Some("f64")) {
        "f64".to_string()
    } else {
        left.or(right).unwrap_or_else(|| fallback.to_string())
    }
}

fn infer_primitive_output_type(
    func_name: &str,
    args: &[Expr],
    state: &RulePatternState<'_>,
) -> Option<String> {
    let inferred = match func_name {
        "rational" => "Rational".to_string(),
        "to-f64" => "f64".to_string(),
        "from-string" | "bigint" | "numer" | "denom" => "BigInt".to_string(),
        "bigrat" => "BigRat".to_string(),
        "abs" | "min" | "max" | "neg" | "round" | "floor" | "ceil" | "sqrt" | "pow" => args
            .first()
            .and_then(|arg| infer_rule_expr_type(arg, state))
            .unwrap_or_else(|| "i64".to_string()),
        "=" | "==" | "!=" | "<" | "<=" | ">" | ">=" | "and" | "or" | "not" => "bool".to_string(),
        "+" | "-" | "*" | "/" | "%" => infer_numeric_operation_type(args, state, "i64"),
        _ => args
            .first()
            .and_then(|arg| infer_rule_expr_type(arg, state))
            .unwrap_or_else(|| "i64".to_string()),
    };
    Some(inferred)
}

fn primitive_call_arg_hints(
    func_name: &str,
    output_type: &str,
    args: &[Expr],
    state: &RulePatternState<'_>,
) -> Option<Vec<Option<String>>> {
    let hints = match func_name {
        "+" | "-" | "*" | "/" | "%" | "min" | "max" => {
            vec![Some(output_type.to_string()); args.len()]
        }
        "abs" | "round" | "floor" | "ceil" | "sqrt" | "neg" => {
            vec![Some(output_type.to_string()); args.len()]
        }
        _ => args
            .iter()
            .map(|arg| infer_rule_expr_type(arg, state))
            .collect::<Vec<_>>(),
    };
    Some(hints)
}

fn infer_primitive_fact_arg_types(
    func_name: &str,
    args: &[Expr],
    state: &RulePatternState<'_>,
) -> Option<Vec<Option<String>>> {
    match func_name {
        "set-contains" | "multiset-contains" if args.len() == 2 => {
            let set_sort = infer_rule_expr_type(&args[0], state)?;
            let element_ty = container_element_type(&set_sort, state)?;
            Some(vec![Some(set_sort), Some(element_ty)])
        }
        _ => None,
    }
}

fn container_element_type(sort_name: &str, state: &RulePatternState<'_>) -> Option<String> {
    let schema = state.sort_schemas.get(&normalize_identifier(sort_name))?;
    if !matches!(schema.name.as_str(), "Set" | "MultiSet" | "Vec") {
        return None;
    }
    schema.args.first().cloned()
}

fn normalize_runtime_call_name(func_name: &str) -> String {
    normalize_identifier(func_name).to_snake_case()
}

fn render_rule_runtime_call_expr(func_name: &str, arg_exprs: &[String]) -> Option<String> {
    match (func_name, arg_exprs.len()) {
        ("rational", 2) => Some(format!(
            "egglog::sort::Q::new(format!(\"{{}}/{{}}\", {}, {}).parse().unwrap())",
            arg_exprs[0], arg_exprs[1]
        )),
        ("min", 2) => Some(format!("std::cmp::min({}, {})", arg_exprs[0], arg_exprs[1])),
        ("max", 2) => Some(format!("std::cmp::max({}, {})", arg_exprs[0], arg_exprs[1])),
        ("abs", 1) => Some(format!("({}).abs()", arg_exprs[0])),
        ("round", 1) => Some(format!("({}).round()", arg_exprs[0])),
        ("floor", 1) => Some(format!("({}).floor()", arg_exprs[0])),
        ("ceil", 1) => Some(format!("({}).ceil()", arg_exprs[0])),
        ("sqrt", 1) => Some(format!("({}).sqrt()", arg_exprs[0])),
        ("neg", 1) => Some(format!("(-{})", arg_exprs[0])),
        ("and", 2) => Some(format!("({} && {})", arg_exprs[0], arg_exprs[1])),
        ("or", 2) => Some(format!("({} || {})", arg_exprs[0], arg_exprs[1])),
        ("not", 1) => Some(format!("(!{})", arg_exprs[0])),
        ("+", 2)
        | ("-", 2)
        | ("*", 2)
        | ("/", 2)
        | ("%", 2)
        | ("<", 2)
        | ("<=", 2)
        | (">", 2)
        | (">=", 2)
        | ("==", 2)
        | ("!=", 2) => Some(format!("({} {} {})", arg_exprs[0], func_name, arg_exprs[1])),
        _ => Some(format!(
            "{}({})",
            normalize_runtime_call_name(func_name),
            arg_exprs.join(", ")
        )),
    }
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
        log::debug!("Condition info: {:?}", condition_info);
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
                    .find(|variant| same_normalized_identifier(&variant.name, constructor_name))
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
        for (condition_index, (operator, e1, e2)) in condition_expressions.iter().enumerate() {
            let mut temp_query_parts = Vec::new();
            let left_handle = render_rewrite_condition_handle_expr(
                e1,
                dsl_types,
                &mut temp_query_parts,
                &mut pattern_vars_variables,
                &mut node_counter,
                &mut variable_constructors,
            );
            let right_handle = render_rewrite_condition_handle_expr(
                e2,
                dsl_types,
                &mut temp_query_parts,
                &mut pattern_vars_variables,
                &mut node_counter,
                &mut variable_constructors,
            );
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
            let condition_var_name = format!("cond_expr_{condition_index}");
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
                        let field_name =
                            get_field_name_for_variable_in_constructor(func_name, index, dsl_types);
                        basic_conditions.push(format!(
                            "pat.{}.{} == {}",
                            constructor_node_name, field_name, arg_node
                        ));
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
                        log::debug!("recognized complex node {:?}", node_name);
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
                    let base_query = format!(
                        "let {} = {}::query()",
                        node_name,
                        normalize_identifier(func_name)
                    );
                    let full_query = if !condition_queries.is_empty() {
                        format!("{}{};", base_query, condition_queries.join(""))
                    } else {
                        format!("{};", base_query)
                    };
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

fn synthesize_implicit_list_helpers(
    commands: &[Command],
    datatype_constructors: &HashMap<String, Vec<DslVariant>>,
    eggplant_commands: &mut Vec<EggplantCommandWithSource>,
    dsl_types: &mut HashMap<String, DslType>,
    relation_types: &mut HashMap<String, RelationType>,
    function_types: &mut HashMap<String, FunctionType>,
    source_file: &Option<String>,
) {
    for list_sort in collect_implicit_list_sorts(commands) {
        let normalized_sort = normalize_identifier(&list_sort);
        let element_sort = list_sort_element_type(&list_sort).unwrap_or_default();
        let element_components = split_tuple_sort_components(&element_sort)
            .into_iter()
            .map(|component| normalize_identifier(&component))
            .collect::<Vec<_>>();

        let mut dsl_type = dsl_types
            .get(&normalized_sort)
            .cloned()
            .unwrap_or_else(|| DslType {
                name: normalized_sort.clone(),
                variants: Vec::new(),
            });

        if let Some(existing_variants) = datatype_constructors.get(&normalized_sort) {
            for variant in existing_variants {
                if !dsl_type
                    .variants
                    .iter()
                    .any(|existing| same_normalized_identifier(&existing.name, &variant.name))
                {
                    dsl_type.variants.push(variant.clone());
                }
            }
        }

        let nil_name = format!("Nil-{list_sort}");
        if !dsl_type
            .variants
            .iter()
            .any(|variant| same_normalized_identifier(&variant.name, &nil_name))
        {
            dsl_type.variants.push(DslVariant {
                name: normalize_identifier(&nil_name),
                fields: Vec::new(),
                source_file: source_file.clone(),
                source_line: None,
            });
        }

        let cons_name = format!("Cons-{list_sort}");
        if !dsl_type
            .variants
            .iter()
            .any(|variant| same_normalized_identifier(&variant.name, &cons_name))
        {
            let mut fields = element_components
                .iter()
                .enumerate()
                .map(|(index, field_type)| DslField {
                    name: format!("arg{index}"),
                    field_type: field_type.clone(),
                })
                .collect::<Vec<_>>();
            fields.push(DslField {
                name: format!("arg{}", fields.len()),
                field_type: normalized_sort.clone(),
            });
            dsl_type.variants.push(DslVariant {
                name: normalize_identifier(&cons_name),
                fields,
                source_file: source_file.clone(),
                source_line: None,
            });
        }

        if !dsl_types.contains_key(&normalized_sort) {
            eggplant_commands.push(EggplantCommandWithSource {
                command: EggplantCommand::DslType(dsl_type.clone()),
                source_file: source_file.clone(),
                source_line: None,
            });
        }
        dsl_types.insert(normalized_sort.clone(), dsl_type);

        let length_name = format!("Length-{list_sort}");
        if !function_types.contains_key(&length_name) {
            let function_type = FunctionType {
                name: normalize_identifier(&length_name),
                fields: vec![DslField {
                    name: "arg0".to_string(),
                    field_type: normalized_sort.clone(),
                }],
                output_type: "i64".to_string(),
                merge_expr: None,
            };
            function_types.insert(length_name, function_type.clone());
            eggplant_commands.push(EggplantCommandWithSource {
                command: EggplantCommand::FunctionType(function_type),
                source_file: source_file.clone(),
                source_line: None,
            });
        }

        let is_non_empty_name = format!("IsNonEmpty-{list_sort}");
        if !function_types.contains_key(&is_non_empty_name) {
            let function_type = FunctionType {
                name: normalize_identifier(&is_non_empty_name),
                fields: vec![DslField {
                    name: "arg0".to_string(),
                    field_type: normalized_sort.clone(),
                }],
                output_type: "bool".to_string(),
                merge_expr: None,
            };
            function_types.insert(is_non_empty_name, function_type.clone());
            eggplant_commands.push(EggplantCommandWithSource {
                command: EggplantCommand::FunctionType(function_type),
                source_file: source_file.clone(),
                source_line: None,
            });
        }

        let at_name = format!("At-{list_sort}");
        if !relation_types.contains_key(&at_name) {
            let mut fields = vec![
                DslField {
                    name: "arg0".to_string(),
                    field_type: normalized_sort.clone(),
                },
                DslField {
                    name: "arg1".to_string(),
                    field_type: "i64".to_string(),
                },
            ];
            fields.extend(
                element_components
                    .iter()
                    .enumerate()
                    .map(|(index, field_type)| DslField {
                        name: format!("arg{}", index + 2),
                        field_type: field_type.clone(),
                    }),
            );
            let relation_type = RelationType {
                name: normalize_identifier(&at_name),
                fields,
            };
            relation_types.insert(at_name, relation_type.clone());
            eggplant_commands.push(EggplantCommandWithSource {
                command: EggplantCommand::RelationType(relation_type),
                source_file: source_file.clone(),
                source_line: None,
            });
        }
    }
}

fn synthesize_constructor_only_sorts(
    datatype_constructors: &HashMap<String, Vec<DslVariant>>,
    declared_datatypes: &HashSet<String>,
    eggplant_commands: &mut Vec<EggplantCommandWithSource>,
    dsl_types: &mut HashMap<String, DslType>,
    source_file: &Option<String>,
) {
    for (sort_name, variants) in datatype_constructors {
        let normalized_sort = normalize_identifier(sort_name);
        if declared_datatypes.contains(&normalized_sort) {
            continue;
        }

        let mut dsl_type = dsl_types
            .get(&normalized_sort)
            .cloned()
            .unwrap_or_else(|| DslType {
                name: normalized_sort.clone(),
                variants: Vec::new(),
            });
        let mut changed = !dsl_types.contains_key(&normalized_sort);

        for variant in variants {
            if dsl_type
                .variants
                .iter()
                .any(|existing| same_normalized_identifier(&existing.name, &variant.name))
            {
                continue;
            }
            dsl_type.variants.push(variant.clone());
            changed = true;
        }

        if !changed {
            continue;
        }

        dsl_types.insert(normalized_sort.clone(), dsl_type.clone());
        upsert_synthetic_dsl_type_command(eggplant_commands, dsl_type, source_file.clone());
    }
}

fn upsert_synthetic_dsl_type_command(
    eggplant_commands: &mut Vec<EggplantCommandWithSource>,
    dsl_type: DslType,
    source_file: Option<String>,
) {
    if let Some(existing) = eggplant_commands.iter_mut().find(|cmd_with_source| {
        cmd_with_source.source_line.is_none()
            && matches!(
                &cmd_with_source.command,
                EggplantCommand::DslType(existing) if existing.name == dsl_type.name
            )
    }) {
        existing.command = EggplantCommand::DslType(dsl_type);
        return;
    }

    eggplant_commands.push(EggplantCommandWithSource {
        command: EggplantCommand::DslType(dsl_type),
        source_file,
        source_line: None,
    });
}

fn collect_implicit_list_sorts(commands: &[Command]) -> Vec<String> {
    let mut list_sorts = Vec::new();

    for command in commands {
        match command {
            Command::Datatype { name, variants, .. } => {
                collect_list_sort_from_type_name(name, &mut list_sorts);
                for variant in variants {
                    for field_type in &variant.types {
                        collect_list_sort_from_type_name(field_type, &mut list_sorts);
                    }
                }
            }
            Command::Constructor { schema, .. } | Command::Function { schema, .. } => {
                collect_list_sort_from_type_name(&schema.output, &mut list_sorts);
                for field_type in &schema.input {
                    collect_list_sort_from_type_name(field_type, &mut list_sorts);
                }
            }
            Command::Relation { inputs, .. } => {
                for field_type in inputs {
                    collect_list_sort_from_type_name(field_type, &mut list_sorts);
                }
            }
            Command::Sort(_, name, _) => collect_list_sort_from_type_name(name, &mut list_sorts),
            _ => {}
        }
    }

    list_sorts
}

fn collect_list_sort_from_type_name(type_name: &str, list_sorts: &mut Vec<String>) {
    if type_name.starts_with("List<")
        && type_name.ends_with('>')
        && !list_sorts.iter().any(|existing| existing == type_name)
    {
        list_sorts.push(type_name.to_string());
    }
}

fn list_sort_element_type(list_sort: &str) -> Option<String> {
    list_sort
        .strip_prefix("List<")
        .and_then(|inner| inner.strip_suffix('>'))
        .map(|inner| inner.to_string())
}

fn split_tuple_sort_components(type_name: &str) -> Vec<String> {
    let mut components = Vec::new();
    let mut current = String::new();
    let mut depth = 0usize;

    for ch in type_name.chars() {
        match ch {
            '<' => {
                depth += 1;
                current.push(ch);
            }
            '>' => {
                depth = depth.saturating_sub(1);
                current.push(ch);
            }
            '+' if depth == 0 => {
                if !current.trim().is_empty() {
                    components.push(current.trim().to_string());
                }
                current.clear();
            }
            _ => current.push(ch),
        }
    }

    if !current.trim().is_empty() {
        components.push(current.trim().to_string());
    }

    if components.is_empty() {
        vec![type_name.to_string()]
    } else {
        components
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
                    .find(|variant| same_normalized_identifier(&variant.name, constructor_name))
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
    let basic_operations = ["+", "-", "*", "/", "%", "<", ">", "<=", ">=", "==", "!="];
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
            .find(|v| same_normalized_identifier(&v.name, constructor_name))
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
            .find(|v| same_normalized_identifier(&v.name, constructor_name))
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
    use std::path::PathBuf;

    fn find_repo_sibling(name: &str) -> Option<PathBuf> {
        let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        manifest_dir
            .ancestors()
            .map(|ancestor| ancestor.join(name))
            .find(|candidate| candidate.is_dir())
    }

    fn upstream_egglog_fixture(rel: &str) -> String {
        find_repo_sibling("upstream_egglog")
            .unwrap_or_else(|| panic!("could not locate sibling repo `upstream_egglog`"))
            .join(rel)
            .to_string_lossy()
            .into_owned()
    }

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

    #[test]
    fn test_relation_conversion_and_codegen() {
        let program = r#"
            (relation Edge (i64 i64))
            (Edge 1 2)
        "#;

        let mut parser = Parser::default();
        let commands = parser.get_program_from_string(None, program).unwrap();
        let eggplant_commands = convert_to_eggplant(&commands);

        let has_relation_type = eggplant_commands.iter().any(|cmd| {
            matches!(
                cmd,
                EggplantCommand::RelationType(RelationType { name, .. }) if name == "Edge"
            )
        });
        assert!(has_relation_type);

        let has_relation_insert = eggplant_commands.iter().any(|cmd| {
            matches!(
                cmd,
                EggplantCommand::RelationInsert { relation, .. } if relation == "Edge"
            )
        });
        assert!(has_relation_insert);

        let rust = EggplantCodeGenerator::new()
            .generate_rust(&convert_to_eggplant_with_source(&commands, None));
        assert!(rust.contains("#[eggplant::relation]"));
        assert!(rust.contains("struct Edge {"));
        assert!(rust.contains("Edge::<MyTx>::insert(1, 2);"));
    }

    #[test]
    fn test_generic_rule_conversion_handles_constructor_fact_and_empty_body_rule() {
        let program = r#"
            (datatype MySort
              (Wrap i64))

            (rule ((Wrap 1))
                  ((let $a (Wrap 1))))

            (function foo () i64 :no-merge)
            (rule () ((set (foo) 4)))
        "#;

        let mut parser = Parser::default();
        let commands = parser.get_program_from_string(None, program).unwrap();
        let rust = EggplantCodeGenerator::new()
            .generate_rust(&convert_to_eggplant_with_source(&commands, None));

        assert!(!rust.contains("TODO: implement action for rule_"));
        assert!(rust.contains("Wrap::query();"));
        assert!(rust.contains("let _a = ctx.insert_wrap(1);"));
        assert!(rust.contains("ctx.set_foo(4);"));
    }

    #[test]
    fn test_generic_rule_conversion_handles_function_query_and_base_bindings() {
        let program = r#"
            (function foo (i64) i64 :no-merge)
            (relation Seen (i64))
            (rule ((= x 1)
                   (= y (foo x)))
                  ((Seen y)))
        "#;

        let mut parser = Parser::default();
        let commands = parser.get_program_from_string(None, program).unwrap();
        let rust = EggplantCodeGenerator::new()
            .generate_rust(&convert_to_eggplant_with_source(&commands, None));

        assert!(!rust.contains("TODO: implement action for rule_"));
        assert!(rust.contains("BaseVar::<i64, MyPatRec>::query_named(\"x\")"));
        assert!(rust.contains("let y = foo::query(&x);"));
        assert!(rust.contains("ctx.insert_seen(ctx.devalue(pat.y));"));
    }

    #[test]
    fn test_extract_conversion_and_codegen() {
        let program = r#"
            (datatype Expr
              (Num i64))
            (let $expr (Num 7))
            (extract $expr 0)
            (extract (Num 9))
        "#;

        let mut parser = Parser::default();
        let commands = parser.get_program_from_string(None, program).unwrap();
        let eggplant_commands = convert_to_eggplant(&commands);

        assert!(eggplant_commands.iter().any(|cmd| {
            matches!(
                cmd,
                EggplantCommand::Extract {
                    expr: Expr::Var(_, name),
                    variants: Some(0),
                } if name == "$expr"
            )
        }));

        let rust = EggplantCodeGenerator::new()
            .generate_rust(&convert_to_eggplant_with_source(&commands, None));

        assert!(rust.contains("let _expr:Num<MyTx> = Num::new(7);"));
        assert!(rust.contains("_expr.pull();"));
        assert!(rust.contains("// extract requested 0 variant(s)"));
        assert!(rust.contains("Num::new(9).pull();"));
    }

    #[test]
    fn test_print_function_conversion_and_codegen() {
        let program = r#"
            (relation R (i64 i64))
            (R 1 2)
            (print-function R 3 :file "output.R3.csv.log" :mode csv)
        "#;

        let mut parser = Parser::default();
        let commands = parser.get_program_from_string(None, program).unwrap();
        let eggplant_commands = convert_to_eggplant(&commands);

        assert!(eggplant_commands.iter().any(|cmd| {
            matches!(
                cmd,
                EggplantCommand::PrintFunction {
                    name,
                    size: Some(3),
                    file: Some(file),
                    mode: Some(mode),
                } if name == "R" && file == "output.R3.csv.log" && mode == "csv"
            )
        }));

        let rust = EggplantCodeGenerator::new()
            .generate_rust(&convert_to_eggplant_with_source(&commands, None));

        assert!(rust.contains("parse_and_run_program"));
        assert!(rust.contains("(print-function R 3 :file \\\"output.R3.csv.log\\\" :mode csv)"));
        assert!(rust.contains("for output in outputs {"));
        assert!(rust.contains("print!(\"{}\", output);"));
    }

    #[test]
    fn test_print_size_conversion_and_codegen() {
        let program = r#"
            (datatype Math
              (Add Math Math))
            (print-size Add)
            (print-size)
        "#;

        let mut parser = Parser::default();
        let commands = parser.get_program_from_string(None, program).unwrap();
        let eggplant_commands = convert_to_eggplant(&commands);

        assert!(eggplant_commands.iter().any(|cmd| {
            matches!(
                cmd,
                EggplantCommand::PrintSize {
                    target: Some(target)
                } if target == "Add"
            )
        }));
        assert!(
            eggplant_commands
                .iter()
                .any(|cmd| { matches!(cmd, EggplantCommand::PrintSize { target: None }) })
        );

        let rust = EggplantCodeGenerator::new()
            .generate_rust(&convert_to_eggplant_with_source(&commands, None));

        assert!(rust.contains("(print-size Add)"));
        assert!(rust.contains("(print-size)"));
        assert!(rust.contains("parse_and_run_program"));
        assert!(rust.contains("print!(\"{}\", output);"));
    }

    #[test]
    fn test_run_until_conversion_and_codegen() {
        let program = r#"
            (datatype G
              (IConst)
              (g* G G))
            (run 10 :until (= (g* a b) (g* (IConst) b)))
        "#;

        let mut parser = Parser::default();
        let commands = parser.get_program_from_string(None, program).unwrap();
        let eggplant_commands = convert_to_eggplant(&commands);

        assert!(eggplant_commands.iter().any(|cmd| {
            matches!(
                cmd,
                EggplantCommand::RunUntil {
                    ruleset,
                    limit: Some(10),
                    until: Fact::Op(_, _, _),
                } if ruleset == "default_ruleset"
            )
        }));

        let rust = EggplantCodeGenerator::new()
            .generate_rust(&convert_to_eggplant_with_source(&commands, None));

        assert!(rust.contains("parse_and_run_program"));
        assert!(rust.contains("(run default_ruleset 10 :until (= (g_ a b) (g_ (IConst) b)))"));
        assert!(rust.contains("for output in outputs {"));
    }

    #[test]
    fn test_run_schedule_conversion_and_codegen() {
        let program = r#"
            (rule () ())
            (run-schedule
              (seq (run :until (= a 1))
                   (run :until (= a "s"))))
        "#;

        let mut parser = Parser::default();
        let commands = parser.get_program_from_string(None, program).unwrap();
        let rust = EggplantCodeGenerator::new()
            .generate_rust(&convert_to_eggplant_with_source(&commands, None));

        assert!(rust.contains("parse_and_run_program"));
        assert!(rust.contains("(run-schedule"), "generated Rust:\n{rust}");
        assert!(
            rust.contains("(run :until (= a 1))"),
            "generated Rust:\n{rust}"
        );
        assert!(
            rust.contains("(run :until (= a \\\"s\\\"))"),
            "generated Rust:\n{rust}"
        );
        assert!(rust.contains("for output in outputs {"));
    }

    #[test]
    fn test_run_schedule_without_until_uses_builder_codegen() {
        let program = r#"
            (ruleset fast-analyses)
            (ruleset subst)
            (run-schedule
              (repeat 2
                (saturate fast-analyses)
                (run)
                (saturate subst)))
        "#;

        let mut parser = Parser::default();
        let commands = parser.get_program_from_string(None, program).unwrap();
        let rust = EggplantCodeGenerator::new()
            .generate_rust(&convert_to_eggplant_with_source(&commands, None));

        assert!(
            rust.contains("RunSchedule::builder()"),
            "generated Rust:\n{rust}"
        );
        assert!(rust.contains(".repeat(2"), "generated Rust:\n{rust}");
        assert!(
            rust.contains(".saturate(fast_analyses)"),
            "generated Rust:\n{rust}"
        );
        assert!(
            rust.contains(".run(default_ruleset)"),
            "generated Rust:\n{rust}"
        );
        assert!(rust.contains(".saturate(subst)"), "generated Rust:\n{rust}");
        assert!(
            !rust.contains("parse_and_run_program"),
            "generated Rust:\n{rust}"
        );
    }

    #[test]
    fn test_include_conversion_and_codegen_uses_resolved_path() {
        let source_path = upstream_egglog_fixture("tests/include.egg");
        let program = std::fs::read_to_string(&source_path).unwrap();

        let mut parser = Parser::default();
        let commands = parser
            .get_program_from_string(Some(source_path.clone()), &program)
            .unwrap();
        let eggplant_commands =
            convert_to_eggplant_with_source(&commands, Some(source_path.clone()));

        assert!(eggplant_commands.iter().any(|cmd| {
            matches!(
                &cmd.command,
                EggplantCommand::Include { file }
                    if file == &upstream_egglog_fixture("tests/web-demo/path.egg")
            )
        }));

        let rust = EggplantCodeGenerator::new().generate_rust(&convert_to_eggplant_with_source(
            &commands,
            Some(source_path.clone()),
        ));

        assert!(rust.contains("parse_and_run_program"));
        assert!(rust.contains(&upstream_egglog_fixture("tests/web-demo/path.egg")));
    }

    #[test]
    fn test_top_level_let_codegen_tracks_basic_vs_complex_bindings() {
        let program = r#"
            (datatype Math
              (Const i64)
              (Add Math Math))
            (let $two 2)
            (let $zero (Const $two))
            (let $sum (Add $zero $zero))
        "#;

        let mut parser = Parser::default();
        let commands = parser.get_program_from_string(None, program).unwrap();
        let rust = EggplantCodeGenerator::new()
            .generate_rust(&convert_to_eggplant_with_source(&commands, None));

        assert!(
            rust.contains("let _two:i64 = 2;"),
            "generated Rust:\n{rust}"
        );
        assert!(!rust.contains("_two.commit();"), "generated Rust:\n{rust}");
        assert!(
            rust.contains("let _zero:Const<MyTx> = Const::new(_two);"),
            "generated Rust:\n{rust}"
        );
        assert!(rust.contains("_zero.commit();"), "generated Rust:\n{rust}");
        assert!(
            rust.contains("let _sum:Add<MyTx> = Add::new(&_zero, &_zero);"),
            "generated Rust:\n{rust}"
        );
    }

    #[test]
    fn test_generic_rule_conversion_handles_alias_equality_after_function_query() {
        let program = r#"
            (function foo () i64 :no-merge)
            (relation Seen (i64))
            (rule ((= a (foo))
                   (= a b))
                  ((Seen b)))
        "#;

        let mut parser = Parser::default();
        let commands = parser.get_program_from_string(None, program).unwrap();
        let rust = EggplantCodeGenerator::new()
            .generate_rust(&convert_to_eggplant_with_source(&commands, None));

        assert!(!rust.contains("TODO: implement action for rule_"));
        assert!(rust.contains("let a = foo::query();"));
        assert!(rust.contains("ctx.insert_seen(ctx.devalue(pat.a));"));
    }

    #[test]
    fn test_generic_rule_conversion_handles_deferred_base_alias_binding() {
        let program = r#"
            (relation R (i64))
            (rule ((= x y)
                   (= y 1))
                  ((R x)))
        "#;

        let mut parser = Parser::default();
        let commands = parser.get_program_from_string(None, program).unwrap();
        let rust = EggplantCodeGenerator::new()
            .generate_rust(&convert_to_eggplant_with_source(&commands, None));

        assert!(
            !rust.contains("TODO: implement action for rule_"),
            "unexpected TODO in generated Rust:\n{rust}"
        );
        assert!(rust.contains("let y = BaseVar::<i64, MyPatRec>::query_named(\"y\");"));
        assert!(
            rust.contains(".assert(y.handle().eq(&1))"),
            "generated Rust:\n{rust}"
        );
        assert!(
            rust.contains("ctx.insert_r(ctx.devalue(pat.y));"),
            "generated Rust:\n{rust}"
        );
    }

    #[test]
    fn test_generic_rule_conversion_handles_deferred_base_expr_binding() {
        let program = r#"
            (relation R (i64))
            (rule ((= x (+ y 1))
                   (= y 1))
                  ((R x)))
        "#;

        let mut parser = Parser::default();
        let commands = parser.get_program_from_string(None, program).unwrap();
        let rust = EggplantCodeGenerator::new()
            .generate_rust(&convert_to_eggplant_with_source(&commands, None));

        assert!(
            !rust.contains("TODO: implement action for rule_"),
            "unexpected TODO in generated Rust:\n{rust}"
        );
        assert!(rust.contains("let x = BaseVar::<i64, MyPatRec>::query_named(\"x\");"));
        assert!(rust.contains("let y = BaseVar::<i64, MyPatRec>::query_named(\"y\");"));
        assert!(
            rust.contains(".assert(x.handle().eq(&prim_call::<i64>(\"+\", vec![y.handle().into_handle_ty(), (&(1_i64)).as_handle().into_handle_ty()])))"),
            "generated Rust:\n{rust}"
        );
        assert!(
            rust.contains("ctx.insert_r(ctx.devalue(pat.x));"),
            "generated Rust:\n{rust}"
        );
    }

    #[test]
    fn test_generic_rule_conversion_handles_constructor_action_head() {
        let program = r#"
            (datatype Type
              (Wrap Expr))
            (datatype Expr
              (Num i64))
            (rule ((= t (Wrap e)))
                  ((Wrap e)))
        "#;

        let mut parser = Parser::default();
        let commands = parser.get_program_from_string(None, program).unwrap();
        let rust = EggplantCodeGenerator::new()
            .generate_rust(&convert_to_eggplant_with_source(&commands, None));

        assert!(!rust.contains("TODO: implement action for rule_"));
        assert!(rust.contains("let _ = ctx.insert_wrap(pat.e);"));
    }

    #[test]
    fn test_generic_rule_conversion_handles_relation_delete_head() {
        let program = r#"
            (relation A ())
            (A)
            (rule ((A))
                  ((delete (A))))
        "#;

        let mut parser = Parser::default();
        let commands = parser.get_program_from_string(None, program).unwrap();
        let rust = EggplantCodeGenerator::new()
            .generate_rust(&convert_to_eggplant_with_source(&commands, None));

        assert!(
            !rust.contains("TODO: implement action for rule_"),
            "unexpected TODO in generated Rust:\n{rust}"
        );
        assert!(rust.contains("ctx.remove_a();"), "generated Rust:\n{rust}");
    }

    #[test]
    fn test_generic_rule_conversion_handles_function_delete_head() {
        let program = r#"
            (datatype Expr
              (Wrap Expr)
              (Leaf))
            (function tuple-length (Expr) i64 :no-merge)
            (rule ((tuple-length e))
                  ((delete (tuple-length e))))
        "#;

        let mut parser = Parser::default();
        let commands = parser.get_program_from_string(None, program).unwrap();
        let rust = EggplantCodeGenerator::new()
            .generate_rust(&convert_to_eggplant_with_source(&commands, None));

        assert!(
            !rust.contains("TODO: implement action for rule_"),
            "unexpected TODO in generated Rust:\n{rust}"
        );
        assert!(
            rust.contains("ctx.remove(\"tuple_length\", &key);"),
            "generated Rust:\n{rust}"
        );
    }

    #[test]
    fn test_generic_rule_conversion_handles_constructor_delete_head() {
        let program = r#"
            (datatype Flag
              (On)
              (Off))
            (rule ((On))
                  ((delete (On))))
        "#;

        let mut parser = Parser::default();
        let commands = parser.get_program_from_string(None, program).unwrap();
        let rust = EggplantCodeGenerator::new()
            .generate_rust(&convert_to_eggplant_with_source(&commands, None));

        assert!(
            !rust.contains("TODO: implement action for rule_"),
            "unexpected TODO in generated Rust:\n{rust}"
        );
        assert!(rust.contains("ctx.remove_on();"), "generated Rust:\n{rust}");
    }

    #[test]
    fn test_generic_rule_conversion_handles_implicit_list_helper_delete_heads() {
        let program = r#"
            (datatype PtrPointees
              (PointsNowhere))
            (datatype Pointees
              (TuplePointsTo List<PtrPointees>))
            (rule ((Nil-List<PtrPointees>))
                  ((delete (Nil-List<PtrPointees>))))
            (rule ((Cons-List<PtrPointees> hd tl))
                  ((delete (Cons-List<PtrPointees> hd tl))))
            (rule ((Length-List<PtrPointees> xs))
                  ((delete (Length-List<PtrPointees> xs))))
            (rule ((At-List<PtrPointees> xs i value))
                  ((delete (At-List<PtrPointees> xs i value))))
            (rule ((IsNonEmpty-List<PtrPointees> xs))
                  ((delete (IsNonEmpty-List<PtrPointees> xs))))
        "#;

        let mut parser = Parser::default();
        let commands = parser.get_program_from_string(None, program).unwrap();
        let rust = EggplantCodeGenerator::new()
            .generate_rust(&convert_to_eggplant_with_source(&commands, None));

        assert!(
            !rust.contains("TODO: implement action for rule_"),
            "unexpected TODO in generated Rust:\n{rust}"
        );
        assert!(
            rust.contains("enum List_PtrPointees_"),
            "generated Rust:\n{rust}"
        );
        assert!(
            rust.contains("ctx.remove_nil_list_ptr_pointees();"),
            "generated Rust:\n{rust}"
        );
        assert!(
            rust.contains("ctx.remove_cons_list_ptr_pointees("),
            "generated Rust:\n{rust}"
        );
        assert!(
            rust.contains("ctx.remove(\"Length_List_PtrPointees_\", &key);"),
            "generated Rust:\n{rust}"
        );
        assert!(
            rust.contains("ctx.remove_at_list_ptr_pointees("),
            "generated Rust:\n{rust}"
        );
        assert!(
            rust.contains("ctx.remove(\"IsNonEmpty_List_PtrPointees_\", &key);"),
            "generated Rust:\n{rust}"
        );
    }

    #[test]
    fn test_generic_rule_conversion_handles_tuple_like_list_helper_delete_heads() {
        let program = r#"
            (datatype IntInterval
              (Interval i64 i64))
            (datatype PtrPointees
              (PointsTo List<i64+IntInterval>))
            (rule ((Cons-List<i64+IntInterval> alloc offset tl))
                  ((delete (Cons-List<i64+IntInterval> alloc offset tl))))
            (rule ((At-List<i64+IntInterval> xs i alloc offset))
                  ((delete (At-List<i64+IntInterval> xs i alloc offset))))
        "#;

        let mut parser = Parser::default();
        let commands = parser.get_program_from_string(None, program).unwrap();
        let rust = EggplantCodeGenerator::new()
            .generate_rust(&convert_to_eggplant_with_source(&commands, None));

        assert!(
            !rust.contains("TODO: implement action for rule_"),
            "unexpected TODO in generated Rust:\n{rust}"
        );
        assert!(
            rust.contains("ctx.remove_cons_list_i64_int_interval("),
            "generated Rust:\n{rust}"
        );
        assert!(
            rust.contains("ctx.remove_at_list_i64_int_interval("),
            "generated Rust:\n{rust}"
        );
    }

    #[test]
    fn test_generic_rule_conversion_handles_declared_list_constructor_delete_heads() {
        let program = r#"
            (datatype PtrPointees
              (PointsNowhere))
            (datatype Expr
              (Seed))
            (constructor TypeListToList<PtrPointees> (Expr) List<PtrPointees>)
            (rule ((= e (Seed)))
                  ((delete (TypeListToList<PtrPointees> e))))
        "#;

        let mut parser = Parser::default();
        let commands = parser.get_program_from_string(None, program).unwrap();
        let rust = EggplantCodeGenerator::new()
            .generate_rust(&convert_to_eggplant_with_source(&commands, None));

        assert!(
            !rust.contains("TODO: implement action for rule_"),
            "unexpected TODO in generated Rust:\n{rust}"
        );
        assert!(
            rust.contains("ctx.remove_type_list_to_list_ptr_pointees("),
            "generated Rust:\n{rust}"
        );
    }

    #[test]
    fn test_generic_rule_conversion_handles_constructor_only_list_sort_body_delete_head() {
        let program = r#"
            (sort TypeList)
            (datatype PtrPointees
              (PointsNowhere))
            (constructor TypeListToList<PtrPointees> (TypeList) List<PtrPointees>)
            (rule ((TypeListToList<PtrPointees> e))
                  ((delete (TypeListToList<PtrPointees> e))))
        "#;

        let mut parser = Parser::default();
        let commands = parser.get_program_from_string(None, program).unwrap();
        let rust = EggplantCodeGenerator::new()
            .generate_rust(&convert_to_eggplant_with_source(&commands, None));

        assert!(
            !rust.contains("TODO: implement action for rule_"),
            "unexpected TODO in generated Rust:\n{rust}"
        );
        assert!(
            rust.contains("ctx.remove_type_list_to_list_ptr_pointees("),
            "generated Rust:\n{rust}"
        );
    }

    #[test]
    fn test_generic_rule_conversion_handles_constructor_subsume_head() {
        let program = r#"
            (datatype Expr
              (Num i64)
              (Add Expr Expr))
            (rule ((= e (Add x y)))
                  ((subsume (Add x y))))
        "#;

        let mut parser = Parser::default();
        let commands = parser.get_program_from_string(None, program).unwrap();
        let rust = EggplantCodeGenerator::new()
            .generate_rust(&convert_to_eggplant_with_source(&commands, None));

        assert!(
            !rust.contains("TODO: implement action for rule_"),
            "unexpected TODO in generated Rust:\n{rust}"
        );
        assert!(rust.contains("ctx.subsume_add(pat.x, pat.y);"));
    }

    #[test]
    fn test_analyze_rule_pattern_handles_relation_body_union_constructor_rhs() {
        let program = r#"
            (datatype Math
              (Add Math Math)
              (Const f64))
            (relation MathU (Math))
            (rule ((MathU a))
                  ((union a (Add a (Const 0.0)))))
        "#;

        let mut parser = Parser::default();
        let commands = parser.get_program_from_string(None, program).unwrap();

        let mut dsl_types: HashMap<String, DslType> = HashMap::new();
        let mut relation_types: HashMap<String, RelationType> = HashMap::new();
        let function_types: HashMap<String, FunctionType> = HashMap::new();
        let global_bindings: HashMap<String, Expr> = HashMap::new();

        for command in &commands {
            match command {
                Command::Datatype { name, variants, .. } => {
                    let dsl_variants = variants
                        .iter()
                        .map(|variant| DslVariant {
                            name: normalize_identifier(&variant.name),
                            fields: variant
                                .types
                                .iter()
                                .enumerate()
                                .map(|(index, field_type)| DslField {
                                    name: format!("arg{index}"),
                                    field_type: normalize_identifier(field_type),
                                })
                                .collect(),
                            source_file: None,
                            source_line: None,
                        })
                        .collect();
                    dsl_types.insert(
                        normalize_identifier(name),
                        DslType {
                            name: normalize_identifier(name),
                            variants: dsl_variants,
                        },
                    );
                }
                Command::Relation { name, inputs, .. } => {
                    relation_types.insert(
                        name.clone(),
                        RelationType {
                            name: normalize_identifier(name),
                            fields: inputs
                                .iter()
                                .enumerate()
                                .map(|(index, field_type)| DslField {
                                    name: format!("arg{index}"),
                                    field_type: normalize_identifier(field_type),
                                })
                                .collect(),
                        },
                    );
                }
                _ => {}
            }
        }

        let Command::Rule { rule, .. } = &commands[2] else {
            panic!("expected third command to be a rule");
        };
        let sort_schemas = HashMap::new();

        let (_, pattern_query, rule_context) = analyze_rule_pattern(
            "rule_test",
            rule,
            &dsl_types,
            &sort_schemas,
            &relation_types,
            &function_types,
            &global_bindings,
        )
        .expect("expected relation-body rule to analyze");

        assert!(pattern_query.contains("let a = Math::query_leaf();"));
        assert!(pattern_query.contains("let math_u_fact_1 = MathU::query(&a);"));

        let Action::Union(_, lhs, rhs) = &rule.head[0] else {
            panic!("expected union head");
        };

        assert_eq!(
            find_variant_output_type("Add", &dsl_types),
            Some("Math".to_string())
        );
        assert_eq!(
            find_variant_output_type("Const", &dsl_types),
            Some("Math".to_string())
        );
        assert_eq!(
            generate_rule_action_expr(
                lhs,
                &rule_context,
                &dsl_types,
                &relation_types,
                &HashMap::new(),
            ),
            Some("pat.a".to_string())
        );
        let Expr::Call(_, _, rhs_args) = rhs else {
            panic!("expected constructor rhs");
        };
        assert!(
            matches!(&rhs_args[1], Expr::Call(_, name, _) if name == "Const"),
            "{:?}",
            rhs_args[1]
        );
        assert_eq!(
            generate_rule_action_expr(
                &rhs_args[1],
                &rule_context,
                &dsl_types,
                &relation_types,
                &HashMap::new(),
            ),
            Some("ctx.insert_const(0_f64)".to_string())
        );
        assert_eq!(
            generate_rule_action_expr(
                rhs,
                &rule_context,
                &dsl_types,
                &relation_types,
                &HashMap::new(),
            ),
            Some("ctx.insert_add(pat.a, ctx.insert_const(0_f64))".to_string())
        );

        let action = generate_rule_action_with_context(
            &rule.head,
            &rule_context,
            &dsl_types,
            &relation_types,
        )
        .expect("expected union action to lower");

        assert!(
            action.contains("ctx.union(pat.a, ctx.insert_add(pat.a, ctx.insert_const(0_f64)));")
        );
    }

    #[test]
    fn test_generic_rule_conversion_handles_function_call_rhs_for_complex_set() {
        let program = r#"
            (datatype Interval
              (Wrap i64))
            (function interval-union (Interval Interval) Interval :no-merge)
            (function slot () Interval :no-merge)
            (rule ((= a (Wrap 1))
                   (= b (Wrap 2)))
                  ((set (slot) (interval-union a b))))
        "#;

        let mut parser = Parser::default();
        let commands = parser.get_program_from_string(None, program).unwrap();
        let rust = EggplantCodeGenerator::new()
            .generate_rust(&convert_to_eggplant_with_source(&commands, None));

        assert!(
            !rust.contains("TODO: implement action for rule_"),
            "unexpected TODO in generated Rust:\n{rust}"
        );
        assert!(
            rust.contains("ctx.set_slot(ctx.insert_interval_union(pat.a, pat.b));"),
            "generated Rust:\n{rust}"
        );
    }

    #[test]
    fn test_generic_rule_conversion_handles_constructor_equality_against_vec_get() {
        let program = r#"
            (datatype Operand
              (Node Body)
              (Project i64 Body))
            (sort VecOperandBase (Vec Operand))
            (datatype VecOperand
              (VO VecOperandBase))
            (sort VecVecOperandBase (Vec VecOperand))
            (datatype VecVecOperand
              (VVO VecVecOperandBase))
            (datatype Body
              (Gamma Operand VecOperand VecVecOperand))
            (datatype Interval
              (IntI i64 i64))
            (function ival (Operand) Interval :no-merge)
            (function interval-union (Interval Interval) Interval :no-merge)
            (rule (
                    (= lhs (Project i (Gamma pred ins (VVO outs))))
                    (= (VO thens) (vec-get outs 1))
                    (= (VO elses) (vec-get outs 0))
                    (= thenival (ival (vec-get thens i)))
                    (= elseival (ival (vec-get elses i)))
                  )
                  (
                    (set (ival lhs) (interval-union thenival elseival))
                  ))
        "#;

        let mut parser = Parser::default();
        let commands = parser.get_program_from_string(None, program).unwrap();
        let rust = EggplantCodeGenerator::new()
            .generate_rust(&convert_to_eggplant_with_source(&commands, None));

        assert!(
            !rust.contains("TODO: implement action for rule_"),
            "unexpected TODO in generated Rust:\n{rust}"
        );
        assert!(
            rust.contains(
                "ctx.set_ival(pat.lhs, ctx.insert_interval_union(pat.thenival, pat.elseival));"
            ),
            "generated Rust:\n{rust}"
        );
    }

    #[test]
    fn test_generic_rule_conversion_handles_set_contains_fact_with_complex_element() {
        let program = r#"
            (sort ExprSetPrim (Set Expr))
            (datatype ExprSet
              (ES ExprSetPrim))
            (datatype Expr
              (Seed))
            (relation ExprSet-contains (ExprSet Expr))
            (rule ((ES set1)
                   (set-contains set1 x))
                  ((ExprSet-contains (ES set1) x)))
        "#;

        let mut parser = Parser::default();
        let commands = parser.get_program_from_string(None, program).unwrap();
        let rust = EggplantCodeGenerator::new()
            .generate_rust(&convert_to_eggplant_with_source(&commands, None));

        assert!(
            !rust.contains("TODO: implement action for rule_"),
            "unexpected TODO in generated Rust:\n{rust}"
        );
        assert!(
            rust.contains("let x = Expr::query_leaf();"),
            "generated Rust:\n{rust}"
        );
        assert!(
            rust.contains("prim_fact(\"set-contains\""),
            "generated Rust:\n{rust}"
        );
        assert!(
            rust.contains("ctx.insert_expr_set_contains("),
            "generated Rust:\n{rust}"
        );
    }

    #[test]
    fn test_generic_rule_conversion_handles_constructor_with_wildcard_base_field() {
        let program = r#"
            (datatype IntOrInfinity
              (Infinity)
              (I i64))
            (relation Resolved-IntOrInfinity (IntOrInfinity))
            (rule ((= f (I _)))
                  ((Resolved-IntOrInfinity f)))
        "#;

        let mut parser = Parser::default();
        let commands = parser.get_program_from_string(None, program).unwrap();
        let rust = EggplantCodeGenerator::new()
            .generate_rust(&convert_to_eggplant_with_source(&commands, None));

        assert!(
            !rust.contains("TODO: implement action for rule_"),
            "unexpected TODO in generated Rust:\n{rust}"
        );
        assert!(
            rust.contains("let f = I::query();"),
            "generated Rust:\n{rust}"
        );
        assert!(
            rust.contains("ctx.insert_resolved_int_or_infinity(pat.f);"),
            "generated Rust:\n{rust}"
        );
    }

    #[test]
    fn test_generic_rule_conversion_keeps_literal_guards_for_leaf_constructors() {
        let program = r#"
            (datatype Expr
              (Num String)
              (Var String)
              (Mul Expr Expr)
              (Div Expr Expr)
              (Neg Expr))
            (rule ((= lhs (Mul (Num "-1.0") a)))
                  ((union lhs (Neg a))))
            (rule ((= lhs (Div a (Num "1.0"))))
                  ((union lhs a)))
        "#;

        let mut parser = Parser::default();
        let commands = parser.get_program_from_string(None, program).unwrap();
        let rust = EggplantCodeGenerator::new()
            .generate_rust(&convert_to_eggplant_with_source(&commands, None));

        assert!(
            rust.contains(
                "num_node_1.handle_arg_String_00().eq(&(&(\"-1.0\".to_owned())).as_handle())"
            ),
            "generated Rust:\n{rust}"
        );
        assert!(
            rust.contains(
                "num_node_1.handle_arg_String_00().eq(&(&(\"1.0\".to_owned())).as_handle())"
            ),
            "generated Rust:\n{rust}"
        );
    }

    #[test]
    fn test_generic_rule_conversion_handles_deferred_index_var_inside_constructor_equality() {
        let program = r#"
            (datatype Expr
              (Input)
              (Get Expr i64)
              (Pair Expr Expr)
              (Keep))
            (relation Seen (Expr))
            (rule ((Seen root)
                   (= root (Pair lhs rhs))
                   (= (Get lhs (+ j 1)) (Get rhs j)))
                  ((Seen lhs)))
        "#;

        let mut parser = Parser::default();
        let commands = parser.get_program_from_string(None, program).unwrap();
        let rust = EggplantCodeGenerator::new()
            .generate_rust(&convert_to_eggplant_with_source(&commands, None));

        assert!(
            !rust.contains("TODO: implement action for rule_"),
            "unexpected TODO in generated Rust:\n{rust}"
        );
        assert!(
            rust.contains("let j = BaseVar::<i64, MyPatRec>::query_named(\"j\");"),
            "generated Rust:\n{rust}"
        );
        assert!(
            rust.contains("prim_call::<i64>(\"+\", vec![j.handle().into_handle_ty(), (&(1_i64)).as_handle().into_handle_ty()])"),
            "generated Rust:\n{rust}"
        );
    }

    #[test]
    fn test_generic_rule_conversion_handles_primitive_action_head() {
        let program = r#"
            (datatype Math
              (Num i64)
              (Product Maths))
            (sort Maths (MultiSet Math))
            (function ms-count (Maths Math) i64 :merge (+ old new))
            (sort MSIndexFn (UnstableFn (Maths Math) i64))
            (rule
                ((= outer (Product inner)))
                ((unstable-multiset-fill-index inner (unstable-fn "ms-count"))))
        "#;

        let mut parser = Parser::default();
        let commands = parser.get_program_from_string(None, program).unwrap();
        let rust = EggplantCodeGenerator::new()
            .generate_rust(&convert_to_eggplant_with_source(&commands, None));

        assert!(
            !rust.contains("TODO: implement action for rule_"),
            "unexpected TODO in generated Rust:\n{rust}"
        );
        assert!(
            rust.contains("let _ = unstable_multiset_fill_index("),
            "generated Rust:\n{rust}"
        );
    }

    #[test]
    fn test_generic_rule_conversion_handles_n_ary_plus_in_action_expr() {
        let program = r#"
            (datatype Program
              (Wrap))
            (function program-text (Program) String :no-merge)
            (rule
                ((= p (Wrap)))
                ((set (program-text p) (+ "a" "b" "c"))))
        "#;

        let mut parser = Parser::default();
        let commands = parser.get_program_from_string(None, program).unwrap();
        let rust = EggplantCodeGenerator::new()
            .generate_rust(&convert_to_eggplant_with_source(&commands, None));

        assert!(
            !rust.contains("TODO: implement action for rule_"),
            "unexpected TODO in generated Rust:\n{rust}"
        );
        assert!(
            rust.contains("ctx.set_program_text(pat.p, ((\"a\".to_owned() + \"b\".to_owned()) + \"c\".to_owned()));"),
            "generated Rust:\n{rust}"
        );
    }

    #[test]
    fn test_generic_rule_fallback_bridges_raw_rule_and_normalizes_ruleset() {
        let program = r#"
            (rule ((unknown-rel a b))
                  ((unknown-action a))
                  :ruleset weird-rules)
        "#;

        let mut parser = Parser::default();
        let commands = parser.get_program_from_string(None, program).unwrap();
        let rust = EggplantCodeGenerator::new()
            .generate_rust(&convert_to_eggplant_with_source(&commands, None));

        assert!(
            !rust.contains("TODO: implement action for rule_"),
            "unexpected TODO in generated Rust:\n{rust}"
        );
        assert!(
            rust.contains("parse_and_run_program"),
            "generated Rust:\n{rust}"
        );
        assert!(
            rust.contains("(rule ((unknown_rel a b)) ((unknown_action a)) :ruleset weird_rules)"),
            "generated Rust:\n{rust}"
        );
        assert!(
            !rust.contains(":name \\\"default\\\""),
            "generated Rust:\n{rust}"
        );
    }

    #[test]
    fn test_generic_rule_fallback_bridges_default_ruleset_name() {
        let program = r#"
            (rule ((unknown-rel a b))
                  ((unknown-action a)))
        "#;

        let mut parser = Parser::default();
        let commands = parser.get_program_from_string(None, program).unwrap();
        let rust = EggplantCodeGenerator::new()
            .generate_rust(&convert_to_eggplant_with_source(&commands, None));

        assert!(
            !rust.contains("TODO: implement action for rule_"),
            "unexpected TODO in generated Rust:\n{rust}"
        );
        assert!(
            rust.contains(
                "(rule ((unknown_rel a b)) ((unknown_action a)) :ruleset default_ruleset)"
            ),
            "generated Rust:\n{rust}"
        );
    }

    #[test]
    fn test_check_with_fact_constraints_bridges_raw_egglog_command() {
        let program = r#"
            (check (View c1 c2)
                   (UF_Exp c1 c1_leader)
                   (guard true))
        "#;

        let mut parser = Parser::default();
        let commands = parser.get_program_from_string(None, program).unwrap();
        let rust = EggplantCodeGenerator::new()
            .generate_rust(&convert_to_eggplant_with_source(&commands, None));

        assert!(
            rust.contains("parse_and_run_program"),
            "generated Rust:\n{rust}"
        );
    }

    #[test]
    fn test_birewrite_with_conditions_bridges_raw_egglog_command() {
        let program = r#"
            (birewrite (compose f (id B)) f
                :when ((= (type A) (Ob))
                       (= (type B) (Ob))))
        "#;

        let mut parser = Parser::default();
        let commands = parser.get_program_from_string(None, program).unwrap();
        let rust = EggplantCodeGenerator::new()
            .generate_rust(&convert_to_eggplant_with_source(&commands, None));

        assert!(
            rust.contains("parse_and_run_program"),
            "generated Rust:\n{rust}"
        );
    }

    #[test]
    fn test_full_program_combined_steps_has_no_generic_rule_todo() {
        assert_program_has_no_generic_rule_todo("tests/test-combined-steps.egg");
    }

    #[test]
    fn test_full_program_combinators_has_no_generic_rule_todo() {
        assert_program_has_no_generic_rule_todo("tests/web-demo/combinators.egg");
    }

    #[test]
    fn test_full_program_eqsolve_has_no_generic_rule_todo() {
        assert_program_has_no_generic_rule_todo("tests/web-demo/eqsolve.egg");
    }

    #[test]
    fn test_full_program_herbie_tutorial_has_no_generic_rule_todo() {
        assert_program_has_no_generic_rule_todo("tests/web-demo/herbie-tutorial.egg");
    }

    #[test]
    fn test_full_program_taylor51_has_no_generic_rule_todo() {
        assert_program_has_no_generic_rule_todo("tests/taylor51.egg");
    }

    #[test]
    fn test_full_program_type_constraints_tests_has_no_generic_rule_todo() {
        assert_program_has_no_generic_rule_todo("tests/type-constraints-tests.egg");
    }

    #[test]
    fn test_full_program_rw_analysis_has_no_generic_rule_todo() {
        assert_program_has_no_generic_rule_todo("tests/web-demo/rw-analysis.egg");
    }

    #[test]
    fn test_full_program_typeinfer_has_no_generic_rule_todo() {
        assert_program_has_no_generic_rule_todo("tests/web-demo/typeinfer.egg");
    }

    #[test]
    fn test_full_program_print_function_has_no_todo() {
        assert_program_has_no_generic_rule_todo("tests/print-function.egg");
    }

    #[test]
    fn test_full_program_hidden_print_size_has_no_todo() {
        assert_program_has_no_generic_rule_todo("tests/hidden_print_size.egg");
    }

    #[test]
    fn test_full_program_internal_let_has_no_todo() {
        assert_program_has_no_generic_rule_todo("tests/internal_let.egg");
    }

    #[test]
    fn test_full_program_until_has_no_todo() {
        assert_program_has_no_generic_rule_todo("tests/until.egg");
    }

    #[test]
    fn test_full_program_calc_has_no_todo() {
        assert_program_has_no_generic_rule_todo("tests/calc.egg");
    }

    #[test]
    fn test_full_program_resolution_has_no_todo() {
        assert_program_has_no_generic_rule_todo("tests/web-demo/resolution.egg");
    }

    #[test]
    fn test_full_program_include_has_no_todo() {
        let path = upstream_egglog_fixture("tests/include.egg");
        let program = std::fs::read_to_string(&path).unwrap();
        let mut parser = crate::ast::parse::Parser::default();
        let commands = parser
            .get_program_from_string(Some(path.to_string()), &program)
            .unwrap();
        let rust = EggplantCodeGenerator::new().generate_rust(&convert_to_eggplant_with_source(
            &commands,
            Some(path.to_string()),
        ));
        assert!(
            !rust.contains("TODO"),
            "unexpected TODO lines in generated Rust for {path}:\n{rust}"
        );
    }

    #[test]
    fn test_full_program_egglog_bridge_math_has_no_todo() {
        assert_program_has_no_generic_rule_todo("egglog-bridge/examples/math.egg");
    }

    #[test]
    fn test_full_program_web_demo_math_has_no_todo() {
        assert_program_has_no_generic_rule_todo("tests/web-demo/math.egg");
    }

    #[test]
    fn test_full_program_combined_nested_has_no_todo() {
        assert_program_has_no_generic_rule_todo("tests/combined-nested.egg");
    }

    #[test]
    fn test_full_program_test_combined_steps_has_no_todo() {
        assert_program_has_no_generic_rule_todo("tests/test-combined-steps.egg");
    }

    #[test]
    fn test_full_program_before_proofs_has_no_todo() {
        assert_program_has_no_generic_rule_todo("tests/before-proofs.egg");
    }

    #[test]
    fn test_full_program_eggcc_extraction_has_no_todo() {
        assert_program_has_no_generic_rule_todo("tests/eggcc-extraction.egg");
    }

    #[test]
    fn test_full_program_web_demo_prims_has_no_todo() {
        assert_program_has_no_generic_rule_todo("tests/web-demo/prims.egg");
    }

    #[test]
    fn test_full_program_web_demo_multiset_has_no_todo() {
        assert_program_has_no_generic_rule_todo("tests/web-demo/multiset.egg");
    }

    #[test]
    fn test_full_program_python_array_optimize_has_no_todo() {
        assert_program_has_no_generic_rule_todo("tests/python_array_optimize.egg");
    }

    #[test]
    fn test_full_program_tricky_type_checking_has_no_todo() {
        assert_program_has_no_generic_rule_todo("tests/tricky-type-checking.egg");
    }

    #[test]
    fn test_full_program_web_demo_bignum_has_no_todo() {
        assert_program_has_no_generic_rule_todo("tests/web-demo/bignum.egg");
    }

    #[test]
    fn test_full_program_web_demo_datatypes_has_no_todo() {
        assert_program_has_no_generic_rule_todo("tests/web-demo/datatypes.egg");
    }

    #[test]
    fn test_full_program_web_demo_eqsat_basic_multiset_has_no_todo() {
        assert_program_has_no_generic_rule_todo("tests/web-demo/eqsat-basic-multiset.egg");
    }
}

#[cfg(test)]
fn find_repo_sibling(name: &str) -> Option<std::path::PathBuf> {
    let manifest_dir = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    manifest_dir
        .ancestors()
        .map(|ancestor| ancestor.join(name))
        .find(|candidate| candidate.is_dir())
}

#[cfg(test)]
fn upstream_egglog_fixture(rel: &str) -> String {
    find_repo_sibling("upstream_egglog")
        .unwrap_or_else(|| panic!("could not locate sibling repo `upstream_egglog`"))
        .join(rel)
        .to_string_lossy()
        .into_owned()
}

#[cfg(test)]
fn assert_program_has_no_generic_rule_todo(rel_path: &str) {
    let path = upstream_egglog_fixture(rel_path);
    let program = std::fs::read_to_string(&path).unwrap();

    let mut parser = crate::ast::parse::Parser::default();
    let commands = parser.get_program_from_string(None, &program).unwrap();
    let rust = EggplantCodeGenerator::new()
        .generate_rust(&convert_to_eggplant_with_source(&commands, None));

    let line_count = rust.lines().count();
    let todo_lines: Vec<String> = rust
        .lines()
        .enumerate()
        .filter(|(_, line)| line.contains("TODO"))
        .map(|(index, _)| {
            let start = index.saturating_sub(16);
            let end = (index + 6).min(line_count.saturating_sub(1));
            rust.lines()
                .enumerate()
                .skip(start)
                .take(end - start + 1)
                .map(|(line_index, line)| format!("{:04}: {}", line_index + 1, line))
                .collect::<Vec<_>>()
                .join("\n")
        })
        .collect();
    assert!(
        todo_lines.is_empty(),
        "unexpected TODO lines in generated Rust for {path}:\n{}",
        todo_lines.join("\n")
    );
}

/// Generate a variable for an expression in condition context
fn render_rewrite_condition_handle_expr(
    expr: &Expr,
    dsl_types: &HashMap<String, DslType>,
    pattern_query_parts: &mut Vec<String>,
    pattern_vars_variables: &mut Vec<PatternVariable>,
    node_counter: &mut usize,
    variable_constructors: &mut HashMap<String, (String, String, usize)>,
) -> String {
    match expr {
        Expr::Var(_, var_name) => {
            let normalized_var_name = normalize_identifier(var_name);
            if let Some((constructor_name, node_name, arg_index)) =
                variable_constructors.get(&normalized_var_name)
            {
                let field_name = get_field_name_for_variable_in_constructor(
                    constructor_name,
                    *arg_index,
                    dsl_types,
                );
                format!("{node_name}.handle_{field_name}()")
            } else {
                format!("{normalized_var_name}.handle()")
            }
        }
        Expr::Lit(_, lit) => literal_handle_expr(lit),
        Expr::Call(_, func_name, _) if find_variant_output_type(func_name, dsl_types).is_some() => {
            let node_name = generate_expression_variable(
                expr,
                dsl_types,
                pattern_query_parts,
                pattern_vars_variables,
                node_counter,
                variable_constructors,
            );
            format!("{node_name}.handle()")
        }
        Expr::Call(_, func_name, args) => {
            let output_type = match func_name.as_str() {
                "<" | "<=" | ">" | ">=" | "=" | "==" | "!=" => "bool".to_string(),
                _ => "i64".to_string(),
            };
            let arg_handles = args
                .iter()
                .map(|arg| {
                    format!(
                        "{}.into_handle_ty()",
                        render_rewrite_condition_handle_expr(
                            arg,
                            dsl_types,
                            pattern_query_parts,
                            pattern_vars_variables,
                            node_counter,
                            variable_constructors,
                        )
                    )
                })
                .collect::<Vec<_>>();
            format!(
                "prim_call::<{}>(\"{}\", vec![{}])",
                constraint_type_name(&output_type),
                func_name,
                arg_handles.join(", ")
            )
        }
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
            let is_known_constructor = find_variant_output_type(func_name, dsl_types).is_some()
                || is_basic_type(func_name);

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
