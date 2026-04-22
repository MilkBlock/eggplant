use clap::Parser as ClapParser;
use eggplant_transpiler::ast::parse::Parser as EgglogParser;
use eggplant_transpiler::ast::{Action, Command};
use eggplant_transpiler::{
    CodeGenOptions, EggplantCodeGenerator, convert_to_eggplant_with_source_and_program,
};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::ExitCode;
use syn::{
    Attribute, Expr as SynExpr, ExprLit, Fields, GenericArgument, Item, ItemEnum, ItemFn,
    ItemStruct, Lit, Meta, PathArguments, Stmt, Type as SynType,
};
use walkdir::{DirEntry, WalkDir};

#[derive(Debug, ClapParser)]
#[command(
    about = "Transpile .egg files and Rust raw-string egglog snippets, with optional batch smoke validation."
)]
struct Args {
    #[arg(required = true)]
    inputs: Vec<PathBuf>,
    #[arg(long = "unit-contains")]
    unit_contains: Vec<String>,
    #[arg(long = "unit-name")]
    unit_names: Vec<String>,
    #[arg(long = "prefix-file")]
    prefix_files: Vec<PathBuf>,
    #[arg(long = "prefix-input")]
    prefix_inputs: Vec<PathBuf>,
    #[arg(long = "prefix-unit-contains")]
    prefix_unit_contains: Vec<String>,
    #[arg(long = "prefix-unit-name")]
    prefix_unit_names: Vec<String>,
    #[arg(long)]
    prefix_declarations_only: bool,
    #[arg(long)]
    emit_dir: Option<PathBuf>,
    #[arg(long)]
    fail_on_error: bool,
    #[arg(long)]
    show_generated: bool,
}

#[derive(Debug, Clone)]
struct SourceUnit {
    display_name: String,
    source_name: String,
    source: String,
    output_stem: String,
}

#[derive(Debug, Clone)]
struct Failure {
    unit: String,
    summary: String,
}

enum TranspileOutcome {
    Success(String),
    Failure {
        summary: String,
        generated: Option<String>,
    },
}

fn main() -> ExitCode {
    let args = Args::parse();
    let mut units = Vec::new();
    let mut failures = Vec::new();
    let prefix = match load_prefix(
        &args.prefix_files,
        &args.prefix_inputs,
        &args.prefix_unit_contains,
        &args.prefix_unit_names,
        args.prefix_declarations_only,
    ) {
        Ok(prefix) => prefix,
        Err(err) => {
            eprintln!("failed to load prefix files: {err}");
            return ExitCode::from(2);
        }
    };

    for path in collect_candidate_paths(&args.inputs) {
        match load_source_units(
            &path,
            &prefix,
            &args.unit_contains,
            &args.unit_names,
            args.prefix_declarations_only,
        ) {
            Ok(mut loaded) => units.append(&mut loaded),
            Err(err) => failures.push(Failure {
                unit: path.display().to_string(),
                summary: err,
            }),
        }
    }

    if units.is_empty() {
        eprintln!("no .egg files or raw egglog Rust constants found under the provided inputs");
        return ExitCode::from(2);
    }

    let mut success_count = 0usize;
    for unit in &units {
        match transpile_unit(unit) {
            TranspileOutcome::Success(generated) => {
                success_count += 1;
                if args.show_generated {
                    println!("=== {} ===", unit.display_name);
                    println!("{generated}");
                }
                if let Some(emit_dir) = &args.emit_dir {
                    if let Err(err) = emit_generated_output(emit_dir, unit, &generated) {
                        failures.push(Failure {
                            unit: unit.display_name.clone(),
                            summary: err,
                        });
                    }
                }
            }
            TranspileOutcome::Failure { summary, generated } => {
                if let (Some(emit_dir), Some(generated)) = (&args.emit_dir, generated.as_ref()) {
                    if let Err(err) = emit_generated_output(emit_dir, unit, generated) {
                        failures.push(Failure {
                            unit: unit.display_name.clone(),
                            summary: format!("{summary} (also failed to emit output: {err})"),
                        });
                        continue;
                    }
                }
                failures.push(Failure {
                    unit: unit.display_name.clone(),
                    summary,
                });
            }
        }
    }

    println!("units: {}", units.len());
    println!("successes: {success_count}");
    println!("failures: {}", failures.len());
    for failure in &failures {
        println!("  {} :: {}", failure.unit, failure.summary);
    }

    if args.fail_on_error && !failures.is_empty() {
        ExitCode::from(1)
    } else {
        ExitCode::SUCCESS
    }
}

fn collect_candidate_paths(inputs: &[PathBuf]) -> Vec<PathBuf> {
    let mut paths = Vec::new();

    for input in inputs {
        if input.is_file() {
            if is_supported_source_file(input) {
                paths.push(input.clone());
            }
            continue;
        }

        for entry in WalkDir::new(input)
            .into_iter()
            .filter_entry(should_walk)
            .flatten()
        {
            let path = entry.path();
            if entry.file_type().is_file() && is_supported_source_file(path) {
                paths.push(path.to_path_buf());
            }
        }
    }

    paths.sort();
    paths.dedup();
    paths
}

fn should_walk(entry: &DirEntry) -> bool {
    let path = entry.path();
    if entry.file_type().is_dir() {
        let name = entry.file_name().to_string_lossy();
        if matches!(name.as_ref(), ".git" | "target" | "snapshots") {
            return false;
        }
    }
    !path.to_string_lossy().contains("/target/")
}

fn is_supported_source_file(path: &Path) -> bool {
    matches!(
        path.extension().and_then(|ext| ext.to_str()),
        Some("egg") | Some("rs")
    )
}

fn load_prefix(
    prefix_files: &[PathBuf],
    prefix_inputs: &[PathBuf],
    prefix_unit_contains: &[String],
    prefix_unit_names: &[String],
    declarations_only: bool,
) -> Result<String, String> {
    let mut prefix = String::new();
    for path in prefix_files {
        let contents =
            fs::read_to_string(path).map_err(|err| format!("{}: {err}", path.display()))?;
        let normalized =
            normalize_prefix_source(&path.display().to_string(), &contents, declarations_only)?;
        prefix.push_str(&normalized);
        if !prefix.ends_with('\n') {
            prefix.push('\n');
        }
    }

    for path in collect_candidate_paths(prefix_inputs) {
        let source = fs::read_to_string(&path).map_err(|err| err.to_string())?;
        match path.extension().and_then(|ext| ext.to_str()) {
            Some("egg") => {
                let normalized = normalize_prefix_source(
                    &path.display().to_string(),
                    &source,
                    declarations_only,
                )?;
                prefix.push_str(&normalized);
                if !prefix.ends_with('\n') {
                    prefix.push('\n');
                }
            }
            Some("rs") => {
                if declarations_only {
                    let extra = extract_declaration_prefix_from_rust_source(
                        &path.display().to_string(),
                        &source,
                    )?;
                    prefix.push_str(&extra);
                    if !prefix.is_empty() && !prefix.ends_with('\n') {
                        prefix.push('\n');
                    }
                }
                for unit in extract_egglog_sites_from_rust_source(
                    &path,
                    &source,
                    prefix_unit_contains,
                    prefix_unit_names,
                ) {
                    let normalized = normalize_prefix_source(
                        &unit.source_name,
                        &unit.source,
                        declarations_only,
                    )?;
                    prefix.push_str(&normalized);
                    if !prefix.ends_with('\n') {
                        prefix.push('\n');
                    }
                }
            }
            _ => {}
        }
    }
    Ok(prefix)
}

fn normalize_prefix_source(
    source_name: &str,
    source: &str,
    declarations_only: bool,
) -> Result<String, String> {
    if !declarations_only {
        return Ok(source.to_string());
    }

    let mut parser = EgglogParser::default();
    let parse_outcome = parser
        .get_program_from_string_with_diagnostics(Some(source_name.to_string()), source)
        .map_err(|err| err.to_string())?;

    if !parse_outcome.diagnostics.is_empty() {
        let first = &parse_outcome.diagnostics[0];
        let extra = parse_outcome.diagnostics.len().saturating_sub(1);
        let mut summary = first.1.clone();
        if extra > 0 {
            summary.push_str(&format!(" (+{extra} more diagnostics)"));
        }
        return Err(format!("{source_name}: {summary}"));
    }

    Ok(parse_outcome
        .commands
        .into_iter()
        .filter(is_prefix_prologue_command)
        .map(|command| command.to_string())
        .collect::<Vec<_>>()
        .join("\n"))
}

fn is_prefix_prologue_command(command: &Command) -> bool {
    matches!(
        command,
        Command::Sort(..)
            | Command::Datatype { .. }
            | Command::Function { .. }
            | Command::Constructor { .. }
            | Command::Relation { .. }
            | Command::AddRuleset(..)
            | Command::Action(Action::Let(..))
    )
}

fn apply_prefix(prefix: &str, source: String) -> String {
    if prefix.is_empty() {
        source
    } else {
        format!("{prefix}{source}")
    }
}

fn load_source_units(
    path: &Path,
    prefix: &str,
    unit_contains: &[String],
    unit_names: &[String],
    declarations_only: bool,
) -> Result<Vec<SourceUnit>, String> {
    let source = fs::read_to_string(path).map_err(|err| err.to_string())?;
    match path.extension().and_then(|ext| ext.to_str()) {
        Some("egg") => Ok(vec![SourceUnit {
            display_name: path.display().to_string(),
            source_name: path.display().to_string(),
            source: apply_prefix(prefix, source),
            output_stem: sanitize_output_stem(path.display().to_string()),
        }]),
        Some("rs") => {
            let extracted =
                extract_egglog_sites_from_rust_source(path, &source, unit_contains, unit_names);
            let rust_declarations = if declarations_only {
                extract_declaration_prefix_from_rust_source(&path.display().to_string(), &source)?
            } else {
                String::new()
            };
            let local_prefixes = build_local_declaration_prefixes(&extracted, declarations_only)?;
            let extracted = extracted
                .into_iter()
                .zip(local_prefixes)
                .map(|(mut unit, local_prefix)| {
                    unit.source = apply_prefix(
                        prefix,
                        apply_prefix(&rust_declarations, apply_prefix(&local_prefix, unit.source)),
                    );
                    unit
                })
                .collect::<Vec<_>>();
            if extracted.is_empty() {
                Ok(Vec::new())
            } else {
                Ok(extracted)
            }
        }
        _ => Ok(Vec::new()),
    }
}

fn build_local_declaration_prefixes(
    units: &[SourceUnit],
    declarations_only: bool,
) -> Result<Vec<String>, String> {
    if !declarations_only {
        return Ok(vec![String::new(); units.len()]);
    }

    let normalized_units = units
        .iter()
        .map(|unit| normalize_prefix_source(&unit.source_name, &unit.source, true))
        .collect::<Result<Vec<_>, _>>()?;

    Ok((0..units.len())
        .map(|index| {
            let mut prefix = String::new();
            for (other_index, normalized) in normalized_units.iter().enumerate() {
                if index == other_index || normalized.is_empty() {
                    continue;
                }
                prefix.push_str(normalized);
                if !prefix.ends_with('\n') {
                    prefix.push('\n');
                }
            }
            prefix
        })
        .collect())
}

fn extract_declaration_prefix_from_rust_source(
    source_name: &str,
    source: &str,
) -> Result<String, String> {
    let file = syn::parse_file(source).map_err(|err| format!("{source_name}: {err}"))?;
    let mut commands = Vec::new();

    for item in &file.items {
        match item {
            Item::Enum(item_enum) if has_eggplant_attr(&item_enum.attrs, "dsl") => {
                for command in render_dsl_enum_declarations(item_enum) {
                    push_unique_command(&mut commands, command);
                }
            }
            Item::Struct(item_struct) if has_eggplant_attr(&item_struct.attrs, "relation") => {
                push_unique_command(
                    &mut commands,
                    render_relation_struct_declaration(item_struct),
                );
            }
            Item::Struct(item_struct) if has_eggplant_attr(&item_struct.attrs, "func") => {
                if let Some(command) = render_function_struct_declaration(item_struct) {
                    push_unique_command(&mut commands, command);
                }
            }
            Item::Fn(item_fn) if item_fn.sig.ident == "generated_declarations_section" => {
                for command in render_generated_declarations_section(item_fn) {
                    push_unique_command(&mut commands, command);
                }
            }
            Item::Fn(item_fn) => {
                for command in render_egglog_declarations_from_function_strings(item_fn) {
                    push_unique_command(&mut commands, command);
                }
            }
            _ => {}
        }
    }

    Ok(commands.join("\n"))
}

fn push_unique_command(commands: &mut Vec<String>, command: String) {
    if !command.is_empty() && !commands.iter().any(|existing| existing == &command) {
        commands.push(command);
    }
}

fn has_eggplant_attr(attrs: &[Attribute], kind: &str) -> bool {
    attrs.iter().any(|attr| {
        let segments = &attr.path().segments;
        segments.len() == 2 && segments[0].ident == "eggplant" && segments[1].ident == kind
    })
}

fn render_dsl_enum_declarations(item: &ItemEnum) -> Vec<String> {
    let type_name = item.ident.to_string();
    let mut commands = vec![format!("(datatype {type_name})")];
    for variant in &item.variants {
        let field_types = fields_to_sort_names(&variant.fields);
        commands.push(format_constructor_command(
            &variant.ident.to_string(),
            &field_types,
            &type_name,
        ));
    }
    commands
}

fn render_relation_struct_declaration(item: &ItemStruct) -> String {
    format_relation_command(&item.ident.to_string(), &fields_to_sort_names(&item.fields))
}

fn render_function_struct_declaration(item: &ItemStruct) -> Option<String> {
    let output = item
        .attrs
        .iter()
        .find(|attr| has_eggplant_attr(std::slice::from_ref(attr), "func"))
        .and_then(parse_func_output_attr)?;
    Some(format_function_command(
        &item.ident.to_string(),
        &fields_to_sort_names(&item.fields),
        &output,
    ))
}

fn parse_func_output_attr(attr: &Attribute) -> Option<String> {
    let mut output = None;
    if let Meta::List(_) = &attr.meta {
        let _ = attr.parse_nested_meta(|meta| {
            if meta.path.is_ident("output") {
                let value = meta.value()?;
                let ty: SynType = value.parse()?;
                output = Some(type_to_sort_name(&ty));
            }
            Ok(())
        });
    }
    output
}

fn render_generated_declarations_section(item: &ItemFn) -> Vec<String> {
    let mut commands = Vec::new();
    for stmt in &item.block.stmts {
        collect_generated_commands_from_stmt(stmt, &mut commands);
    }
    commands
}

fn render_egglog_declarations_from_function_strings(item: &ItemFn) -> Vec<String> {
    let mut commands = Vec::new();
    for fragment in collect_egglog_string_fragments_from_block(&item.block.stmts) {
        let sanitized = strip_rust_format_placeholders(&fragment);
        if !looks_like_egglog_program(&sanitized) {
            continue;
        }

        let Ok(normalized) = normalize_prefix_source(
            &format!("function-string:{}", item.sig.ident),
            &sanitized,
            true,
        ) else {
            continue;
        };

        for command in normalized.lines() {
            push_unique_command(&mut commands, command.trim().to_string());
        }
    }
    commands
}

fn collect_egglog_string_fragments_from_block(stmts: &[Stmt]) -> Vec<String> {
    let mut fragments = Vec::new();
    for stmt in stmts {
        collect_egglog_string_fragments_from_stmt(stmt, &mut fragments);
    }
    fragments
}

fn collect_egglog_string_fragments_from_stmt(stmt: &Stmt, fragments: &mut Vec<String>) {
    match stmt {
        Stmt::Local(local) => {
            if let Some(init) = &local.init {
                collect_egglog_string_fragments_from_expr(&init.expr, fragments);
                if let Some((_, diverge)) = &init.diverge {
                    collect_egglog_string_fragments_from_expr(diverge, fragments);
                }
            }
        }
        Stmt::Expr(expr, _) => collect_egglog_string_fragments_from_expr(expr, fragments),
        Stmt::Item(_) | Stmt::Macro(_) => {}
    }
}

fn collect_egglog_string_fragments_from_expr(expr: &SynExpr, fragments: &mut Vec<String>) {
    if let Some(fragment) = string_literal_expr_value(expr) {
        fragments.push(fragment);
    }

    match expr {
        SynExpr::Array(array) => {
            for elem in &array.elems {
                collect_egglog_string_fragments_from_expr(elem, fragments);
            }
        }
        SynExpr::Assign(assign) => {
            collect_egglog_string_fragments_from_expr(&assign.left, fragments);
            collect_egglog_string_fragments_from_expr(&assign.right, fragments);
        }
        SynExpr::Binary(binary) => {
            collect_egglog_string_fragments_from_expr(&binary.left, fragments);
            collect_egglog_string_fragments_from_expr(&binary.right, fragments);
        }
        SynExpr::Block(block) => {
            for stmt in &block.block.stmts {
                collect_egglog_string_fragments_from_stmt(stmt, fragments);
            }
        }
        SynExpr::Call(call) => {
            collect_egglog_string_fragments_from_expr(&call.func, fragments);
            for arg in &call.args {
                collect_egglog_string_fragments_from_expr(arg, fragments);
            }
        }
        SynExpr::Closure(closure) => {
            collect_egglog_string_fragments_from_expr(&closure.body, fragments)
        }
        SynExpr::Field(field) => collect_egglog_string_fragments_from_expr(&field.base, fragments),
        SynExpr::ForLoop(expr_for) => {
            collect_egglog_string_fragments_from_expr(&expr_for.expr, fragments);
            for stmt in &expr_for.body.stmts {
                collect_egglog_string_fragments_from_stmt(stmt, fragments);
            }
        }
        SynExpr::Group(group) => collect_egglog_string_fragments_from_expr(&group.expr, fragments),
        SynExpr::If(expr_if) => {
            collect_egglog_string_fragments_from_expr(&expr_if.cond, fragments);
            for stmt in &expr_if.then_branch.stmts {
                collect_egglog_string_fragments_from_stmt(stmt, fragments);
            }
            if let Some((_, else_branch)) = &expr_if.else_branch {
                collect_egglog_string_fragments_from_expr(else_branch, fragments);
            }
        }
        SynExpr::Index(index) => {
            collect_egglog_string_fragments_from_expr(&index.expr, fragments);
            collect_egglog_string_fragments_from_expr(&index.index, fragments);
        }
        SynExpr::Let(expr_let) => {
            collect_egglog_string_fragments_from_expr(&expr_let.expr, fragments)
        }
        SynExpr::Loop(expr_loop) => {
            for stmt in &expr_loop.body.stmts {
                collect_egglog_string_fragments_from_stmt(stmt, fragments);
            }
        }
        SynExpr::Macro(expr_macro) => {
            if let Some(fragment) = extract_macro_string_literal(&expr_macro.mac) {
                fragments.push(fragment);
            }
        }
        SynExpr::Match(expr_match) => {
            collect_egglog_string_fragments_from_expr(&expr_match.expr, fragments);
            for arm in &expr_match.arms {
                if let Some((_, guard)) = &arm.guard {
                    collect_egglog_string_fragments_from_expr(guard, fragments);
                }
                collect_egglog_string_fragments_from_expr(&arm.body, fragments);
            }
        }
        SynExpr::MethodCall(method_call) => {
            collect_egglog_string_fragments_from_expr(&method_call.receiver, fragments);
            for arg in &method_call.args {
                collect_egglog_string_fragments_from_expr(arg, fragments);
            }
        }
        SynExpr::Paren(paren) => collect_egglog_string_fragments_from_expr(&paren.expr, fragments),
        SynExpr::Reference(reference) => {
            collect_egglog_string_fragments_from_expr(&reference.expr, fragments)
        }
        SynExpr::Repeat(repeat) => {
            collect_egglog_string_fragments_from_expr(&repeat.expr, fragments)
        }
        SynExpr::Return(expr_return) => {
            if let Some(return_expr) = &expr_return.expr {
                collect_egglog_string_fragments_from_expr(return_expr, fragments);
            }
        }
        SynExpr::Struct(expr_struct) => {
            for field in &expr_struct.fields {
                collect_egglog_string_fragments_from_expr(&field.expr, fragments);
            }
            if let Some(rest) = &expr_struct.rest {
                collect_egglog_string_fragments_from_expr(rest, fragments);
            }
        }
        SynExpr::Try(expr_try) => {
            collect_egglog_string_fragments_from_expr(&expr_try.expr, fragments)
        }
        SynExpr::Tuple(tuple) => {
            for elem in &tuple.elems {
                collect_egglog_string_fragments_from_expr(elem, fragments);
            }
        }
        SynExpr::Unary(unary) => collect_egglog_string_fragments_from_expr(&unary.expr, fragments),
        _ => {}
    }
}

fn extract_macro_string_literal(mac: &syn::Macro) -> Option<String> {
    if path_last_ident(&mac.path).as_deref() != Some("format") {
        return None;
    }
    let args = mac
        .parse_body_with(syn::punctuated::Punctuated::<SynExpr, syn::Token![,]>::parse_terminated)
        .ok()?;
    args.first().and_then(string_literal_expr_value)
}

fn string_literal_expr_value(expr: &SynExpr) -> Option<String> {
    match expr {
        SynExpr::Lit(ExprLit {
            lit: Lit::Str(lit_str),
            ..
        }) => Some(lit_str.value()),
        SynExpr::Group(group) => string_literal_expr_value(&group.expr),
        SynExpr::Paren(paren) => string_literal_expr_value(&paren.expr),
        SynExpr::Reference(reference) => string_literal_expr_value(&reference.expr),
        _ => None,
    }
}

fn strip_rust_format_placeholders(source: &str) -> String {
    let mut sanitized = String::new();
    let mut chars = source.chars().peekable();

    while let Some(ch) = chars.next() {
        match ch {
            '{' => {
                if chars.peek() == Some(&'{') {
                    sanitized.push('{');
                    chars.next();
                    continue;
                }

                while let Some(inner) = chars.next() {
                    if inner == '}' {
                        break;
                    }
                }
            }
            '}' => {
                if chars.peek() == Some(&'}') {
                    sanitized.push('}');
                    chars.next();
                }
            }
            _ => sanitized.push(ch),
        }
    }

    sanitized
}

fn collect_generated_commands_from_stmt(stmt: &Stmt, commands: &mut Vec<String>) {
    match stmt {
        Stmt::Local(local) => {
            if let Some(init) = &local.init {
                collect_generated_commands_from_expr(&init.expr, commands);
                if let Some((_, diverge)) = &init.diverge {
                    collect_generated_commands_from_expr(diverge, commands);
                }
            }
        }
        Stmt::Item(_) => {}
        Stmt::Expr(expr, _) => collect_generated_commands_from_expr(expr, commands),
        Stmt::Macro(_) => {}
    }
}

fn collect_generated_commands_from_expr(expr: &SynExpr, commands: &mut Vec<String>) {
    match expr {
        SynExpr::Array(array) => {
            for elem in &array.elems {
                if let Some(command) = render_generated_command_expr(elem) {
                    push_unique_command(commands, command);
                }
                collect_generated_commands_from_expr(elem, commands);
            }
        }
        SynExpr::Assign(assign) => {
            collect_generated_commands_from_expr(&assign.left, commands);
            collect_generated_commands_from_expr(&assign.right, commands);
        }
        SynExpr::Binary(binary) => {
            collect_generated_commands_from_expr(&binary.left, commands);
            collect_generated_commands_from_expr(&binary.right, commands);
        }
        SynExpr::Block(block) => {
            for stmt in &block.block.stmts {
                collect_generated_commands_from_stmt(stmt, commands);
            }
        }
        SynExpr::Call(call) => {
            collect_generated_commands_from_expr(&call.func, commands);
            for arg in &call.args {
                collect_generated_commands_from_expr(arg, commands);
            }
        }
        SynExpr::Closure(closure) => collect_generated_commands_from_expr(&closure.body, commands),
        SynExpr::Field(field) => collect_generated_commands_from_expr(&field.base, commands),
        SynExpr::Group(group) => collect_generated_commands_from_expr(&group.expr, commands),
        SynExpr::If(expr_if) => {
            collect_generated_commands_from_expr(&expr_if.cond, commands);
            for stmt in &expr_if.then_branch.stmts {
                collect_generated_commands_from_stmt(stmt, commands);
            }
            if let Some((_, else_branch)) = &expr_if.else_branch {
                collect_generated_commands_from_expr(else_branch, commands);
            }
        }
        SynExpr::Index(index) => {
            collect_generated_commands_from_expr(&index.expr, commands);
            collect_generated_commands_from_expr(&index.index, commands);
        }
        SynExpr::Let(expr_let) => collect_generated_commands_from_expr(&expr_let.expr, commands),
        SynExpr::Match(expr_match) => {
            collect_generated_commands_from_expr(&expr_match.expr, commands);
            for arm in &expr_match.arms {
                if let Some((_, guard)) = &arm.guard {
                    collect_generated_commands_from_expr(guard, commands);
                }
                collect_generated_commands_from_expr(&arm.body, commands);
            }
        }
        SynExpr::MethodCall(method_call) => {
            collect_generated_commands_from_expr(&method_call.receiver, commands);
            for arg in &method_call.args {
                collect_generated_commands_from_expr(arg, commands);
            }
        }
        SynExpr::Paren(paren) => collect_generated_commands_from_expr(&paren.expr, commands),
        SynExpr::Reference(reference) => {
            collect_generated_commands_from_expr(&reference.expr, commands)
        }
        SynExpr::Return(expr_return) => {
            if let Some(return_expr) = &expr_return.expr {
                collect_generated_commands_from_expr(return_expr, commands);
            }
        }
        SynExpr::Struct(expr_struct) => {
            for field in &expr_struct.fields {
                collect_generated_commands_from_expr(&field.expr, commands);
            }
            if let Some(rest) = &expr_struct.rest {
                collect_generated_commands_from_expr(rest, commands);
            }
        }
        SynExpr::Try(expr_try) => collect_generated_commands_from_expr(&expr_try.expr, commands),
        SynExpr::Unary(unary) => collect_generated_commands_from_expr(&unary.expr, commands),
        _ => {}
    }
}

fn render_generated_command_expr(expr: &SynExpr) -> Option<String> {
    match expr {
        SynExpr::Call(call) => {
            let command_name = expr_path_last_ident(&call.func)?;
            if command_name != "AddRuleset"
                || !path_ends_with(&call.func, &["Command", "AddRuleset"])
            {
                return None;
            }
            let ruleset = call.args.first().and_then(string_expr_value)?;
            Some(format!("(ruleset {ruleset})"))
        }
        SynExpr::Struct(expr_struct) => {
            let command_name = path_last_ident(&expr_struct.path)?;
            match command_name.as_str() {
                "Function" => {
                    let name =
                        struct_field_value(expr_struct, "name").and_then(string_expr_value)?;
                    let schema =
                        struct_field_value(expr_struct, "schema").and_then(schema_expr_value)?;
                    Some(format_function_command(&name, &schema.0, &schema.1))
                }
                "Constructor" => {
                    let name =
                        struct_field_value(expr_struct, "name").and_then(string_expr_value)?;
                    let schema =
                        struct_field_value(expr_struct, "schema").and_then(schema_expr_value)?;
                    Some(format_constructor_command(&name, &schema.0, &schema.1))
                }
                "Relation" => {
                    let name =
                        struct_field_value(expr_struct, "name").and_then(string_expr_value)?;
                    let schema =
                        struct_field_value(expr_struct, "schema").and_then(schema_expr_value)?;
                    Some(format_relation_command(&name, &schema.0))
                }
                _ => None,
            }
        }
        _ => None,
    }
}

fn struct_field_value<'a>(expr_struct: &'a syn::ExprStruct, name: &str) -> Option<&'a SynExpr> {
    expr_struct
        .fields
        .iter()
        .find_map(|field| match &field.member {
            syn::Member::Named(ident) if ident == name => Some(&field.expr),
            _ => None,
        })
}

fn schema_expr_value(expr: &SynExpr) -> Option<(Vec<String>, String)> {
    let SynExpr::Call(call) = expr else {
        return None;
    };
    if expr_path_last_ident(&call.func)? != "schema" {
        return None;
    }
    let mut args = call.args.iter();
    let inputs = string_array_expr_value(args.next()?)?;
    let output = string_expr_value(args.next()?)?;
    Some((inputs, output))
}

fn string_array_expr_value(expr: &SynExpr) -> Option<Vec<String>> {
    match expr {
        SynExpr::Array(array) => array.elems.iter().map(string_expr_value).collect(),
        SynExpr::Reference(reference) => string_array_expr_value(&reference.expr),
        SynExpr::Paren(paren) => string_array_expr_value(&paren.expr),
        _ => None,
    }
}

fn string_expr_value(expr: &SynExpr) -> Option<String> {
    match expr {
        SynExpr::Lit(ExprLit {
            lit: Lit::Str(value),
            ..
        }) => Some(value.value()),
        SynExpr::MethodCall(method_call) if method_call.method == "into" => {
            string_expr_value(&method_call.receiver)
        }
        SynExpr::Paren(paren) => string_expr_value(&paren.expr),
        SynExpr::Reference(reference) => string_expr_value(&reference.expr),
        _ => None,
    }
}

fn fields_to_sort_names(fields: &Fields) -> Vec<String> {
    match fields {
        Fields::Named(named) => named
            .named
            .iter()
            .map(|field| type_to_sort_name(&field.ty))
            .collect(),
        Fields::Unnamed(unnamed) => unnamed
            .unnamed
            .iter()
            .map(|field| type_to_sort_name(&field.ty))
            .collect(),
        Fields::Unit => Vec::new(),
    }
}

fn type_to_sort_name(ty: &SynType) -> String {
    match ty {
        SynType::Path(type_path) => {
            let Some(segment) = type_path.path.segments.last() else {
                return "Unknown".to_string();
            };
            match &segment.arguments {
                PathArguments::None => segment.ident.to_string(),
                PathArguments::AngleBracketed(arguments) => {
                    let inner = arguments
                        .args
                        .iter()
                        .filter_map(|arg| match arg {
                            GenericArgument::Type(inner_ty) => Some(type_to_sort_name(inner_ty)),
                            _ => None,
                        })
                        .collect::<Vec<_>>();
                    if inner.is_empty() {
                        segment.ident.to_string()
                    } else {
                        format!("{}_{}", segment.ident, inner.join("_"))
                    }
                }
                _ => segment.ident.to_string(),
            }
        }
        SynType::Reference(reference) => type_to_sort_name(&reference.elem),
        _ => "Unknown".to_string(),
    }
}

fn format_constructor_command(name: &str, field_types: &[String], output_type: &str) -> String {
    format!(
        "(constructor {name} ({}) {output_type})",
        join_sort_names(field_types)
    )
}

fn format_relation_command(name: &str, field_types: &[String]) -> String {
    format!("(relation {name} ({}))", join_sort_names(field_types))
}

fn format_function_command(name: &str, field_types: &[String], output_type: &str) -> String {
    format!(
        "(function {name} ({}) {output_type} :no-merge)",
        join_sort_names(field_types)
    )
}

fn join_sort_names(field_types: &[String]) -> String {
    if field_types.is_empty() {
        String::new()
    } else {
        field_types.join(" ")
    }
}

fn expr_path_last_ident(expr: &SynExpr) -> Option<String> {
    match expr {
        SynExpr::Path(path) => path_last_ident(&path.path),
        SynExpr::Paren(paren) => expr_path_last_ident(&paren.expr),
        _ => None,
    }
}

fn path_last_ident(path: &syn::Path) -> Option<String> {
    path.segments
        .last()
        .map(|segment| segment.ident.to_string())
}

fn path_ends_with(expr: &SynExpr, suffix: &[&str]) -> bool {
    let SynExpr::Path(path) = expr else {
        return false;
    };
    let segments = path
        .path
        .segments
        .iter()
        .map(|segment| segment.ident.to_string())
        .collect::<Vec<_>>();
    segments.ends_with(
        &suffix
            .iter()
            .map(|value| value.to_string())
            .collect::<Vec<_>>(),
    )
}

fn extract_egglog_sites_from_rust_source(
    path: &Path,
    source: &str,
    unit_contains: &[String],
    unit_names: &[String],
) -> Vec<SourceUnit> {
    let mut units = Vec::new();
    let mut offset = 0usize;

    while let Some(relative_const_index) = source[offset..].find("const ") {
        let const_index = offset + relative_const_index;
        let name_start = const_index + "const ".len();
        let Some(colon_index) = source[name_start..].find(':') else {
            break;
        };
        let colon_index = name_start + colon_index;
        let const_name = source[name_start..colon_index].trim();
        if const_name.is_empty()
            || !const_name
                .chars()
                .all(|ch| ch.is_ascii_uppercase() || ch.is_ascii_digit() || ch == '_')
        {
            offset = name_start;
            continue;
        }

        let Some(equal_index) = source[colon_index..].find('=') else {
            break;
        };
        let equal_index = colon_index + equal_index;
        let mut cursor = equal_index + 1;
        while let Some(ch) = source[cursor..].chars().next() {
            if ch.is_whitespace() {
                cursor += ch.len_utf8();
            } else {
                break;
            }
        }

        let Some((raw_contents, next_offset)) = parse_raw_string_literal(source, cursor) else {
            offset = cursor;
            continue;
        };

        if looks_like_complete_egglog_program(&raw_contents)
            && unit_matches_filter(const_name, unit_contains, unit_names)
        {
            let display_name = format!("{}::{const_name}", path.display());
            let output_stem = sanitize_output_stem(format!("{}__{const_name}", path.display()));
            units.push(SourceUnit {
                display_name: display_name.clone(),
                source_name: display_name,
                source: raw_contents,
                output_stem,
            });
        }

        offset = next_offset;
    }

    units
}

fn unit_matches_filter(const_name: &str, unit_contains: &[String], unit_names: &[String]) -> bool {
    let matches_contains = unit_contains.is_empty()
        || unit_contains
            .iter()
            .any(|needle| !needle.is_empty() && const_name.contains(needle));
    let matches_name = unit_names.is_empty()
        || unit_names
            .iter()
            .any(|name| !name.is_empty() && const_name == name);

    matches_contains && matches_name
}

fn parse_raw_string_literal(source: &str, start: usize) -> Option<(String, usize)> {
    if !source.get(start..)?.starts_with('r') {
        return None;
    }

    let mut cursor = start + 1;
    let mut hash_count = 0usize;
    while source.get(cursor..)?.starts_with('#') {
        hash_count += 1;
        cursor += 1;
    }

    if !source.get(cursor..)?.starts_with('"') {
        return None;
    }
    cursor += 1;

    let closing_delim = format!("\"{}", "#".repeat(hash_count));
    let close_relative = source.get(cursor..)?.find(&closing_delim)?;
    let close_index = cursor + close_relative;
    let contents = source[cursor..close_index].to_string();
    Some((contents, close_index + closing_delim.len()))
}

fn looks_like_egglog_program(source: &str) -> bool {
    [
        "(datatype",
        "(datatype*",
        "(sort",
        "(function",
        "(relation",
        "(constructor",
        "(ruleset",
        "(rule",
        "(rewrite",
        "(birewrite",
        "(run",
        "(run-schedule",
        "(check",
    ]
    .iter()
    .any(|needle| source.contains(needle))
}

fn looks_like_complete_egglog_program(source: &str) -> bool {
    if !looks_like_egglog_program(source) {
        return false;
    }

    let mut depth = 0usize;
    let mut in_comment = false;
    let mut in_string = false;
    let mut escape_next = false;

    for ch in source.chars() {
        if in_comment {
            if ch == '\n' {
                in_comment = false;
            }
            continue;
        }

        if in_string {
            if escape_next {
                escape_next = false;
                continue;
            }
            match ch {
                '\\' => escape_next = true,
                '"' => in_string = false,
                _ => {}
            }
            continue;
        }

        match ch {
            ';' => in_comment = true,
            '"' => in_string = true,
            '(' => depth += 1,
            ')' => {
                if depth == 0 {
                    return false;
                }
                depth -= 1;
            }
            _ => {}
        }
    }

    !in_string && !escape_next && depth == 0
}

fn transpile_unit(unit: &SourceUnit) -> TranspileOutcome {
    let mut parser = EgglogParser::default();
    let parse_outcome = parser
        .get_program_from_string_with_diagnostics(Some(unit.source_name.clone()), &unit.source)
        .map_err(|err| err.to_string());

    let parse_outcome = match parse_outcome {
        Ok(parse_outcome) => parse_outcome,
        Err(summary) => {
            return TranspileOutcome::Failure {
                summary,
                generated: None,
            };
        }
    };

    if !parse_outcome.diagnostics.is_empty() {
        let first = &parse_outcome.diagnostics[0];
        let extra = parse_outcome.diagnostics.len().saturating_sub(1);
        let mut summary = first.1.clone();
        if extra > 0 {
            summary.push_str(&format!(" (+{extra} more diagnostics)"));
        }
        return TranspileOutcome::Failure {
            summary,
            generated: None,
        };
    }

    let lowered = convert_to_eggplant_with_source_and_program(
        &parse_outcome.commands,
        Some(unit.source_name.clone()),
    );
    let mut generator = EggplantCodeGenerator::with_options(CodeGenOptions {
        omit_head_annotation: true,
        ..CodeGenOptions::default()
    });
    let generated = generator.generate_rust(&lowered);

    if let Some(todo_line) = generated.lines().find(|line| line.contains("TODO")) {
        return TranspileOutcome::Failure {
            summary: todo_line.trim().to_string(),
            generated: Some(generated),
        };
    }

    TranspileOutcome::Success(generated)
}

fn emit_generated_output(
    emit_dir: &Path,
    unit: &SourceUnit,
    generated: &str,
) -> Result<(), String> {
    fs::create_dir_all(emit_dir).map_err(|err| err.to_string())?;
    let output_path = emit_dir.join(format!("{}.rs", unit.output_stem));
    fs::write(output_path, generated).map_err(|err| err.to_string())
}

fn sanitize_output_stem(input: String) -> String {
    input
        .chars()
        .map(|ch| {
            if ch.is_ascii_alphanumeric() || ch == '_' {
                ch
            } else {
                '_'
            }
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_raw_string_literal_supports_hash_variants() {
        let source = "const EXPR: &str = r###\"(rule ((A)) ((B)))\"###;";
        let (raw, _) = parse_raw_string_literal(source, source.find("r###").unwrap()).unwrap();
        assert_eq!(raw, "(rule ((A)) ((B)))");
    }

    #[test]
    fn test_extract_egglog_sites_from_rust_source_filters_non_egglog_consts() {
        let path = Path::new("/tmp/sample.rs");
        let source = r###"
            const NOTE: &str = r#"hello world"#;
            const PROGRAM: &str = r#"(ruleset demo)
            (rule ((A x)) ((B x)) :ruleset demo)"#;
        "###;

        let units = extract_egglog_sites_from_rust_source(path, source, &[], &[]);
        assert_eq!(units.len(), 1);
        assert_eq!(units[0].display_name, "/tmp/sample.rs::PROGRAM");
        assert!(units[0].source.contains("(ruleset demo)"));
    }

    #[test]
    fn test_extract_egglog_sites_from_rust_source_applies_const_name_filter() {
        let path = Path::new("/tmp/sample.rs");
        let source = r###"
            const DECLARATIONS: &str = r#"(ruleset demo)"#;
            const RULES: &str = r#"(rule ((A x)) ((B x)) :ruleset demo)"#;
        "###;

        let units =
            extract_egglog_sites_from_rust_source(path, source, &[String::from("DECLAR")], &[]);
        assert_eq!(units.len(), 1);
        assert_eq!(units[0].display_name, "/tmp/sample.rs::DECLARATIONS");
    }

    #[test]
    fn test_extract_egglog_sites_from_rust_source_applies_exact_const_name_filter() {
        let path = Path::new("/tmp/sample.rs");
        let source = r###"
            const DECLARATIONS: &str = r#"(ruleset demo)"#;
            const RULES: &str = r#"(rule ((A x)) ((B x)) :ruleset demo)"#;
        "###;

        let units =
            extract_egglog_sites_from_rust_source(path, source, &[], &[String::from("RULES")]);
        assert_eq!(units.len(), 1);
        assert_eq!(units[0].display_name, "/tmp/sample.rs::RULES");
    }

    #[test]
    fn test_extract_egglog_sites_from_rust_source_exact_and_contains_narrow_together() {
        let path = Path::new("/tmp/sample.rs");
        let source = r###"
            const DECLARATIONS: &str = r#"(ruleset demo)"#;
            const RULES: &str = r#"(rule ((A x)) ((B x)) :ruleset demo)"#;
        "###;

        let units = extract_egglog_sites_from_rust_source(
            path,
            source,
            &[String::from("DECLAR")],
            &[String::from("RULES")],
        );
        assert!(units.is_empty());
    }

    #[test]
    fn test_extract_egglog_sites_from_rust_source_ignores_partial_marker_consts() {
        let path = Path::new("/tmp/schema.rs");
        let source = r###"
            const OPERATORS_CONSTRUCTORS_START: &str = r#"; Operators
            (constructor Top"#;
            const SCHEMA: &str = r#"
                (datatype Expr)
                (constructor Top () Expr)
            "#;
        "###;

        let units = extract_egglog_sites_from_rust_source(path, source, &[], &[]);
        assert_eq!(units.len(), 1);
        assert_eq!(units[0].display_name, "/tmp/schema.rs::SCHEMA");
        assert!(units[0].source.contains("(constructor Top () Expr)"));
    }

    #[test]
    fn test_extract_egglog_sites_from_rust_source_keeps_balanced_invalid_programs() {
        let path = Path::new("/tmp/schema.rs");
        let source = r###"
            const BROKEN: &str = r#"(constructor Top () )"#;
        "###;

        let units = extract_egglog_sites_from_rust_source(path, source, &[], &[]);
        assert_eq!(units.len(), 1);
        assert_eq!(units[0].display_name, "/tmp/schema.rs::BROKEN");
        assert_eq!(units[0].source.trim(), "(constructor Top () )");
    }

    #[test]
    fn test_apply_prefix_prepends_source() {
        let prefixed = apply_prefix("(ruleset seed)\n", "(rule ((A)) ((B)))".to_string());
        assert!(prefixed.starts_with("(ruleset seed)\n"));
        assert!(prefixed.contains("(rule ((A)) ((B)))"));
    }

    #[test]
    fn test_normalize_prefix_source_declarations_only_filters_rules() {
        let source = r#"
            (ruleset demo)
            (constructor A () Expr)
            (relation Seen (Expr))
            (let seed (A))
            (rule ((Seen x)) ((delete (Seen x))) :ruleset demo)
        "#;

        let filtered = normalize_prefix_source("/tmp/prefix.egg", source, true).unwrap();
        assert!(filtered.contains("(ruleset demo)"));
        assert!(filtered.contains("(constructor A () Expr)"));
        assert!(filtered.contains("(relation Seen (Expr))"));
        assert!(filtered.contains("(let seed (A"));
        assert!(!filtered.contains("(rule ("));
        assert!(!filtered.contains("(delete"));
    }

    #[test]
    fn test_build_local_declaration_prefixes_uses_sibling_declarations_only() {
        let units = vec![
            SourceUnit {
                display_name: "/tmp/sample.rs::DECLS".to_string(),
                source_name: "/tmp/sample.rs::DECLS".to_string(),
                source: "(ruleset demo)\n(constructor A () Expr)\n".to_string(),
                output_stem: "decls".to_string(),
            },
            SourceUnit {
                display_name: "/tmp/sample.rs::RULE".to_string(),
                source_name: "/tmp/sample.rs::RULE".to_string(),
                source: "(rule ((= lhs (A))) ((A)) :ruleset demo)\n".to_string(),
                output_stem: "rule".to_string(),
            },
        ];

        let prefixes = build_local_declaration_prefixes(&units, true).unwrap();
        assert_eq!(prefixes.len(), 2);
        assert_eq!(prefixes[0], "");
        assert!(prefixes[1].contains("(ruleset demo)"));
        assert!(prefixes[1].contains("(constructor A () Expr)"));
        assert!(!prefixes[1].contains("(rule ("));
    }

    #[test]
    fn test_extract_declaration_prefix_from_rust_source_reads_eggplant_macros() {
        let source = r#"
            #[eggplant::dsl]
            enum Expr {
                Nil {},
                Pair { lhs: Expr, rhs: Expr },
            }

            #[eggplant::relation]
            struct Seen {
                expr: Expr,
                depth: i64,
            }

            #[eggplant::func(output = Expr, no_merge)]
            struct Parent {
                child: Expr,
            }
        "#;

        let prefix =
            extract_declaration_prefix_from_rust_source("/tmp/schema_dsl.rs", source).unwrap();
        assert!(prefix.contains("(datatype Expr)"));
        assert!(prefix.contains("(constructor Nil () Expr)"));
        assert!(prefix.contains("(constructor Pair (Expr Expr) Expr)"));
        assert!(prefix.contains("(relation Seen (Expr i64))"));
        assert!(prefix.contains("(function Parent (Expr) Expr :no-merge)"));
    }

    #[test]
    fn test_extract_declaration_prefix_from_rust_source_reads_generated_command_arrays() {
        let source = r#"
            use egglog::ast::{Command, Schema, Span};

            fn generated_declarations_section() -> String {
                [
                    Command::AddRuleset("demo".into()),
                    Command::Function {
                        span: Span::Panic,
                        name: "tuple-length".into(),
                        schema: schema(&["Expr"], "i64"),
                        merge: None,
                    },
                    Command::Constructor {
                        span: Span::Panic,
                        name: "TmpCtx".into(),
                        schema: schema(&[], "Assumption"),
                        cost: None,
                        unextractable: false,
                    },
                ]
                .into_iter()
                .map(|command| command.to_string())
                .collect::<Vec<_>>()
                .join("\n")
            }
        "#;

        let prefix = extract_declaration_prefix_from_rust_source("/tmp/util.rs", source).unwrap();
        assert!(prefix.contains("(ruleset demo)"));
        assert!(prefix.contains("(function tuple-length (Expr) i64 :no-merge)"));
        assert!(prefix.contains("(constructor TmpCtx () Assumption)"));
    }

    #[test]
    fn test_extract_declaration_prefix_from_rust_source_reads_rules_format_fragments() {
        let source = r###"
            fn rules() -> String {
                format!(
                    r#"
                        (datatype IntOrInfinity
                            (Infinity)
                            (I i64))
                        {extra}
                        (constructor PointsTo (Expr) Expr)
                        (rule ((PointsTo x)) ((panic "unused")))
                    "#
                )
            }
        "###;

        let prefix = extract_declaration_prefix_from_rust_source("/tmp/memory.rs", source).unwrap();
        assert!(prefix.contains("(datatype IntOrInfinity (Infinity) (I i64))"));
        assert!(prefix.contains("(constructor PointsTo (Expr) Expr)"));
        assert!(!prefix.contains("(rule ("));
    }

    #[test]
    fn test_strip_rust_format_placeholders_keeps_literal_braces() {
        let source = "a {value} b {{keep}} c";
        assert_eq!(strip_rust_format_placeholders(source), "a  b {keep} c");
    }
}
