use clap::Parser as ClapParser;
use eggplant_transpiler::ast::parse::Parser as EgglogParser;
use eggplant_transpiler::{
    CodeGenOptions, Command, EggplantCodeGenerator, convert_to_eggplant_with_source_and_program,
};
use std::any::Any;
use std::collections::BTreeMap;
use std::fs;
use std::panic::{self, AssertUnwindSafe};
use std::path::{Path, PathBuf};
use std::process::ExitCode;
use walkdir::{DirEntry, WalkDir};

#[derive(Debug, ClapParser)]
#[command(about = "Sweep egglog programs through parse -> lower -> codegen and bucket failures.")]
struct Args {
    #[arg(required = true)]
    roots: Vec<PathBuf>,
    #[arg(long = "exclude-subpath")]
    exclude_subpaths: Vec<String>,
    #[arg(long, default_value_t = 8)]
    top: usize,
    #[arg(long)]
    fail_on_error: bool,
    #[arg(long)]
    show_failures: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum FailureStage {
    Read,
    Parse,
    Lower,
    Codegen,
    Todo,
}

impl FailureStage {
    const ALL: [Self; 5] = [
        Self::Read,
        Self::Parse,
        Self::Lower,
        Self::Codegen,
        Self::Todo,
    ];
}

impl std::fmt::Display for FailureStage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let label = match self {
            Self::Read => "read",
            Self::Parse => "parse",
            Self::Lower => "lower",
            Self::Codegen => "codegen",
            Self::Todo => "todo",
        };
        write!(f, "{label}")
    }
}

#[derive(Debug, Clone)]
struct FailureRecord {
    path: PathBuf,
    stage: FailureStage,
    summary: String,
}

#[derive(Debug, Default)]
struct Bucket {
    count: usize,
    samples: Vec<PathBuf>,
}

fn main() -> ExitCode {
    let args = Args::parse();
    let files = collect_egg_files(&args.roots, &args.exclude_subpaths);
    if files.is_empty() {
        eprintln!("no .egg files found under the provided roots");
        return ExitCode::from(2);
    }

    let mut failures = Vec::new();
    let mut success_count = 0usize;

    for path in &files {
        match sweep_file(path) {
            Ok(()) => success_count += 1,
            Err(failure) => failures.push(failure),
        }
    }

    print_summary(&args, &files, success_count, &failures);

    if args.fail_on_error && !failures.is_empty() {
        ExitCode::from(1)
    } else {
        ExitCode::SUCCESS
    }
}

fn collect_egg_files(roots: &[PathBuf], exclude_subpaths: &[String]) -> Vec<PathBuf> {
    let mut files = Vec::new();

    for root in roots {
        if should_skip_path(root, exclude_subpaths) {
            continue;
        }

        if root.is_file() {
            if root.extension().is_some_and(|ext| ext == "egg") {
                files.push(root.clone());
            }
            continue;
        }

        for entry in WalkDir::new(root)
            .into_iter()
            .filter_entry(|entry| should_walk(entry, exclude_subpaths))
        {
            let Ok(entry) = entry else {
                continue;
            };
            let path = entry.path();
            if entry.file_type().is_file()
                && path.extension().is_some_and(|ext| ext == "egg")
                && !should_skip_path(path, exclude_subpaths)
            {
                files.push(path.to_path_buf());
            }
        }
    }

    files.sort();
    files.dedup();
    files
}

fn should_walk(entry: &DirEntry, exclude_subpaths: &[String]) -> bool {
    let path = entry.path();
    if entry.file_type().is_dir() {
        let name = entry.file_name().to_string_lossy();
        if matches!(name.as_ref(), ".git" | "target" | "snapshots") {
            return false;
        }
    }
    !should_skip_path(path, exclude_subpaths)
}

fn should_skip_path(path: &Path, exclude_subpaths: &[String]) -> bool {
    let path_str = path.to_string_lossy();
    exclude_subpaths
        .iter()
        .any(|needle| !needle.is_empty() && path_str.contains(needle))
}

fn sweep_file(path: &Path) -> Result<(), FailureRecord> {
    let source = fs::read_to_string(path).map_err(|err| FailureRecord {
        path: path.to_path_buf(),
        stage: FailureStage::Read,
        summary: err.to_string(),
    })?;

    let mut parser = EgglogParser::default();
    let parse_outcome = parser
        .get_program_from_string_with_diagnostics(Some(path.display().to_string()), &source)
        .map_err(|err| FailureRecord {
            path: path.to_path_buf(),
            stage: FailureStage::Parse,
            summary: err.to_string(),
        })?;

    if !parse_outcome.diagnostics.is_empty() {
        let first = &parse_outcome.diagnostics[0];
        let extra = parse_outcome.diagnostics.len().saturating_sub(1);
        let mut summary = first.1.clone();
        if extra > 0 {
            summary.push_str(&format!(" (+{extra} more diagnostics)"));
        }
        return Err(FailureRecord {
            path: path.to_path_buf(),
            stage: FailureStage::Parse,
            summary,
        });
    }

    if let Some(summary) = first_unsupported_command(&parse_outcome.commands) {
        return Err(FailureRecord {
            path: path.to_path_buf(),
            stage: FailureStage::Todo,
            summary,
        });
    }

    let lowered = catch_unwind_silent(AssertUnwindSafe(|| {
        convert_to_eggplant_with_source_and_program(
            &parse_outcome.commands,
            Some(path.display().to_string()),
        )
    }))
    .map_err(|payload| FailureRecord {
        path: path.to_path_buf(),
        stage: FailureStage::Lower,
        summary: panic_message(payload),
    })?;

    let mut generator = EggplantCodeGenerator::with_options(CodeGenOptions {
        omit_head_annotation: true,
        ..CodeGenOptions::default()
    });
    let generated = catch_unwind_silent(AssertUnwindSafe(|| generator.generate_rust(&lowered)))
        .map_err(|payload| FailureRecord {
            path: path.to_path_buf(),
            stage: FailureStage::Codegen,
            summary: panic_message(payload),
        })?;

    if let Some(todo_line) = generated.lines().find(|line| line.contains("TODO")) {
        return Err(FailureRecord {
            path: path.to_path_buf(),
            stage: FailureStage::Todo,
            summary: todo_line.trim().to_string(),
        });
    }

    Ok(())
}

fn first_unsupported_command(commands: &[Command]) -> Option<String> {
    for command in commands {
        match command {
            Command::Include(_, _) => {
                continue;
            }
            Command::Input { .. } => return Some("input lowering not implemented".to_string()),
            Command::Output { .. } => return Some("output lowering not implemented".to_string()),
            _ => {}
        }
    }
    None
}

fn panic_message(payload: Box<dyn Any + Send>) -> String {
    match payload.downcast::<String>() {
        Ok(message) => *message,
        Err(payload) => match payload.downcast::<&'static str>() {
            Ok(message) => (*message).to_string(),
            Err(_) => "panic without string payload".to_string(),
        },
    }
}

fn catch_unwind_silent<F, T>(f: F) -> std::thread::Result<T>
where
    F: FnOnce() -> T + panic::UnwindSafe,
{
    let hook = panic::take_hook();
    panic::set_hook(Box::new(|_| {}));
    let result = panic::catch_unwind(f);
    panic::set_hook(hook);
    result
}

fn print_summary(args: &Args, files: &[PathBuf], success_count: usize, failures: &[FailureRecord]) {
    let mut stage_counts: BTreeMap<FailureStage, usize> = BTreeMap::new();
    let mut buckets: BTreeMap<(FailureStage, String), Bucket> = BTreeMap::new();

    for failure in failures {
        *stage_counts.entry(failure.stage).or_default() += 1;
        let key = (failure.stage, normalize_message(&failure.summary));
        let bucket = buckets.entry(key).or_default();
        bucket.count += 1;
        if bucket.samples.len() < 3 {
            bucket.samples.push(failure.path.clone());
        }
    }

    println!("roots:");
    for root in &args.roots {
        println!("  {}", root.display());
    }
    if !args.exclude_subpaths.is_empty() {
        println!("excluded subpaths:");
        for excluded in &args.exclude_subpaths {
            println!("  {excluded}");
        }
    }
    println!("files: {}", files.len());
    println!("successes: {}", success_count);
    println!("failures: {}", failures.len());
    for stage in FailureStage::ALL {
        let count = stage_counts.get(&stage).copied().unwrap_or(0);
        println!("  {stage}: {count}");
    }

    if !buckets.is_empty() {
        let mut bucket_rows: Vec<_> = buckets.into_iter().collect();
        bucket_rows.sort_by(|left, right| {
            right
                .1
                .count
                .cmp(&left.1.count)
                .then_with(|| left.0.cmp(&right.0))
        });

        println!("top failure buckets:");
        for ((stage, summary), bucket) in bucket_rows.into_iter().take(args.top) {
            println!("  [{stage}] {} ({})", summary, bucket.count);
            for sample in bucket.samples {
                println!("    {}", sample.display());
            }
        }
    }

    if args.show_failures && !failures.is_empty() {
        println!("failures by file:");
        for failure in failures {
            println!(
                "  [{}] {} :: {}",
                failure.stage,
                failure.path.display(),
                failure.summary
            );
        }
    }
}

fn normalize_message(message: &str) -> String {
    let mut normalized = String::with_capacity(message.len());
    let mut in_number = false;
    let mut last_was_space = false;

    for ch in message.chars() {
        if ch.is_ascii_digit() {
            if !in_number {
                normalized.push_str("<n>");
                in_number = true;
                last_was_space = false;
            }
            continue;
        }

        in_number = false;
        if ch.is_whitespace() {
            if !last_was_space {
                normalized.push(' ');
                last_was_space = true;
            }
        } else {
            normalized.push(ch);
            last_was_space = false;
        }
    }

    normalized.trim().to_string()
}
