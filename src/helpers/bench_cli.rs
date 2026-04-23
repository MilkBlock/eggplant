use clap::{Arg, Command, value_parser};

pub const DEFAULT_EXTRACTORS: &str = "default,eboost,layered,rustsat";

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExtractBenchCliArgs {
    pub rewrite_iters: usize,
    pub max_rewrite_mem_gib: u64,
    pub extractors: Vec<String>,
    pub max_extract_time_secs: Option<u64>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TimelineExportCliArgs {
    pub rewrite_iters: usize,
    pub max_rewrite_mem_gib: u64,
    pub json_out: String,
    pub version_nickname: Option<String>,
    pub extractors: Vec<String>,
    pub max_extract_time_secs: Option<u64>,
}

fn split_extractors(raw: Option<&String>) -> Vec<String> {
    raw.map(|value| {
        value
            .split(',')
            .filter(|entry| !entry.is_empty())
            .map(ToOwned::to_owned)
            .collect()
    })
    .unwrap_or_default()
}

fn leak_string(value: String) -> &'static str {
    Box::leak(value.into_boxed_str())
}

pub fn parse_extract_bench_args<I, T>(
    name: &'static str,
    about: &'static str,
    default_rewrite_iters: usize,
    default_max_rewrite_mem_gib: u64,
    args: I,
) -> ExtractBenchCliArgs
where
    I: IntoIterator<Item = T>,
    T: Into<std::ffi::OsString> + Clone,
{
    let matches = Command::new(name)
        .about(about)
        .arg(
            Arg::new("max_iter")
                .long("max_iter")
                .value_parser(value_parser!(usize))
                .default_value(leak_string(default_rewrite_iters.to_string())),
        )
        .arg(
            Arg::new("max_mem")
                .long("max_mem")
                .value_parser(value_parser!(u64))
                .default_value(leak_string(default_max_rewrite_mem_gib.to_string())),
        )
        .arg(
            Arg::new("extractor")
                .long("extractor")
                .default_value(DEFAULT_EXTRACTORS),
        )
        .arg(
            Arg::new("max-extract-time")
                .long("max-extract-time")
                .value_parser(value_parser!(u64)),
        )
        .get_matches_from(args);

    ExtractBenchCliArgs {
        rewrite_iters: *matches.get_one::<usize>("max_iter").unwrap(),
        max_rewrite_mem_gib: *matches.get_one::<u64>("max_mem").unwrap(),
        extractors: split_extractors(matches.get_one::<String>("extractor")),
        max_extract_time_secs: matches.get_one::<u64>("max-extract-time").copied(),
    }
}

pub fn parse_timeline_export_args<I, T>(
    name: &'static str,
    about: &'static str,
    default_rewrite_iters: usize,
    default_max_rewrite_mem_gib: u64,
    default_json_out: &'static str,
    args: I,
) -> TimelineExportCliArgs
where
    I: IntoIterator<Item = T>,
    T: Into<std::ffi::OsString> + Clone,
{
    let matches = Command::new(name)
        .about(about)
        .arg(
            Arg::new("max_iter")
                .long("max_iter")
                .value_parser(value_parser!(usize))
                .default_value(leak_string(default_rewrite_iters.to_string())),
        )
        .arg(
            Arg::new("max_mem")
                .long("max_mem")
                .value_parser(value_parser!(u64))
                .default_value(leak_string(default_max_rewrite_mem_gib.to_string())),
        )
        .arg(
            Arg::new("json-out")
                .long("json-out")
                .default_value(default_json_out),
        )
        .arg(Arg::new("version-nickname").long("version-nickname"))
        .arg(
            Arg::new("extractor")
                .long("extractor")
                .default_value(DEFAULT_EXTRACTORS),
        )
        .arg(
            Arg::new("max-extract-time")
                .long("max-extract-time")
                .value_parser(value_parser!(u64)),
        )
        .get_matches_from(args);

    TimelineExportCliArgs {
        rewrite_iters: *matches.get_one::<usize>("max_iter").unwrap(),
        max_rewrite_mem_gib: *matches.get_one::<u64>("max_mem").unwrap(),
        json_out: matches.get_one::<String>("json-out").unwrap().clone(),
        version_nickname: matches.get_one::<String>("version-nickname").cloned(),
        extractors: split_extractors(matches.get_one::<String>("extractor")),
        max_extract_time_secs: matches.get_one::<u64>("max-extract-time").copied(),
    }
}
