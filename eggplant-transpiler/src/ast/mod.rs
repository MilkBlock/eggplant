pub mod expr;
pub mod parse;

use std::fmt::{Display, Formatter};

#[derive(Clone, Debug, PartialEq, Eq, Hash, Default)]
pub struct Span {
    pub file: Option<String>,
    pub line: usize,
    pub col: usize,
}

impl Span {
    pub fn new(file: Option<String>, line: usize, col: usize) -> Self {
        Self { file, line, col }
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.file {
            Some(filename) => write!(f, "{}:{}:{}", filename, self.line, self.col),
            None => write!(f, "{}:{}", self.line, self.col),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ParseError(pub Span, pub String);

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}: parse error: {}", self.0, self.1)
    }
}

impl std::error::Error for ParseError {}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Literal {
    Int(i64),
    Float(ordered_float::OrderedFloat<f64>),
    String(String),
    Bool(bool),
    Unit,
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Literal::Int(i) => Display::fmt(i, f),
            Literal::Float(n) => write!(f, "{}", n.0),
            Literal::Bool(b) => Display::fmt(b, f),
            Literal::String(s) => write!(f, "\"{}\"", s),
            Literal::Unit => write!(f, "()"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum GenericExpr<Head, Leaf> {
    Lit(Span, Literal),
    Var(Span, Leaf),
    Call(Span, Head, Vec<Self>),
}

impl<Head: Display, Leaf: Display> Display for GenericExpr<Head, Leaf> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            GenericExpr::Lit(_ann, lit) => write!(f, "{}", lit),
            GenericExpr::Var(_ann, var) => write!(f, "{}", var),
            GenericExpr::Call(_ann, op, children) => {
                write!(f, "({} {})", op, ListDisplay(children, " "))
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum GenericFact<Head, Leaf> {
    Op(Span, GenericExpr<Head, Leaf>, GenericExpr<Head, Leaf>),
    Fact(GenericExpr<Head, Leaf>),
}

impl<Head: Display, Leaf: Display> Display for GenericFact<Head, Leaf> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            GenericFact::Op(span, e1, e2) => {
                // Extract operator from span file field if available
                let operator = if let Some(ref file) = span.file {
                    if file.starts_with("operator:") {
                        file.trim_start_matches("operator:")
                    } else {
                        "="
                    }
                } else {
                    "="
                };
                write!(f, "({} {} {})", operator, e1, e2)
            }
            GenericFact::Fact(expr) => write!(f, "{}", expr),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum GenericAction<Head, Leaf> {
    Let(Span, Leaf, GenericExpr<Head, Leaf>),
    Set(
        Span,
        Head,
        Vec<GenericExpr<Head, Leaf>>,
        GenericExpr<Head, Leaf>,
    ),
    Union(Span, GenericExpr<Head, Leaf>, GenericExpr<Head, Leaf>),
    Delete(Span, GenericExpr<Head, Leaf>),
    Expr(Span, GenericExpr<Head, Leaf>),
}

impl<Head: Display, Leaf: Display> Display for GenericAction<Head, Leaf> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            GenericAction::Let(_ann, lhs, rhs) => write!(f, "(let {} {})", lhs, rhs),
            GenericAction::Set(_ann, lhs, args, rhs) => {
                write!(f, "(set ({} {}) {})", lhs, ListDisplay(args, " "), rhs)
            }
            GenericAction::Union(_ann, lhs, rhs) => write!(f, "(union {} {})", lhs, rhs),
            GenericAction::Delete(_ann, expr) => write!(f, "(delete {})", expr),
            GenericAction::Expr(_ann, e) => write!(f, "{}", e),
        }
    }
}

impl<Head, Leaf> GenericAction<Head, Leaf> {
    pub fn span(&self) -> &Span {
        match self {
            GenericAction::Let(span, _, _)
            | GenericAction::Set(span, _, _, _)
            | GenericAction::Union(span, _, _)
            | GenericAction::Delete(span, _)
            | GenericAction::Expr(span, _) => span,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct GenericRule<Head, Leaf> {
    pub span: Span,
    pub head: Vec<GenericAction<Head, Leaf>>,
    pub body: Vec<GenericFact<Head, Leaf>>,
}
impl<Head, Leaf> GenericRule<Head, Leaf>
where
    Head: Clone + Display,
    Leaf: Clone + PartialEq + Eq + Display,
{
    pub(crate) fn fmt_with_ruleset(
        &self,
        f: &mut Formatter,
        ruleset: &str,
        name: &str,
    ) -> std::fmt::Result {
        let indent = " ".repeat(7);
        write!(f, "(rule (")?;
        for (i, fact) in self.body.iter().enumerate() {
            if i > 0 {
                write!(f, "{}", indent)?;
            }

            if i != self.body.len() - 1 {
                writeln!(f, "{}", fact)?;
            } else {
                write!(f, "{}", fact)?;
            }
        }
        write!(f, ")\n      (")?;
        for (i, action) in self.head.iter().enumerate() {
            if i > 0 {
                write!(f, "{}", indent)?;
            }
            if i != self.head.len() - 1 {
                writeln!(f, "{}", action)?;
            } else {
                write!(f, "{}", action)?;
            }
        }
        let ruleset = if !ruleset.is_empty() {
            format!(":ruleset {}", ruleset)
        } else {
            "".into()
        };
        let name = if !name.is_empty() {
            format!(":name \"{}\"", name)
        } else {
            "".into()
        };
        write!(f, ")\n{} {} {})", indent, ruleset, name)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum GenericSchedule<Head, Leaf> {
    Run {
        ruleset: Option<String>,
        limit: Option<usize>,
        until: Option<GenericFact<Head, Leaf>>,
    },
    Named(String),
    Seq(Vec<Self>),
    Saturate(Vec<Self>),
    Repeat(usize, Box<Self>),
}

impl<Head: Display, Leaf: Display> Display for GenericSchedule<Head, Leaf> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            GenericSchedule::Run {
                ruleset,
                limit,
                until,
            } => {
                write!(f, "(run")?;
                if let Some(ruleset) = ruleset {
                    write!(f, " {ruleset}")?;
                }
                if let Some(limit) = limit {
                    write!(f, " {limit}")?;
                }
                if let Some(until) = until {
                    write!(f, " :until {until}")?;
                }
                write!(f, ")")
            }
            GenericSchedule::Named(name) => write!(f, "{name}"),
            GenericSchedule::Seq(items) => write!(f, "(seq {})", ListDisplay(items, " ")),
            GenericSchedule::Saturate(items) => {
                write!(f, "(saturate {})", ListDisplay(items, " "))
            }
            GenericSchedule::Repeat(count, schedule) => {
                write!(f, "(repeat {count} {schedule})")
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Schema {
    pub input: Vec<String>,
    pub output: String,
}

impl Display for Schema {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}) {}", ListDisplay(&self.input, " "), self.output)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Variant {
    pub span: Span,
    pub name: String,
    pub types: Vec<String>,
    pub field_names: Vec<String>,
}

impl Display for Variant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}", self.name)?;
        if !self.types.is_empty() {
            write!(f, " {}", ListDisplay(&self.types, " "))?;
        }
        if !self.field_names.is_empty() {
            write!(f, ":args_name \"{}\"", self.field_names.join(","))?;
        }
        write!(f, ")")
    }
}

#[derive(Debug, Clone)]
pub enum GenericCommand<Head, Leaf> {
    Sort(Span, String, Option<(String, Vec<GenericExpr<Head, Leaf>>)>),
    Datatype {
        span: Span,
        name: String,
        variants: Vec<Variant>,
    },
    Function {
        span: Span,
        name: String,
        schema: Schema,
        merge: Option<GenericExpr<Head, Leaf>>,
    },
    Constructor {
        span: Span,
        name: String,
        schema: Schema,
        cost: Option<usize>,
    },
    Relation {
        span: Span,
        name: String,
        inputs: Vec<String>,
    },
    AddRuleset(Span, String),
    Rule {
        name: String,
        ruleset: String,
        rule: GenericRule<Head, Leaf>,
    },
    Rewrite(String, GenericRewrite<Head, Leaf>, bool, Option<String>),
    BiRewrite(String, GenericRewrite<Head, Leaf>),
    Action(GenericAction<Head, Leaf>),
    Check(Span, Vec<GenericFact<Head, Leaf>>),
    Run {
        span: Span,
        ruleset: Option<String>,
        limit: Option<usize>,
        until: Option<GenericFact<Head, Leaf>>,
    },
    RunSchedule {
        span: Span,
        schedules: Vec<GenericSchedule<Head, Leaf>>,
    },
    Extract {
        span: Span,
        expr: GenericExpr<Head, Leaf>,
        variants: Option<usize>,
    },
    Push(usize),
    Pop(Span, usize),
    PrintFunction(Span, String, Option<usize>, Option<String>, Option<String>),
    Input {
        span: Span,
        name: String,
        file: String,
    },
    Output {
        span: Span,
        file: String,
        exprs: Vec<GenericExpr<Head, Leaf>>,
    },
    Include(Span, String),
    Fail(Span, Box<GenericCommand<Head, Leaf>>),
}
impl<Head, Leaf> Display for GenericCommand<Head, Leaf>
where
    Head: Clone + Display,
    Leaf: Clone + PartialEq + Eq + Display,
{
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            GenericCommand::Rewrite(name, rewrite, subsume, rule_name) => {
                rewrite.fmt_with_ruleset(f, name, false, *subsume, rule_name.as_deref())
            }
            GenericCommand::BiRewrite(name, rewrite) => {
                rewrite.fmt_with_ruleset(f, name, true, false, None)
            }
            GenericCommand::Datatype {
                span: _,
                name,
                variants,
            } => write!(f, "(datatype {name} {})", ListDisplay(variants, " ")),
            GenericCommand::Action(a) => write!(f, "{a}"),
            GenericCommand::Sort(_span, name, None) => write!(f, "(sort {name})"),
            GenericCommand::Sort(_span, name, Some((name2, args))) => {
                write!(f, "(sort {name} ({name2} {}))", ListDisplay(args, " "))
            }
            GenericCommand::Function {
                span: _,
                name,
                schema,
                merge,
            } => {
                write!(f, "(function {name} {schema}")?;
                if let Some(merge) = &merge {
                    write!(f, " :merge {merge}")?;
                } else {
                    write!(f, " :no-merge")?;
                }
                write!(f, ")")
            }
            GenericCommand::Constructor {
                span: _,
                name,
                schema,
                cost,
            } => {
                write!(f, "(constructor {name} {schema}")?;
                if let Some(cost) = cost {
                    write!(f, " :cost {cost}")?;
                }
                write!(f, ")")
            }
            GenericCommand::Relation {
                span: _,
                name,
                inputs,
            } => {
                write!(f, "(relation {name} ({}))", ListDisplay(inputs, " "))
            }
            GenericCommand::AddRuleset(_span, name) => write!(f, "(ruleset {name})"),
            GenericCommand::Rule {
                ruleset,
                name,
                rule,
            } => rule.fmt_with_ruleset(f, ruleset, name),
            GenericCommand::Check(_ann, facts) => {
                write!(f, "(check {})", ListDisplay(facts, "\n"))
            }
            GenericCommand::Run {
                span: _,
                ruleset,
                limit,
                until,
            } => {
                write!(f, "(run")?;
                if let Some(ruleset) = ruleset {
                    write!(f, " {ruleset}")?;
                }
                if let Some(limit) = limit {
                    write!(f, " {limit}")?;
                }
                if let Some(until) = until {
                    write!(f, " :until {until}")?;
                }
                write!(f, ")")
            }
            GenericCommand::RunSchedule { span: _, schedules } => {
                write!(f, "(run-schedule {})", ListDisplay(schedules, " "))
            }
            GenericCommand::Extract {
                span: _,
                expr,
                variants,
            } => {
                write!(f, "(extract {expr}")?;
                if let Some(variants) = variants {
                    write!(f, " {variants}")?;
                }
                write!(f, ")")
            }
            GenericCommand::Push(n) => write!(f, "(push {n})"),
            GenericCommand::Pop(_span, n) => write!(f, "(pop {n})"),
            GenericCommand::Input {
                span: _,
                name,
                file,
            } => write!(f, "(input {name} {file:?})"),
            GenericCommand::Output {
                span: _,
                file,
                exprs,
            } => write!(f, "(output {file:?} {})", ListDisplay(exprs, " ")),
            GenericCommand::Fail(_span, cmd) => write!(f, "(fail {cmd})"),
            GenericCommand::Include(_span, file) => write!(f, "(include {file:?})"),
            GenericCommand::PrintFunction(_span, name, size, file, mode) => {
                write!(f, "(print-function {name}")?;
                if let Some(size) = size {
                    write!(f, " {size}")?;
                }
                if let Some(file) = file {
                    write!(f, " :file {file:?}")?;
                }
                if let Some(mode) = mode {
                    write!(f, " :mode {mode}")?;
                }
                write!(f, ")")
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct GenericRewrite<Head, Leaf> {
    pub span: Span,
    pub lhs: GenericExpr<Head, Leaf>,
    pub rhs: GenericExpr<Head, Leaf>,
    pub conditions: Vec<GenericFact<Head, Leaf>>,
}

pub type Expr = GenericExpr<String, String>;
pub type Fact = GenericFact<String, String>;
pub type Action = GenericAction<String, String>;
pub type Rule = GenericRule<String, String>;
pub type Schedule = GenericSchedule<String, String>;
pub type Command = GenericCommand<String, String>;
pub type Rewrite = GenericRewrite<String, String>;

struct ListDisplay<'a, T>(&'a [T], &'a str);

impl<'a, T: Display> Display for ListDisplay<'a, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, item) in self.0.iter().enumerate() {
            if i > 0 {
                write!(f, "{}", self.1)?;
            }
            write!(f, "{}", item)?;
        }
        Ok(())
    }
}

impl<Head: Display, Leaf: Display> GenericRewrite<Head, Leaf> {
    /// Converts the rewrite into an s-expression.
    pub fn fmt_with_ruleset(
        &self,
        f: &mut Formatter,
        ruleset: &str,
        is_bidirectional: bool,
        subsume: bool,
        name: Option<&str>,
    ) -> std::fmt::Result {
        let direction = if is_bidirectional {
            "birewrite"
        } else {
            "rewrite"
        };
        write!(f, "({direction} {} {}", self.lhs, self.rhs)?;
        if subsume {
            write!(f, " :subsume")?;
        }
        if !self.conditions.is_empty() {
            write!(f, " :when ({})", ListDisplay(&self.conditions, " "))?;
        }
        if !ruleset.is_empty() {
            write!(f, " :ruleset {ruleset}")?;
        }
        if let Some(name) = name {
            write!(f, " :name {name}")?;
        }
        write!(f, ")")
    }
}
