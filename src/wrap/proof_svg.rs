use std::{
    collections::{BTreeMap, HashSet},
    fmt::Write,
    path::{Path, PathBuf},
};

use super::{
    RenderedTemplateField, compile_typst_document_to_svg_string, render_template_with_precedence,
};
use serde::Deserialize;

#[derive(Clone, Debug, Default)]
pub struct ProofRulesTemplateIndex {
    by_name: BTreeMap<String, Vec<ProofRuleTemplate>>,
    by_variant_name: BTreeMap<String, Vec<ProofVariantTemplate>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ProofRuleTemplate {
    pub id: String,
    pub display_name: String,
    pub rule_name: Option<String>,
    pub file: String,
    pub line: usize,
    pub column: usize,
    pub formula: Option<String>,
    pub formula_colored: Option<String>,
    pub math_view: Option<RulesTemplateMathView>,
    pub typst_templates: Vec<ProofVariantTemplate>,
    pub ok: bool,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ProofVariantTemplate {
    pub variant_name: String,
    pub template: String,
    pub fields: Vec<String>,
    pub precedence: u16,
}

#[derive(Debug, Deserialize)]
struct RulesTemplateDocument {
    #[serde(default)]
    entries: Vec<RulesTemplateEntry>,
}

#[derive(Debug, Deserialize)]
struct RulesTemplateEntry {
    #[serde(default)]
    id: String,
    #[serde(default)]
    display_name: String,
    #[serde(default)]
    rule_name: Option<String>,
    #[serde(default)]
    file: String,
    #[serde(default)]
    line: usize,
    #[serde(default)]
    column: usize,
    #[serde(default = "default_true")]
    ok: bool,
    #[serde(default)]
    pattern: Option<RulesTemplatePattern>,
}

#[derive(Debug, Deserialize)]
struct RulesTemplatePattern {
    #[serde(default)]
    math_view: Option<RulesTemplateMathView>,
    #[serde(default)]
    typst_templates: Vec<RulesTemplateVariantTemplate>,
    #[serde(default)]
    precedence_templates: Vec<RulesTemplatePrecedenceTemplate>,
}

#[derive(Clone, Debug, Deserialize, PartialEq, Eq)]
pub struct RulesTemplateMathView {
    #[serde(default)]
    pub rule_name: Option<String>,
    #[serde(default)]
    pub premises: Vec<RulesTemplateMathViewField>,
    #[serde(default)]
    pub formula_source: Option<RulesTemplateFormulaSource>,
}

#[derive(Clone, Debug, Deserialize, PartialEq, Eq)]
pub struct RulesTemplateMathViewField {
    #[serde(default)]
    pub target_id: String,
    #[serde(default)]
    pub label: String,
    #[serde(default)]
    pub plain_source: String,
    #[serde(default)]
    pub colored_source: String,
}

#[derive(Clone, Debug, Deserialize, PartialEq, Eq)]
pub struct RulesTemplateFormulaSource {
    #[serde(default)]
    pub plain: Option<String>,
    #[serde(default)]
    pub colored: Option<String>,
}

#[derive(Debug, Deserialize)]
struct RulesTemplateVariantTemplate {
    #[serde(default)]
    variant_name: String,
    #[serde(default)]
    template: String,
    #[serde(default)]
    fields: Vec<String>,
}

#[derive(Debug, Deserialize)]
struct RulesTemplatePrecedenceTemplate {
    #[serde(default)]
    variant_name: String,
    #[serde(default)]
    precedence: u16,
}

fn default_true() -> bool {
    true
}

impl ProofRulesTemplateIndex {
    pub fn from_path(path: impl AsRef<Path>) -> Result<Self, egglog::Error> {
        let path = path.as_ref();
        let json = std::fs::read_to_string(path).map_err(|err| {
            egglog::Error::BackendError(format!(
                "failed to read rules.template `{}`: {err}",
                path.display()
            ))
        })?;
        Self::from_json_str(&json)
    }

    pub fn from_default_path() -> Result<Self, egglog::Error> {
        let path = std::env::var_os("EGGPLANT_RULES_TEMPLATE")
            .map(PathBuf::from)
            .unwrap_or_else(|| PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("rules.template"));
        Self::from_path(path)
    }

    pub fn from_json_str(json: &str) -> Result<Self, egglog::Error> {
        let document: RulesTemplateDocument = serde_json::from_str(json).map_err(|err| {
            egglog::Error::BackendError(format!("failed to parse rules.template JSON: {err}"))
        })?;
        Ok(Self::from_entries(document.entries))
    }

    fn from_entries(entries: Vec<RulesTemplateEntry>) -> Self {
        let mut index = ProofRulesTemplateIndex::default();
        for entry in entries {
            let (formula, formula_colored, math_view) = entry
                .pattern
                .as_ref()
                .and_then(|pattern| pattern.math_view.as_ref())
                .map(|math_view| {
                    let (formula, formula_colored) = math_view
                        .formula_source
                        .as_ref()
                        .map(|formula_source| {
                            (formula_source.plain.clone(), formula_source.colored.clone())
                        })
                        .unwrap_or((None, None));
                    (formula, formula_colored, Some(math_view.clone()))
                })
                .unwrap_or((None, None, None));

            let mut typst_templates = Vec::new();
            if let Some(pattern) = entry.pattern.as_ref() {
                for template in &pattern.typst_templates {
                    let precedence = pattern
                        .precedence_templates
                        .iter()
                        .find(|candidate| candidate.variant_name == template.variant_name)
                        .map(|candidate| candidate.precedence)
                        .unwrap_or(u16::MAX);
                    let variant = ProofVariantTemplate {
                        variant_name: template.variant_name.clone(),
                        template: template.template.clone(),
                        fields: template.fields.clone(),
                        precedence,
                    };
                    index
                        .by_variant_name
                        .entry(variant.variant_name.clone())
                        .or_default()
                        .push(variant.clone());
                    typst_templates.push(variant);
                }
            }

            let template = ProofRuleTemplate {
                id: entry.id,
                display_name: entry.display_name,
                rule_name: entry.rule_name,
                file: entry.file,
                line: entry.line,
                column: entry.column,
                formula,
                formula_colored,
                math_view,
                typst_templates,
                ok: entry.ok,
            };
            for key in rule_lookup_keys(&template) {
                index.by_name.entry(key).or_default().push(template.clone());
            }
        }
        index
    }

    pub fn lookup(&self, proof_rule_name: &str) -> ProofRuleTemplateMatch<'_> {
        for key in proof_rule_lookup_keys(proof_rule_name) {
            if let Some(matches) = self.by_name.get(&key) {
                return match matches.as_slice() {
                    [one] => ProofRuleTemplateMatch::Unique(one),
                    many => ProofRuleTemplateMatch::Ambiguous(many),
                };
            }
        }
        ProofRuleTemplateMatch::Missing
    }

    pub fn lookup_variant(&self, variant_name: &str) -> Option<&ProofVariantTemplate> {
        self.by_variant_name
            .get(variant_name)
            .and_then(|variants| variants.first())
    }
}

pub enum ProofRuleTemplateMatch<'a> {
    Unique(&'a ProofRuleTemplate),
    Ambiguous(&'a [ProofRuleTemplate]),
    Missing,
}

impl ProofRuleTemplate {
    fn lookup_variant(&self, variant_name: &str) -> Option<&ProofVariantTemplate> {
        self.typst_templates
            .iter()
            .find(|template| template.variant_name == variant_name)
    }
}

#[derive(Clone, Debug)]
enum ProofSexp {
    Atom(String),
    String(String),
    List(Vec<ProofSexp>),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ProofHighlightKind {
    Matched,
    Result,
}

#[derive(Clone, Debug)]
struct ProofStep {
    formula: String,
    proposition_after_rewrites: Option<BTreeMap<String, RenderedProofTerm>>,
    matched_key: Option<String>,
    rule_name: Option<String>,
    rule_detail: Option<String>,
    substitution: Option<String>,
    rule_formula: Option<String>,
}

#[derive(Clone, Copy)]
struct PropositionRenderContext;

fn render_structured_proof_text_typst(
    proof_text: &str,
    templates: &ProofRulesTemplateIndex,
    concise: bool,
) -> Option<String> {
    render_structured_proof_text_typst_inner(proof_text, templates, concise, false)
}

fn render_structured_value_proof_text_typst(
    proof_text: &str,
    templates: &ProofRulesTemplateIndex,
    concise: bool,
) -> Option<String> {
    render_structured_proof_text_typst_inner(proof_text, templates, concise, true)
}

fn render_structured_proof_text_typst_inner(
    proof_text: &str,
    templates: &ProofRulesTemplateIndex,
    concise: bool,
    include_target_row: bool,
) -> Option<String> {
    let sexps = parse_proof_sexps(proof_text)?;
    let bindings = collect_proof_bindings(&sexps);
    let root = sexps.iter().find(|sexp| !is_let_binding(sexp))?;
    let proposition_rule_template = find_preferred_rule_template(root, &bindings, templates);
    let proposition_eq = extract_proof_proposition_eq(root, &bindings)?;
    let target_term = if include_target_row {
        render_proof_target_term(
            proposition_eq,
            &bindings,
            templates,
            proposition_rule_template,
        )
    } else {
        None
    };
    let mut rows = Vec::new();
    let mut seen = HashSet::new();
    let mut rewrites = BTreeMap::new();
    let proposition_ctx = Some(PropositionRenderContext);

    for sexp in &sexps {
        if is_let_binding(sexp) {
            continue;
        }
        collect_proof_steps(
            sexp,
            &bindings,
            templates,
            &mut rows,
            &mut seen,
            &mut rewrites,
            proposition_ctx,
            false,
        );
    }

    rows = filter_administrative_proof_steps(rows);
    if rows.is_empty() {
        return None;
    }
    Some(render_proof_steps_typst(
        proposition_eq,
        target_term.as_deref(),
        &bindings,
        templates,
        proposition_rule_template,
        &rows,
        concise,
    )?)
}

fn render_proof_steps_typst(
    proposition_eq: &ProofSexp,
    target_term: Option<&str>,
    bindings: &BTreeMap<String, ProofSexp>,
    templates: &ProofRulesTemplateIndex,
    proposition_rule_template: Option<&ProofRuleTemplate>,
    steps: &[ProofStep],
    concise: bool,
) -> Option<String> {
    let mut out = String::new();
    out.push_str(PROOF_TYPST_PREAMBLE);
    let empty_rewrites = BTreeMap::new();
    let initial_highlights = proof_row_highlights(
        None,
        steps.first().and_then(|step| step.matched_key.as_deref()),
    );
    let initial_link_targets = proof_row_link_targets(
        steps.first().and_then(|step| step.matched_key.as_deref()),
        steps.first().map(|_| proof_step_label(1)).as_deref(),
    );
    let initial_proposition = render_proof_proposition_eq(
        proposition_eq,
        bindings,
        templates,
        proposition_rule_template,
        &empty_rewrites,
        Some(&initial_highlights),
        if concise {
            Some(&initial_link_targets)
        } else {
            None
        },
    )?;
    let _ = writeln!(
        out,
        r##"#box(
  fill: rgb("#ffffff"),
  stroke: rgb("#d0d7de"),
  radius: 8pt,
  inset: (x: 14pt, y: 14pt),
)[
  #stack(spacing: 10pt)[
"##
    );
    if let Some(target_term) = target_term {
        let _ = writeln!(
            out,
            r##"    #grid(
      columns: (auto, 1fr),
      gutter: 10pt,
      align: horizon,
    )[
      #text(size: 9pt, weight: "bold", fill: rgb("#101820"))[Target]
      {}
    ]
    #line(length: 100%, stroke: rgb("#d0d7de"))
"##,
            typst_math(target_term),
        );
    }
    let _ = writeln!(
        out,
        r##"    #grid(
      columns: (auto, 1fr),
      gutter: 10pt,
      align: horizon,
    )[
      #text(size: 9pt, weight: "bold", fill: rgb("#101820"))[Proposition]
      #grid(
        columns: (auto, 1fr),
        gutter: 8pt,
        align: horizon,
      )[
        #text(size: 8pt, weight: "bold", fill: rgb("#101820"), "[0]")
        {}
      ]
    ]
    #line(length: 100%, stroke: rgb("#d0d7de"))
"##,
        typst_math(&initial_proposition),
    );
    for (idx, step) in steps.iter().enumerate() {
        if idx > 0 {
            let _ = writeln!(out, r##"    #line(length: 100%, stroke: rgb("#d0d7de"))"##);
        }
        let proposition_after = step
            .proposition_after_rewrites
            .as_ref()
            .and_then(|rewrites| {
                let next_match = steps
                    .get(idx + 1)
                    .and_then(|next| next.matched_key.as_deref());
                let highlights = proof_row_highlights(step.matched_key.as_deref(), next_match);
                let next_step_label = steps.get(idx + 1).map(|_| proof_step_label(idx + 2));
                let link_targets = proof_row_link_targets(next_match, next_step_label.as_deref());
                render_proof_proposition_eq(
                    proposition_eq,
                    bindings,
                    templates,
                    proposition_rule_template,
                    rewrites,
                    Some(&highlights),
                    if concise { Some(&link_targets) } else { None },
                )
            });
        let _ = writeln!(
            out,
            r##"    {}
"##,
            render_step_formula_typst(step, idx + 1, proposition_after.as_deref(), !concise),
        );
    }
    if concise {
        if let Some(details) = render_step_details_typst(steps) {
            let _ = writeln!(out, "    {details}");
        }
    }
    out.push_str("  ]\n]\n");
    Some(out)
}

fn render_step_formula_typst(
    step: &ProofStep,
    prop_number: usize,
    proposition_after: Option<&str>,
    include_step_details: bool,
) -> String {
    let mut lines = Vec::new();
    if include_step_details {
        lines.push(typst_math(&step.formula));
        if let Some(rule_info) = render_step_rule_info_typst(step) {
            lines.push(rule_info);
        }
    }
    if let Some(proposition_after) = proposition_after {
        lines.push(format!(
            r##"#grid(
          columns: (auto, 1fr),
          gutter: 8pt,
          align: horizon,
        )[
          #text(size: 8pt, weight: "bold", fill: rgb("#101820"), "[{prop_number}]")
          {}
        ]"##,
            typst_math(proposition_after),
        ));
    }
    if lines.is_empty() {
        lines.push(typst_math(&step.formula));
    }
    let right_column = if lines.len() == 1 {
        lines.pop().unwrap_or_default()
    } else {
        format!(
            r##"#stack(spacing: 4pt)[
        {}
      ]"##,
            lines.join("\n        ")
        )
    };
    if !include_step_details {
        return right_column;
    }
    format!(
        r##"#grid(
      columns: (auto, 1fr),
      gutter: 12pt,
      align: top,
    )[
      #text(size: 22pt, weight: "bold", fill: rgb("#52606d"))[↓]
      {}
    ]"##,
        right_column
    )
}

fn render_step_details_typst(steps: &[ProofStep]) -> Option<String> {
    if steps.is_empty() {
        return None;
    }
    let details = steps
        .iter()
        .enumerate()
        .map(|(idx, step)| render_step_detail_typst(step, idx + 1))
        .collect::<Vec<_>>()
        .join("\n        ");
    Some(format!(
        r##"#line(length: 100%, stroke: rgb("#d0d7de"))
    #stack(spacing: 6pt)[
        {}
    ]"##,
        details
    ))
}

fn filter_administrative_proof_steps(steps: Vec<ProofStep>) -> Vec<ProofStep> {
    let has_user_visible_step = steps.iter().any(|step| !is_administrative_proof_step(step));
    if has_user_visible_step {
        return steps
            .into_iter()
            .filter(|step| !is_administrative_proof_step(step))
            .collect();
    }

    steps
        .into_iter()
        .rev()
        .find(is_administrative_proof_step)
        .map(|step| vec![step])
        .unwrap_or_default()
}

fn is_administrative_proof_step(step: &ProofStep) -> bool {
    step.rule_name
        .as_deref()
        .map(is_administrative_proof_rule_name)
        .unwrap_or(false)
}

fn is_administrative_proof_rule_name(rule_name: &str) -> bool {
    rule_name.starts_with("@commit")
}

fn render_step_detail_typst(step: &ProofStep, step_number: usize) -> String {
    let mut lines = vec![typst_math(&step.formula)];
    if let Some(rule_info) = render_step_rule_info_typst(step) {
        lines.push(rule_info);
    }
    let body = if lines.len() == 1 {
        lines.pop().unwrap_or_default()
    } else {
        format!(
            r##"#stack(spacing: 4pt)[
        {}
      ]"##,
            lines.join("\n        ")
        )
    };
    let label = proof_step_label(step_number);
    format!(
        r##"#box(
      fill: rgb("#f8fafc"),
      stroke: rgb("#d0d7de"),
      radius: 6pt,
      inset: (x: 10pt, y: 8pt),
    )[
      #grid(
        columns: (auto, 1fr),
        gutter: 8pt,
        align: top,
      )[
        #text(size: 8pt, weight: "bold", fill: rgb("#101820"), "[{step_number}]")
        {}
      ]
    ] {}"##,
        body,
        typst_label_ref(&label),
    )
}

fn render_step_rule_info_typst(step: &ProofStep) -> Option<String> {
    if step
        .rule_name
        .as_deref()
        .map(is_administrative_proof_rule_name)
        .unwrap_or(false)
    {
        return Some(format!(
            r##"#text(size: 8pt, weight: "bold", fill: rgb("#101820"))[Initialization]"##
        ));
    }

    let mut lines = Vec::new();
    if let Some(rule_name) = step
        .rule_name
        .as_deref()
        .filter(|rule_name| !rule_name.is_empty())
    {
        lines.push(format!(
            r##"#text(size: 8pt, weight: "bold", fill: rgb("#101820"))[{}]"##,
            typst_raw_text(rule_name)
        ));
    }
    if let Some(detail) = step.rule_detail.as_ref() {
        lines.push(format!(
            r##"#text(size: 8pt, fill: rgb("#52606d"))[{}]"##,
            typst_raw_text(detail)
        ));
    }
    let rule_formula = step.rule_formula.as_ref().map(|rule_formula| {
        format!(
            r##"#text(size: 8pt, fill: rgb("#52606d"))[{}]"##,
            typst_math(rule_formula)
        )
    });
    let substitution = step.substitution.as_ref().map(|subst| {
        format!(
            r##"#text(size: 8pt, fill: rgb("#52606d"))[{}]"##,
            typst_math(subst)
        )
    });
    match (rule_formula, substitution) {
        (Some(rule_formula), Some(substitution)) => lines.push(format!(
            r##"#grid(
        columns: (1fr, auto),
        gutter: 12pt,
        align: top,
      )[
        {}
        {}
      ]"##,
            rule_formula, substitution
        )),
        (Some(rule_formula), None) => lines.push(rule_formula),
        (None, Some(substitution)) => lines.push(substitution),
        (None, None) => {}
    }
    if lines.is_empty() {
        None
    } else {
        Some(format!(
            r##"#stack(spacing: 2pt)[
        {}
      ]"##,
            lines.join("\n        ")
        ))
    }
}

fn render_proof_proposition(
    sexp: &ProofSexp,
    bindings: &BTreeMap<String, ProofSexp>,
    templates: &ProofRulesTemplateIndex,
    rule_template: Option<&ProofRuleTemplate>,
) -> Option<String> {
    let eq = extract_proof_proposition_eq(sexp, bindings)?;
    let rewrites = BTreeMap::new();
    render_proof_proposition_eq(
        eq,
        bindings,
        templates,
        rule_template,
        &rewrites,
        None,
        None,
    )
}

fn render_proof_proposition_eq(
    eq: &ProofSexp,
    bindings: &BTreeMap<String, ProofSexp>,
    templates: &ProofRulesTemplateIndex,
    rule_template: Option<&ProofRuleTemplate>,
    rewrites: &BTreeMap<String, RenderedProofTerm>,
    highlights: Option<&BTreeMap<String, ProofHighlightKind>>,
    highlight_links: Option<&BTreeMap<String, String>>,
) -> Option<String> {
    let Some((lhs_sexp, rhs_sexp)) = displayed_equality_terms(eq, bindings) else {
        return None;
    };
    let lhs = render_proof_term(
        lhs_sexp,
        bindings,
        templates,
        rule_template,
        rewrites,
        highlights,
        highlight_links,
    );
    let rhs = render_proof_term(
        rhs_sexp,
        bindings,
        templates,
        rule_template,
        rewrites,
        highlights,
        highlight_links,
    );
    Some(format!("{} = {}", lhs.text, rhs.text))
}

fn render_proof_target_term(
    eq: &ProofSexp,
    bindings: &BTreeMap<String, ProofSexp>,
    templates: &ProofRulesTemplateIndex,
    rule_template: Option<&ProofRuleTemplate>,
) -> Option<String> {
    let (lhs_sexp, _) = displayed_equality_terms(eq, bindings)?;
    let rewrites = BTreeMap::new();
    Some(
        render_proof_term(
            lhs_sexp,
            bindings,
            templates,
            rule_template,
            &rewrites,
            None,
            None,
        )
        .text,
    )
}

fn displayed_equality_terms<'a>(
    eq: &'a ProofSexp,
    bindings: &'a BTreeMap<String, ProofSexp>,
) -> Option<(&'a ProofSexp, &'a ProofSexp)> {
    let eq = resolve_proof_reference(eq, bindings);
    let ProofSexp::List(items) = eq else {
        return None;
    };
    if items.len() != 3 || items[0].as_atom() != Some("=") {
        return None;
    }
    Some((&items[1], &items[2]))
}

fn extract_proof_proposition_eq<'a>(
    sexp: &'a ProofSexp,
    bindings: &'a BTreeMap<String, ProofSexp>,
) -> Option<&'a ProofSexp> {
    let sexp = resolve_proof_reference(sexp, bindings);
    let ProofSexp::List(items) = sexp else {
        return None;
    };
    let eq = items.get(1)?;
    let eq = resolve_proof_reference(eq, bindings);
    let ProofSexp::List(items) = eq else {
        return None;
    };
    if items.len() != 3 || items[0].as_atom() != Some("=") {
        return None;
    }
    Some(eq)
}

fn find_preferred_rule_template<'a>(
    sexp: &'a ProofSexp,
    bindings: &'a BTreeMap<String, ProofSexp>,
    templates: &'a ProofRulesTemplateIndex,
) -> Option<&'a ProofRuleTemplate> {
    find_preferred_rule_template_inner(sexp, bindings, templates, &mut HashSet::new())
}

fn find_preferred_rule_template_inner<'a>(
    sexp: &'a ProofSexp,
    bindings: &'a BTreeMap<String, ProofSexp>,
    templates: &'a ProofRulesTemplateIndex,
    seen: &mut HashSet<String>,
) -> Option<&'a ProofRuleTemplate> {
    let sexp = resolve_proof_reference(sexp, bindings);
    let key = sexp_key(sexp);
    if !seen.insert(key) {
        return None;
    }

    match sexp {
        ProofSexp::Atom(name) => bindings
            .get(name)
            .and_then(|bound| find_preferred_rule_template_inner(bound, bindings, templates, seen)),
        ProofSexp::String(_) => None,
        ProofSexp::List(items) => {
            if items.first().and_then(ProofSexp::as_atom) == Some("Rule") {
                if let Some(rule_name) = extract_rule_name(sexp) {
                    if let ProofRuleTemplateMatch::Unique(template) = templates.lookup(&rule_name) {
                        return Some(template);
                    }
                }
            }
            for item in items.iter().skip(1) {
                if let Some(template) =
                    find_preferred_rule_template_inner(item, bindings, templates, seen)
                {
                    return Some(template);
                }
            }
            None
        }
    }
}

fn extract_rule_name(sexp: &ProofSexp) -> Option<String> {
    let ProofSexp::List(items) = sexp else {
        return None;
    };
    if items.first().and_then(ProofSexp::as_atom) != Some("Rule") {
        return None;
    }
    items.get(2).and_then(|sexp| match sexp {
        ProofSexp::List(list) if list.len() == 2 => list
            .get(1)
            .and_then(ProofSexp::as_string)
            .or_else(|| list.get(1).and_then(ProofSexp::as_atom))
            .map(str::to_owned),
        _ => None,
    })
}

fn collect_proof_steps(
    sexp: &ProofSexp,
    bindings: &BTreeMap<String, ProofSexp>,
    templates: &ProofRulesTemplateIndex,
    steps: &mut Vec<ProofStep>,
    seen: &mut HashSet<String>,
    rewrites: &mut BTreeMap<String, RenderedProofTerm>,
    proposition_ctx: Option<PropositionRenderContext>,
    reverse_eq: bool,
) {
    match sexp {
        ProofSexp::Atom(name) => {
            if let Some(bound) = bindings.get(name) {
                let key = format!("binding:{reverse_eq}:{name}");
                if seen.insert(key) {
                    collect_proof_steps(
                        bound,
                        bindings,
                        templates,
                        steps,
                        seen,
                        rewrites,
                        proposition_ctx,
                        reverse_eq,
                    );
                }
            }
        }
        ProofSexp::String(_) => {}
        ProofSexp::List(items) => {
            let Some(head) = items.first().and_then(ProofSexp::as_atom) else {
                return;
            };
            let key = format!("{}:{reverse_eq}", sexp_key(sexp));
            if !seen.insert(key) {
                return;
            }

            match head {
                "let" => {
                    if let Some(bound) = items.get(2) {
                        collect_proof_steps(
                            bound,
                            bindings,
                            templates,
                            steps,
                            seen,
                            rewrites,
                            proposition_ctx,
                            reverse_eq,
                        );
                    }
                }
                "Fiat" => {
                    if let Some(eq) = items
                        .get(1)
                        .filter(|eq| !is_reflexive_equality(eq, bindings))
                    {
                        push_rendered_proof_step(
                            steps,
                            rewrites,
                            eq,
                            None,
                            None,
                            None,
                            None,
                            bindings,
                            templates,
                            None,
                            proposition_ctx,
                            reverse_eq,
                        );
                    }
                }
                "Rule" => {
                    let eq = items.get(1);
                    let rule_name = extract_rule_name(sexp).unwrap_or_else(|| "unknown".to_owned());
                    let local_rule = templates.lookup(&rule_name);
                    let (rule_template, rule_label, rule_detail) = match local_rule {
                        ProofRuleTemplateMatch::Unique(template) => (
                            Some(template),
                            non_empty_or(&template.display_name, &rule_name),
                            None,
                        ),
                        ProofRuleTemplateMatch::Ambiguous(matches) => (
                            None,
                            format!("{rule_name} (ambiguous)"),
                            Some(format!("{} rules.template matches", matches.len())),
                        ),
                        ProofRuleTemplateMatch::Missing => (
                            None,
                            rule_name.clone(),
                            Some("no rules.template match".to_owned()),
                        ),
                    };
                    if let Some(rule_template) = rule_template {
                        if let Some(premises) = items.get(3).and_then(extract_named_list) {
                            for premise in premises {
                                collect_proof_steps(
                                    premise,
                                    bindings,
                                    templates,
                                    steps,
                                    seen,
                                    rewrites,
                                    proposition_ctx,
                                    false,
                                );
                            }
                        }
                        let substitution = items
                            .get(4)
                            .and_then(extract_named_list)
                            .map(|pairs| {
                                render_substitution_pairs(
                                    pairs,
                                    bindings,
                                    templates,
                                    Some(rule_template),
                                    rewrites,
                                )
                            })
                            .filter(|text| !text.is_empty());
                        push_rendered_proof_step(
                            steps,
                            rewrites,
                            eq.unwrap_or(sexp),
                            Some(rule_label),
                            rule_detail,
                            substitution,
                            rule_template
                                .formula_colored
                                .as_ref()
                                .or(rule_template.formula.as_ref())
                                .cloned(),
                            bindings,
                            templates,
                            Some(rule_template),
                            proposition_ctx,
                            reverse_eq,
                        );
                    } else {
                        if let Some(premises) = items.get(3).and_then(extract_named_list) {
                            for premise in premises {
                                collect_proof_steps(
                                    premise,
                                    bindings,
                                    templates,
                                    steps,
                                    seen,
                                    rewrites,
                                    proposition_ctx,
                                    false,
                                );
                            }
                        }
                        let substitution = items
                            .get(4)
                            .and_then(extract_named_list)
                            .map(|pairs| {
                                render_substitution_pairs(
                                    pairs, bindings, templates, None, rewrites,
                                )
                            })
                            .filter(|text| !text.is_empty());
                        push_rendered_proof_step(
                            steps,
                            rewrites,
                            eq.unwrap_or(sexp),
                            Some(rule_label),
                            rule_detail,
                            substitution,
                            None,
                            bindings,
                            templates,
                            None,
                            proposition_ctx,
                            reverse_eq,
                        );
                    }
                }
                "Trans" => {
                    let left = items.get(2);
                    let right = items.get(3);
                    if reverse_eq {
                        if let Some(right) = right {
                            collect_proof_steps(
                                right,
                                bindings,
                                templates,
                                steps,
                                seen,
                                rewrites,
                                proposition_ctx,
                                true,
                            );
                        }
                        if let Some(left) = left {
                            collect_proof_steps(
                                left,
                                bindings,
                                templates,
                                steps,
                                seen,
                                rewrites,
                                proposition_ctx,
                                true,
                            );
                        }
                    } else {
                        if let Some(left) = left {
                            collect_proof_steps(
                                left,
                                bindings,
                                templates,
                                steps,
                                seen,
                                rewrites,
                                proposition_ctx,
                                false,
                            );
                        }
                        if let Some(right) = right {
                            collect_proof_steps(
                                right,
                                bindings,
                                templates,
                                steps,
                                seen,
                                rewrites,
                                proposition_ctx,
                                false,
                            );
                        }
                    }
                }
                "Sym" => {
                    if let Some(inner) = items.get(2) {
                        collect_proof_steps(
                            inner,
                            bindings,
                            templates,
                            steps,
                            seen,
                            rewrites,
                            proposition_ctx,
                            !reverse_eq,
                        );
                    }
                }
                "Merge" => {
                    if let Some(old) = items.get(2) {
                        collect_proof_steps(
                            old,
                            bindings,
                            templates,
                            steps,
                            seen,
                            rewrites,
                            proposition_ctx,
                            reverse_eq,
                        );
                    }
                    if let Some(new) = items.get(3) {
                        collect_proof_steps(
                            new,
                            bindings,
                            templates,
                            steps,
                            seen,
                            rewrites,
                            proposition_ctx,
                            reverse_eq,
                        );
                    }
                    if let Some(eq) = items.get(1) {
                        push_rendered_proof_step(
                            steps,
                            rewrites,
                            eq,
                            Some("Merge".to_owned()),
                            None,
                            None,
                            None,
                            bindings,
                            templates,
                            None,
                            proposition_ctx,
                            reverse_eq,
                        );
                    }
                }
                "Congr" => {
                    if let Some(base) = items.get(2) {
                        collect_proof_steps(
                            base,
                            bindings,
                            templates,
                            steps,
                            seen,
                            rewrites,
                            proposition_ctx,
                            reverse_eq,
                        );
                    }
                    if let Some(child) = items.get(4) {
                        collect_proof_steps(
                            child,
                            bindings,
                            templates,
                            steps,
                            seen,
                            rewrites,
                            proposition_ctx,
                            reverse_eq,
                        );
                    }
                    if let Some(eq) = items.get(1) {
                        push_rendered_proof_step(
                            steps,
                            rewrites,
                            eq,
                            Some("Congr".to_owned()),
                            None,
                            None,
                            None,
                            bindings,
                            templates,
                            None,
                            proposition_ctx,
                            reverse_eq,
                        );
                    }
                }
                _ => {
                    if let Some(bound) = bindings.get(head) {
                        collect_proof_steps(
                            bound,
                            bindings,
                            templates,
                            steps,
                            seen,
                            rewrites,
                            proposition_ctx,
                            reverse_eq,
                        );
                    }
                }
            }
        }
    }
}

fn push_rendered_proof_step(
    steps: &mut Vec<ProofStep>,
    rewrites: &mut BTreeMap<String, RenderedProofTerm>,
    eq: &ProofSexp,
    rule_name: Option<String>,
    rule_detail: Option<String>,
    substitution: Option<String>,
    rule_formula: Option<String>,
    bindings: &BTreeMap<String, ProofSexp>,
    templates: &ProofRulesTemplateIndex,
    rule_template: Option<&ProofRuleTemplate>,
    proposition_ctx: Option<PropositionRenderContext>,
    reverse_eq: bool,
) {
    let mut step = render_proof_step(
        eq,
        rule_name,
        rule_detail,
        substitution,
        rule_formula,
        bindings,
        templates,
        rule_template,
        rewrites,
        reverse_eq,
    );
    record_display_rewrite(eq, bindings, templates, rule_template, rewrites, reverse_eq);
    if proposition_ctx.is_some() {
        step.proposition_after_rewrites = Some(rewrites.clone());
    }
    steps.push(step);
}

fn render_proof_step(
    eq: &ProofSexp,
    rule_name: Option<String>,
    rule_detail: Option<String>,
    substitution: Option<String>,
    rule_formula: Option<String>,
    bindings: &BTreeMap<String, ProofSexp>,
    templates: &ProofRulesTemplateIndex,
    rule_template: Option<&ProofRuleTemplate>,
    rewrites: &BTreeMap<String, RenderedProofTerm>,
    reverse_eq: bool,
) -> ProofStep {
    let formula = render_proof_equality(
        eq,
        bindings,
        templates,
        rule_template,
        rewrites,
        reverse_eq,
        None,
        None,
    );
    let matched_key = displayed_equality_terms(eq, bindings).map(|(lhs, rhs)| {
        let target = if reverse_eq { rhs } else { lhs };
        sexp_key(resolve_proof_reference(target, bindings))
    });
    ProofStep {
        formula,
        proposition_after_rewrites: None,
        matched_key,
        rule_name,
        rule_detail,
        substitution,
        rule_formula,
    }
}

fn render_proof_equality(
    sexp: &ProofSexp,
    bindings: &BTreeMap<String, ProofSexp>,
    templates: &ProofRulesTemplateIndex,
    rule_template: Option<&ProofRuleTemplate>,
    rewrites: &BTreeMap<String, RenderedProofTerm>,
    reverse_eq: bool,
    highlights: Option<&BTreeMap<String, ProofHighlightKind>>,
    highlight_links: Option<&BTreeMap<String, String>>,
) -> String {
    let sexp = resolve_proof_reference(sexp, bindings);
    match sexp {
        ProofSexp::List(items) if items.len() == 3 && items[0].as_atom() == Some("=") => {
            let (lhs_sexp, rhs_sexp) = if reverse_eq {
                (&items[2], &items[1])
            } else {
                (&items[1], &items[2])
            };
            let lhs = render_proof_term(
                lhs_sexp,
                bindings,
                templates,
                rule_template,
                rewrites,
                highlights,
                highlight_links,
            );
            let rhs = render_proof_term(
                rhs_sexp,
                bindings,
                templates,
                rule_template,
                rewrites,
                highlights,
                highlight_links,
            );
            format!("{} arrow.r.double {}", lhs.text, rhs.text)
        }
        _ => {
            render_proof_term(
                sexp,
                bindings,
                templates,
                rule_template,
                rewrites,
                highlights,
                highlight_links,
            )
            .text
        }
    }
}

fn is_reflexive_equality(sexp: &ProofSexp, bindings: &BTreeMap<String, ProofSexp>) -> bool {
    let sexp = resolve_proof_reference(sexp, bindings);
    matches!(
        sexp,
        ProofSexp::List(items)
            if items.len() == 3
                && items[0].as_atom() == Some("=")
                && sexp_key(resolve_proof_reference(&items[1], bindings))
                    == sexp_key(resolve_proof_reference(&items[2], bindings))
    )
}

#[derive(Clone, Debug)]
struct RenderedProofTerm {
    text: String,
    precedence: u16,
}

impl RenderedProofTerm {
    fn atom(text: impl Into<String>) -> Self {
        Self {
            text: text.into(),
            precedence: u16::MAX,
        }
    }
}

fn maybe_highlight_rendered_term(
    term: RenderedProofTerm,
    highlight: Option<ProofHighlightKind>,
    link_target: Option<&str>,
) -> RenderedProofTerm {
    let Some(highlight) = highlight else {
        return term;
    };
    RenderedProofTerm {
        text: typst_highlight_math_fragment(&term.text, highlight, link_target),
        precedence: term.precedence,
    }
}

fn render_proof_term(
    sexp: &ProofSexp,
    bindings: &BTreeMap<String, ProofSexp>,
    templates: &ProofRulesTemplateIndex,
    rule_template: Option<&ProofRuleTemplate>,
    rewrites: &BTreeMap<String, RenderedProofTerm>,
    highlights: Option<&BTreeMap<String, ProofHighlightKind>>,
    highlight_links: Option<&BTreeMap<String, String>>,
) -> RenderedProofTerm {
    let sexp = resolve_proof_reference(sexp, bindings);
    let sexp_key = sexp_key(sexp);
    let highlight = highlights.and_then(|highlights| highlights.get(&sexp_key).copied());
    let link_target = match highlight {
        Some(ProofHighlightKind::Matched) => {
            highlight_links.and_then(|links| links.get(&sexp_key).map(String::as_str))
        }
        _ => None,
    };
    if let Some(rewritten) = rewrites.get(&sexp_key) {
        return maybe_highlight_rendered_term(rewritten.clone(), highlight, link_target);
    }
    let rendered = match sexp {
        ProofSexp::Atom(atom) => render_atom_term(atom),
        ProofSexp::String(text) => RenderedProofTerm::atom(typst_string_literal(text)),
        ProofSexp::List(items) => {
            let Some(head) = items.first().and_then(ProofSexp::as_atom) else {
                return RenderedProofTerm::atom("()");
            };
            let variant = rule_template
                .and_then(|template| template.lookup_variant(head))
                .or_else(|| templates.lookup_variant(head));
            if let Some(variant) = variant {
                if variant.fields.len() == items.len() - 1 {
                    let rendered_fields = variant
                        .fields
                        .iter()
                        .zip(items.iter().skip(1))
                        .map(|(field_name, value)| {
                            (
                                field_name.as_str(),
                                render_proof_term(
                                    value,
                                    bindings,
                                    templates,
                                    rule_template,
                                    rewrites,
                                    highlights,
                                    highlight_links,
                                ),
                            )
                        })
                        .collect::<Vec<_>>();
                    let field_refs = rendered_fields
                        .iter()
                        .map(|(field_name, value)| {
                            (
                                *field_name,
                                RenderedTemplateField::new(value.text.as_str(), value.precedence),
                            )
                        })
                        .collect::<Vec<_>>();
                    return maybe_highlight_rendered_term(
                        RenderedProofTerm {
                            text: render_template_with_precedence(
                                &variant.template,
                                variant.precedence,
                                &field_refs,
                            ),
                            precedence: variant.precedence,
                        },
                        highlight,
                        link_target,
                    );
                }
            }

            let rendered_args = items
                .iter()
                .skip(1)
                .map(|item| {
                    render_proof_term(
                        item,
                        bindings,
                        templates,
                        rule_template,
                        rewrites,
                        highlights,
                        highlight_links,
                    )
                })
                .collect::<Vec<_>>();
            let args = rendered_args
                .iter()
                .map(|arg| arg.text.as_str())
                .collect::<Vec<_>>()
                .join(", ");
            RenderedProofTerm {
                text: format!("upright({})({args})", typst_string_literal(head)),
                precedence: u16::MAX,
            }
        }
    };
    maybe_highlight_rendered_term(rendered, highlight, link_target)
}

fn render_atom_term(atom: &str) -> RenderedProofTerm {
    if atom.parse::<i64>().is_ok() || atom.parse::<f64>().is_ok() {
        RenderedProofTerm::atom(atom.to_owned())
    } else if is_single_math_identifier(atom) {
        RenderedProofTerm::atom(atom.to_owned())
    } else {
        RenderedProofTerm::atom(format!("upright({})", typst_string_literal(atom)))
    }
}

fn is_single_math_identifier(atom: &str) -> bool {
    let mut chars = atom.chars();
    matches!(chars.next(), Some(first) if first.is_ascii_alphabetic()) && chars.next().is_none()
}

fn render_substitution_pairs(
    pairs: &[ProofSexp],
    bindings: &BTreeMap<String, ProofSexp>,
    templates: &ProofRulesTemplateIndex,
    rule_template: Option<&ProofRuleTemplate>,
    rewrites: &BTreeMap<String, RenderedProofTerm>,
) -> String {
    if let Some(text) =
        render_semantic_bindings(pairs, bindings, templates, rule_template, rewrites)
    {
        return text;
    }
    let rendered = pairs
        .iter()
        .filter_map(|pair| {
            let ProofSexp::List(items) = pair else {
                return None;
            };
            if items.len() != 2 {
                return None;
            }
            let name = items[0].as_atom().or_else(|| items[0].as_string())?;
            let value = render_proof_term(
                &items[1],
                bindings,
                templates,
                rule_template,
                rewrites,
                None,
                None,
            );
            Some(format!("{} = {}", render_atom_term(name).text, value.text))
        })
        .collect::<Vec<_>>();
    rendered.join(", ")
}

#[derive(Clone)]
struct RenderedBindingCandidate {
    sort_key: (u8, usize, usize),
    value: RenderedProofTerm,
}

fn render_semantic_bindings(
    pairs: &[ProofSexp],
    bindings: &BTreeMap<String, ProofSexp>,
    templates: &ProofRulesTemplateIndex,
    rule_template: Option<&ProofRuleTemplate>,
    rewrites: &BTreeMap<String, RenderedProofTerm>,
) -> Option<String> {
    let rule_template = rule_template?;
    let math_view = rule_template.math_view.as_ref()?;
    // The proof encoder leaks internal `expr*` placeholders here.
    // For display, prefer the rule's semantic premise ids from the DSL/template.
    let labels = math_view
        .premises
        .iter()
        .map(|premise| premise.target_id.trim())
        .filter(|label| !label.is_empty())
        .map(str::to_owned)
        .collect::<Vec<_>>();
    if labels.is_empty() {
        return None;
    }

    let mut candidates =
        collect_binding_candidates(pairs, bindings, templates, Some(rule_template), rewrites);
    if candidates.is_empty() || candidates.len() < labels.len() {
        return None;
    }
    candidates.sort_by_key(|candidate| candidate.sort_key);

    let label_count = labels.len();
    Some(
        labels
            .into_iter()
            .zip(candidates.into_iter().take(label_count))
            .map(|(label, candidate)| format!("{label} = {}", candidate.value.text))
            .collect::<Vec<_>>()
            .join(", "),
    )
}

fn collect_binding_candidates(
    pairs: &[ProofSexp],
    bindings: &BTreeMap<String, ProofSexp>,
    templates: &ProofRulesTemplateIndex,
    rule_template: Option<&ProofRuleTemplate>,
    rewrites: &BTreeMap<String, RenderedProofTerm>,
) -> Vec<RenderedBindingCandidate> {
    pairs
        .iter()
        .enumerate()
        .filter_map(|(idx, pair)| {
            let ProofSexp::List(items) = pair else {
                return None;
            };
            if items.len() != 2 {
                return None;
            }
            let name = items[0].as_atom().or_else(|| items[0].as_string())?;
            if name.ends_with("num") {
                return None;
            }
            let sort_key = binding_sort_key(name, idx);
            let value = render_proof_term(
                &items[1],
                bindings,
                templates,
                rule_template,
                rewrites,
                None,
                None,
            );
            Some(RenderedBindingCandidate { sort_key, value })
        })
        .collect()
}

fn record_display_rewrite(
    eq: &ProofSexp,
    bindings: &BTreeMap<String, ProofSexp>,
    templates: &ProofRulesTemplateIndex,
    rule_template: Option<&ProofRuleTemplate>,
    rewrites: &mut BTreeMap<String, RenderedProofTerm>,
    reverse_eq: bool,
) {
    let sexp = resolve_proof_reference(eq, bindings);
    let ProofSexp::List(items) = sexp else {
        return;
    };
    if items.len() != 3 || items[0].as_atom() != Some("=") {
        return;
    }
    let (lhs_sexp, rhs_sexp) = if reverse_eq {
        (&items[2], &items[1])
    } else {
        (&items[1], &items[2])
    };
    let lhs_key = sexp_key(resolve_proof_reference(lhs_sexp, bindings));
    let lhs = render_proof_term(
        lhs_sexp,
        bindings,
        templates,
        rule_template,
        rewrites,
        None,
        None,
    );
    let rhs = render_proof_term(
        rhs_sexp,
        bindings,
        templates,
        rule_template,
        rewrites,
        None,
        None,
    );
    if lhs.text == rhs.text {
        return;
    }
    rewrites.insert(lhs_key, rhs);
}

fn binding_sort_key(name: &str, index: usize) -> (u8, usize, usize) {
    if let Some(num) = trailing_digits(name) {
        return (0, num, index);
    }
    (1, usize::MAX, index)
}

fn trailing_digits(text: &str) -> Option<usize> {
    let mut start = text.len();
    for (idx, ch) in text.char_indices().rev() {
        if ch.is_ascii_digit() {
            start = idx;
        } else {
            break;
        }
    }
    if start == text.len() {
        return None;
    }
    text[start..].parse().ok()
}

fn collect_proof_bindings(sexps: &[ProofSexp]) -> BTreeMap<String, ProofSexp> {
    let mut bindings = BTreeMap::new();
    for sexp in sexps {
        let Some((head, items)) = sexp.as_list_head() else {
            continue;
        };
        if head == "let" && items.len() == 2 {
            if let Some(name) = items[0].as_atom() {
                bindings.insert(name.to_owned(), items[1].clone());
            }
        }
    }
    bindings
}

fn is_let_binding(sexp: &ProofSexp) -> bool {
    matches!(sexp.as_list_head(), Some((head, items)) if head == "let" && items.len() == 2)
}

fn resolve_proof_reference<'a>(
    sexp: &'a ProofSexp,
    bindings: &'a BTreeMap<String, ProofSexp>,
) -> &'a ProofSexp {
    let mut current = sexp;
    let mut seen = HashSet::new();
    while let ProofSexp::Atom(name) = current {
        if !seen.insert(name) {
            break;
        }
        let Some(next) = bindings.get(name) else {
            break;
        };
        current = next;
    }
    current
}

fn extract_named_list(sexp: &ProofSexp) -> Option<&[ProofSexp]> {
    let ProofSexp::List(items) = sexp else {
        return None;
    };
    if items.len() < 1 {
        return None;
    }
    Some(&items[1..])
}

fn parse_proof_sexps(text: &str) -> Option<Vec<ProofSexp>> {
    let mut parser = ProofSexpParser::new(text);
    let mut out = Vec::new();
    parser.skip_ws();
    while !parser.eof() {
        out.push(parser.parse_sexp().ok()?);
        parser.skip_ws();
    }
    Some(out)
}

struct ProofSexpParser<'a> {
    chars: Vec<char>,
    idx: usize,
    _text: &'a str,
}

impl<'a> ProofSexpParser<'a> {
    fn new(text: &'a str) -> Self {
        Self {
            chars: text.chars().collect(),
            idx: 0,
            _text: text,
        }
    }

    fn eof(&self) -> bool {
        self.idx >= self.chars.len()
    }

    fn skip_ws(&mut self) {
        while let Some(ch) = self.chars.get(self.idx) {
            if ch.is_whitespace() {
                self.idx += 1;
            } else {
                break;
            }
        }
    }

    fn parse_sexp(&mut self) -> Result<ProofSexp, ()> {
        self.skip_ws();
        let Some(ch) = self.chars.get(self.idx).copied() else {
            return Err(());
        };
        match ch {
            '(' => self.parse_list(),
            '"' => self.parse_string(),
            _ => Ok(self.parse_atom()),
        }
    }

    fn parse_list(&mut self) -> Result<ProofSexp, ()> {
        self.idx += 1;
        let mut items = Vec::new();
        loop {
            self.skip_ws();
            let Some(ch) = self.chars.get(self.idx).copied() else {
                return Err(());
            };
            if ch == ')' {
                self.idx += 1;
                return Ok(ProofSexp::List(items));
            }
            items.push(self.parse_sexp()?);
        }
    }

    fn parse_string(&mut self) -> Result<ProofSexp, ()> {
        self.idx += 1;
        let mut out = String::new();
        let mut escaped = false;
        while let Some(ch) = self.chars.get(self.idx).copied() {
            self.idx += 1;
            if escaped {
                out.push(match ch {
                    'n' => '\n',
                    't' => '\t',
                    'r' => '\r',
                    '"' => '"',
                    '\\' => '\\',
                    other => other,
                });
                escaped = false;
            } else if ch == '\\' {
                escaped = true;
            } else if ch == '"' {
                return Ok(ProofSexp::String(out));
            } else {
                out.push(ch);
            }
        }
        Err(())
    }

    fn parse_atom(&mut self) -> ProofSexp {
        let start = self.idx;
        while let Some(ch) = self.chars.get(self.idx).copied() {
            if ch.is_whitespace() || ch == '(' || ch == ')' {
                break;
            }
            self.idx += 1;
        }
        ProofSexp::Atom(self.chars[start..self.idx].iter().collect())
    }
}

impl ProofSexp {
    fn as_atom(&self) -> Option<&str> {
        match self {
            ProofSexp::Atom(atom) => Some(atom.as_str()),
            _ => None,
        }
    }

    fn as_string(&self) -> Option<&str> {
        match self {
            ProofSexp::String(text) => Some(text.as_str()),
            _ => None,
        }
    }

    fn as_list_head(&self) -> Option<(&str, &[ProofSexp])> {
        match self {
            ProofSexp::List(items) if !items.is_empty() => Some((items[0].as_atom()?, &items[1..])),
            _ => None,
        }
    }
}

fn sexp_key(sexp: &ProofSexp) -> String {
    match sexp {
        ProofSexp::Atom(atom) => format!("atom:{atom}"),
        ProofSexp::String(text) => format!("string:{text:?}"),
        ProofSexp::List(items) => {
            let mut out = String::from("list:");
            for item in items {
                out.push_str(&sexp_key(item));
                out.push(';');
            }
            out
        }
    }
}

pub fn render_proof_svg_from_rules_template(
    proof_text: &str,
    rules_template_path: impl AsRef<Path>,
) -> Result<String, egglog::Error> {
    let index = ProofRulesTemplateIndex::from_path(rules_template_path)?;
    render_proof_text_svg(proof_text, &index)
}

pub fn render_proof_svg_from_rules_template_with_options(
    proof_text: &str,
    rules_template_path: impl AsRef<Path>,
    concise: bool,
) -> Result<String, egglog::Error> {
    let index = ProofRulesTemplateIndex::from_path(rules_template_path)?;
    render_proof_text_svg_with_options(proof_text, &index, concise)
}

/// Render egglog proof text into a standalone Typst document.
///
/// The proof formatter keeps egglog's proof-line structure intact, but replaces
/// `(name "...")` fragments with Typst rule badges resolved from `rules.template`.
pub fn render_proof_text_typst(proof_text: &str, templates: &ProofRulesTemplateIndex) -> String {
    ProofSvgFormatter::default().render_document_typst(proof_text, templates)
}

pub fn render_proof_text_typst_with_options(
    proof_text: &str,
    templates: &ProofRulesTemplateIndex,
    concise: bool,
) -> String {
    ProofSvgFormatter::default()
        .concise(concise)
        .render_document_typst(proof_text, templates)
}

pub fn render_value_proof_text_typst(
    proof_text: &str,
    templates: &ProofRulesTemplateIndex,
) -> String {
    ProofSvgFormatter::default().render_value_document_typst(proof_text, templates)
}

pub fn render_value_proof_text_typst_with_options(
    proof_text: &str,
    templates: &ProofRulesTemplateIndex,
    concise: bool,
) -> String {
    ProofSvgFormatter::default()
        .concise(concise)
        .render_value_document_typst(proof_text, templates)
}

/// Render egglog proof text to SVG by compiling the generated Typst document.
///
/// This requires the `typst` CLI to be available on `PATH`.
pub fn render_proof_text_svg(
    proof_text: &str,
    templates: &ProofRulesTemplateIndex,
) -> Result<String, egglog::Error> {
    ProofSvgFormatter::default().render(proof_text, templates)
}

pub fn render_proof_text_svg_with_options(
    proof_text: &str,
    templates: &ProofRulesTemplateIndex,
    concise: bool,
) -> Result<String, egglog::Error> {
    ProofSvgFormatter::default()
        .concise(concise)
        .render(proof_text, templates)
}

pub fn render_value_proof_text_svg(
    proof_text: &str,
    templates: &ProofRulesTemplateIndex,
) -> Result<String, egglog::Error> {
    ProofSvgFormatter::default().render_value(proof_text, templates)
}

pub fn render_value_proof_text_svg_with_options(
    proof_text: &str,
    templates: &ProofRulesTemplateIndex,
    concise: bool,
) -> Result<String, egglog::Error> {
    ProofSvgFormatter::default()
        .concise(concise)
        .render_value(proof_text, templates)
}

#[derive(Clone, Debug)]
pub struct ProofSvgFormatter {
    row_gap_pt: f32,
    concise: bool,
}

impl Default for ProofSvgFormatter {
    fn default() -> Self {
        Self {
            row_gap_pt: 7.0,
            concise: false,
        }
    }
}

impl ProofSvgFormatter {
    pub fn concise(mut self, concise: bool) -> Self {
        self.concise = concise;
        self
    }

    pub fn render(
        &self,
        proof_text: &str,
        templates: &ProofRulesTemplateIndex,
    ) -> Result<String, egglog::Error> {
        let svg = compile_typst_document_to_svg_string(
            &self.render_document_typst(proof_text, templates),
        )?;
        Ok(if self.concise {
            inject_concise_proof_svg_links(&svg)
        } else {
            svg
        })
    }

    fn render_document_typst(
        &self,
        proof_text: &str,
        templates: &ProofRulesTemplateIndex,
    ) -> String {
        render_structured_proof_text_typst(proof_text, templates, self.concise)
            .unwrap_or_else(|| self.render_typst(proof_text, templates))
    }

    fn render_value_document_typst(
        &self,
        proof_text: &str,
        templates: &ProofRulesTemplateIndex,
    ) -> String {
        render_structured_value_proof_text_typst(proof_text, templates, self.concise)
            .unwrap_or_else(|| self.render_typst(proof_text, templates))
    }

    pub fn render_typst(&self, proof_text: &str, templates: &ProofRulesTemplateIndex) -> String {
        let parsed_lines = if proof_text.is_empty() {
            vec![parse_proof_line("")]
        } else {
            proof_text.lines().map(parse_proof_line).collect::<Vec<_>>()
        };
        let mut out = String::new();
        out.push_str(PROOF_TYPST_PREAMBLE);
        let _ = writeln!(
            out,
            r##"#box(
  fill: rgb("#ffffff"),
  stroke: rgb("#d0d7de"),
  radius: 8pt,
  inset: (x: 14pt, y: 14pt),
)[
  #stack(spacing: {:.1}pt,"##,
            self.row_gap_pt
        );
        for line in &parsed_lines {
            let _ = writeln!(out, "    [{}],", self.render_line_typst(line, templates));
        }
        out.push_str("  )\n]\n");
        out
    }

    fn render_value(
        &self,
        proof_text: &str,
        templates: &ProofRulesTemplateIndex,
    ) -> Result<String, egglog::Error> {
        let svg = compile_typst_document_to_svg_string(
            &self.render_value_document_typst(proof_text, templates),
        )?;
        Ok(if self.concise {
            inject_concise_proof_svg_links(&svg)
        } else {
            svg
        })
    }

    fn render_line_typst(
        &self,
        line: &ParsedProofLine,
        templates: &ProofRulesTemplateIndex,
    ) -> String {
        let mut out = String::new();
        let mut emitted_segment = false;
        for segment in &line.segments {
            match segment {
                ProofLineSegment::Text(text) => {
                    if !text.is_empty() {
                        let _ = write!(out, "#raw({})", typst_string_literal(text));
                        emitted_segment = true;
                    }
                }
                ProofLineSegment::RuleName(name) => {
                    if emitted_segment {
                        out.push_str(" #h(8pt) ");
                    }
                    let badge = badge_for_rule(name, templates);
                    let _ = write!(
                        out,
                        "#proof-badge({}, {}, rgb(\"{}\"), rgb(\"{}\"))",
                        typst_string_literal(&badge.label),
                        typst_string_literal(&badge.detail),
                        badge.fill,
                        badge.stroke
                    );
                    emitted_segment = true;
                }
            }
        }
        if !emitted_segment {
            out.push_str("#raw(\"\")");
        }
        out
    }
}

const PROOF_TYPST_PREAMBLE: &str = r##"#set page(width: auto, height: auto, margin: 8pt)
#set text(size: 13pt, fill: rgb("#17212b"))
#let proof-badge(label, detail, fill, stroke) = box(
  fill: fill,
  stroke: stroke,
  radius: 5pt,
  inset: (x: 8pt, y: 5pt),
)[
  #text(size: 10pt, weight: "bold", fill: rgb("#101820"))[#label]
  #linebreak()
  #text(size: 8pt, fill: rgb("#52606d"))[#detail]
]

"##;

#[derive(Clone, Debug)]
struct ParsedProofLine {
    segments: Vec<ProofLineSegment>,
}

#[derive(Clone, Debug)]
enum ProofLineSegment {
    Text(String),
    RuleName(String),
}

#[derive(Clone, Debug)]
struct RuleBadge {
    label: String,
    detail: String,
    fill: &'static str,
    stroke: &'static str,
}

fn parse_proof_line(line: &str) -> ParsedProofLine {
    let mut segments = Vec::new();
    let mut cursor = 0;
    while let Some(relative_start) = line[cursor..].find("(name \"") {
        let start = cursor + relative_start;
        let name_start = start + "(name \"".len();
        let Some(name_end) = find_unescaped_quote(line, name_start) else {
            break;
        };
        if start > cursor {
            segments.push(ProofLineSegment::Text(line[cursor..start].to_owned()));
        }
        segments.push(ProofLineSegment::RuleName(
            line[name_start..name_end].to_owned(),
        ));
        cursor = if line.as_bytes().get(name_end + 1) == Some(&b')') {
            name_end + 2
        } else {
            name_end + 1
        };
    }
    if cursor < line.len() || segments.is_empty() {
        segments.push(ProofLineSegment::Text(line[cursor..].to_owned()));
    }
    ParsedProofLine { segments }
}

fn find_unescaped_quote(text: &str, start: usize) -> Option<usize> {
    let bytes = text.as_bytes();
    let mut idx = start;
    let mut escaped = false;
    while idx < bytes.len() {
        let byte = bytes[idx];
        if escaped {
            escaped = false;
        } else if byte == b'\\' {
            escaped = true;
        } else if byte == b'"' {
            return Some(idx);
        }
        idx += 1;
    }
    None
}

fn badge_for_rule(rule_name: &str, templates: &ProofRulesTemplateIndex) -> RuleBadge {
    match templates.lookup(rule_name) {
        ProofRuleTemplateMatch::Unique(template) => {
            let label = non_empty_or(&template.display_name, rule_name);
            let detail = template
                .formula
                .as_deref()
                .filter(|formula| !formula.trim().is_empty())
                .map(|formula| truncate_text(formula, 72))
                .unwrap_or_else(|| {
                    format!("{}:{}:{}", template.file, template.line, template.column)
                });
            make_badge(
                label,
                detail,
                if template.ok { BADGE_OK } else { BADGE_FAILED },
            )
        }
        ProofRuleTemplateMatch::Ambiguous(matches) => {
            // Proof text only carries the printed rule name (for example `@fib`).
            // If several generated rules share that name, we cannot safely pick one
            // without extra source identity from upstream proof encoding.
            make_badge(
                format!("{rule_name} (ambiguous)"),
                format!("{} rules.template matches", matches.len()),
                BADGE_AMBIGUOUS,
            )
        }
        ProofRuleTemplateMatch::Missing => make_badge(
            rule_name.to_owned(),
            "no rules.template match".to_owned(),
            BADGE_MISSING,
        ),
    }
}

#[derive(Clone, Copy, Debug)]
struct BadgeTone {
    fill: &'static str,
    stroke: &'static str,
}

const BADGE_OK: BadgeTone = BadgeTone {
    fill: "#f7fbff",
    stroke: "#7aa7d9",
};
const BADGE_FAILED: BadgeTone = BadgeTone {
    fill: "#fff5f5",
    stroke: "#d97878",
};
const BADGE_AMBIGUOUS: BadgeTone = BadgeTone {
    fill: "#fff8e6",
    stroke: "#d39c28",
};
const BADGE_MISSING: BadgeTone = BadgeTone {
    fill: "#f3f4f6",
    stroke: "#9ca3af",
};

fn make_badge(label: String, detail: String, tone: BadgeTone) -> RuleBadge {
    let label = truncate_text(&label, 48);
    let detail = truncate_text(&detail, 72);
    RuleBadge {
        label,
        detail,
        fill: tone.fill,
        stroke: tone.stroke,
    }
}

fn rule_lookup_keys(template: &ProofRuleTemplate) -> Vec<String> {
    let mut keys = Vec::new();
    if let Some(rule_name) = template.rule_name.as_deref() {
        push_rule_name_keys(&mut keys, rule_name);
    }
    if !template.display_name.is_empty() {
        push_rule_name_keys(&mut keys, &template.display_name);
    }
    if !template.id.is_empty() {
        push_unique(&mut keys, template.id.clone());
    }
    keys
}

fn proof_rule_lookup_keys(proof_rule_name: &str) -> Vec<String> {
    let mut keys = Vec::new();
    push_rule_name_keys(&mut keys, proof_rule_name);
    keys
}

fn push_rule_name_keys(keys: &mut Vec<String>, name: &str) {
    if name.is_empty() {
        return;
    }
    push_unique(keys, name.to_owned());
    if let Some(stripped) = name.strip_prefix('@') {
        push_unique(keys, stripped.to_owned());
    } else {
        push_unique(keys, format!("@{name}"));
    }
}

fn push_unique(keys: &mut Vec<String>, key: String) {
    if !key.is_empty() && !keys.iter().any(|existing| existing == &key) {
        keys.push(key);
    }
}

fn non_empty_or(value: &str, fallback: &str) -> String {
    if value.is_empty() {
        fallback.to_owned()
    } else {
        value.to_owned()
    }
}

fn truncate_text(text: &str, max_chars: usize) -> String {
    if text.chars().count() <= max_chars {
        return text.to_owned();
    }
    let keep = max_chars.saturating_sub(1);
    let mut out = text.chars().take(keep).collect::<String>();
    out.push_str("...");
    out
}

fn typst_string_literal(text: &str) -> String {
    let mut out = String::with_capacity(text.len());
    out.push('"');
    for ch in text.chars() {
        match ch {
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            _ => out.push(ch),
        }
    }
    out.push('"');
    out
}

fn typst_raw_text(text: &str) -> String {
    let mut out = String::from("#raw(");
    out.push_str(&typst_string_literal(text));
    out.push(')');
    out
}

fn typst_math(text: &str) -> String {
    format!("${text}$")
}

const MATCH_HIGHLIGHT_FILL: &str = "#fff4a3";
const RESULT_HIGHLIGHT_FILL: &str = "#d7f7dd";
const HIGHLIGHT_RADIUS_PT: &str = "2pt";
const HIGHLIGHT_INSET_X_PT: &str = "2pt";
const HIGHLIGHT_INSET_Y_PT: &str = "0.5pt";

fn proof_row_highlights(
    previous_result_key: Option<&str>,
    next_match_key: Option<&str>,
) -> BTreeMap<String, ProofHighlightKind> {
    let mut highlights = BTreeMap::new();
    if let Some(key) = previous_result_key {
        highlights.insert(key.to_owned(), ProofHighlightKind::Result);
    }
    if let Some(key) = next_match_key {
        highlights.insert(key.to_owned(), ProofHighlightKind::Matched);
    }
    highlights
}

fn proof_row_link_targets(
    next_match_key: Option<&str>,
    next_step_label: Option<&str>,
) -> BTreeMap<String, String> {
    let mut targets = BTreeMap::new();
    if let (Some(key), Some(label)) = (next_match_key, next_step_label) {
        targets.insert(key.to_owned(), label.to_owned());
    }
    targets
}

fn proof_step_label(step_number: usize) -> String {
    format!("proof-step-{step_number}")
}

fn proof_step_link_label(step_label: &str) -> String {
    format!("proof-link-{step_label}")
}

fn typst_label_ref(label: &str) -> String {
    format!("<{label}>")
}

fn typst_highlight_math_fragment(
    text: &str,
    highlight: ProofHighlightKind,
    link_target: Option<&str>,
) -> String {
    let fill = match highlight {
        ProofHighlightKind::Matched => MATCH_HIGHLIGHT_FILL,
        ProofHighlightKind::Result => RESULT_HIGHLIGHT_FILL,
    };
    let box_markup = format!(
        r##"#box(
  fill: rgb("{}"),
  radius: {},
  inset: (x: {}, y: {}),
)[${text}$]"##,
        fill, HIGHLIGHT_RADIUS_PT, HIGHLIGHT_INSET_X_PT, HIGHLIGHT_INSET_Y_PT
    );
    match (highlight, link_target) {
        (ProofHighlightKind::Matched, Some(target)) => {
            let link_label = proof_step_link_label(target);
            format!(
                "#link({})[{} {}]",
                typst_label_ref(target),
                box_markup,
                typst_label_ref(&link_label)
            )
        }
        _ => box_markup,
    }
}

const PROOF_SVG_STEP_LABEL_PREFIX: &str = "proof-step-";
const PROOF_SVG_LINK_LABEL_PREFIX: &str = "proof-link-";

fn inject_concise_proof_svg_links(svg: &str) -> String {
    let with_ids = add_proof_step_svg_ids(svg);
    wrap_proof_link_svg_groups(&with_ids)
}

fn add_proof_step_svg_ids(svg: &str) -> String {
    rewrite_svg_group_start_tags(svg, |tag| {
        let Some(label) = svg_attr_value(tag, "data-typst-label") else {
            return None;
        };
        if !label.starts_with(PROOF_SVG_STEP_LABEL_PREFIX) || svg_attr_value(tag, "id").is_some() {
            return None;
        }
        insert_svg_attr(tag, "id", label)
    })
}

fn rewrite_svg_group_start_tags(
    svg: &str,
    mut rewrite: impl FnMut(&str) -> Option<String>,
) -> String {
    let mut out = String::with_capacity(svg.len());
    let mut cursor = 0;
    while let Some(relative_start) = svg[cursor..].find("<g") {
        let tag_start = cursor + relative_start;
        if !is_svg_group_start_at(svg, tag_start) {
            out.push_str(&svg[cursor..tag_start + 2]);
            cursor = tag_start + 2;
            continue;
        }
        let Some(relative_end) = svg[tag_start..].find('>') else {
            break;
        };
        let tag_end = tag_start + relative_end + 1;
        out.push_str(&svg[cursor..tag_start]);
        let tag = &svg[tag_start..tag_end];
        if let Some(rewritten) = rewrite(tag) {
            out.push_str(&rewritten);
        } else {
            out.push_str(tag);
        }
        cursor = tag_end;
    }
    out.push_str(&svg[cursor..]);
    out
}

fn wrap_proof_link_svg_groups(svg: &str) -> String {
    let mut out = String::with_capacity(svg.len());
    let mut cursor = 0;
    while let Some(relative_start) = svg[cursor..].find("<g") {
        let group_start = cursor + relative_start;
        if !is_svg_group_start_at(svg, group_start) {
            out.push_str(&svg[cursor..group_start + 2]);
            cursor = group_start + 2;
            continue;
        }
        let Some(tag_end) = svg[group_start..]
            .find('>')
            .map(|idx| group_start + idx + 1)
        else {
            break;
        };
        let tag = &svg[group_start..tag_end];
        let Some(label) = svg_attr_value(tag, "data-typst-label") else {
            out.push_str(&svg[cursor..tag_end]);
            cursor = tag_end;
            continue;
        };
        let Some(target) = label.strip_prefix(PROOF_SVG_LINK_LABEL_PREFIX) else {
            out.push_str(&svg[cursor..tag_end]);
            cursor = tag_end;
            continue;
        };
        let Some(group_end) = find_matching_svg_group_end(svg, group_start) else {
            out.push_str(&svg[cursor..tag_end]);
            cursor = tag_end;
            continue;
        };
        out.push_str(&svg[cursor..group_start]);
        let escaped_target = escape_svg_attr_value(target);
        let _ = write!(
            out,
            r##"<a href="#{}" xlink:href="#{}" style="cursor: pointer">"##,
            escaped_target, escaped_target
        );
        out.push_str(&svg[group_start..group_end]);
        out.push_str("</a>");
        cursor = group_end;
    }
    out.push_str(&svg[cursor..]);
    out
}

fn find_matching_svg_group_end(svg: &str, group_start: usize) -> Option<usize> {
    let mut cursor = group_start;
    let mut depth = 0usize;
    loop {
        let start = find_next_svg_group_start(svg, cursor);
        let close = svg[cursor..].find("</g>").map(|idx| cursor + idx);
        match (start, close) {
            (Some(start), Some(close)) if start < close => {
                depth = depth.saturating_add(1);
                cursor = svg[start..].find('>').map(|idx| start + idx + 1)?;
            }
            (Some(start), None) => {
                depth = depth.saturating_add(1);
                cursor = svg[start..].find('>').map(|idx| start + idx + 1)?;
            }
            (_, Some(close)) => {
                depth = depth.checked_sub(1)?;
                cursor = close + "</g>".len();
                if depth == 0 {
                    return Some(cursor);
                }
            }
            (None, None) => return None,
        }
    }
}

fn find_next_svg_group_start(svg: &str, cursor: usize) -> Option<usize> {
    let mut search_from = cursor;
    while let Some(relative_start) = svg[search_from..].find("<g") {
        let start = search_from + relative_start;
        if is_svg_group_start_at(svg, start) {
            return Some(start);
        }
        search_from = start + 2;
    }
    None
}

fn is_svg_group_start_at(svg: &str, idx: usize) -> bool {
    svg[idx..].starts_with("<g")
        && svg[idx + 2..]
            .chars()
            .next()
            .is_some_and(|ch| ch == '>' || ch == '/' || ch.is_ascii_whitespace())
}

fn svg_attr_value<'a>(tag: &'a str, attr_name: &str) -> Option<&'a str> {
    let needle = format!(r#"{attr_name}=""#);
    let value_start = tag.find(&needle)? + needle.len();
    let value_end = tag[value_start..].find('"')? + value_start;
    Some(&tag[value_start..value_end])
}

fn insert_svg_attr(tag: &str, attr_name: &str, attr_value: &str) -> Option<String> {
    let insert_at = tag.rfind('>')?;
    let mut out = String::with_capacity(tag.len() + attr_name.len() + attr_value.len() + 5);
    out.push_str(&tag[..insert_at]);
    let _ = write!(
        out,
        r#" {}="{}""#,
        attr_name,
        escape_svg_attr_value(attr_value)
    );
    out.push_str(&tag[insert_at..]);
    Some(out)
}

fn escape_svg_attr_value(value: &str) -> String {
    let mut out = String::with_capacity(value.len());
    for ch in value.chars() {
        match ch {
            '&' => out.push_str("&amp;"),
            '"' => out.push_str("&quot;"),
            '<' => out.push_str("&lt;"),
            '>' => out.push_str("&gt;"),
            _ => out.push(ch),
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    fn template_json(rule_entries: &str) -> String {
        format!(
            r#"{{
  "schema_version": 1,
  "entries": [
    {rule_entries}
  ]
}}"#
        )
    }

    #[test]
    fn renders_proof_as_typst_svg() {
        if std::process::Command::new("typst")
            .arg("--version")
            .status()
            .is_err()
        {
            eprintln!("skipping proof svg render test because `typst` CLI is unavailable");
            return;
        }

        let index = ProofRulesTemplateIndex::from_json_str(&template_json(
            r#"{
      "id": "src_test_rs__1_1__MulPat",
      "display_name": "MulPat",
      "rule_name": "MulPat",
      "file": "src/test.rs",
      "line": 1,
      "column": 1,
      "ok": true,
      "pattern": {
      "math_view": {
        "formula_source": {
            "plain": "upright(\"ProofMul\")(l, r) arrow.r.double upright(\"ProofConst\")(l * r)"
        }
      }
      }
    }"#,
        ))
        .expect("template should parse");

        let svg = render_proof_text_svg(
            "(Rule (= (ProofMul 3 2) (ProofConst 6)) (name \"@MulPat\") (premises) (substitution))",
            &index,
        )
        .expect("proof svg should render");

        assert!(svg.contains("<svg"));
        assert!(svg.contains("class=\"typst-doc\""));
        assert!(svg.contains("typst-text"));
        assert!(!svg.contains("class=\"proof-text\""));
    }

    #[test]
    fn marks_duplicate_template_rule_names_as_ambiguous() {
        let index = ProofRulesTemplateIndex::from_json_str(&template_json(
            r#"{
      "id": "src_lib_rs__1_1__demo",
      "display_name": "demo @ src/lib.rs:1",
      "rule_name": "demo",
      "file": "src/lib.rs",
      "line": 1,
      "column": 1
    },
    {
      "id": "examples_demo_rs__1_1__demo",
      "display_name": "demo @ examples/demo.rs:1",
      "rule_name": "demo",
      "file": "examples/demo.rs",
      "line": 1,
      "column": 1
    }"#,
        ))
        .expect("template should parse");

        let typst = render_proof_text_typst("(Rule (= a b) (name \"@demo\"))", &index);

        assert!(typst.contains("@demo (ambiguous)"));
        assert!(typst.contains("2 rules.template matches"));
    }

    #[test]
    fn renders_rule_step_with_dsl_typst_templates_and_substitution() {
        let index = ProofRulesTemplateIndex::from_json_str(&template_json(
            r#"{
      "id": "src_test_rs__1_1__MulPat",
      "display_name": "MulPat",
      "rule_name": "MulPat",
      "file": "src/test.rs",
      "line": 1,
      "column": 1,
      "ok": true,
      "pattern": {
        "typst_templates": [
          {
            "variant_name": "Const",
            "template": "{num}",
            "fields": ["num"]
          },
          {
            "variant_name": "Mul",
            "template": "{lhs} * {rhs}",
            "fields": ["lhs", "rhs"]
          }
        ],
        "precedence_templates": [
          {
            "variant_name": "Const",
            "precedence": 100
          },
          {
            "variant_name": "Mul",
            "precedence": 20
          }
        ],
        "math_view": {
          "formula_source": {
            "plain": "frac(l \\ r \\ l * r, l * r arrow.r.double upright(\"Const\")(upright(\"cal\")))"
          }
        }
      }
    }"#,
        ))
        .expect("template should parse");

        let typst = render_proof_text_typst(
            r#"(let prop0 (= (Mul (Const 3) (Const 2)) (Const 6)))
(let prf0 (Rule prop0 (name "@MulPat") (premises) (substitution (lhs (Const 3)) (rhs (Const 2)))))
prf0"#,
            &index,
        );

        assert!(typst.contains("$3 * 2 arrow.r.double 6$"));
        assert!(typst.contains("$upright(\"lhs\") = 3, upright(\"rhs\") = 2$"));
        assert!(typst.contains("MulPat"));
        assert!(!typst.contains("(Rule"));
        assert!(!typst.contains("(Mul"));
    }

    #[test]
    fn omits_reflexive_and_administrative_proof_steps() {
        let index = ProofRulesTemplateIndex::from_json_str(&template_json(
            r#"{
      "id": "src_test_rs__1_1__MulPat",
      "display_name": "MulPat",
      "rule_name": "MulPat",
      "file": "src/test.rs",
      "line": 1,
      "column": 1,
      "ok": true,
      "pattern": {
        "typst_templates": [
          {
            "variant_name": "Const",
            "template": "{num}",
            "fields": ["num"]
          },
          {
            "variant_name": "Mul",
            "template": "{lhs} * {rhs}",
            "fields": ["lhs", "rhs"]
          }
        ],
        "precedence_templates": [
          {
            "variant_name": "Const",
            "precedence": 100
          },
          {
            "variant_name": "Mul",
            "precedence": 20
          }
        ],
        "math_view": {
          "formula_source": {
            "plain": "frac(l \\ r \\ l * r, l * r arrow.r.double upright(\"Const\")(upright(\"cal\")))"
          }
        }
      }
    }"#,
        ))
        .expect("template should parse");

        let typst = render_proof_text_typst(
            r#"(let t0 (Mul (Const 3) (Const 2)))
(let prop0 (= t0 (Const 6)))
(let prf0 (Fiat (= t0 t0)))
(Trans
  prop0
  prf0
  (Sym
    prop0
    (Rule
      (= (Const 6) t0)
      (name "@MulPat")
      (premises
        prf0
        (Fiat (= (Const 3) (Const 3)))
        (Fiat (= (Const 2) (Const 2))))
      (substitution
        (lhs (Const 3))
        (rhs (Const 2))))))"#,
            &index,
        );

        assert!(typst.contains("$3 * 2 arrow.r.double 6$"));
        assert!(typst.contains("MulPat"));
        assert!(!typst.contains("$2 arrow.r.double 2$"));
        assert!(!typst.contains("$3 arrow.r.double 3$"));
        assert!(!typst.contains("$6 arrow.r.double 3 * 2$"));
        assert!(!typst.contains("Sym"));
        assert!(!typst.contains("Trans"));
    }

    #[test]
    fn hides_internal_commit_proof_steps_when_user_rules_are_present() {
        let index = ProofRulesTemplateIndex::from_json_str(&template_json(
            r#"{
      "id": "src_test_rs__1_1__MulPat",
      "display_name": "MulPat",
      "rule_name": "MulPat",
      "file": "src/test.rs",
      "line": 1,
      "column": 1,
      "ok": true,
      "pattern": {
        "typst_templates": [
          {
            "variant_name": "Const",
            "template": "{num}",
            "fields": ["num"]
          },
          {
            "variant_name": "Mul",
            "template": "{lhs} * {rhs}",
            "fields": ["lhs", "rhs"]
          }
        ],
        "precedence_templates": [
          {
            "variant_name": "Const",
            "precedence": 100
          },
          {
            "variant_name": "Mul",
            "precedence": 20
          }
        ],
        "math_view": {
          "formula_source": {
            "plain": "frac(l \\ r \\ l * r, l * r arrow.r.double upright(\"Const\")(upright(\"cal\")))"
          }
        }
      }
    }"#,
        ))
        .expect("template should parse");

        let typst = render_proof_text_typst(
            r#"(let t0 (Mul (Const 3) (Const 2)))
(Rule
  (= t0 (Const 6))
  (name "@MulPat")
  (premises
    (Rule
      (= (Const 3) (Const 3))
      (name "@commit0")
      (premises)
      (substitution (__eggplant_commit_trigger ())))
    (Rule
      (= (Const 2) (Const 2))
      (name "@commit0")
      (premises)
      (substitution (__eggplant_commit_trigger ()))))
  (substitution
    (lhs (Const 3))
    (rhs (Const 2))))"#,
            &index,
        );

        assert!(typst.contains("MulPat"));
        assert!(typst.contains("$3 * 2 arrow.r.double 6$"));
        assert!(!typst.contains("@commit0"));
        assert!(!typst.contains("__eggplant_commit_trigger"));
    }

    #[test]
    fn renders_commit_only_proof_as_initialization() {
        let index = ProofRulesTemplateIndex::from_json_str(&template_json(
            r#"{
      "id": "src_test_rs__1_1__MulPat",
      "display_name": "MulPat",
      "rule_name": "MulPat",
      "file": "src/test.rs",
      "line": 1,
      "column": 1,
      "ok": true
    }"#,
        ))
        .expect("template should parse");

        let typst = render_proof_text_typst(
            r#"(Rule
  (= (Const 3) (Const 3))
  (name "@commit0")
  (premises)
  (substitution (__eggplant_commit_trigger ())))"#,
            &index,
        );

        assert!(typst.contains("Initialization"));
        assert!(!typst.contains("@commit0"));
    }

    #[test]
    fn wraps_multiple_proof_steps_in_one_outer_box() {
        let index = ProofRulesTemplateIndex::from_json_str(&template_json(
            r#"{
      "id": "src_test_rs__1_1__MulPat",
      "display_name": "MulPat",
      "rule_name": "MulPat",
      "file": "src/test.rs",
      "line": 1,
      "column": 1,
      "ok": true,
      "pattern": {
        "typst_templates": [
          {
            "variant_name": "Const",
            "template": "{num}",
            "fields": ["num"]
          },
          {
            "variant_name": "Mul",
            "template": "{lhs} * {rhs}",
            "fields": ["lhs", "rhs"]
          },
          {
            "variant_name": "Add",
            "template": "{lhs} + {rhs}",
            "fields": ["lhs", "rhs"]
          }
        ],
        "precedence_templates": [
          {
            "variant_name": "Const",
            "precedence": 100
          },
          {
            "variant_name": "Mul",
            "precedence": 20
          },
          {
            "variant_name": "Add",
            "precedence": 10
          }
        ],
        "math_view": {
          "formula_source": {
            "plain": "frac(l \\ r \\ l * r, l * r arrow.r.double upright(\"Const\")(upright(\"cal\")))"
          }
        }
      }
    }"#,
        ))
        .expect("template should parse");

        let typst = render_proof_text_typst(
            r#"(Rule (= (Mul (Const 3) (Const 2)) (Const 6)) (name "@MulPat") (premises) (substitution (lhs (Const 3)) (rhs (Const 2))))
(Rule (= (Add (Const 6) (Const 4)) (Const 10)) (name "@AddPat") (premises) (substitution (lhs (Const 6)) (rhs (Const 4))))"#,
            &index,
        );

        assert_eq!(typst.matches("#box(\n  fill: rgb(\"#ffffff\")").count(), 1);
        assert!(typst.contains("3 * 2"));
        assert!(typst.contains("6 + 4"));
    }

    #[test]
    fn rule_premises_keep_their_own_orientation() {
        let index = ProofRulesTemplateIndex::from_json_str(&template_json(
            r#"{
      "id": "src_test_rs__1_1__ConstPropMulPat",
      "display_name": "ConstPropMulPat",
      "rule_name": "ConstPropMulPat",
      "file": "src/test.rs",
      "line": 1,
      "column": 1,
      "ok": true,
      "pattern": {
        "typst_templates": [
          {
            "variant_name": "Const",
            "template": "{num}",
            "fields": ["num"]
          },
          {
            "variant_name": "Mul",
            "template": "{lhs} * {rhs}",
            "fields": ["lhs", "rhs"]
          },
          {
            "variant_name": "Add",
            "template": "{lhs} + {rhs}",
            "fields": ["lhs", "rhs"]
          }
        ],
        "precedence_templates": [
          {
            "variant_name": "Const",
            "precedence": 100
          },
          {
            "variant_name": "Mul",
            "precedence": 20
          },
          {
            "variant_name": "Add",
            "precedence": 10
          }
        ],
        "math_view": {
          "formula_source": {
            "plain": "frac(l \\ r \\ l * r, l * r arrow.r.double upright(\"Const\")(upright(\"cal\")))"
          }
        }
      }
    },
    {
      "id": "src_test_rs__1_2__ConstPropAddPat",
      "display_name": "ConstPropAddPat",
      "rule_name": "ConstPropAddPat",
      "file": "src/test.rs",
      "line": 2,
      "column": 1,
      "ok": true,
      "pattern": {
        "typst_templates": [
          {
            "variant_name": "Const",
            "template": "{num}",
            "fields": ["num"]
          },
          {
            "variant_name": "Mul",
            "template": "{lhs} * {rhs}",
            "fields": ["lhs", "rhs"]
          },
          {
            "variant_name": "Add",
            "template": "{lhs} + {rhs}",
            "fields": ["lhs", "rhs"]
          }
        ],
        "precedence_templates": [
          {
            "variant_name": "Const",
            "precedence": 100
          },
          {
            "variant_name": "Mul",
            "precedence": 20
          },
          {
            "variant_name": "Add",
            "precedence": 10
          }
        ],
        "math_view": {
          "formula_source": {
            "plain": "frac(l \\ r \\ l + r, l + r arrow.r.double upright(\"Const\")(upright(\"cal\")))"
          }
        }
      }
    }"#,
        ))
        .expect("template should parse");

        let typst = render_proof_text_typst(
            r#"(let t0 (Mul (Const 3) (Const 2)))
(let t1 (Add t0 (Const 4)))
(let prop0 (= t1 (Const 10)))
(let prf0 (Fiat (= t1 t1)))
(Trans
  prop0
  prf0
  (Sym
    prop0
    (Rule
      (= (Const 10) t1)
      (name "@ConstPropAddPat")
      (premises
        (Fiat (= (Const 4) (Const 4)))
        (Sym
          (= t0 (Const 6))
          (Rule
            (= (Const 6) t0)
            (name "@ConstPropMulPat")
            (premises
              prf0
              (Fiat (= (Const 3) (Const 3)))
              (Fiat (= (Const 2) (Const 2))))
            (substitution
              (lhs (Const 3))
              (rhs (Const 2)))))
        prf0)
      (substitution
        (lhs t0)
        (rhs (Const 4))))))"#,
            &index,
        );

        assert!(typst.contains("$3 * 2 arrow.r.double 6$"));
        assert!(typst.contains("$6 + 4 arrow.r.double 10$"));
        assert!(!typst.contains("$3 * 2 + 4 arrow.r.double 10$"));
        assert!(!typst.contains("$6 arrow.r.double 3 * 2$"));
    }

    #[test]
    fn constant_prop_proof_uses_typst_term_templates_from_rules_template() {
        let templates = ProofRulesTemplateIndex::from_default_path()
            .expect("rules.template should be available for const prop proof rendering");

        let typst = render_proof_text_typst(
            r#"(let t0 (Mul (Const 3) (Const 2)))
(let t1 (Add t0 (Const 4)))
(let prop0 (= t1 (Const 10)))
(let prf0 (Fiat (= t1 t1)))
(Trans
  prop0
  prf0
  (Sym
    prop0
    (Rule
      (= (Const 10) t1)
      (name "@ConstPropAddPat")
      (premises
        (Fiat (= (Const 4) (Const 4)))
        (Sym
          (= t0 (Const 6))
          (Rule
            (= (Const 6) t0)
            (name "@ConstPropMulPat")
            (premises
              (Fiat (= (Const 3) (Const 3)))
              (Fiat (= (Const 2) (Const 2)))
              (Fiat (= t0 t0)))
            (substitution
              (expr13 (Const 3))
              (expr14 (Const 2))
              (expr15 t0)
              (expr13num 3)
              (expr14num 2))))
        prf0)
      (substitution
        (expr9 t1)
        (expr8 (Const 4))
        (expr7 t0)
        (expr7num 6)
        (expr8num 4)))))"#,
            &templates,
        );

        let proposition_idx = typst
            .find("Proposition")
            .expect("proposition title should be rendered first");
        let first_step_idx = typst
            .find("$3 * 2 arrow.r.double 6$")
            .expect("first proof step should still be rendered");
        let state0_idx = typst
            .find("[0]")
            .expect("initial proposition should be numbered");
        let state1_idx = typst
            .find("[1]")
            .expect("first derived proposition should be numbered");
        let state2_idx = typst
            .find("[2]")
            .expect("second derived proposition should be numbered");
        let downward_arrow_idx = typst
            .find("↓")
            .expect("each step should show a downward arrow between propositions");
        let arrow_step_grid_idx = typst
            .find(
                r#"columns: (auto, 1fr),
      gutter: 12pt,
      align: top"#,
            )
            .expect("step rewrite details should be top-aligned beside the downward arrow");
        let first_rule_name_idx = typst
            .find("ConstPropMulPat")
            .expect("first proof step should render its rule name");
        let rule_detail_grid_idx = typst
            .find("columns: (1fr, auto)")
            .expect("rule formula should be rendered to the left of substitutions");
        let first_substitution_idx = typst
            .find("$l = 3, r = 2, p = 3 * 2$")
            .expect("first proof step should render its substitution");
        let first_rule_formula_idx = typst
            .find("upright(\"None\")")
            .expect("first proof step should render its rule formula");
        assert!(proposition_idx < state0_idx);
        assert!(state0_idx < first_step_idx);
        assert!(arrow_step_grid_idx < downward_arrow_idx);
        assert!(state0_idx < downward_arrow_idx);
        assert!(downward_arrow_idx < first_step_idx);
        assert!(first_step_idx < first_rule_name_idx);
        assert!(first_rule_name_idx < rule_detail_grid_idx);
        assert!(rule_detail_grid_idx < first_rule_formula_idx);
        assert!(first_rule_formula_idx < first_substitution_idx);
        assert!(first_substitution_idx < state1_idx);
        assert!(first_step_idx < state1_idx);
        assert!(state1_idx < state2_idx);
        assert!(!typst.contains("Prop 0"));
        assert!(typst.contains(
            r##"$#box(
  fill: rgb("#fff4a3"),
  radius: 2pt,
  inset: (x: 2pt, y: 0.5pt),
)[$3 * 2$] + 4 = 10$"##
        ));
        assert!(typst.contains(
            r##"$#box(
  fill: rgb("#fff4a3"),
  radius: 2pt,
  inset: (x: 2pt, y: 0.5pt),
)[$#box(
  fill: rgb("#d7f7dd"),
  radius: 2pt,
  inset: (x: 2pt, y: 0.5pt),
)[$6$] + 4$] = 10$"##
        ));
        assert!(typst.contains(
            r##"$#box(
  fill: rgb("#d7f7dd"),
  radius: 2pt,
  inset: (x: 2pt, y: 0.5pt),
)[$10$] = 10$"##
        ));
        assert!(typst.contains("$3 * 2 arrow.r.double 6$"));
        assert!(typst.contains("$6 + 4 arrow.r.double 10$"));
        assert!(!typst.contains("$3 * 2 + 4 arrow.r.double 10$"));
        assert!(!typst.contains("upright(\"Mul\")"));
        assert!(!typst.contains("upright(\"Add\")"));
    }

    #[test]
    fn concise_proof_moves_step_details_below_props_and_links_highlights() {
        let templates = ProofRulesTemplateIndex::from_default_path()
            .expect("rules.template should be available for const prop proof rendering");

        let typst = ProofSvgFormatter::default()
            .concise(true)
            .render_document_typst(
                r#"(let t0 (Mul (Const 3) (Const 2)))
(let t1 (Add t0 (Const 4)))
(let prop0 (= t1 (Const 10)))
(let prf0 (Fiat (= t1 t1)))
(Trans
  prop0
  prf0
  (Sym
    prop0
    (Rule
      (= (Const 10) t1)
      (name "@ConstPropAddPat")
      (premises
        (Fiat (= (Const 4) (Const 4)))
        (Sym
          (= t0 (Const 6))
          (Rule
            (= (Const 6) t0)
            (name "@ConstPropMulPat")
            (premises
              (Fiat (= (Const 3) (Const 3)))
              (Fiat (= (Const 2) (Const 2)))
              (Fiat (= t0 t0)))
            (substitution
              (expr13 (Const 3))
              (expr14 (Const 2))
              (expr15 t0)
              (expr13num 3)
              (expr14num 2))))
        prf0)
      (substitution
        (expr9 t1)
        (expr8 (Const 4))
        (expr7 t0)
        (expr7num 6)
        (expr8num 4)))))"#,
                &templates,
            );

        let state0_idx = typst
            .find("[0]")
            .expect("initial proposition should be numbered");
        let state1_idx = typst
            .find("[1]")
            .expect("first derived proposition should be numbered");
        let state2_idx = typst
            .find("[2]")
            .expect("second derived proposition should be numbered");
        let step1_anchor_idx = typst
            .rfind("<proof-step-1>")
            .expect("first step detail should be anchored");
        let step2_anchor_idx = typst
            .rfind("<proof-step-2>")
            .expect("second step detail should be anchored");

        assert!(typst.contains(r##"#link(<proof-step-1>)["##));
        assert!(typst.contains(r##"#link(<proof-step-2>)["##));
        assert!(state0_idx < state1_idx);
        assert!(state1_idx < state2_idx);
        assert!(state2_idx < step1_anchor_idx);
        assert!(step1_anchor_idx < step2_anchor_idx);
        assert!(typst.find("ConstPropMulPat").unwrap() > state2_idx);
        assert!(typst.find("ConstPropAddPat").unwrap() > state2_idx);
        assert!(typst.contains("$3 * 2 arrow.r.double 6$"));
        assert!(typst.contains("$6 + 4 arrow.r.double 10$"));
        assert!(!typst.contains("↓"));
        assert!(!typst.contains("Prop 0"));
    }

    #[test]
    fn concise_svg_postprocess_makes_highlight_labels_clickable() {
        let svg = r##"<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
  <g class="typst-group" data-typst-label="proof-link-proof-step-1">
    <g><path fill="#fff4a3"/></g>
  </g>
  <g class="typst-group" data-typst-label="proof-step-1">
    <path fill="#f8fafc"/>
  </g>
</svg>"##;
        let processed = inject_concise_proof_svg_links(svg);
        let link_idx = processed
            .find(r##"<a href="#proof-step-1" xlink:href="#proof-step-1""##)
            .expect("proof link label should be wrapped in an SVG anchor");
        let highlight_idx = processed
            .find(r##"data-typst-label="proof-link-proof-step-1""##)
            .expect("linked highlight group should be preserved");
        let target_idx = processed
            .find(r##"data-typst-label="proof-step-1" id="proof-step-1""##)
            .expect("proof step label should become an SVG id target");

        assert!(link_idx < highlight_idx);
        assert!(highlight_idx < target_idx);
        assert!(processed.contains("</a>"));
    }

    #[test]
    fn prefers_semantic_rule_variable_labels_over_internal_expr_names() {
        let index = ProofRulesTemplateIndex::from_json_str(&template_json(
            r#"{
      "id": "src_test_rs__1_1__ConstPropAddPat",
      "display_name": "ConstPropAddPat",
      "rule_name": "ConstPropAddPat",
      "file": "src/test.rs",
      "line": 1,
      "column": 1,
      "ok": true,
      "pattern": {
        "typst_templates": [
          {
            "variant_name": "Const",
            "template": "{num}",
            "fields": ["num"]
          },
          {
            "variant_name": "Mul",
            "template": "{lhs} * {rhs}",
            "fields": ["lhs", "rhs"]
          },
          {
            "variant_name": "Add",
            "template": "{lhs} + {rhs}",
            "fields": ["lhs", "rhs"]
          }
        ],
        "precedence_templates": [
          {
            "variant_name": "Const",
            "precedence": 100
          },
          {
            "variant_name": "Mul",
            "precedence": 20
          },
          {
            "variant_name": "Add",
            "precedence": 10
          }
        ],
        "math_view": {
          "premises": [
            {
              "target_id": "l",
              "label": "l: Const",
              "plain_source": "l",
              "colored_source": "l"
            },
            {
              "target_id": "r",
              "label": "r: Const",
              "plain_source": "r",
              "colored_source": "r"
            },
            {
              "target_id": "p",
              "label": "p: Add",
              "plain_source": "upright(\"Add\")(l, r)",
              "colored_source": "upright(\"Add\")(l, r)"
            }
          ],
          "formula_source": {
            "plain": "frac(l \\ r \\ upright(\"Add\")(l, r), upright(\"Add\")(l, r) arrow.r.double upright(\"Const\")(upright(\"cal\")))"
          }
        }
      }
    }"#,
        ))
        .expect("template should parse");

        let typst = render_proof_text_typst(
            r#"(let t0 (Mul (Const 3) (Const 2)))
(let t1 (Add t0 (Const 4)))
(Rule (= (Const 10) t1)
  (name "@ConstPropAddPat")
  (premises)
  (substitution
    (expr9 t1)
    (expr8 (Const 4))
    (expr7num 6)
    (expr7 t0)
    (expr8num 4)))"#,
            &index,
        );

        assert!(typst.contains("l ="));
        assert!(typst.contains("r ="));
        assert!(typst.contains("p ="));
        assert!(!typst.contains("expr7num"));
        assert!(!typst.contains("expr8num"));
    }
}
