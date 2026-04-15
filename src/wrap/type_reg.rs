use std::collections::HashMap;

use derive_more::Deref;
use egglog::{
    EGraph, Term, TermDag, TermId,
    ast::{Command, GenericExpr, Literal, RustSpan, Schema, Span, Subdatatypes, Variant},
    prelude::BaseSort,
    sort::Q,
    span, var,
};

use crate::{
    prelude::SlotMeta,
    wrap::{
        EgglogEnumVariantTy, FromPlainValues, PatRecSgl, PatVars, RenderedTemplateField,
        TermToNode, ToStrArcSort, Value, render_template_with_precedence,
    },
};

pub trait EgglogContainerTy: EgglogTy {
    type EleTy: EgglogTy;
}
pub trait EgglogMultiConTy: EgglogTy {
    const CONSTRUCTORS: TyConstructors;
}
impl EgglogTy for i64 {
    const TY_NAME: &'static str = "i64";
    const TY_NAME_LOWER: &'static str = "i64";
    type Valued = Value<Self>;
    type EnumVariantMarker = ();
}
impl EgglogTy for f64 {
    const TY_NAME: &'static str = "f64";
    const TY_NAME_LOWER: &'static str = "f64";
    type Valued = Value<Self>;
    type EnumVariantMarker = ();
}
impl EgglogTy for bool {
    const TY_NAME: &'static str = "bool";
    const TY_NAME_LOWER: &'static str = "bool";
    type Valued = Value<Self>;
    type EnumVariantMarker = ();
}
impl EgglogTy for String {
    const TY_NAME: &'static str = "String";
    const TY_NAME_LOWER: &'static str = "string";
    type Valued = Value<Self>;
    type EnumVariantMarker = ();
}
impl EgglogTy for &'static str {
    const TY_NAME: &'static str = "&'static str";
    const TY_NAME_LOWER: &'static str = "&'static str";
    type Valued = Value<Self>;
    type EnumVariantMarker = ();
}
/// basic type only need default [`EgglogTy::Valued`]
/// while for EnumTy they need to specify ValuedVars when pattern recognized to be values
pub trait EgglogTy: 'static {
    const TY_NAME: &'static str;
    const TY_NAME_LOWER: &'static str;
    type Valued: FromPlainValues;
    type EnumVariantMarker: EgglogEnumVariantTy;
    fn get_arc_sort(egraph: &EGraph) -> egglog::ArcSort {
        egraph
            .get_sort_by_name(Self::TY_NAME)
            .expect("sort should be registered before get")
            .clone()
    }
}
impl<T: EgglogTy + ToStrArcSort, PR: PatRecSgl> PatVars<PR> for T {
    type Valued = T::Valued;
    fn metas_iter(&self) -> impl Iterator<Item = SlotMeta> {
        std::iter::empty()
    }
}
impl<T: EgglogTy> ToStrArcSort for T {
    fn to_str_arcsort(&self, _egraph: &egglog::EGraph) -> Vec<(super::VarName, egglog::ArcSort)> {
        todo!()
    }
}
impl EgglogTy for Q {
    const TY_NAME: &'static str = "BigRational";
    const TY_NAME_LOWER: &'static str = "big_rational";
    type Valued = Value<Self>;
    type EnumVariantMarker = ();
}

#[derive(Deref, Debug)]
pub struct TyConstructors(pub &'static [TyConstructor]);
pub struct TySortString(pub &'static str);
pub struct FuncSortString(pub &'static str);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SchemaFieldKind {
    Base,
    Complex,
    Container,
}
#[derive(Debug)]
pub struct TyConstructor {
    pub cons_name: &'static str,
    pub input: &'static [&'static str],
    pub input_field_names: &'static [&'static str],
    pub input_field_kinds: &'static [SchemaFieldKind],
    pub output: &'static str,
    pub cost: Option<u64>,
    pub unextractable: bool,
    pub display_template: Option<&'static str>,
    pub typst_template: Option<&'static str>,
    pub precedence: u16,
    pub term_to_node: TermToNode,
}
pub struct UserBaseSort {
    pub sort_insert_fn: fn(&mut EGraph),
}
pub struct UserContainerSort {
    pub sort_insert_fn: fn(&mut EGraph),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DslFieldKind {
    Base,
    UserBase,
    Complex,
    Container,
}

#[derive(Debug)]
pub struct DslFieldDecl {
    pub name: &'static str,
    pub ty: &'static str,
    pub kind: DslFieldKind,
}

#[derive(Debug)]
pub struct DslVariantDecl {
    pub owner_ty: &'static str,
    pub variant_name: &'static str,
    pub fields: &'static [DslFieldDecl],
    pub display_template: Option<&'static str>,
    pub typst_template: Option<&'static str>,
    pub precedence: u16,
}

// collect all sorts into inventory, so that we could send the definitions of types.
inventory::collect!(Decl);
inventory::collect!(UserBaseSort);
inventory::collect!(UserContainerSort);
inventory::collect!(DslVariantDecl);

#[derive(Debug)]
pub enum Decl {
    EgglogMultiConTy {
        name: &'static str,
        cons: &'static TyConstructors,
    },
    EgglogContainerTy {
        name: &'static str,
        ele_ty_name: &'static str,
        constructor_str: &'static str,
        ty_str: &'static str,
        term_to_node: TermToNode,
    },
    EgglogFuncTy {
        name: &'static str,
        input: &'static [&'static str],
        input_field_names: &'static [&'static str],
        output: &'static str,
        display_template: Option<&'static str>,
        typst_template: Option<&'static str>,
        merge: Option<&'static str>,
        precedence: u16,
    },
    EgglogRelationTy {
        name: &'static str,
        input: &'static [&'static str],
        typst_template: Option<&'static str>,
        precedence: u16,
    },
    EgglogRule {
        name: &'static str,
        input: &'static [&'static str],
        output: &'static str,
    },
}

#[derive(Debug)]
pub struct EgglogTypeRegistry {
    enum_node_fns_map: HashMap<&'static str, TermToNode>,
    variant2type_map: HashMap<&'static str, &'static str>,
    constructor_meta_map: HashMap<&'static str, &'static TyConstructor>,
    dsl_variant_map: HashMap<(&'static str, &'static str), &'static DslVariantDecl>,
    func_template_map:
        HashMap<&'static str, (Option<&'static str>, Option<&'static str>, u16)>,
    func_field_names_map: HashMap<&'static str, &'static [&'static str]>,
    container_node_fns_map: HashMap<(&'static str, &'static str), TermToNode>,
}
impl EgglogTypeRegistry {
    pub fn new_with_inventory() -> Self {
        let (enum_node_fns_map, variant2type_map, constructor_meta_map) = Self::collect_enum_fns();
        let dsl_variant_map = Self::collect_dsl_variants();
        let (func_template_map, func_field_names_map) = Self::collect_func_templates();
        let container_node_fns_map = Self::collect_container_fns();
        log::debug!("container node:{:?}", container_node_fns_map);
        Self {
            enum_node_fns_map,
            container_node_fns_map,
            variant2type_map,
            constructor_meta_map,
            dsl_variant_map,
            func_template_map,
            func_field_names_map,
        }
    }
    pub fn collect_enum_fns() -> (
        HashMap<&'static str, TermToNode>,
        HashMap<&'static str, &'static str>,
        HashMap<&'static str, &'static TyConstructor>,
    ) {
        let mut fns_map = HashMap::new();
        let mut variant2type_map = HashMap::new();
        let mut constructor_meta_map = HashMap::new();
        inventory::iter::<Decl>
            .into_iter()
            .for_each(|decl| match decl {
                Decl::EgglogMultiConTy { name, cons } => cons.iter().for_each(|con| {
                    fns_map.insert(con.cons_name, con.term_to_node);
                    variant2type_map.insert(con.cons_name, *name);
                    constructor_meta_map.insert(con.cons_name, con);
                }),
                _ => {}
            });
        (fns_map, variant2type_map, constructor_meta_map)
    }
    pub fn collect_container_fns() -> HashMap<(&'static str, &'static str), TermToNode> {
        let mut map = HashMap::new();
        inventory::iter::<Decl>
            .into_iter()
            .for_each(|decl| match *decl {
                Decl::EgglogContainerTy {
                    name: _,
                    ele_ty_name,
                    constructor_str,
                    term_to_node,
                    ty_str: _,
                } => {
                    map.insert((ele_ty_name, constructor_str), term_to_node);
                }
                _ => {}
            });
        map
    }

    pub fn collect_dsl_variants() -> HashMap<(&'static str, &'static str), &'static DslVariantDecl> {
        let mut map = HashMap::new();
        for decl in inventory::iter::<DslVariantDecl> {
            map.insert((decl.owner_ty, decl.variant_name), decl);
        }
        map
    }

    pub fn collect_func_templates(
    ) -> (
        HashMap<&'static str, (Option<&'static str>, Option<&'static str>, u16)>,
        HashMap<&'static str, &'static [&'static str]>,
    ) {
        let mut map = HashMap::new();
        let mut fields = HashMap::new();
        for decl in inventory::iter::<Decl> {
            if let Decl::EgglogFuncTy {
                name,
                input_field_names,
                display_template,
                typst_template,
                precedence,
                ..
            } = decl
            {
                map.insert(*name, (*display_template, *typst_template, *precedence));
                fields.insert(*name, *input_field_names);
            }
        }
        (map, fields)
    }
    pub fn collect_type_defs() -> Vec<Command> {
        let mut commands = vec![];
        // split decls to avoid undefined sort
        let mut types = Vec::<(Span, String, Subdatatypes)>::new();
        for decl in inventory::iter::<Decl> {
            match decl {
                Decl::EgglogMultiConTy { name, cons } => {
                    types.push((
                        span!(),
                        name.to_string(),
                        Subdatatypes::Variants(
                            cons.iter()
                                .map(|x| Variant {
                                    span: span!(),
                                    name: x.cons_name.to_string(),
                                    types: x.input.iter().map(|y| y.to_string()).collect(),
                                    cost: x.cost,
                                    unextractable: x.unextractable,
                                })
                                .collect(),
                        ),
                    ));
                }
                Decl::EgglogContainerTy {
                    name,
                    ele_ty_name,
                    constructor_str: _,
                    term_to_node: _,
                    ty_str,
                } => {
                    let ele_ty = ele_ty_name.to_owned();
                    let ele = var!(ele_ty);
                    types.push((
                        span!(),
                        name.to_string(),
                        Subdatatypes::NewSort(ty_str.to_string(), vec![ele]),
                    ));
                }
                _ => {
                    // do nothing
                }
            }
        }
        commands.push(Command::Datatypes {
            span: span!(),
            datatypes: types,
        });
        for decl in inventory::iter::<Decl> {
            match decl {
                Decl::EgglogFuncTy {
                    name,
                    input,
                    output,
                    merge,
                    ..
                } => {
                    commands.push(Command::Function {
                        span: span!(),
                        name: name.to_string(),
                        schema: Schema {
                            input: input.iter().map(<&str>::to_string).collect(),
                            output: output.to_string(),
                        },
                        merge: merge.map(|m| GenericExpr::Var(span!(), m.to_owned())),
                    });
                }
                Decl::EgglogRelationTy { name, input, .. } => {
                    commands.push(Command::Relation {
                        span: span!(),
                        name: name.to_string(),
                        inputs: input.iter().map(<&str>::to_string).collect(),
                    });
                }
                _ => {}
            }
        }
        commands
    }
    /// warnning: This funciton returns things like Expr<(),Num> which means you should reform
    /// it into () after
    pub fn get_fn(&self, term_id: TermId, term_dag: &TermDag) -> Option<TermToNode> {
        match term_dag.get(term_id) {
            Term::Lit(_) => None,
            Term::Var(_) => None,
            Term::App(name, items) => self.enum_node_fns_map.get(name.as_str()).or_else(|| {
                self.container_node_fns_map.get(&(
                    match term_dag.get(*items.get(0).unwrap()) {
                        Term::Lit(literal) => match literal {
                            Literal::Int(_) => "i64",
                            Literal::Float(_) => "f64",
                            Literal::String(_) => "string",
                            Literal::Bool(_) => "bool",
                            Literal::Unit => {
                                panic!()
                            }
                        },
                        Term::Var(_) => {
                            panic!()
                        }
                        Term::App(succ_variant, _) => {
                            log::trace!("sub_name {}", succ_variant);
                            let ty = self.variant2type_map.get(succ_variant.as_str()).unwrap();
                            ty
                        }
                    },
                    name.as_str(),
                ))
            }),
        }
        .cloned()
    }

    pub fn get_constructor(&self, cons_name: &str) -> Option<&TyConstructor> {
        self.constructor_meta_map.get(cons_name).copied()
    }

    pub fn get_dsl_variant(
        &self,
        owner_ty: &str,
        variant_name: &str,
    ) -> Option<&DslVariantDecl> {
        self.dsl_variant_map.get(&(owner_ty, variant_name)).copied()
    }

    pub fn get_dsl_variant_for_constructor(&self, cons_name: &str) -> Option<&DslVariantDecl> {
        let owner_ty = self.variant2type_map.get(cons_name).copied()?;
        self.get_dsl_variant(owner_ty, cons_name)
    }

    pub fn get_func_templates(
        &self,
        func_name: &str,
    ) -> Option<(Option<&'static str>, Option<&'static str>, u16)> {
        self.func_template_map.get(func_name).copied()
    }

    pub fn get_func_field_names(&self, func_name: &str) -> Option<&'static [&'static str]> {
        self.func_field_names_map.get(func_name).copied()
    }
}

pub fn render_term_with_registry(
    registry: &EgglogTypeRegistry,
    termdag: &TermDag,
    term: &Term,
    prefer_typst: bool,
) -> RenderedTemplateField<'static> {
    match term {
        Term::Lit(lit) => RenderedTemplateField::atom(lit.to_string()),
        Term::Var(v) => RenderedTemplateField::atom(v.clone()),
        Term::App(head, children) => {
            let (template, precedence, field_names): (Option<&'static str>, u16, Vec<&'static str>) =
                if let Some(variant) = registry.get_dsl_variant_for_constructor(head) {
                    (
                        if prefer_typst {
                            variant.typst_template.or(variant.display_template)
                        } else {
                            variant.display_template.or(variant.typst_template)
                        },
                        variant.precedence,
                        variant.fields.iter().map(|field| field.name).collect(),
                    )
                } else if let Some(constructor) = registry.get_constructor(head) {
                    (
                        if prefer_typst {
                            constructor.typst_template.or(constructor.display_template)
                        } else {
                            constructor.display_template.or(constructor.typst_template)
                        },
                        constructor.precedence,
                        constructor.input_field_names.to_vec(),
                    )
                } else if let Some((display_template, typst_template, precedence)) =
                    registry.get_func_templates(head)
                {
                    let field_names = registry
                        .get_func_field_names(head)
                        .map(|names| names.to_vec())
                        .unwrap_or_default();
                    (
                        if prefer_typst {
                            typst_template.or(display_template)
                        } else {
                            display_template.or(typst_template)
                        },
                        precedence,
                        field_names,
                    )
                } else {
                    return RenderedTemplateField::atom(termdag.to_string(term));
                };

            let Some(template) = template else {
                return RenderedTemplateField::atom(termdag.to_string(term));
            };

            let rendered_children = children
                .iter()
                .zip(field_names.iter())
                .map(|(child_id, field_name)| {
                    (
                        *field_name,
                        render_term_with_registry(registry, termdag, termdag.get(*child_id), prefer_typst),
                    )
                })
                .collect::<Vec<_>>();

            RenderedTemplateField::new(
                render_template_with_precedence(template, precedence, &rendered_children),
                precedence,
            )
        }
    }
}

pub fn extract_value_template_string(
    egraph: &EGraph,
    sort: &egglog::ArcSort,
    value: egglog::Value,
    prefer_typst: bool,
) -> Result<String, String> {
    let canonical = egraph.get_canonical_value(value, sort);
    let (termdag, term, _) = egraph
        .extract_value(sort, canonical)
        .map_err(|err| err.to_string())?;
    let registry = EgglogTypeRegistry::new_with_inventory();
    Ok(render_term_with_registry(&registry, &termdag, &term, prefer_typst)
        .text
        .into_owned())
}

impl<T> FromPlainValues for Value<T> {
    fn from_plain_values(values: &mut impl Iterator<Item = egglog::Value>) -> Self {
        Value::new(values.next().unwrap())
    }
}

#[derive(Debug)]
pub struct StaticStrSort;
impl BaseSort for StaticStrSort {
    type Base = &'static str;

    fn name(&self) -> &str {
        "& 'static str"
    }

    fn reconstruct_termdag(
        &self,
        base_values: &egglog::sort::BaseValues,
        value: egglog::Value,
        term_dag: &mut TermDag,
    ) -> Term {
        let str: &'static str = base_values.unwrap(value);
        let term = term_dag.lit(Literal::String(str.to_string()));
        term
    }
}
