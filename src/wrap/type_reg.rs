use std::collections::HashMap;

use derive_more::Deref;
use egglog::{
    Term, TermDag, TermId,
    ast::{Command, GenericExpr, Literal, Schema, Span, Subdatatypes, Variant},
    sort::Q,
    span, var,
};

use crate::wrap::{
    EgglogEnumVariantTy, FromPlainValues, PatRecSgl, PatVars, TermToNode, ToStrArcSort, Value,
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
/// basic type only need default [`EgglogTy::Valued`]
/// while for EnumTy they need to specify ValuedVars when pattern recognized to be values
pub trait EgglogTy {
    const TY_NAME: &'static str;
    const TY_NAME_LOWER: &'static str;
    type Valued: FromPlainValues;
    type EnumVariantMarker: EgglogEnumVariantTy;
}
impl<T: EgglogTy + ToStrArcSort, PR: PatRecSgl> PatVars<PR> for T {
    type Valued = T::Valued;
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

#[derive(Deref)]
pub struct TyConstructors(pub &'static [TyConstructor]);
pub struct TySortString(pub &'static str);
pub struct FuncSortString(pub &'static str);
pub struct TyConstructor {
    pub cons_name: &'static str,
    pub input: &'static [&'static str],
    pub output: &'static str,
    pub cost: Option<u64>,
    pub unextractable: bool,
    pub term_to_node: TermToNode,
}

// collect all sorts into inventory, so that we could send the definitions of types.
inventory::collect!(Decl);

pub enum Decl {
    EgglogMultiConTy {
        name: &'static str,
        cons: &'static TyConstructors,
    },
    EgglogContainerTy {
        name: &'static str,
        ele_ty_name: &'static str,
        def_operator: &'static str,
        term_to_node: TermToNode,
    },
    EgglogFuncTy {
        name: &'static str,
        input: &'static [&'static str],
        output: &'static str,
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
    container_node_fns_map: HashMap<(&'static str, &'static str), TermToNode>,
}
impl EgglogTypeRegistry {
    pub fn new_with_inventory() -> Self {
        let (enum_node_fns_map, variant2type_map) = Self::collect_enum_fns();
        let container_node_fns_map = Self::collect_container_fns();
        log::debug!("container node:{:?}", container_node_fns_map);
        Self {
            enum_node_fns_map,
            container_node_fns_map,
            variant2type_map,
        }
    }
    pub fn collect_enum_fns() -> (
        HashMap<&'static str, TermToNode>,
        HashMap<&'static str, &'static str>,
    ) {
        let mut fns_map = HashMap::new();
        let mut variant2type_map = HashMap::new();
        inventory::iter::<Decl>
            .into_iter()
            .for_each(|decl| match decl {
                Decl::EgglogMultiConTy { name, cons } => cons.iter().for_each(|con| {
                    fns_map.insert(con.cons_name, con.term_to_node);
                    variant2type_map.insert(con.cons_name, *name);
                }),
                _ => {}
            });
        (fns_map, variant2type_map)
    }
    pub fn collect_container_fns() -> HashMap<(&'static str, &'static str), TermToNode> {
        let mut map = HashMap::new();
        inventory::iter::<Decl>
            .into_iter()
            .for_each(|decl| match *decl {
                Decl::EgglogContainerTy {
                    name: _,
                    ele_ty_name,
                    def_operator,
                    term_to_node,
                } => {
                    map.insert((ele_ty_name, def_operator), term_to_node);
                }
                _ => {}
            });
        map
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
                                })
                                .collect(),
                        ),
                    ));
                }
                Decl::EgglogContainerTy {
                    name,
                    ele_ty_name,
                    def_operator: _,
                    term_to_node: _,
                } => {
                    let ele_ty = ele_ty_name.to_owned();
                    let ele = var!(ele_ty);
                    types.push((
                        span!(),
                        name.to_string(),
                        Subdatatypes::NewSort("Vec".to_string(), vec![ele]),
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
                } => {
                    commands.push(Command::Function {
                        span: span!(),
                        name: name.to_string(),
                        schema: Schema {
                            input: input.iter().map(<&str>::to_string).collect(),
                            output: output.to_string(),
                        },
                        merge: Some(GenericExpr::Var(span!(), "new".to_owned())),
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
}

impl<T> FromPlainValues for Value<T> {
    fn from_plain_values(values: &mut impl Iterator<Item = egglog::Value>) -> Self {
        Value::new(values.next().unwrap())
    }
}
