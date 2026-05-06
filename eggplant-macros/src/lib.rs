// use core::panic;
use eggplant_transpiler::CodeGenOptions;
use helper::*;
use proc_macro2::TokenStream;
use quote::quote;
use syn::{Data, DeriveInput, parse_macro_input};
mod helper;
use helper::{E, W};

mod enum_related;
mod enum_slot_related;
mod slotted;
mod transpiler;
mod vanilla;

#[proc_macro_attribute]
pub fn func(
    attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    vanilla::func(attr, item)
}

#[proc_macro_attribute]
pub fn relation(
    attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    vanilla::relation(attr, item)
}

/// generate `egglog` language from `rust native structure`   
///
/// # Example:  
///     
/// ```ignore
/// #[allow(unused)]
/// #[derive(Debug, Clone, EgglogTy)]
/// enum Duration {
///     DurationBySecs {
///         seconds: f64,
///     },
///     DurationByMili {
///         milliseconds: f64,
///     },
/// }
/// ```
/// is transformed to
///
///
/// ```ignore
/// #[derive(Debug, Clone)]
/// pub struct DurationNode {
///     ty: _DurationNode,
///     #[deref]
///     sym: DurationSym,
/// }
///
/// fn to_egglog(&self) -> String {
///     match &self.ty {
///         _DurationNode::DurationBySecs { seconds } => {
///             format!("(let {} (DurationBySecs  {:.3}))", self.sym, seconds)
///         }
///         _DurationNode::DurationByMili { milliseconds } => {
///             format!("(let {} (DurationByMili  {:.3}))", self.sym, milliseconds)
///         }
///     }
/// }
/// impl crate::EgglogTy for Duration {
///     const SORT_DEF: crate::TySort =
///         crate::TySort(stringify!((Duration()(DurationByMili f64))));
/// }
/// ```
/// so that you can directly use to_egglog to generate let statement in eggglog
///
/// also there is a type def statement generated and specialized new function
///
///
#[proc_macro_attribute]
pub fn dsl(
    attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    vanilla::dsl(attr, item)
}
#[proc_macro_attribute]
pub fn slotted_dsl(
    attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    slotted::slotted_dsl(attr, item)
}

#[proc_macro_attribute]
/// pattern vars
pub fn pat_vars(
    attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    vanilla::pat_vars(attr, item)
}
#[proc_macro_attribute]
/// pattern vars
pub fn slotted_pat_vars(
    attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    slotted::slotted_pat_vars(attr, item)
}

#[proc_macro_attribute]
/// pattern vars
pub fn pat_vars_catch(
    _attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let input = parse_macro_input!(item as DeriveInput);
    let expanded = match &input.data {
        Data::Struct(data_struct) => {
            let field_idents = data_struct
                .fields
                .iter()
                .map(|f| f.ident.as_ref().unwrap())
                .collect::<Vec<_>>();

            let ident = &input.ident;
            quote! {
                #[#EP::pat_vars]
                #input
                #ident::new(
                    #(#field_idents),*
                )
            }
        }
        _ => {
            panic!("only struct is supported")
        }
    };
    expanded.into()
}
#[proc_macro_attribute]
pub fn slotted_pat_vars_catch(
    _attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let input = parse_macro_input!(item as DeriveInput);
    let expanded = match &input.data {
        Data::Struct(data_struct) => {
            let field_idents = data_struct
                .fields
                .iter()
                .map(|f| f.ident.as_ref().unwrap())
                .collect::<Vec<_>>();

            let ident = &input.ident;
            quote! {
                #[#EP::slotted_pat_vars]
                #input
                #ident::new(
                    #(#field_idents),*
                )
            }
        }
        _ => {
            panic!("only struct is supported")
        }
    };
    expanded.into()
}

#[proc_macro_attribute]
/// base_ty define,
///
/// # Mention
/// this macro requires enum and should impl Default Serialize Deserialize Hash Debug trait
pub fn base_ty(
    attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    vanilla::base_ty(attr, item)
}

#[proc_macro_attribute]
/// # Mention
/// this macro requires enum and should impl Default Serialize Deserialize Hash Debug trait
pub fn slotted_base_ty(
    attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    vanilla::base_ty(attr, item)
}

#[proc_macro_attribute]
pub fn container(
    attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    dsl(attr, item)
}

#[proc_macro_attribute]
pub fn singleton_getter(
    attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    vanilla::singleton_getter(attr, item)
}

/// Transpile macro that converts egglog DSL to Rust code
///
/// # Usage
/// ```ignore
/// datatype! {
///     (datatype Math (MNum i64:args_name "num") (MAdd Math Math:args_name "l,r"))
/// }
/// ```
#[proc_macro]
pub fn datatype(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let dsl_code = input.to_string();

    // Use the actual transpiler from eggplant_transpiler
    let transpiler = transpiler::Transpiler::with_options(CodeGenOptions {
        omit_main: true,
        omit_use_statements: true,
        omit_ruleset_definitions: true,
        omit_run_ruleset_calls: true,
        omit_global_singleton_definitions: true,
        omit_datatype: false,
        omit_head_annotation: true,
    });
    let rust_code = transpiler.transpile(&dsl_code);
    let rust_code = rust_code.parse::<TokenStream>().unwrap();

    let input = TokenStream::from(input);
    let expanded = quote! {
        datatype(#input)
        #[doc = "DSl Generated"]
        #rust_code
    };

    proc_macro::TokenStream::from(expanded)
}
#[proc_macro]
/// Transpile egglog Rewrite into Rust code
/// ```ignore
/// rule! {
///     (datatype Math (MNum i64:args_name "num") (MAdd Math Math:args_name "l,r"))
///     (rewrite (MAdd x y) (MAdd y x))
/// }
/// ```
pub fn rule(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let dsl_code = input.to_string();
    // Use the actual transpiler from eggplant_transpiler
    let transpiler = transpiler::Transpiler::with_options(CodeGenOptions {
        omit_main: true,
        omit_use_statements: true,
        omit_ruleset_definitions: true,
        omit_run_ruleset_calls: true,
        omit_global_singleton_definitions: true,
        omit_datatype: true,
        omit_head_annotation: true,
    });
    let rust_code = transpiler.transpile(&dsl_code);
    eprintln!("{}", rust_code);
    if rust_code == "" {
        panic!(
            "no rule is generated, please check your dsl code: {}",
            dsl_code
        );
    }
    let rust_code = rust_code.parse::<TokenStream>().unwrap();
    // let doc_dsl_code = format!("rule!({})", dsl_code);

    let input = TokenStream::from(input);
    let expanded = quote! {
        rule(#input)
        #[doc = "Rule Generated"]
        #rust_code
    };

    proc_macro::TokenStream::from(expanded)
}

#[proc_macro]
/// Transpile egglog Rewrite into Rust code
/// ```ignore
/// egglog! {
///     (datatype Math (MNum i64:args_name "num") (MAdd Math Math:args_name "l,r"))
///     (rewrite (MAdd x y) (MAdd y x))
/// }
/// ```
pub fn egglog(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let dsl_code = input.to_string();
    // Use the actual transpiler from eggplant_transpiler
    let transpiler = transpiler::Transpiler::with_options(CodeGenOptions {
        omit_main: false,
        omit_use_statements: false,
        omit_ruleset_definitions: false,
        omit_run_ruleset_calls: false,
        omit_global_singleton_definitions: false,
        omit_datatype: false,
        omit_head_annotation: false,
    });
    let rust_code = transpiler.transpile(&dsl_code);
    eprintln!("{}", rust_code);
    if rust_code == "" {
        panic!(
            "no rule is generated, please check your dsl code: {}",
            dsl_code
        );
    }
    let rust_code = rust_code.parse::<TokenStream>().unwrap();
    // let doc_dsl_code = format!("rule!({})", dsl_code);

    let input = TokenStream::from(input);
    let expanded = quote! {
        rule(#input)
        #[doc = "Rule Generated"]
        #rust_code
    };

    proc_macro::TokenStream::from(expanded)
}
