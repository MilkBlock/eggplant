use core::panic;
use proc_macro_crate::{FoundCrate, crate_name};
use proc_macro2::{Ident, Span, TokenStream};
use quote::{ToTokens, format_ident, quote};
use std::{
    marker::PhantomData,
    sync::{LazyLock, Mutex, MutexGuard},
};
use syn::{
    DataEnum, Expr, Fields, GenericArgument, Path, PathArguments, Type, Variant, parse::Parse,
    parse_str,
};

pub const PANIC_TY_LIST: [&'static str; 4] = ["i32", "u32", "u64", "f32"];
pub const EGGLOG_BASE_TY_LIST: [&'static str; 4] = ["String", "i64", "f64", "& 'static str"];
pub const EGGLOG_BASIC_TY_DEFAULT_LIST: [LazyTokenStream<Expr>; 4] = [
    LazyTokenStream::new(|| "String::new()".to_owned()),
    LazyTokenStream::new(|| "0".to_owned()),
    LazyTokenStream::new(|| "0.".to_owned()),
    LazyTokenStream::new(|| r#""""#.to_owned()),
];
pub struct EgglogUserDefined {
    user_defined: Mutex<UserDefined>,
}
unsafe impl Send for EgglogUserDefined {}
unsafe impl Sync for EgglogUserDefined {}
impl EgglogUserDefined {
    fn sgl() -> MutexGuard<'static, UserDefined> {
        static INSTANCE: std::sync::OnceLock<EgglogUserDefined> = std::sync::OnceLock::new();
        INSTANCE
            .get_or_init(|| -> Self {
                Self {
                    user_defined: Mutex::new(UserDefined {
                        containers: vec![],
                        base_types: vec![],
                    }),
                }
            })
            .user_defined
            .lock()
            .unwrap()
    }
    pub fn set(containers: Vec<Ident>, base_types: Vec<Ident>) {
        Self::sgl().containers = containers;
        Self::sgl().base_types = base_types;
    }
    pub fn contain_container(ty: &str) -> bool {
        Self::sgl()
            .containers
            .iter()
            .position(|x| x.to_string() == ty)
            .is_some()
    }
    pub fn contain_base_type(ty: &str) -> bool {
        Self::sgl()
            .base_types
            .iter()
            .position(|x| x.to_string() == ty)
            .is_some()
    }
}

pub struct UserDefined {
    containers: Vec<Ident>,
    base_types: Vec<Ident>,
}

pub static E: LazyTokenStream = LazyTokenStream::new(|| format!("{}::egglog", *EP.s));
pub static EP: LazyTokenStream = LazyTokenStream::new(|| eggplant_path());
// pub static DE: LazyTokenStream = LazyTokenStream::new(|| format!("{}::derive_more", *EP.s));
pub static W: LazyTokenStream = LazyTokenStream::new(|| format!("{}::wrap", *EP.s));
pub static ENM: LazyTokenStream = LazyTokenStream::new(|| format!("{}::strum_macros", *EP.s));
pub static EN: LazyTokenStream = LazyTokenStream::new(|| format!("{}::strum", *EP.s));
pub static INVE: LazyTokenStream = LazyTokenStream::new(|| format!("{}::inventory", *EP.s));
pub(crate) struct LazyTokenStream<T: Parse + ToTokens = Path> {
    s: LazyLock<String>,
    p: PhantomData<T>,
}
impl<T: Parse + ToTokens> LazyTokenStream<T> {
    pub const fn new(f: fn() -> String) -> Self {
        Self {
            s: LazyLock::new(f),
            p: PhantomData,
        }
    }
}
unsafe impl<T: Parse + ToTokens> Sync for LazyTokenStream<T> {}
unsafe impl<T: Parse + ToTokens> Send for LazyTokenStream<T> {}
impl<T: Parse + ToTokens> ToTokens for LazyTokenStream<T> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let p = parse_str(&*self.s).expect("can't parse this");
        T::to_tokens(&p, tokens);
    }
}

pub fn _egglog_path() -> String {
    match (
        crate_name("egglog"),
        std::env::var("CARGO_CRATE_NAME").as_deref(),
    ) {
        (Ok(FoundCrate::Itself), Ok(_)) => "::egglog".to_string(),
        (Ok(FoundCrate::Name(name)), _) => {
            let ident = proc_macro2::Ident::new(&name, Span::call_site());
            format!("::{}", ident)
        }
        _ => panic!("can't find egglog"),
    }
}
pub fn eggplant_path() -> String {
    match (
        crate_name("eggplant"),
        std::env::var("CARGO_CRATE_NAME").as_deref(),
    ) {
        (Ok(FoundCrate::Itself), Ok(_)) => "eggplant".to_string(),
        (Ok(FoundCrate::Name(name)), _) => {
            let ident = proc_macro2::Ident::new(&name, Span::call_site());
            format!("::{}", ident)
        }
        _ => panic!("can't find eggplant"),
    }
}
pub fn _derive_more_path() -> String {
    match (
        crate_name("derive_more"),
        std::env::var("CARGO_CRATE_NAME").as_deref(),
    ) {
        (Ok(FoundCrate::Itself), Ok(_)) => "::derive_more".to_string(),
        (Ok(FoundCrate::Name(name)), _) => {
            let ident = proc_macro2::Ident::new(&name, Span::call_site());
            format!("::{}", ident)
        }
        _ => panic!("can't find derive_more"),
    }
}
pub fn _inventory_path() -> String {
    match (
        crate_name("inventory"),
        std::env::var("CARGO_CRATE_NAME").as_deref(),
    ) {
        (Ok(FoundCrate::Itself), Ok(_)) => "::inventory".to_string(),
        (Ok(FoundCrate::Name(name)), _) => {
            let ident = proc_macro2::Ident::new(&name, Span::call_site());
            format!("::{}", ident)
        }
        _ => panic!("can't find inventory"),
    }
}

pub fn variant2mapped_ident_type_list(
    variant: &Variant,
    mut map_basic_ident: impl FnMut(&Ident, &TokenStream) -> Option<TokenStream>,
    mut map_complex_ident: impl FnMut(&Ident, &TokenStream) -> Option<TokenStream>,
) -> Vec<proc_macro2::TokenStream> {
    let types_and_idents = match &variant.fields {
        Fields::Named(fields_named) => fields_named.named.iter(),
        Fields::Unit => {
            panic!("add `{{}}` to the unit variant")
        }
        _ => panic!("only support named fields"),
    }
    .map(|f| {
        let f_ident = f
            .ident
            .as_ref()
            .expect("don't support unnamed field")
            .clone();
        // if it's a box type we should read the first generic
        if is_box_type(&f.ty) {
            (get_first_generic(&f.ty).clone(), f_ident)
        } else {
            (f.ty.clone(), f_ident)
        }
    })
    .map(|(f1, f2)| {
        let f1 = &f1.to_token_stream();
        match f1.to_string().as_str() {
            x if PANIC_TY_LIST.contains(&x) => {
                panic!("{} not supported", x)
            }
            x if BasicOrComplex::from(x).is_basic() => map_basic_ident(&f2, &f1),
            _ => map_complex_ident(&f2, &f1),
        }
        .map(|x| quote! {  #x})
    })
    .flatten()
    .collect();
    types_and_idents
}
pub fn variant2mapped_ident_type_list_detailed(
    variant: &Variant,
    mut map_ident: impl FnMut(&Ident, &TokenStream, BasicOrComplex) -> Option<TokenStream>,
) -> Vec<proc_macro2::TokenStream> {
    let types_and_idents = match &variant.fields {
        Fields::Named(fields_named) => fields_named.named.iter(),
        Fields::Unit => {
            panic!("add `{{}}` to the unit variant")
        }
        _ => panic!("only support named fields"),
    }
    .map(|f| {
        let f_ident = f
            .ident
            .as_ref()
            .expect("don't support unnamed field")
            .clone();
        // if it's a box type we should read the first generic
        if is_box_type(&f.ty) {
            (get_first_generic(&f.ty).clone(), f_ident)
        } else {
            (f.ty.clone(), f_ident)
        }
    })
    .map(|(f1, f2)| {
        let f1 = &f1.to_token_stream();
        match f1.to_string().as_str() {
            x if PANIC_TY_LIST.contains(&x) => {
                panic!("{} not supported", x)
            }
            _ => map_ident(&f2, &f1, BasicOrComplex::from(&f1.to_token_stream())),
        }
        .map(|x| quote! {  #x})
    })
    .flatten()
    .collect();
    types_and_idents
}
#[allow(unused)]
pub fn get_ref_type(ty: &Type) -> proc_macro2::TokenStream {
    match ty {
        Type::Path(type_path) => {
            let type_name = &type_path.path.segments.last().unwrap().ident;
            let sym_name = format_ident!("&{}", type_name); // concatenate `Sym`
            quote! { #sym_name }
        }
        _ => panic!("Unsupported type for `WithSymNode`"),
    }
}
pub fn is_container_type(ty: &Type) -> (bool, Ident) {
    if let Type::Path(type_path) = ty {
        if let Some(segment) = type_path.path.segments.last() {
            let ty_name = segment.ident.to_string();
            match ty_name.as_str() {
                "VecContainer" => {
                    return (true, segment.ident.clone());
                }
                "SetContainer" => {
                    return (true, segment.ident.clone());
                }
                _ => {
                    panic!(
                        "{} only VecContainer and SetContainer are supported",
                        ty_name,
                    );
                }
            }
        }
    }
    panic!("you should enter some container with one generic")
}
pub fn is_box_type(ty: &Type) -> bool {
    if let Type::Path(type_path) = ty {
        if let Some(segment) = type_path.path.segments.last() {
            if segment.ident == "Box" {
                return true;
            }
        }
    }
    false
}
/// mention there is no UserDefinedContainerTy
/// since egglog's default container is not typed and is not recommended
#[derive(strum_macros::EnumDiscriminants)]
pub enum BasicOrComplex {
    BaseType,
    /// User defined basic type, you can specify them in eggplant's attribute like `#[eggplant(base= X)]`
    UserDefinedBaseType,
    /// User defined container type, you can specify them in eggplant's attribute like `#[eggplant(container= X)]`
    UserDefinedContainerType,
    ComplexType,
}
impl BasicOrComplex {
    pub fn is_basic(&self) -> bool {
        match self {
            BasicOrComplex::BaseType => true,
            BasicOrComplex::UserDefinedBaseType => true,
            BasicOrComplex::UserDefinedContainerType => true,
            BasicOrComplex::ComplexType => false,
        }
    }
}
impl From<&proc_macro2::TokenStream> for BasicOrComplex {
    fn from(ty: &proc_macro2::TokenStream) -> Self {
        Self::from(ty.to_string().as_str())
    }
}
impl From<&str> for BasicOrComplex {
    fn from(ty: &str) -> Self {
        if EGGLOG_BASE_TY_LIST.contains(&ty) {
            return BasicOrComplex::BaseType;
        }
        if EgglogUserDefined::contain_base_type(&ty) {
            return BasicOrComplex::UserDefinedBaseType;
        }
        if EgglogUserDefined::contain_container(&ty) {
            return BasicOrComplex::UserDefinedContainerType;
        }
        BasicOrComplex::ComplexType
    }
}
pub fn get_first_generic(ty: &Type) -> &Type {
    if let Type::Path(type_path) = ty
        && let Some(segment) = type_path.path.segments.last()
        && let PathArguments::AngleBracketed(args) = &segment.arguments
    {
        let arg = args
            .args
            .iter()
            .nth(0)
            .expect("type should at least have one generic");
        if let GenericArgument::Type(inner_ty) = arg {
            // inner_ty is Vec<T>'s T
            return inner_ty;
        }
    }
    panic!("first generic generic can only be Type")
}

/// given variant a{ x:X, y:Y}
/// return vec![ x:XSym, y:YSym ]
pub fn variant2sym_typed_ident_list(variant: &Variant) -> Vec<proc_macro2::TokenStream> {
    variant2mapped_ident_type_list_view_container_as_complex(
        variant,
        |ident, ty| Some(quote! {#ident:#ty}),
        |ident, ty| Some(quote! {#ident:#W::Sym<#ty>}),
    )
}
pub fn variants2sym_type_list(variant: &Variant) -> Vec<proc_macro2::TokenStream> {
    variant2mapped_ident_type_list_view_container_as_complex(
        variant,
        |_ident, ty| Some(quote! {#ty}),
        |_ident, ty| Some(quote! {#W::Sym<#ty>}),
    )
}
pub fn variant2ref_node_list(variant: &Variant) -> Vec<proc_macro2::TokenStream> {
    variant2mapped_ident_type_list_view_container_as_complex(
        variant,
        |ident, ty| Some(quote! {#ident:#ty}),
        |ident, ty| Some(quote! {#ident: &#ty<T>}),
    )
}
pub fn variant2ref_node_list_except_basic(variant: &Variant) -> Vec<proc_macro2::TokenStream> {
    variant2mapped_ident_type_list_view_container_as_complex(
        variant,
        |_ident, _ty| None,
        |ident, ty| Some(quote! {#ident: &#ty<T>}),
    )
}
pub fn variant2valued_ref_node_list(variant: &Variant) -> Vec<proc_macro2::TokenStream> {
    variant2mapped_ident_type_list_view_container_as_complex(
        variant,
        |ident, ty| Some(quote! {#ident:#ty}),
        |ident, ty| Some(quote! {#ident: impl #W::Insertable<#ty<(), ()>>}),
    )
}
pub fn variant2ref_node_list_without_type(variant: &Variant) -> Vec<proc_macro2::TokenStream> {
    variant2mapped_ident_type_list_view_container_as_complex(
        variant,
        |basic, _basic_ty| Some(quote! {#basic}),
        |complex, _complex_ty| Some(quote! {#complex}),
    )
}
pub fn variant2ident_list_except_basic(variant: &Variant) -> Vec<proc_macro2::TokenStream> {
    variant2mapped_ident_type_list_view_container_as_complex(
        variant,
        |_, _| None,
        |complex, _complex_ty| Some(quote! {#complex}),
    )
}
pub fn variant2sym_list(variant: &Variant) -> Vec<proc_macro2::TokenStream> {
    variant2mapped_ident_type_list_view_container_as_complex(
        variant,
        |basic, basic_ty| Some(quote!(#basic:#basic_ty)),
        |complex, _| Some(quote!(#complex:#W::Sym)),
    )
}
pub fn variant2sym_list_except_basic(variant: &Variant) -> Vec<proc_macro2::TokenStream> {
    variant2mapped_ident_type_list_view_container_as_complex(
        variant,
        |_, _| None,
        |complex, _| Some(quote!(#complex:#W::Sym)),
    )
}
pub fn variant2assign_node_field_list(variant: &Variant) -> Vec<proc_macro2::TokenStream> {
    variant2mapped_ident_type_list_view_container_as_complex(
        variant,
        |basic, _| Some(quote! {#basic}),
        |complex, _| Some(quote!(#complex:#complex.node.sym)),
    )
}
pub fn variant2assign_node_field_typed(variant: &Variant) -> Vec<proc_macro2::TokenStream> {
    variant2mapped_ident_type_list_view_container_as_complex(
        variant,
        |basic, _| Some(quote! {#basic}),
        |complex, _| Some(quote!(#complex:#complex.typed())),
    )
}
pub fn variant2assign_node_field_list_without_prefixed_ident(
    variant: &Variant,
) -> Vec<proc_macro2::TokenStream> {
    variant2mapped_ident_type_list_view_container_as_complex(
        variant,
        |basic, _| Some(quote! {#basic}),
        |complex, _| Some(quote! {#complex.node.sym}),
    )
}
pub fn variant2field_list_complex_ident_only(variant: &Variant) -> Vec<proc_macro2::TokenStream> {
    variant2mapped_ident_type_list(variant, |_, _| None, |ident, _| Some(quote! {#ident}))
}

/// given variant a{ x:X, y:Y}
/// return vec![ X, Y ]
pub fn variant2tys(variant: &Variant) -> Vec<proc_macro2::TokenStream> {
    variant2mapped_ident_type_list(
        variant,
        |_, ty| Some(quote! {#ty}),
        |_, ty| Some(quote! {#ty}),
    )
}

/// given variant a{ x:X, y:Y}
/// return iterator [ x, y ].iter()
pub fn variant2field_ident(variant: &Variant) -> Vec<proc_macro2::TokenStream> {
    variant2mapped_ident_type_list(
        variant,
        |ident, _| Some(quote! {#ident}),
        |ident, _| Some(quote! {#ident}),
    )
}
// pub fn _variant2field_ident_with_all_default(variant: &Variant) -> Vec<proc_macro2::TokenStream> {
//     variant2mapped_ident_type_list(
//         variant,
//         |ident, ty| {
//             Some({
//                 let default = match ty.to_string().as_str() {
//                     "String" => &EGGLOG_BASIC_TY_DEFAULT_LIST[0],
//                     "i64" => &EGGLOG_BASIC_TY_DEFAULT_LIST[1],
//                     "f64" => &EGGLOG_BASIC_TY_DEFAULT_LIST[2],
//                     _ => panic!("can't be {}", ident.to_string()),
//                 };
//                 quote! {#ident: #default}
//                 // quote! {#ident}
//             })
//         },
//         |ident, ty| Some(quote! {#ident: #W::Sym::<self::#ty>::default()}),
//     )
// }
// pub fn _variant2field_ident_with_basic_default(variant: &Variant) -> Vec<proc_macro2::TokenStream> {
//     variant2mapped_ident_type_list(
//         variant,
//         |ident, ty| {
//             Some({
//                 let default = match ty.to_string().as_str() {
//                     "String" => &EGGLOG_BASIC_TY_DEFAULT_LIST[0],
//                     "i64" => &EGGLOG_BASIC_TY_DEFAULT_LIST[1],
//                     "f64" => &EGGLOG_BASIC_TY_DEFAULT_LIST[2],
//                     _ => panic!("can't be {}", ident.to_string()),
//                 };
//                 quote! {#ident: #default}
//                 // quote! {#ident}
//             })
//         },
//         |ident, _| Some(quote! {#ident}),
//     )
// }
// pub fn variant2field_ident_assign_with_basic_default(
//     variant: &Variant,
// ) -> Vec<proc_macro2::TokenStream> {
//     variant2mapped_ident_type_list(
//         variant,
//         |ident, ty| {
//             Some({
//                 let default = get_default_by_ty(ty);
//                 quote! {#ident: #default}
//             })
//         },
//         |ident, _| Some(quote! {#ident: #ident.node.sym}),
//     )
// }
pub fn get_default_by_ty(ty: &TokenStream) -> TokenStream {
    enum Pos {
        BasePos(usize),
        UserDefined,
    }
    let pos = match (
        EGGLOG_BASE_TY_LIST
            .iter()
            .position(|x| x == &ty.to_string().as_str()),
        EgglogUserDefined::contain_base_type(&ty.to_string())
            || EgglogUserDefined::contain_container(&ty.to_string()),
    ) {
        (None, true) => Pos::UserDefined,
        (None, false) => {
            panic!(
                "{} might be a user defined base type or a typo of default base type",
                ty
            )
        }
        (Some(_), true) => panic!("user defined base type name {} conflict", ty),
        (Some(pos), false) => Pos::BasePos(pos),
    };
    match pos {
        Pos::BasePos(pos) => EGGLOG_BASIC_TY_DEFAULT_LIST[pos].to_token_stream(),
        Pos::UserDefined => quote!(Default::default()),
    }
    .to_token_stream()
}
pub fn variant2assign_node_field_typed_with_basic_default(
    variant: &Variant,
) -> Vec<proc_macro2::TokenStream> {
    variant2mapped_ident_type_list(
        variant,
        |ident, ty| {
            Some({
                let default = get_default_by_ty(ty);
                quote! {#ident: #default}
            })
        },
        |ident, _| Some(quote! {#ident: #ident.typed()}),
    )
}

pub fn variant2marker_name(variant: &Variant) -> (Ident, Ident) {
    (
        format_ident!("{}Ty", variant.ident),
        format_ident!("{}", variant.ident),
    )
}
pub fn variant_marker_names(data_enum: &DataEnum) -> (Vec<Ident>, Vec<Ident>) {
    data_enum
        .variants
        .iter()
        .map(|v| variant2marker_name(v))
        .collect()
}

pub fn variant2valued_struct_fields(variant: &Variant) -> Vec<TokenStream> {
    variant2mapped_ident_type_list_detailed(variant, |ident, ty, b_or_c| {
        // println!(
        //     "{} {} is recognized as basic",
        //     ident.to_string(),
        //     ty.to_string()
        // );
        match b_or_c {
            BasicOrComplex::BaseType | BasicOrComplex::UserDefinedBaseType => {
                Some(quote! {pub #ident: #W::Value<#ty>})
            }
            BasicOrComplex::UserDefinedContainerType => Some(quote! {pub #ident: #W::Value<#ty>}),
            BasicOrComplex::ComplexType => None,
        }
    })
}

pub fn variant2mapped_ident_type_list_view_container_as_complex(
    variant: &Variant,
    mut map_basic_ident_except_container: impl FnMut(&Ident, &TokenStream) -> Option<TokenStream>,
    mut map_complex_ident_include_container: impl FnMut(&Ident, &TokenStream) -> Option<TokenStream>,
) -> Vec<proc_macro2::TokenStream> {
    variant2mapped_ident_type_list_detailed(variant, |ident, ty, b_or_c| match b_or_c {
        BasicOrComplex::BaseType | BasicOrComplex::UserDefinedBaseType => {
            map_basic_ident_except_container(ident, ty)
        }
        BasicOrComplex::UserDefinedContainerType | BasicOrComplex::ComplexType => {
            map_complex_ident_include_container(ident, ty)
        }
    })
}
