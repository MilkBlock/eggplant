use core::panic;
use darling::{Error, FromMeta, ast::NestedMeta};

use heck::ToSnakeCase;
use helper::*;
use proc_macro2::{Ident, TokenStream};
use quote::{ToTokens, format_ident, quote};
use syn::{Data, DeriveInput, GenericParam, ItemFn, Type, TypeParam, parse_macro_input};
mod helper;
use helper::{DE, E, INVE, W};

#[proc_macro_attribute]
pub fn eggplant_func(
    attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    #[derive(Debug, FromMeta)]
    struct FuncMeta {
        output: Ident,
        // sg: Ident,
    }

    let input = parse_macro_input!(item as DeriveInput);
    let name = &input.ident;

    let attr_args = match NestedMeta::parse_meta_list(attr.into()) {
        Ok(v) => v,
        Err(e) => return proc_macro::TokenStream::from(Error::from(e).write_errors()),
    };
    let args = match FuncMeta::from_list(&attr_args) {
        Ok(v) => v,
        Err(e) => return proc_macro::TokenStream::from(e.write_errors()),
    };
    // let default_tx = args.sg;

    let output = args.output.to_token_stream();
    let output_with_generic = if is_basic_ty(&output) {
        quote!( #output )
    } else {
        quote!(#output<T,()>)
    };
    let output_ref = if is_basic_ty(&output_with_generic) {
        quote!( &#output_with_generic )
    } else {
        quote!(&dyn AsRef<#output_with_generic>)
    };
    let output_whether_as_ref = if is_basic_ty(&output_with_generic) {
        quote!()
    } else {
        quote!(.as_ref())
    };

    let struct_def_expanded = match &input.data {
        Data::Struct(data_struct) => {
            let name_func = format_ident!("{}", name);
            // let derive_more_path  = derive_more_path();
            let input_types = data_struct.fields.iter().map(|x| &x.ty).collect::<Vec<_>>();
            let input_types_with_generic = data_struct
                .fields
                .iter()
                .map(|x| &x.ty)
                .map(|x| {
                    if is_basic_ty(&x.to_token_stream()) {
                        x.to_token_stream()
                    } else {
                        quote!(#x<T,()>)
                    }
                })
                .collect::<Vec<_>>();
            let input_ref_types = data_struct
                .fields
                .iter()
                .map(|x| &x.ty)
                .map(|x| {
                    if is_basic_ty(&x.to_token_stream()) {
                        quote!(&#x)
                    } else {
                        quote!(&dyn AsRef<#x<T,()>>)
                    }
                })
                .collect::<Vec<_>>();
            let _generic_decl = data_struct
                .fields
                .iter()
                .enumerate()
                .map(|(count, field)| {
                    let generic = format_ident!("T{}", count);
                    let ty = &field.ty;
                    quote!(#generic:AsRef<#ty>)
                })
                .collect::<Vec<_>>();

            let _merge_option: proc_macro2::TokenStream = "no-merge".to_token_stream();
            let _merge_option: proc_macro2::TokenStream = "merge new".to_token_stream();
            quote! {
                #[allow(unused)]
                pub struct #name_func<T>{_p:std::marker::PhantomData<T>}
                const _:() = {
                    use #W::EgglogNode;
                    impl<T:#W::NodeDropperSgl> #W::EgglogFunc for #name_func<T>{
                        type Output=#output_with_generic;
                        type Input=(#(#input_types_with_generic),*);
                        const FUNC_NAME:&'static str = stringify!(#name_func);
                    }
                    impl<'a, T:#W::TxSgl> #name_func<T> {
                        pub fn set(input: (#(#input_ref_types),*), output: #output_ref){
                            T::on_func_set::<#name_func<T>>(input, output #output_whether_as_ref);
                        }
                    }
                    impl<'a, T:#W::RxSgl> #name_func<T> {
                        pub fn get(input: (#(#input_ref_types),*)) -> #output_with_generic{
                            T::on_func_get::<#name_func<T>>(input)#output_whether_as_ref.clone()
                        }
                    }
                    #INVE::submit!{
                        #W::Decl::EgglogFuncTy{
                            name: stringify!(#name_func),
                            input: &[ #(stringify!(#input_types)),*],
                            output: &(stringify!(#output))
                        }
                    }
                };
            }
            .into()
        }
        _ => {
            panic!()
        }
    };
    struct_def_expanded
}

/// generate `egglog` language from `rust native structure`   
///
/// # Example:  
///     
/// ```
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
/// ```
/// #[derive(Debug, Clone, ::derive_more::Deref)]
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
pub fn eggplant_ty(
    attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    #[derive(Debug, FromMeta)]
    struct TyMeta {
        // sg: Ident,
    }
    let attr_args = match NestedMeta::parse_meta_list(attr.into()) {
        Ok(v) => v,
        Err(e) => return proc_macro::TokenStream::from(Error::from(e).write_errors()),
    };
    let _args = match TyMeta::from_list(&attr_args) {
        Ok(v) => v,
        Err(e) => return proc_macro::TokenStream::from(e.write_errors()),
    };

    let input = parse_macro_input!(item as DeriveInput);
    let name = &input.ident;

    let name_lowercase = format_ident!("{}", name.to_string().to_lowercase());
    let name_egglogty_impl = format_ident!("{}", name);

    let type_def_expanded = match &input.data {
        Data::Enum(data_enum) => {
            let constructors = data_enum
                .variants
                .iter()
                .map(|variant| {
                    let tys = variant_to_tys(&variant);
                    let (_variant_marker, variant_name) = variant_marker_name(variant);
                    let new_from_term_dyn_fn_name = format_ident!(
                        "new_{}_from_term_dyn",
                        variant_name.to_string().to_snake_case()
                    );
                    quote! {  #W::TyConstructor {
                        cons_name: stringify!(#variant_name),
                        input:&[ #(stringify!(#tys)),* ] ,
                        output:stringify!(#name),
                        cost :None,
                        term_to_node: #name::<(),()>::#new_from_term_dyn_fn_name,
                        unextractable :false,
                    } }
                })
                .collect::<Vec<_>>();
            let expanded = quote! {
                impl<T:#W::NodeDropperSgl,V:#W::EgglogEnumVariantTy> #W::EgglogTy for #name_egglogty_impl<T,V> {
                    const TY_NAME:&'static str = stringify!(#name);
                    const TY_NAME_LOWER:&'static str = stringify!(#name_lowercase);
                    type NodeTypeCon<T1:#W::NodeDropperSgl, V1:#W::EgglogEnumVariantTy> = #name_egglogty_impl<T1,V1>;
                }
                impl<T:#W::NodeDropperSgl,V:#W::EgglogEnumVariantTy> #W::EgglogMultiConTy for #name_egglogty_impl<T,V> {
                    const CONSTRUCTORS : #W::TyConstructors= #W::TyConstructors(&[
                        #(#constructors),*
                    ]);
                }
                #INVE::submit!{
                    #W::Decl::EgglogMultiConTy {
                        name: <#name_egglogty_impl::<()> as #W::EgglogTy>::TY_NAME,
                        cons: &<#name_egglogty_impl::<()> as #W::EgglogMultiConTy>::CONSTRUCTORS
                    }
                }

            };
            expanded
        }
        Data::Struct(data_struct) => {
            // process (sort A (Vec M))  such things ..
            let f = data_struct
                .fields
                .iter()
                .nth(0)
                .expect("Struct should only have one Vec field");
            let first_generic = get_first_generic(&f.ty);
            let first_generic_ty = format_ident!("{}", first_generic.to_token_stream().to_string());
            if is_vec_type(&f.ty) {
                let vec_expanded = quote! {
                    impl<T:#W::NodeDropperSgl,V:#W::EgglogEnumVariantTy> #W::EgglogTy for #name_egglogty_impl<T,V> {
                        const TY_NAME:&'static str = stringify!(#name);
                        const TY_NAME_LOWER:&'static str = stringify!(#name_lowercase);
                        type NodeTypeCon<T1:#W::NodeDropperSgl, V1:#W::EgglogEnumVariantTy> = #name_egglogty_impl<T1,V1>;
                    }
                    impl #W::EgglogContainerTy for #name_egglogty_impl {
                        type EleTy = #first_generic_ty;
                    }
                    #INVE::submit!{
                        #W::Decl::EgglogContainerTy {
                            name: <#name_egglogty_impl::<()> as #W::EgglogTy>::TY_NAME,
                            ele_ty_name: <<#name_egglogty_impl as #W::EgglogContainerTy>::EleTy as #W::EgglogTy>::TY_NAME,
                            def_operator:"vec-of",
                            term_to_node: #name::<(),()>::new_from_term_dyn
                        }
                    }
                };
                vec_expanded
            } else {
                panic!("only support Vec for struct")
            }
        }
        _ => panic!("only support enum"),
    };
    let struct_def_expanded = match &input.data {
        Data::Struct(data_struct) => {
            // process (sort A (Vec M))  such things ..
            let name_node_alias = format_ident!("{}NodeAlias", name);
            let name_node = format_ident!("{}", name);
            let name_inner = format_ident!("{}Inner", name);
            let name_counter = format_ident!("{}_COUNTER", name.to_string().to_uppercase());
            let f = data_struct
                .fields
                .iter()
                .nth(0)
                .expect("Struct should only have one Vec field");
            let field_name = &f.ident.as_ref().unwrap();
            let first_generic = get_first_generic(&f.ty);
            // let field_sym_ty = get_sym_type(first_generic);
            let (field_node, is_basic_ty) =
                match first_generic.to_token_stream().to_string().as_str() {
                    x if PANIC_TY_LIST.contains(&x) => {
                        panic!("{} not supported", x)
                    }
                    x if EGGLOG_BASIC_TY_LIST.contains(&x) => {
                        (first_generic.to_token_stream(), true)
                    }
                    _ => {
                        let first_generic_ident = match &first_generic {
                            syn::Type::Path(type_path) => {
                                type_path
                                    .path
                                    .segments
                                    .last()
                                    .expect("impossible")
                                    .clone()
                                    .ident
                            }
                            _ => panic!(
                                "{} type should be simple path",
                                first_generic.to_token_stream().to_string()
                            ),
                        };
                        let _first_generic = format_ident!("{}", first_generic_ident);
                        // postfix_type(&first_generic,"Node",Some("T")
                        (quote!(dyn AsRef<#_first_generic<T, ()>>), false)
                    }
                };
            let to_egglog_impl = if is_basic_ty {
                quote! {
                    impl<T:#W::NodeDropperSgl, V:#W::EgglogEnumVariantTy> #W::ToEgglog for self::#name_node<T,V>
                    {
                        fn to_egglog_string(&self) -> String{
                            format!("(let {} (vec-of {}))",self.node.sym,self.node.ty.v.iter_mut().fold("".to_owned(), |s,item| s+ item.as_str()+" " ))
                        }
                        fn to_egglog(&self) -> #W::EgglogAction{
                            #E::ast::GenericAction::Let(span!(), self.cur_sym().to_string(),
                                #E::ast::GenericExpr::Call(self.span.to_span(),"vec-of", self.node.ty.v.iter().map(|x| x.to_var()).collect()).to_owned_str()
                            )
                        }
                    }
                }
            } else {
                quote! {
                    impl<T:#W::NodeDropperSgl, V:#W::EgglogEnumVariantTy> #W::ToEgglog for self::#name_node<T,V> {
                        fn to_egglog_string(&self) -> String{
                            format!("(let {} (vec-of {}))",self.cur_sym(),self.node.ty.v.iter().fold("".to_owned(), |s,item| s+ item.as_str()+" " ))
                        }
                        fn to_egglog(&self) -> #W::EgglogAction{
                            #E::ast::GenericAction::Let(span!(), self.cur_sym().to_string(),
                                #E::ast::GenericExpr::Call(self.span.to_span(), "vec-of", self.node.ty.v.iter().map(|x| x.to_var()).collect()).to_owned_str()
                            )
                        }
                    }
                    impl<T:#W::TxSgl + #W::VersionCtlSgl, V:#W::EgglogEnumVariantTy> #W::LocateVersion for self::#name_node<T,V> {
                        fn locate_latest(&mut self){
                            T::set_latest(self.cur_sym_mut());
                            self.node.ty.v.iter_mut().for_each(|item| {T::set_latest(item.erase_mut())});
                        }
                        fn locate_next(&mut self){
                            T::set_next(self.cur_sym_mut());
                            self.node.ty.v.iter_mut().for_each(|item| {T::set_next(item.erase_mut())});
                        }
                        fn locate_prev(&mut self){
                            T::set_prev(self.cur_sym_mut());
                            self.node.ty.v.iter_mut().for_each(|item| {T::set_next(item.erase_mut())});
                        }
                    }
                }
            };
            let field_assignment = if is_basic_ty {
                quote! {
                    children.map(|x| match term_dag.get(x) {
                        #E::Term::Lit(lit) => lit.deliteral(),
                        #E::Term::Var(v) => panic!(),
                        #E::Term::App(app,v) => panic!(),
                    }).collect()
                }
            } else {
                quote! {
                    children.iter().map(|x| term2sym.get(x).unwrap().typed()).collect()
                }
            };
            let field_ty = match first_generic.to_token_stream().to_string().as_str() {
                x if PANIC_TY_LIST.contains(&x) => {
                    panic!("{} not supported", x)
                }
                x if EGGLOG_BASIC_TY_LIST.contains(&x) => first_generic.to_token_stream(),
                _ => {
                    let first_generic = match &first_generic {
                        Type::Path(type_path) => {
                            type_path
                                .path
                                .segments
                                .last()
                                .expect("impossible")
                                .clone()
                                .ident
                        }
                        _ => panic!(
                            "{} keep the type simple!",
                            first_generic.to_token_stream().to_string()
                        ),
                    };
                    format_ident!("{}", first_generic).to_token_stream()
                }
            };
            if is_vec_type(&f.ty) {
                // MARK: Struct Expanded
                let vec_expanded = quote! {
                    pub type #name_node_alias<T,V> = #W::Node<#name_egglogty_impl,T,#name_inner,V>;
                    #[allow(unused)]
                    #[derive(#DE::DerefMut,#DE::Deref)]
                    pub struct #name_node<T: #W::NodeDropperSgl =(), V: #W::EgglogEnumVariantTy=()>
                    where Self: #W::EgglogNode + #W::EgglogTy {
                        node:#name_node_alias<T,V>
                    }
                    #[allow(unused)]
                    #[derive(Clone)]
                    pub struct #name_inner {
                        v:#W::Syms<#field_ty>
                    }
                    const _:() = {
                        use #E::prelude::*;
                        use #E::*;
                        use #W::{EgglogNode, ToSpan, ToVar, ToOwnedStr};
                        impl #W::NodeInner for #name_inner{
                            fn succs_mut(&mut self) -> Vec<&mut #W::Sym>{
                                self.v.iter_mut().map(|s| s.erase_mut()).collect()
                            }
                            fn succs(&self) -> Vec<#W::Sym>{
                                self.v.iter().map(|s| s.erase()).collect()
                            }
                        }
                        use std::marker::PhantomData;
                        static #name_counter: #W::TyCounter<#name_egglogty_impl> = #W::TyCounter::new();
                        impl<T:#W::TxSgl> self::#name_node<T,()> {
                            #[track_caller]
                            pub fn new(#field_name:Vec<&#field_node>) -> self::#name_node<T,()>{
                                let #field_name = #field_name.into_iter().map(|r| r.as_ref().sym).collect();
                                let node = #W::Node{
                                    ty: #name_inner{v:#field_name},
                                    span:Some(std::panic::Location::caller()),
                                    sym: #name_counter.next_sym(),
                                    _p: PhantomData, _s: PhantomData,
                                    sgl_specific: T::OwnerSpecDataInNode::default()
                                };
                                let node = self::#name_node {node};
                                T::on_new(&node);
                                node
                            }
                            // /// with no side-effect (will not send command to EGraph or change node in WorkAreaGraph)
                            // #[track_caller]
                            // pub fn _new(#field_name:Vec<&#field_node>) -> #name_node<T,()>{
                            //     let ty = #name_inner{ v: #field_name.into() };
                            //     use std::panic::Location;
                            //     let node = Node { ty, sym: #name_counter.next_sym(),span:Some(Location::caller()), _p:PhantomData, _s:PhantomData::<()>};
                            //     let node = #name_node {node};
                            //     node
                            // }
                            #[track_caller]
                            pub fn new_from_term(term_id:#E::TermId, term_dag: &#E::TermDag, term2sym:&mut std::collections::HashMap<#E::TermId, #W::Sym>) -> self::#name_node<T,()>{
                                let children = match term_dag.get(term_id){
                                    #E::Term::App(app,v) => v,
                                    _=> panic!()
                                };
                                let ty = #name_inner{v:#field_assignment };
                                let node = #W::Node {
                                    ty,
                                    sym: #name_counter.next_sym(),
                                    span:Some(std::panic::Location::caller()),
                                    _p:PhantomData,
                                    _s:PhantomData::<()>,
                                    sgl_specific:T::OwnerSpecDataInNode::default()
                                };

                                let node = self::#name_node {node};
                                term2sym.insert(term_id, node.cur_sym());
                                node
                            }
                            #[track_caller]
                            pub fn new_from_term_dyn(term_id:#E::TermId, term_dag: &#E::TermDag, term2sym:&mut std::collections::HashMap<#E::TermId, #W::Sym>) -> Box<dyn #W::EgglogNode>{
                                Box::new(Self::new_from_term(term_id, term_dag, term2sym))
                            }
                        }
                        impl<T:#W::NodeDropperSgl, V:#W::EgglogEnumVariantTy> #W::EgglogNode for self::#name_node<T,V> {
                            fn succs_mut(&mut self) -> Vec<&mut #W::Sym>{
                                use #W::NodeInner;
                                self.node.ty.succs_mut()
                            }
                            fn succs(&self) -> Vec<#W::Sym>{
                                use #W::NodeInner;
                                self.node.ty.succs()
                            }
                            fn roll_sym(&mut self) -> #W::Sym{
                                let next_sym = #name_counter.next_sym();
                                self.node.sym = next_sym;
                                next_sym.erase()
                            }
                            fn cur_sym(&self) -> #W::Sym{
                                self.node.sym.erase()
                            }
                            fn cur_sym_mut(&mut self) -> &mut #W::Sym{
                                self.node.sym.erase_mut()
                            }
                            fn clone_dyn(&self) -> Box<dyn #W::EgglogNode>{
                                Box::new(self.clone())
                            }
                        }
                        impl<T: #W::NodeDropperSgl, V: #W::EgglogEnumVariantTy> AsRef<self::#name_node<T, ()>> for self::#name_node<T, V> {
                            fn as_ref(&self) -> &self::#name_node<T, ()> {
                                unsafe {
                                    &*(self as *const self::#name_node<T,V> as *const self::#name_node<T,()>)
                                }
                            }
                        }
                        impl<T:#W::NodeDropperSgl,V:#W::EgglogEnumVariantTy > Clone for self::#name_node<T,V> {
                            fn clone(&self) -> Self {
                                Self {
                                    node:
                                        #W::Node {
                                            ty: self.node.ty.clone(),
                                            span: self.span,
                                            sym: self.node.sym.clone(),
                                            _p:PhantomData,
                                            _s:PhantomData,
                                            sgl_specific: T::OwnerSpecDataInNode::default()
                                        }
                                }
                            }
                        }

                        impl<T:#W::NodeDropperSgl ,V: #W::EgglogEnumVariantTy> Drop for self::#name_node<T,V>
                        {
                            fn drop(&mut self) {
                                T::on_drop(self);
                            }
                        }

                        #to_egglog_impl
                    };
                };
                vec_expanded
            } else {
                panic!("only support Vec for struct")
            }
        }
        Data::Enum(data_enum) => {
            let name_node_alias = format_ident!("{}NodeAlias", name);
            let name_node = format_ident!("{}", name);
            let _name_node = format_ident!("_{}", name);
            let name_inner = format_ident!("{}Inner", name);
            let name_counter = format_ident!("{}_COUNTER", name.to_string().to_uppercase());

            let variants_def_of_node_with_syms = data_enum
                .variants
                .iter()
                .map(|variant| {
                    let types_and_idents = variants_to_sym_typed_ident_list(variant);
                    let (_variant_marker, variant_name) = variant_marker_name(variant);
                    quote! {#variant_name {#( #types_and_idents ),*  }}
                })
                .collect::<Vec<_>>();

            let to_egglog_string_match_arms = data_enum.variants.iter().map(|variant| {
                let variant_idents = variant_to_field_ident(variant);
                let (variant_marker, variant_name) = variant_marker_name(variant);
                let s = " {:.3}".repeat(variant_idents.len());
                let format_str = format!("(let {{}} ({} {}))", variant_marker, s);
                quote! {#name_inner::#variant_name {#( #variant_idents ),*  } => {
                    format!(#format_str ,self.node.sym, #(#variant_idents),*)
                }}
            });
            let succs_match_arms = data_enum.variants.iter().map(|variant| {
                let variant_idents = variant_to_field_ident(variant);
                let (_variant_marker, variant_name) = variant_marker_name(variant);
                let vec_needed_syms: Vec<_> =
                    variant_to_field_list_without_prefixed_ident_filter_out_basic_ty(variant);
                quote! {#name_inner::#variant_name {#( #variant_idents ),*  } => {
                    vec![#(#vec_needed_syms.erase()),*]
                }}
            });
            let succs_mut_match_arms = data_enum.variants.iter().map(|variant| {
                let variant_idents = variant_to_field_ident(variant);
                let (_variant_marker, variant_name) = variant_marker_name(variant);
                let vec_needed_syms: Vec<_> =
                    variant_to_field_list_without_prefixed_ident_filter_out_basic_ty(variant);
                quote! {#name_inner::#variant_name {#( #variant_idents ),*  } => {
                    vec![#(#vec_needed_syms.erase_mut()),*]
                }}
            });
            let to_egglog_match_arms = data_enum.variants.iter().map(|variant| {
                let variant_fields = variant_to_field_ident(variant);
                let (_variant_marker, variant_name) = variant_marker_name(variant);
                quote! {#name_inner::#variant_name {#( #variant_fields ),*  } => {
                    #E::ast::GenericAction::Let(span!(), self.cur_sym().to_string(),
                        #E::ast::GenericExpr::Call(span!(),
                            stringify!(#variant_name),
                            vec![#(#variant_fields.to_var()),*]).to_owned_str()
                    )
                }}
            });
            let locate_latest_match_arms = data_enum.variants.iter().map(|variant| {
                let variant_idents = variant_to_field_ident(variant);
                let mapped_variant_idents = variant_to_mapped_ident_type_list(
                    variant,
                    |_, _| Some(quote! {}),
                    |x, _| Some(quote! { T::set_latest(#x.erase_mut());}),
                );
                let (_variant_marker, variant_name) = variant_marker_name(variant);
                quote! {
                    #name_inner::#variant_name {#(#variant_idents),* } => {
                        T::set_latest(self.node.sym.erase_mut());
                        #(#mapped_variant_idents)*
                    }
                }
            });

            let locate_next_match_arms = data_enum.variants.iter().map(|variant| {
                let variant_idents = variant_to_field_ident(variant);
                let mapped_variant_idents = variant_to_mapped_ident_type_list(
                    variant,
                    |_, _| Some(quote! {}),
                    |x, _| Some(quote! {T::set_next(#x.erase_mut());}),
                );
                let (_variant_marker, variant_name) = variant_marker_name(variant);
                quote! {
                    #name_inner::#variant_name {#(#variant_idents),* } => {
                        T::set_next(self.node.sym.erase_mut());
                        #(#mapped_variant_idents)*
                    }
                }
            });
            let locate_prev_match_arms = data_enum.variants.iter().map(|variant| {
                let variant_idents = variant_to_field_ident(variant);
                let mapped_variant_idents = variant_to_mapped_ident_type_list(
                    variant,
                    |_, _| Some(quote! {}),
                    |x, _| Some(quote! {T::set_prev(#x.erase_mut());}),
                );
                let (_variant_marker, variant_name) = variant_marker_name(variant);
                quote! {
                    #name_inner::#variant_name {#(#variant_idents),* } => {
                        T::set_prev(self.node.sym.erase_mut());
                        #(#mapped_variant_idents)*
                    }
                }
            });
            let (new_fns, new_fn_names, new_fn_args, new_fn_arg_idents):
                (Vec<proc_macro2::TokenStream>,Vec<Ident>
                    ,Vec<Vec<TokenStream>>
                    ,Vec<Vec<TokenStream>>
                ) = data_enum.variants.iter().map(|variant|{
                let ref_node_list = variant_to_ref_node_list(&variant);
                let ref_node_list_leave_idents = variant_to_ref_node_list_leave_ident(&variant);

                let _new_fn_args= variant_to_sym_list(&variant);
                let field_idents_assign = variant_to_assign_node_field_list(&variant);
                let _new_fn_field_idents_assign = variant_to_typed_assign_node_field_list(&variant);
                let field_idents = variant_to_field_ident(&variant);
                let (variant_marker,variant_name) = variant_marker_name(variant);
                let new_fn_name = format_ident!("new_{}",variant_name.to_string().to_snake_case());
                let _new_fn_name = format_ident!("_new_{}",variant_name.to_string().to_snake_case());
                let new_from_term_fn_name = format_ident!("new_{}_from_term",variant_name.to_string().to_snake_case());
                let new_from_term_dyn_fn_name = format_ident!("new_{}_from_term_dyn",variant_name.to_string().to_snake_case());
                let field_ty = variant_to_tys(&variant);
                let field_assignments: Vec<_> = field_idents.into_iter().zip(field_ty.iter()).enumerate().map(|(i, (ident, ty))| {
                    if is_basic_ty(ty) {
                        quote! {
                            #ident: match term_dag.get(children[#i]) {
                                #E::Term::Lit(lit) => lit.deliteral(),
                                #E::Term::Var(v) => panic!(),
                                #E::Term::App(app,v) => panic!(),
                            }
                        }
                    } else {
                        quote! {
                            #ident: term2sym[&children[#i]].typed()
                        }
                    }
                }).collect();

                // MARK: Enum New Fns
                (quote! {
                    #[track_caller]
                    pub fn #new_fn_name(#(#ref_node_list),*) -> self::#name_node<T,#variant_marker>{
                        let ty = #name_inner::#variant_name {#(#field_idents_assign),*  };
                        let node = #W::Node {
                            ty,
                            sym: #name_counter.next_sym(),
                            span:Some(std::panic::Location::caller()),
                            _p:PhantomData,
                            _s:PhantomData::<#variant_marker>,
                            sgl_specific: T::OwnerSpecDataInNode::default()
                        };
                        let node = #name_node {node};
                        T::on_new(&node);
                        node
                    }
                    /// with no side-effect (will not send command to EGraph or change node in WorkAreaGraph)
                    #[track_caller]
                    pub fn #_new_fn_name(#(#_new_fn_args),*) -> #name_node<T,#variant_marker>{
                        let ty = #name_inner::#variant_name {#(#_new_fn_field_idents_assign),*  };
                        let node = #W::Node {
                            ty,
                            sym: #name_counter.next_sym(),
                            span:Some(std::panic::Location::caller()),
                            _p:PhantomData,
                            _s:PhantomData::<#variant_marker>,
                            sgl_specific: T::OwnerSpecDataInNode::default()
                        };
                        let node = #name_node {node};
                        node
                    }
                    /// you should guarantee all the dep nodes has been init and store as (TermId, Sym) in term2sym
                    #[track_caller]
                    pub fn #new_from_term_fn_name(term_id:#E::TermId, term_dag: &#E::TermDag, term2sym:&mut std::collections::HashMap<#E::TermId, #W::Sym>) -> #name_node<T,#variant_marker>{
                        let children = match term_dag.get(term_id){
                            #E::Term::App(app,v) => v,
                            _=> panic!()
                        };
                        let ty = #name_inner::#variant_name {#(#field_assignments),*  };
                        let node = #W::Node {
                            ty,
                            sym: #name_counter.next_sym(),
                            span:Some(std::panic::Location::caller()),
                            _p:PhantomData,
                            _s:PhantomData,
                            sgl_specific: T::OwnerSpecDataInNode::default()
                        };
                        let node = self::#name_node {node};
                        term2sym.insert(term_id, node.cur_sym());
                        node
                    }
                    #[track_caller]
                    pub fn #new_from_term_dyn_fn_name(term_id:#E::TermId, term_dag: &#E::TermDag, term2sym:&mut std::collections::HashMap<#E::TermId, #W::Sym>) -> Box<dyn #W::EgglogNode>{
                        Box::new(Self::#new_from_term_fn_name(term_id, term_dag, term2sym))
                    }
                }, new_fn_name, ref_node_list, ref_node_list_leave_idents)
            }).collect();
            let (new_ph_fns, _new_ph_fn_names, _new_ph_fn_args, _new_ph_fn_arg_idents): (
                Vec<proc_macro2::TokenStream>,
                Vec<Ident>,
                Vec<Vec<TokenStream>>,
                Vec<Vec<TokenStream>>,
            ) = data_enum
                .variants
                .iter()
                .map(|variant| {
                    let ref_node_list = variant_to_ref_node_list(&variant);
                    let ref_node_list_leave_idents = variant_to_ref_node_list_leave_ident(&variant);

                    let _new_fn_args = variant_to_sym_list(&variant);
                    let field_idents_assign = variant_to_field_ident_with_default(&variant);
                    let _new_fn_field_idents_assign =
                        variant_to_typed_assign_node_field_list(&variant);
                    let field_idents = variant_to_field_ident(&variant);
                    let (variant_marker, variant_name) = variant_marker_name(variant);
                    let new_ph_fn_name =
                        format_ident!("new_{}_ph", variant_name.to_string().to_snake_case());
                    let _new_fn_name =
                        format_ident!("_new_{}", variant_name.to_string().to_snake_case());
                    // let new_from_term_fn_name = format_ident!("new_{}_from_term",variant_name.to_string().to_snake_case());
                    // let new_from_term_dyn_fn_name = format_ident!("new_{}_from_term_dyn",variant_name.to_string().to_snake_case());
                    let field_ty = variant_to_tys(&variant);
                    let _field_assignments: Vec<_> = field_idents
                        .into_iter()
                        .zip(field_ty.iter())
                        .enumerate()
                        .map(|(i, (ident, ty))| {
                            if is_basic_ty(ty) {
                                quote! {
                                    #ident: match term_dag.get(children[#i]) {
                                        #E::Term::Lit(lit) => lit.deliteral(),
                                        #E::Term::Var(v) => panic!(),
                                        #E::Term::App(app,v) => panic!(),
                                    }
                                }
                            } else {
                                quote! {
                                    #ident: term2sym[&children[#i]].typed()
                                }
                            }
                        })
                        .collect();

                    // MARK: Enum New Fns
                    (
                        quote! {
                            #[track_caller]
                            pub fn #new_ph_fn_name() -> #W::PH<self::#name_node<T,#variant_marker>>{
                                let ty = #name_inner::#variant_name {#(#field_idents_assign),*  };
                                let node = #W::Node {
                                    ty,
                                    sym: #name_counter.next_sym(),
                                    span:Some(std::panic::Location::caller()),
                                    _p:PhantomData,
                                    _s:PhantomData::<#variant_marker>,
                                    sgl_specific: T::OwnerSpecDataInNode::default()
                                };
                                let node = #name_node {node};
                                T::on_new(&node);
                                #W::PH::new(node)
                            }
                        },
                        new_ph_fn_name,
                        ref_node_list,
                        ref_node_list_leave_idents,
                    )
                })
                .collect();
            let enum_variant_tys_def = data_enum.variants.iter().map(|variant| {
                let (variant_marker, variant_name) = variant_marker_name(variant);

                quote! {
                    #[derive(Clone)]
                    pub struct #variant_marker;
                    impl #W::EgglogEnumVariantTy for #variant_marker {
                        const TY_NAME:&'static str = stringify!(#variant_name);
                    }
                }
            });

            let set_fns = data_enum.variants.iter().map(|variant|{
                let ref_node_list = variant_to_ref_node_list(&variant);
                let assign_node_field_list = variant_to_assign_node_field_list_without_prefixed_ident(&variant);
                let field_idents = variant_to_field_ident(variant);
                let (variant_marker,variant_name) = variant_marker_name(variant);

                let set_fns = assign_node_field_list.iter().zip(ref_node_list.iter().zip(field_idents.iter()
                    )).map(
                    |(assign_node_field,(ref_node,field_ident))|{
                        let set_fn_name = format_ident!("set_{}",field_ident.to_string());
                        quote! {
                            /// set fn of node, firstly update the sym version and specified field and then informs rx what happen on this node
                            /// rx's behavior depends on whether version control is enabled
                            #[track_caller]
                            pub fn #set_fn_name(&mut self,#ref_node) -> &mut Self{
                                let ___sym = #assign_node_field;
                                if let #name_inner::#variant_name{ #(#field_idents),*} = &mut self.node.ty{
                                    *#field_ident = ___sym
                                };
                                T::on_set(self);
                                self
                            }
                        }
                    }
                );
                let sym_list = variants_to_sym_type_list(variant);
                let get_sym_fns = sym_list.iter().zip(field_idents.iter()
                    ).map(
                    |(sym,field_ident)|{
                        let get_fn_name = format_ident!("{}_sym",field_ident.to_string());
                        quote! {
                            pub fn #get_fn_name(&self) -> #sym{
                                if let #name_inner::#variant_name{ #(#field_idents),*} = &self.node.ty{
                                    #field_ident.clone()
                                }else{
                                    panic!()
                                }
                            }
                        }
                    }
                );
                let get_mut_sym_fns = sym_list.iter().zip(field_idents.iter()
                    ).map(
                    |(sym,field_ident)|{
                        let get_fn_name = format_ident!("{}_sym_mut",field_ident.to_string());
                        quote! {
                            pub fn #get_fn_name(&mut self) -> &mut #sym{
                                if let #name_inner::#variant_name{ #(#field_idents),*} = &mut self.node.ty{
                                    #field_ident
                                }else{
                                    panic!()
                                }
                            }
                        }
                    }
                );

                quote! {
                    #[allow(unused_variables)]
                    impl<T:#W::TxSgl> self::#name_node<T,#variant_marker>{
                        #(
                            #get_sym_fns
                        )*
                        #(
                            #get_mut_sym_fns
                        )*
                    }
                    impl<T: #W::TxSgl + #W::NodeSetterSgl> self::#name_node<T,#variant_marker>{
                        #(
                            #set_fns
                        )*
                    }
                }
            });

            let (variant_markers, variant_names) = variant_marker_names(data_enum);
            // MARK: Enum Expanded
            let expanded = quote! {
                pub type #name_node_alias<T,V> = #W::Node<#name_egglogty_impl,T,#name_inner,V>;
                #(pub type #variant_names<T> = self::#name_node<T, #variant_markers>;)*
                #(#enum_variant_tys_def)*
                #[allow(unused)]
                #[derive(#DE::Deref,)]
                pub struct #name_node<T: #W::NodeDropperSgl = (),V:#W::EgglogEnumVariantTy=()>
                where Self: #W::EgglogNode + #W::EgglogTy {
                    node:#name_node_alias<T,V>
                }
                #[allow(unused)]
                #[derive(Clone)]
                pub enum #name_inner {
                    #(#variants_def_of_node_with_syms),*
                }
                #[allow(unused_variables)]
                const _:() = {
                    use #W::{EgglogNode, ToSpan, ToVar, ToOwnedStr};
                    use std::marker::PhantomData;
                    use #W::{DeLiteral};
                    use #E::prelude::*;
                    use #E::ast::{GenericAction, GenericExpr};
                    impl<T:#W::TxSgl> self::#name_node<T,()> {
                        #(#new_fns)*
                    }
                    impl<T:#W::TxSgl + #W::PatRecSgl> self::#name_node<T,()> {
                        #(#new_ph_fns)*
                    }
                    use #W::TxSgl;
                    #(
                        impl<T:TxSgl> self::#name_node<T, #variant_markers> {
                            pub fn new(#(#new_fn_args),*) -> Self{
                                #name_node::<T,()>::#new_fn_names(#(#new_fn_arg_idents),*)
                            }
                        }
                    )*
                    impl<T:#W::RxSgl, S: #W::EgglogEnumVariantTy> self::#name_node<T,S>{
                        pub fn pull(&self){
                            T::on_pull::<#name_egglogty_impl<T>>(self)
                        }
                    }
                    impl<T:#W::NodeDropperSgl, V:#W::EgglogEnumVariantTy> #W::EgglogNode for self::#name_node<T,V> {
                        fn succs_mut(&mut self) -> Vec<&mut #W::Sym>{
                            use #W::NodeInner;
                            self.node.ty.succs_mut()
                        }
                        fn succs(&self) -> Vec<#W::Sym>{
                            use #W::NodeInner;
                            self.node.ty.succs()
                        }
                        fn roll_sym(&mut self) -> #W::Sym{
                            let next_sym = #name_counter.next_sym();
                            self.node.sym = next_sym;
                            next_sym.erase()
                        }
                        fn cur_sym(&self) -> #W::Sym{
                            self.node.sym.erase()
                        }
                        fn cur_sym_mut(&mut self) -> &mut #W::Sym{
                            self.node.sym.erase_mut()
                        }
                        fn clone_dyn(&self) -> Box<dyn #W::EgglogNode>{
                            Box::new(self.clone())
                        }
                    }
                    impl<T:#W::NodeDropperSgl, V:#W::EgglogEnumVariantTy> #W::ToEgglog for self::#name_node<T,V> {
                        fn to_egglog_string(&self) -> String{
                            match &self.node.ty{
                                #(#to_egglog_string_match_arms),*
                            }
                        }
                        fn to_egglog(&self) -> Action{
                            match &self.node.ty{
                                #(#to_egglog_match_arms),*
                            }
                        }
                    }
                    #[allow(unused_variables)]
                    impl<T:#W::TxSgl + #W::VersionCtlSgl, V:#W::EgglogEnumVariantTy> #W::LocateVersion for self::#name_node<T,V> {
                        fn locate_latest(&mut self) {
                            match &mut self.node.ty{
                                #(#locate_latest_match_arms),*
                            }
                        }
                        fn locate_next(&mut self) {
                            match &mut self.node.ty{
                                #(#locate_next_match_arms),*
                            }
                        }
                        fn locate_prev(&mut self) {
                            match &mut self.node.ty{
                                #(#locate_prev_match_arms),*
                            }
                        }
                    }
                    impl<T: #W::NodeDropperSgl,  V: #W::EgglogEnumVariantTy> AsRef<self::#name_node<T, ()>> for self::#name_node<T, V> {
                        fn as_ref(&self) -> &self::#name_node<T, ()> {
                            unsafe {
                                &*(self as *const self::#name_node<T,V> as *const self::#name_node<T,()>)
                            }
                        }
                    }
                    impl<T: #W::NodeDropperSgl,  V: #W::EgglogEnumVariantTy> AsRef<self::#name_node<T, ()>> for #W::PH<self::#name_node<T, V>> {
                        fn as_ref(&self) -> &self::#name_node<T, ()> {
                            self.node.as_ref()
                        }
                    }

                    impl<T:#W::NodeDropperSgl,V:#W::EgglogEnumVariantTy > Clone for self::#name_node<T,V> {
                        fn clone(&self) -> Self {
                            Self {
                                node:
                                    #W::Node {
                                        ty: self.ty.clone(),
                                        span: self.span ,
                                        sym: self.sym.clone(),
                                        _p: PhantomData,
                                        _s: PhantomData,
                                        sgl_specific : T::OwnerSpecDataInNode::default(),
                                    }
                            }
                        }
                    }

                    impl<T:#W::TxSgl+ #W::VersionCtlSgl + #W::TxCommitSgl,S: #W::EgglogEnumVariantTy> #W::Commit for self::#name_node<T,S>
                    {
                        fn commit(&self) {
                            T::on_commit(self);
                        }
                        fn stage(&self) {
                            T::on_stage(self);
                        }
                    }

                    impl<T:#W::NodeDropperSgl, S: #W::EgglogEnumVariantTy> Drop for self::#name_node<T,S> {
                        fn drop(&mut self) {
                            T::on_drop(self);
                        }
                    }

                    impl #W::NodeInner for #name_inner {
                        fn succs_mut(&mut self) -> Vec<&mut #W::Sym>{
                            match self {
                                #(#succs_mut_match_arms),*
                            }
                        }
                        fn succs(&self) -> Vec<#W::Sym>{
                            match self{
                                #(#succs_match_arms),*
                            }
                        }
                    }
                    static #name_counter: #W::TyCounter<#name_egglogty_impl<()>> = #W::TyCounter::new();
                    #(#set_fns)*
                };
            };
            expanded
        }
        Data::Union(_) => todo!(),
    };

    quote! {
    #type_def_expanded
    #struct_def_expanded
    }
    .into()
}

#[proc_macro_attribute]
pub fn eggplant_pattern(
    _attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    // #[derive(Debug, FromMeta)]
    // struct TyMeta {
    //     // sg: Ident,
    // }
    // let attr_args = match NestedMeta::parse_meta_list(attr.into()) {
    //     Ok(v) => v,
    //     Err(e) => return TokenStream::from(Error::from(e).write_errors()),
    // };
    // let _args = match TyMeta::from_list(&attr_args) {
    //     Ok(v) => v,
    //     Err(e) => return TokenStream::from(e.write_errors()),
    // };

    // let input = parse_macro_input!(item as DeriveInput);
    // let name = &input.ident;

    // let name_lowercase = format_ident!("{}", name.to_string().to_lowercase());
    // let name_egglogty_impl = format_ident!("{}", name);
    // Parse the input function
    let input_fn = parse_macro_input!(item as ItemFn);

    // Get the function signature

    // Create generic parameter "T: Tx"
    let type_param: GenericParam = syn::parse_str::<TypeParam>("T: Tx").unwrap().into();
    eprintln!(
        "Generated tokens: {}",
        type_param.to_token_stream().to_string()
    );

    let mut sig = input_fn.sig;
    let block = input_fn.block;

    // Add the generic parameter to the function
    if sig.generics.params.is_empty() {
        sig.generics.params.push(type_param);
    } else {
        // If there are already generics, just add ours
        sig.generics.params.push(type_param);
    }

    // // Transform the return type to use the generic parameter
    if let syn::ReturnType::Type(_arrow, ty) = &mut sig.output {
        if let syn::Type::Tuple(tuple) = ty.as_mut() {
            for elem in &mut tuple.elems {
                if let syn::Type::Path(type_path) = elem {
                    if let Some(_ident) = type_path.path.get_ident() {
                        // Add <T> to the type
                        // let generic_arg: syn::GenericArgument = syn::parse_str("T").unwrap();
                        // type_path.path.segments.push(syn::parse_str::<syn::PathSegment>("<T>").unwrap());
                        // type_path
                    }
                }
            }
        }
        //     sig.output = syn::ReturnType::Type(arrow, ty);
    }

    // Reconstruct the function
    quote! {
        #sig {
            #block
        }
    }
    .into()
}
