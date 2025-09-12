use core::panic;
use darling::{Error, FromMeta, ast::NestedMeta};

use heck::ToSnakeCase;
use helper::*;
use proc_macro2::{Ident, TokenStream};
use quote::{ToTokens, format_ident, quote};
use syn::{Data, DeriveInput, Field, Type, Visibility, parse_macro_input, parse_quote};
mod helper;
use helper::{E, INVE, W};

use crate::enum_related::*;
mod enum_related;

#[proc_macro_attribute]
pub fn func(
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
                    use #INVE;
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
pub fn ty(attr: proc_macro::TokenStream, item: proc_macro::TokenStream) -> proc_macro::TokenStream {
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
                    let tys = variant2tys(&variant);
                    let (_variant_marker, variant_name) = variant2marker_name(variant);
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
                    type Valued = V::ValuedWithDefault<Self>;
                    type EnumVariantMarker = V;
                }
                impl<T:#W::NodeDropperSgl,V:#W::EgglogEnumVariantTy> #W::EgglogMultiConTy for #name_egglogty_impl<T,V> {
                    const CONSTRUCTORS : #W::TyConstructors= #W::TyConstructors(&[
                        #(#constructors),*
                    ]);
                }
                const _:() = {
                    use #INVE;
                    #INVE::submit!{
                        #W::Decl::EgglogMultiConTy {
                            name: <#name_egglogty_impl::<()> as #W::EgglogTy>::TY_NAME,
                            cons: &<#name_egglogty_impl::<()> as #W::EgglogMultiConTy>::CONSTRUCTORS
                        }
                    }
                };
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
                        type Valued = V::ValuedWithDefault<#W::Value<Self>>;
                        type EnumVariantMarker = V;
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
                        fn to_egglog_string(&self) -> Option<String>{
                            Some(format!("(let {} (vec-of {}))",self.node.sym,self.node.ty.unwrap_mut().iter_mut().fold("".to_owned(), |s,item| s+ item.as_str()+" " )))
                        }
                        fn to_egglog(&self) -> #W::EgglogAction{
                            #E::ast::GenericAction::Let(span!(), self.cur_sym().to_string(),
                                #E::ast::GenericExpr::Call(self.node.span.to_span(),"vec-of", self.node.ty.unwrap_ref().iter().map(|x| x.to_var()).collect()).to_owned_str()
                            )
                        }
                        fn native_egglog(&self, ctx: &mut #W::RuleCtx, sym_to_value_map: &#EP::dashmap::DashMap<#W::Sym, egglog::Value>) -> egglog::Value {
                            // use ctx.insert to insert
                            // todo
                            let sym = self.cur_sym();
                            if let Some(value) = sym_to_value_map.get(&sym) {
                                value.clone()
                            } else {
                                panic!("{}'s value not found, maybe haven't committed", sym)
                            }
                        }
                    }
                }
            } else {
                quote! {
                    impl<T:#W::NodeDropperSgl, V:#W::EgglogEnumVariantTy> #W::ToEgglog for self::#name_node<T,V> {
                        fn to_egglog_string(&self) -> Option<String>{
                            Some(format!("(let {} (vec-of {}))",self.cur_sym(),self.node.ty.unwrap_ref().iter().fold("".to_owned(), |s,item| s+ item.as_str()+" " )))
                        }
                        fn to_egglog(&self) -> #W::EgglogAction{
                            #E::ast::GenericAction::Let(span!(), self.cur_sym().to_string(),
                                #E::ast::GenericExpr::Call(self.node.span.to_span(), "vec-of", self.node.ty.unwrap_ref().iter().map(|x| x.to_var()).collect()).to_owned_str()
                            )
                        }
                        fn native_egglog(&self, ctx: &mut #W::RuleCtx, sym_to_value_map: &dashmap::DashMap<#W::Sym, egglog::Value>) -> egglog::Value {
                            // use ctx.insert to insert
                            // todo
                            let sym = self.cur_sym();
                            if let Some(value) = sym_to_value_map.get(&sym) {
                                value.clone()
                            } else {
                                panic!("{}'s value not found, maybe haven't committed", sym)
                            }
                        }
                    }
                    impl<T:#W::TxSgl + #W::VersionCtlSgl, V:#W::EgglogEnumVariantTy> #W::LocateVersion for self::#name_node<T,V> {
                        fn locate_latest(&mut self){
                            T::set_latest(self.cur_sym_mut());
                            self.node.ty.unwrap_mut().iter_mut().for_each(|item| {T::set_latest(item.erase_mut())});
                        }
                        fn locate_next(&mut self){
                            T::set_next(self.cur_sym_mut());
                            self.node.ty.unwrap_mut().iter_mut().for_each(|item| {T::set_next(item.erase_mut())});
                        }
                        fn locate_prev(&mut self){
                            T::set_prev(self.cur_sym_mut());
                            self.node.ty.unwrap_mut().iter_mut().for_each(|item| {T::set_next(item.erase_mut())});
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
                    pub struct #name_node<T: #W::NodeDropperSgl =(), V: #W::EgglogEnumVariantTy=()>
                    where Self: #W::EgglogNode + #W::EgglogTy {
                        node:#name_node_alias<T,V>
                    }
                    #[allow(unused)]
                    #[derive(Clone, #ENM::EnumDiscriminants)]
                    pub enum #name_inner {
                        Inner {inner:#W::Syms<#field_ty> },
                    }
                    const _:() = {
                        use #E::prelude::*;
                        use #E::*;
                        use #W::{EgglogNode, ToSpan, ToVar, ToOwnedStr};
                        use #INVE;
                        impl #W::NodeInner for #name_inner{
                            fn succs_mut(&mut self) -> Vec<&mut #W::Sym>{
                                self.iter_mut().map(|s| s.erase_mut()).collect()
                            }
                            fn succs(&self) -> Vec<#W::Sym>{
                                self.iter().map(|s| s.erase()).collect()
                            }
                        }
                        impl std::ops::Deref for #name_inner { type Target = #W::Syms<#field_ty> ; fn deref(&self) -> &Self::Target { let #name_inner::Inner { inner } = self; inner } }
                        impl std::ops::DerefMut for #name_inner { fn deref_mut(&mut self) -> &mut Self::Target { let #name_inner::Inner { inner } = self; inner } }
                        use std::marker::PhantomData;
                        static #name_counter: #W::TyCounter<#name_egglogty_impl> = #W::TyCounter::new();
                        impl<T:#W::TxSgl + #W::PatRecSgl> self::#name_node<T,()> {
                            #[track_caller]
                            pub fn query(#field_name:Vec<&#field_node>) -> self::#name_node<T,()>{
                                let #field_name = #field_name.into_iter().map(|r| r.as_ref().node.sym).collect();
                                let node = #W::Node{
                                    ty: #W::TyPH::Ty(#name_inner::Inner{inner:#field_name}),
                                    span:Some(std::panic::Location::caller()),
                                    sym: #name_counter.next_sym(),
                                    _p: PhantomData, _s: PhantomData,
                                    sgl_specific: T::OwnerSpecDataInNode::default()
                                };
                                let node = self::#name_node {node};
                                T::on_new(&node);
                                node
                            }
                        }
                        impl<T:#W::TxSgl + #W::NonPatRecSgl> self::#name_node<T,()> {
                            #[track_caller]
                            pub fn new(#field_name:Vec<&#field_node>) -> self::#name_node<T,()>{
                                let #field_name = #field_name.into_iter().map(|r| r.as_ref().node.sym).collect();
                                let node = #W::Node{
                                    ty: #W::TyPH::Ty(#name_inner::Inner{inner:#field_name}),
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
                                let ty = #W::TyPH::Ty(#name_inner::Inner{inner:#field_assignment });
                                let node = #W::Node {
                                    ty,
                                    sym: #name_counter.next_sym(),
                                    span:Some(std::panic::Location::caller()),
                                    _p:PhantomData,
                                    _s:PhantomData,
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
                            fn succs_mut(&mut self) -> Vec<&mut #W::Sym>{ use #W::NodeInner;
                                self.node.ty.map_ty_mut_or_else(||vec![],|_,x|x.iter_mut().collect(),|ty|ty.succs_mut())
                            }
                            fn succs(&self) -> Vec<#W::Sym>{ use #W::NodeInner;
                                self.node.ty.map_ty_ref_or_else(||vec![],|_,x|x.clone(),|ty|ty.succs())
                            }
                            fn roll_sym(&mut self) -> #W::Sym{
                                let next_sym = #name_counter.next_sym();
                                self.node.sym = next_sym;
                                next_sym.erase()
                            }
                            fn cur_sym(&self) -> #W::Sym{ self.node.sym.erase() }
                            fn cur_sym_mut(&mut self) -> &mut #W::Sym{ self.node.sym.erase_mut() } fn clone_dyn(&self) -> Box<dyn #W::EgglogNode>{ Box::new(self.clone()) }
                            fn ty_name(&self) -> &'static str{ <#name_node::<(),()> as #W::EgglogTy>::TY_NAME }
                            fn ty_name_lower(&self) -> &'static str{ <#name_node::<(),()> as #W::EgglogTy>::TY_NAME_LOWER }
                            fn variant_name(&self) -> Option<&'static str>{ if V::TY_NAME==""{None}else{ Some(V::TY_NAME)} }
                            fn basic_field_names(&self) -> &[&'static str]{ V::BASIC_FIELD_NAMES }
                            fn basic_field_types(&self) -> &[&'static str]{ V::BASIC_FIELD_TYPES }
                            fn complex_field_names(&self) -> &[&'static str]{ V::COMPLEX_FIELD_NAMES }
                            fn complex_field_types(&self) -> &[&'static str]{ V::COMPLEX_FIELD_TYPES }
                            #[track_caller]
                            fn to_term(&self,term_dag: &mut #E::TermDag,
                                sym2term: &mut std::collections::HashMap< #W::Sym, #E::TermId>,
                                sym2ph_name: & std::collections::HashMap<#W::Sym, &'static str>) -> #E::TermId{
                                panic!()
                            }
                            #[track_caller]
                            fn add_atom( &self, query_builder:&mut #W::QueryBuilder) {
                                use #W::EgglogTy;

                            }
                        }
                        impl<T:#W::NodeDropperSgl, V:#W::EgglogEnumVariantTy> #W::VarsCollector for self::#name_node<T,V> {
                            #[track_caller] fn collect_vars( &self, query_builder:&mut Vec<(#W::VarName, #W::SortName)>
                            ) {
                                use #W::EgglogTy;
                                todo!()
                            }
                        }
                        impl<T: #W::NodeDropperSgl, V: #W::EgglogEnumVariantTy> AsRef<self::#name_node<T, ()>> for self::#name_node<T, V> {
                            fn as_ref(&self) -> &self::#name_node<T, ()> {
                                unsafe {
                                    &*(self as *const self::#name_node<T,V> as *const self::#name_node<T,()>)
                                }
                            }
                        }
                        impl<T: #W::NodeDropperSgl, V: #W::EgglogEnumVariantTy> #W::ToValue<self::#name_node<(), ()>> for #W::Value<self::#name_node<T, V>> {
                            fn to_value(&self, rule_ctx: &mut #W::RuleCtx<'_,'_,'_>) -> #W::Value<self::#name_node<(), ()>> {
                                #W::Value::new(self.erase())
                            }
                        }
                        impl<T:#W::NodeDropperSgl,V:#W::EgglogEnumVariantTy > Clone for self::#name_node<T,V> {
                            fn clone(&self) -> Self {
                                Self {
                                    node:
                                        #W::Node {
                                            ty: self.node.ty.clone(),
                                            span: self.node.span,
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
                    let types_and_idents = variant2sym_typed_ident_list(variant);
                    let (_variant_marker, variant_name) = variant2marker_name(variant);
                    quote! {#variant_name {#( #types_and_idents ),*  }}
                })
                .collect::<Vec<_>>();

            let to_egglog_string_match_arms = data_enum.variants.iter().map(|variant| {
                let variant_idents = variant2field_ident(variant);
                let (_variant_marker, variant_name) = variant2marker_name(variant);
                let s = " {:.3}".repeat(variant_idents.len());
                let format_str = format!("(let {{}} ({} {}))", variant_name, s);
                quote! {#name_inner::#variant_name {#( #variant_idents ),*  } => {
                    Some(format!(#format_str ,self.node.sym, #(#variant_idents),*))
                }}
            });
            let succs_match_arms = data_enum.variants.iter().map(|variant| {
                let variant_idents = variant2field_ident(variant);
                let (_variant_marker, variant_name) = variant2marker_name(variant);
                let vec_needed_syms: Vec<_> = variant2field_list_complex_ident_only(variant);
                quote! {#name_inner::#variant_name {#( #variant_idents ),*  } => {
                    vec![#(#vec_needed_syms.erase()),*]
                }}
            });
            let succs_mut_match_arms = data_enum.variants.iter().map(|variant| {
                let variant_idents = variant2field_ident(variant);
                let (_variant_marker, variant_name) = variant2marker_name(variant);
                let vec_needed_syms: Vec<_> = variant2field_list_complex_ident_only(variant);
                quote! {#name_inner::#variant_name {#( #variant_idents ),*  } => {
                    vec![#(#vec_needed_syms.erase_mut()),*]
                }}
            });
            let to_egglog_match_arms = data_enum.variants.iter().map(|variant| {
                let variant_fields = variant2field_ident(variant);
                let (_variant_marker, variant_name) = variant2marker_name(variant);
                quote! {#name_inner::#variant_name {#( #variant_fields ),*  } => {
                    #E::ast::GenericAction::Let(span!(), self.cur_sym().to_string(),
                        #E::ast::GenericExpr::Call(span!(),
                            stringify!(#variant_name),
                            vec![#(#variant_fields.to_var()),*]).to_owned_str()
                    )
                }}
            });
            let locate_latest_match_arms = data_enum.variants.iter().map(|variant| {
                let variant_idents = variant2field_ident(variant);
                let mapped_variant_idents = variant2mapped_ident_type_list(
                    variant,
                    |_, _| Some(quote! {}),
                    |x, _| Some(quote! { T::set_latest(#x.erase_mut());}),
                );
                let (_variant_marker, variant_name) = variant2marker_name(variant);
                quote! {
                    #name_inner::#variant_name {#(#variant_idents),* } => {
                        T::set_latest(self.node.sym.erase_mut());
                        #(#mapped_variant_idents)*
                    }
                }
            });

            let locate_next_match_arms = data_enum
                .variants
                .iter()
                .map(|x| locate_next_match_arms_ts(x, &name_inner));
            let locate_prev_match_arms = data_enum
                .variants
                .iter()
                .map(|x| locate_prev_match_arms_ts(x, &name_inner));

            let to_term_match_arms = data_enum
                .variants
                .iter()
                .map(|x| to_term_match_arms_ts(x, &name_inner));
            let add_atom_match_arms = data_enum
                .variants
                .iter()
                .map(|x| add_atom_match_arms_ts(x, &name_inner))
                .collect::<Vec<_>>();
            let collect_var_match_arms = data_enum
                .variants
                .iter()
                .map(|x| collect_var_match_arms_ts(x, &name_inner))
                .collect::<Vec<_>>();

            let native_egglog_match_arms = data_enum.variants.iter().map(|variant| {
                let variant_idents = variant2field_ident(variant);
                let (_variant_marker, variant_name) = variant2marker_name(variant);
                let insert_fn_name =
                    format_ident!("insert_{}", variant_name.to_string().to_snake_case());

                // 使用 variant2mapped_ident_type_list 来处理字段映射
                let recursive_calls = variant2mapped_ident_type_list(
                    variant,
                    |ident, _| {
                        // for basic type use source value
                        Some(quote! {
                            let #ident = #ident.clone();
                        })
                    },
                    |complex_ident, complex_ty| {
                        // for complex type，fetch value from sym_to_value_map 
                        // transform Sym<Expr> into Sym as key
                        Some(quote! {
                            let #complex_ident: #W::Value<#complex_ty<(),()>> = #W::Value::new(sym_to_value_map.get(&#complex_ident.erase()).unwrap().clone());
                        })
                    },
                );

                let field_args = variant2mapped_ident_type_list(
                    variant,
                    |ident, _| Some(quote! { #ident }),
                    |ident, _| Some(quote! { #ident }),
                );

                quote! {
                    #name_inner::#variant_name { #(#variant_idents),* } => {
                        #(#recursive_calls)*
                        let value = ctx.#insert_fn_name(#(#field_args),*);
                        sym_to_value_map.insert(sym, value.erase());
                        value.erase()
                    }
                }
            });

            let (new_fns, new_fn_names, new_fn_args, new_fn_arg_idents): (
                Vec<proc_macro2::TokenStream>,
                Vec<Ident>,
                Vec<Vec<TokenStream>>,
                Vec<Vec<TokenStream>>,
            ) = data_enum
                .variants
                .iter()
                .map(|x| new_fn_ts(x, &name_node, &name_inner, &name_counter))
                .collect();

            let (query_fns, query_fn_names, query_fn_args, query_fn_arg_idents): (
                Vec<proc_macro2::TokenStream>,
                Vec<Ident>,
                Vec<Vec<TokenStream>>,
                Vec<Vec<TokenStream>>,
            ) = data_enum
                .variants
                .iter()
                .map(|x| query_fn_ts(x, &name_node, &name_inner, &name_counter))
                .collect();

            let (
                query_leaf_fns,
                _query_leaf_fn_names,
                _query_leaf_fn_args,
                _query_leaf_fn_arg_idents,
            ): (
                Vec<proc_macro2::TokenStream>,
                Vec<Ident>,
                Vec<Vec<TokenStream>>,
                Vec<Vec<TokenStream>>,
            ) = data_enum
                .variants
                .iter()
                .map(|x| query_leaf_fns_tt(x, &name_node, &name_inner, &name_counter))
                .collect();
            let enum_variant_tys_def = data_enum.variants.iter().map(|variant| {
                let (variant_marker, variant_name) = variant2marker_name(variant);

                let valued_variant_name = format_ident!("Valued{}", variant_name);
                let values_with_types = variant2valued_struct_fields(variant);
                let basic_field_idents = variant2mapped_ident_type_list(
                    variant,
                    |basic, _| Some(quote!(#basic)),
                    |_, _| None,
                );
                let complex_field_idents = variant2mapped_ident_type_list(
                    variant,
                    |_, _| None,
                    |complex, _| Some(quote!(#complex)),
                );
                let basic_field_types = variant2mapped_ident_type_list(
                    variant,
                    |_, basic_type| Some(quote!(#basic_type)),
                    |_, _| None,
                );
                let complex_field_types = variant2mapped_ident_type_list(
                    variant,
                    |_, _| None,
                    |_, complex_type| Some(quote!(#complex_type)),
                );
                let value_iter = variant2mapped_ident_type_list(
                    variant,
                    |basic, _| Some(quote!(#basic: #W::Value::new(vals.next().unwrap()))),
                    |_, _| None,
                );

                quote! {
                    #[derive(Clone)]
                    pub struct #variant_marker;
                    #[derive(Debug,Clone,Copy)]
                    pub struct #valued_variant_name{
                        _itself: #W::Value<#name_node<(), #variant_marker>>,
                        #(#values_with_types),*
                    }
                    impl #valued_variant_name{
                        pub fn new(_itself : #W::Value<#name_node<(), #variant_marker>>, #(#values_with_types),*) -> Self{
                            Self {
                                _itself,
                                #(#basic_field_idents),*
                            }
                        }
                    }
                    impl #W::FromPlainValues for #valued_variant_name {
                        fn from_plain_values(vals: &mut impl Iterator<Item=#E::Value>) -> Self{
                            Self {
                                _itself: #W::Value::new(vals.next().unwrap()),
                                #(#value_iter),*
                            }
                        }
                    }
                    // impl #W::ToValue<#name_node<(),#variant_marker>> for #valued_variant_name {
                    //     fn to_value(&self, rule_ctx: &mut #W::RuleCtx<'_,'_,'_>) -> #W::Value<#name_node<(),#variant_marker>> {
                    //         self._itself
                    //     }
                    // }
                    impl #W::ToValue<#name_node<(),()>> for #valued_variant_name {
                        fn to_value(&self, rule_ctx: &mut #W::RuleCtx<'_,'_,'_>) -> #W::Value<#name_node<(),()>> {
                            #W::Value::new(self._itself.val)
                        }
                    }
                    impl #W::EgglogEnumVariantTy for #variant_marker {
                        const TY_NAME:&'static str = stringify!(#variant_name);
                        const BASIC_FIELD_NAMES:&[&'static str] = &[#(stringify!(#basic_field_idents)),* ];
                        const BASIC_FIELD_TYPES:&[&'static str] = &[#(stringify!(#basic_field_types)),* ];
                        const COMPLEX_FIELD_NAMES:&[&'static str] = &[#(stringify!(#complex_field_idents)),* ];
                        const COMPLEX_FIELD_TYPES:&[&'static str] = &[#(stringify!(#complex_field_types)),* ];
                        type ValuedWithDefault<T> = #valued_variant_name;
                    }
                }
            });

            let set_fns = data_enum
                .variants
                .iter()
                .map(|x| set_fns_tt(x, &name_inner, &name_node));

            let (variant_markers, variant_names) = variant_marker_names(data_enum);
            let rule_ctx_trait_and_impl = {
                let (insert_fns, insert_fn_decls): (Vec<TokenStream>, Vec<TokenStream>) = data_enum
                    .variants
                    .iter()
                    .map(|x| ctx_insert_fn_ts(x, &name_node))
                    .collect();
                let ctx_trait_name = format_ident!("{}RuleCtx", name_node);
                quote! {
                    pub trait #ctx_trait_name {
                        #(#insert_fn_decls)*
                    }
                    impl #ctx_trait_name for #W::RuleCtx<'_,'_,'_> {
                        #(#insert_fns)*
                    }
                }
            };
            // MARK: Enum Expanded
            let expanded = quote! {
                pub type #name_node_alias<T,V> = #W::Node<#name_egglogty_impl,T,#name_inner,V>;
                #(pub type #variant_names<T> = self::#name_node<T, #variant_markers>;)*
                #(#enum_variant_tys_def)*
                #[allow(unused)]
                // #[derive(#DE::Deref)]
                pub struct #name_node<T: #W::NodeDropperSgl = (),V:#W::EgglogEnumVariantTy=()>
                where Self: #W::EgglogNode + #W::EgglogTy {
                    node:#name_node_alias<T,V>
                }
                #[allow(unused)]
                #[derive(Clone, #ENM::EnumIs, #ENM::EnumDiscriminants)]
                pub enum #name_inner {
                    #(#variants_def_of_node_with_syms),*
                }
                #[allow(unused_variables)]
                const _:() = {
                    use #W::{EgglogNode, ToSpan, ToVar, ToOwnedStr, DeLiteral};
                    use std::marker::PhantomData;
                    use std::collections::HashMap;
                    use #E::prelude::*;
                    use #E::ast::{GenericAction, GenericExpr};
                    use #INVE;
                    impl<T:#W::TxSgl + #W::NonPatRecSgl> self::#name_node<T,()> {
                        #(#new_fns)*
                    }
                    impl<T:#W::TxSgl + #W::PatRecSgl> self::#name_node<T,()> {
                        #(#query_fns)*
                    }
                    impl<T:#W::TxSgl + #W::PatRecSgl> self::#name_node<T,()> {
                        #(#query_leaf_fns)*
                        #[track_caller]
                        pub fn query_leaf() -> self::#name_node<T,()> {
                            let node = #W::Node {
                                ty: #W::TyPH::PH,
                                sym: #name_counter.next_sym(),
                                span:Some(std::panic::Location::caller()),
                                _p:PhantomData,
                                _s:PhantomData,
                                sgl_specific: T::OwnerSpecDataInNode::default()
                            };
                            let node = #name_node {node};
                            T::on_new_query_leaf(&node);
                            node
                        }
                    }
                    use #W::TxSgl;
                    use #W::NonPatRecSgl;
                    use #W::PatRecSgl;
                    #(
                        impl<T:TxSgl + NonPatRecSgl> self::#name_node<T, #variant_markers> {
                            pub fn new(#(#new_fn_args),*) -> Self{
                                #name_node::<T,()>::#new_fn_names(#(#new_fn_arg_idents),*)
                            }
                        }
                        impl<T:TxSgl + PatRecSgl> self::#name_node<T, #variant_markers> {
                            pub fn query(#(#query_fn_args),*) -> Self{
                                #name_node::<T,()>::#query_fn_names(#(#query_fn_arg_idents),*)
                            }
                        }
                        impl<T:TxSgl> std::ops::Deref for self::#name_node<T, #variant_markers> {
                            type Target = #name_node<T>;

                            fn deref(&self) -> &Self::Target {
                                unsafe { std::mem::transmute(self) }
                            }
                        }
                    )*
                    impl<T:#W::RxSgl + #W::NonPatRecSgl, S: #W::EgglogEnumVariantTy> self::#name_node<T,S>{
                        pub fn pull(&self){
                            T::on_pull::<#name_egglogty_impl<T>>(self)
                        }
                    }
                    impl<T:#W::NodeDropperSgl, V:#W::EgglogEnumVariantTy> #W::EgglogNode for self::#name_node<T,V> {
                        fn succs_mut(&mut self) -> Vec<&mut #W::Sym>{
                            use #W::NodeInner;
                            self.node.ty.map_ty_mut_or_else(||vec![],|_,x|x.iter_mut().collect(),|ty|ty.succs_mut())
                        }
                        fn succs(&self) -> Vec<#W::Sym>{ use #W::NodeInner;
                            self.node.ty.map_ty_ref_or_else(||vec![],|_,x|x.clone(),|ty|ty.succs())
                        }
                        fn roll_sym(&mut self) -> #W::Sym{
                            let next_sym = #name_counter.next_sym();
                            self.node.sym = next_sym;
                            next_sym.erase()
                        }
                        fn cur_sym(&self) -> #W::Sym{ self.node.sym.erase() }
                        fn cur_sym_mut(&mut self) -> &mut #W::Sym{ self.node.sym.erase_mut() }
                        fn clone_dyn(&self) -> Box<dyn #W::EgglogNode>{ Box::new(self.clone()) }
                        fn ty_name(&self) -> &'static str{ <#name_node::<()> as #W::EgglogTy>::TY_NAME }
                        fn ty_name_lower(&self) -> &'static str{ <#name_node::<()> as #W::EgglogTy>::TY_NAME_LOWER }
                        fn variant_name(&self) -> Option<&'static str>{ if V::TY_NAME==""{None}else{ Some(V::TY_NAME)} }
                        fn basic_field_names(&self) -> &[&'static str]{ V::BASIC_FIELD_NAMES }
                        fn basic_field_types(&self) -> &[&'static str]{ V::BASIC_FIELD_TYPES }
                        fn complex_field_names(&self) -> &[&'static str]{ V::COMPLEX_FIELD_NAMES }
                        fn complex_field_types(&self) -> &[&'static str]{ V::COMPLEX_FIELD_TYPES }
                        #[track_caller]
                        fn to_term(&self,term_dag: &mut #E::TermDag,
                            sym2term: &mut HashMap< #W::Sym, #E::TermId>,
                            sym2ph_name:& HashMap<#W::Sym, &'static str>) -> #E::TermId{
                            use #W::{FromBase,EgglogTy,EgglogNode};
                            if let #W::TyPH::Ty(ty) = &self.node.ty{
                                match ty{
                                    #(#to_term_match_arms)*,
                                }
                            }else{
                                let term = term_dag.var(sym2ph_name.get(&self.cur_sym()).unwrap().to_string());
                                let term_id = term_dag.lookup(&term);
                                sym2term.insert(self.cur_sym(), term_id);
                                term_id
                            }
                        }
                        #[track_caller]
                        fn add_atom(
                            &self,
                            query_builder:&mut #W::QueryBuilder
                        ) {
                            use #W::EgglogTy;
                            match &self.node.ty{
                                #W::TyPH::Ty(_) => {
                                        panic!("can't call add_atom on non-pattern-def EgglogNode")
                                },
                                #W::TyPH::VarPH(dis, succs) => {
                                    let mut succs = succs.iter().cloned();
                                    match dis{
                                        #(#add_atom_match_arms),*
                                    }
                                },
                                #W::TyPH::PH => {
                                    // do nothing because you can't add entry for EgglogNode of unknown variant
                                }
                            }
                        }
                    }
                    impl<T:#W::NodeDropperSgl, V:#W::EgglogEnumVariantTy> #W::VarsCollector for self::#name_node<T,V> {
                        #[track_caller]
                        fn collect_vars(
                            &self,
                            vars: &mut Vec<(#W::VarName, #W::SortName)>
                        ) {
                            use #W::EgglogTy;
                            match &self.node.ty{
                                #W::TyPH::Ty(_) => {
                                        panic!("can't call collect_var on non-pattern-def EgglogNode")
                                },
                                // with basic fields
                                #W::TyPH::VarPH(dis, succs) => {
                                    vars.push((self.cur_sym().to_string(), Self::TY_NAME.to_string()));
                                    match dis{
                                        #(#collect_var_match_arms),*
                                    }
                                },
                                // only itself as complex field
                                #W::TyPH::PH => {
                                    vars.push((self.cur_sym().to_string(), <Self as #W::EgglogTy>::TY_NAME.to_string()));
                                }
                            }
                        }
                    }
                    impl<T:#W::NodeDropperSgl, V:#W::EgglogEnumVariantTy> #W::ToEgglog for self::#name_node<T,V> {
                        fn to_egglog_string(&self) -> Option<String>{
                            match self.node.ty.ty_ref()?{
                                #(#to_egglog_string_match_arms),*
                            }
                        }
                        fn to_egglog(&self) -> Action{
                            match self.node.ty.unwrap_ref(){
                                #(#to_egglog_match_arms),*
                            }
                        }
                        fn native_egglog(&self, ctx: &mut #W::RuleCtx, sym_to_value_map: &dashmap::DashMap<#W::Sym, egglog::Value>) -> egglog::Value {
                            // 使用 ctx.insert 将节点插入到 egraph 中
                            // 这里可以根据 sym_to_value_map 查询已有的值
                            let sym = self.cur_sym();
                            if let Some(value) = sym_to_value_map.get(&sym) {
                                // 如果 sym 已经在 map 中，返回对应的值
                                value.clone()
                            } else {
                                // 否则，使用 ctx.insert 插入新节点
                                // 根据节点类型调用相应的 insert 方法
                                match &*self.node.ty.unwrap_ref() {
                                    inner => {
                                        // 动态调用相应的 insert 方法
                                        match inner {
                                            #(#native_egglog_match_arms),*
                                        }
                                    }
                                }
                            }
                        }
                    }
                    #[allow(unused_variables)]
                    impl<T:#W::TxSgl + #W::VersionCtlSgl + #W::NonPatRecSgl, V:#W::EgglogEnumVariantTy> #W::LocateVersion for self::#name_node<T,V> {
                        fn locate_latest(&mut self) {
                            match self.node.ty.unwrap_mut(){
                                #(#locate_latest_match_arms),*
                            }
                        }
                        fn locate_next(&mut self) {
                            match self.node.ty.unwrap_mut(){
                                #(#locate_next_match_arms),*
                            }
                        }
                        fn locate_prev(&mut self) {
                            match self.node.ty.unwrap_mut(){
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
                                        ty: self.node.ty.clone(),
                                        span: self.node.span ,
                                        sym: self.node.sym.clone(),
                                        _p: PhantomData,
                                        _s: PhantomData,
                                        sgl_specific : T::OwnerSpecDataInNode::default(),
                                    }
                            }
                        }
                    }

                    impl<T:#W::TxSgl+ #W::VersionCtlSgl + #W::TxCommitSgl + #W::NonPatRecSgl,S: #W::EgglogEnumVariantTy> #W::Commit for self::#name_node<T,S>
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
                impl<T: #W::NodeDropperSgl, V: #W::EgglogEnumVariantTy> #W::ToValue<self::#name_node<(), ()>> for #W::Value<self::#name_node<T, V>> {
                    fn to_value(&self, rule_ctx: &mut #W::RuleCtx<'_,'_,'_>) -> #W::Value<self::#name_node<(), ()>> {
                        #W::Value::new(self.erase())
                    }
                }
                #rule_ctx_trait_and_impl
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
/// pattern vars
pub fn pat_vars(
    _attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let mut input = parse_macro_input!(item as DeriveInput);

    // append <PR> if it donesn't exist
    if input.generics.params.is_empty() {
        input.generics.params.push(parse_quote!(PR: PatRecSgl));
    } else {
        // check whether PR is contained
        let has_pr = input.generics.params.iter().any(|param| {
            if let syn::GenericParam::Type(type_param) = param {
                type_param.ident == "PR"
            } else {
                false
            }
        });

        if !has_pr {
            // append PR: PatRecSgl
            input.generics.params.insert(0, parse_quote!(PR: PatRecSgl));
        }
    }

    // append <PR> to struct fields' generic
    if let Data::Struct(data_struct) = &mut input.data {
        for field in &mut data_struct.fields {
            // check whether PR is contained
            if let syn::Type::Path(type_path) = &mut field.ty {
                if let Some(last_segment) = type_path.path.segments.last_mut() {
                    // check whether the field has generic
                    if last_segment.arguments.is_empty() {
                        // if do not have generic append <PR>
                        let mut args = syn::punctuated::Punctuated::new();
                        args.push(syn::GenericArgument::Type(parse_quote!(PR)));
                        last_segment.arguments = syn::PathArguments::AngleBracketed(
                            syn::AngleBracketedGenericArguments {
                                colon2_token: None,
                                lt_token: syn::token::Lt::default(),
                                args,
                                gt_token: syn::token::Gt::default(),
                            },
                        );
                    } else if let syn::PathArguments::AngleBracketed(args) =
                        &mut last_segment.arguments
                    {
                        // check whether PR is contained
                        let has_pr = args.args.iter().any(|arg| {
                            if let syn::GenericArgument::Type(type_arg) = arg {
                                if let syn::Type::Path(arg_path) = type_arg {
                                    return arg_path.path.segments.last().unwrap().ident == "PR";
                                }
                            }
                            false
                        });

                        if !has_pr {
                            if let Some(first_arg) = args.args.first_mut() {
                                if let syn::GenericArgument::Type(_) = first_arg {
                                    // replace
                                    *first_arg = syn::GenericArgument::Type(parse_quote!(PR));
                                }
                            } else {
                                args.args.push(syn::GenericArgument::Type(parse_quote!(PR)));
                            }
                        }
                    }
                }
            }
        }
    }

    let expanded = match &input.data {
        Data::Struct(data_struct) => {
            let members = data_struct.fields.members();
            let field_idents = data_struct
                .fields
                .iter()
                .map(|f| f.ident.as_ref().unwrap())
                .collect::<Vec<_>>();
            let field_types = data_struct.fields.iter().map(|f| &f.ty).collect::<Vec<_>>();
            let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

            let ident = &input.ident;

            let mut valued_input_struct = input.clone();
            let valued_ident = format_ident!("Valued{}", valued_input_struct.ident);
            valued_input_struct.ident = valued_ident.clone();
            let mut valued_tys = Vec::new();
            match &mut valued_input_struct.data {
                Data::Struct(valued_struct) => {
                    valued_struct.fields.iter_mut().for_each(|x| {
                        let ty_itself = &x.ty;
                        // let last_seg;
                        // if let Type::Path(type_path) = &x.ty {
                        //     last_seg = type_path.path.segments.last().clone().unwrap();
                        // } else {
                        //     panic!()
                        // }
                        x.ty = parse_quote!(<#ty_itself as #W::PatVars<PR>>::Valued);
                        valued_tys.push(x.ty.clone())
                    });
                }
                _ => panic!(),
            };
            // add PhantomData<PR> to struct
            if let Data::Struct(data_struct) = &mut valued_input_struct.data
                && let syn::Fields::Named(named_fields) = &mut data_struct.fields
            {
                let fields = &mut named_fields.named;
                fields.push(Field {
                    attrs: vec![parse_quote!(#[doc(hidden)])],
                    vis: Visibility::Inherited,
                    ident: Some(format_ident!("_p")),
                    colon_token: None,
                    ty: parse_quote!(std::marker::PhantomData<PR>),
                    mutability: syn::FieldMutability::None,
                });
            }
            quote! {
                #[derive(Debug)]
                #valued_input_struct
                impl #impl_generics #W::FromPlainValues for #valued_ident #ty_generics #where_clause {
                    fn from_plain_values(values:&mut impl Iterator<Item=#E::Value>) -> Self {
                        use #W::Value;
                        Self {
                            #(#members: #valued_tys::from_plain_values(values),)*
                            _p: std::marker::PhantomData
                        }
                    }
                }
                impl #impl_generics #W::PatVars<PR> for #ident #ty_generics #where_clause {
                    type Valued = #valued_ident<PR>;
                }
                impl #impl_generics #W::ToStrArcSort for #ident #ty_generics #where_clause{
                    fn to_str_arcsort(&self, egraph: &#E::EGraph) -> Vec<(#W::VarName, #E::ArcSort)> {
                        let mut v = Vec::new();
                        use #W::{EgglogNode, EgglogTy, VarsCollector};
                        let mut vars = Vec::new();
                        self.collect_vars(&mut vars);
                        for (basic_field_name,basic_field_type) in vars{
                            v.push(( basic_field_name, egraph
                                .get_sort_by_name(basic_field_type.as_str())
                                .unwrap()
                                .clone()));
                        }
                        v.into()
                    }
                }
                impl #impl_generics #W::VarsCollector for #ident #ty_generics #where_clause{
                    #[track_caller]
                    fn collect_vars(&self, vars:&mut Vec<(#W::VarName, #W::SortName)>){
                        #(
                            self.#field_idents.collect_vars(vars);
                        )*
                    }
                }
                impl #impl_generics #ident #ty_generics #where_clause{
                    fn new( #(#field_idents:#field_types,)* ) -> Self{
                        Self {
                            #(#field_idents,)*
                            _p: std::marker::PhantomData
                        }
                    }
                }
            }
        }
        _ => panic!("pattern_extracted's input can only be struct"),
    };

    // add PhantomData<PR> to struct
    if let Data::Struct(data_struct) = &mut input.data
        && let syn::Fields::Named(named_fields) = &mut data_struct.fields
    {
        let fields = &mut named_fields.named;
        fields.push(Field {
            attrs: vec![parse_quote!(#[doc(hidden)])],
            vis: Visibility::Inherited,
            ident: Some(format_ident!("_p")),
            colon_token: None,
            ty: parse_quote!(std::marker::PhantomData<PR>),
            mutability: syn::FieldMutability::None,
        });
    }
    quote! {
        #input
        #expanded
    }
    .into()
}
