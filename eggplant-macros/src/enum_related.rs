use heck::ToSnakeCase;
use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::Ident;

use crate::*;

pub fn to_term_match_arms_ts(variant: &syn::Variant, name_inner: &Ident) -> TokenStream {
    let variant_idents = variant_to_field_ident(variant);
    let variant_name = &variant.ident;
    let body = variant_to_mapped_ident_type_list(
        variant,
        |basic_ident, _| {
            Some(quote! {
                let #basic_ident = term_dag.lit(#E::ast::Literal::from_base(#basic_ident.clone()));
            })
        },
        |complex_ident, _| {
            Some(quote! {
                let #complex_ident = sym2term.get(&#complex_ident.erase()).cloned().unwrap();
                let #complex_ident = term_dag.get(#complex_ident).clone();
            })
        },
    );
    quote! {#name_inner::#variant_name {#( #variant_idents ),*  } => {
        #(#body)*
        let term =term_dag.app(stringify!(#variant_name).to_string(),vec![#( #variant_idents ),* ]);
        let term_id = term_dag.lookup(&term);
        sym2term.insert(self.cur_sym(), term_id);
        term_id
    }}
}

pub fn locate_prev_match_arms_ts(variant: &syn::Variant, name_inner: &Ident) -> TokenStream {
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
}

pub fn locate_next_match_arms_ts(variant: &syn::Variant, name_inner: &Ident) -> TokenStream {
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
}

pub fn new_fn_ts(
    variant: &syn::Variant,
    name_node: &Ident,
    name_inner: &Ident,
    name_counter: &Ident,
) -> (TokenStream, Ident, Vec<TokenStream>, Vec<TokenStream>) {
    let ref_node_list: Vec<TokenStream> = variant_to_ref_node_list(&variant);
    let ref_node_list_leave_idents = variant_to_ref_node_list_leave_ident(&variant);

    let _new_fn_args = variant_to_sym_list(&variant);
    let field_idents_assign = variant_to_assign_node_field_list(&variant);
    let _new_fn_field_idents_assign = variant_to_typed_assign_node_field_list(&variant);
    let field_idents = variant_to_field_ident(&variant);
    let (variant_marker, variant_name) = variant_marker_name(variant);
    let new_fn_name = format_ident!("new_{}", variant_name.to_string().to_snake_case());
    let _new_fn_name = format_ident!("_new_{}", variant_name.to_string().to_snake_case());
    let new_from_term_fn_name =
        format_ident!("new_{}_from_term", variant_name.to_string().to_snake_case());
    let new_from_term_dyn_fn_name = format_ident!(
        "new_{}_from_term_dyn",
        variant_name.to_string().to_snake_case()
    );

    let field_ty = variant_to_tys(&variant);
    let field_assignments: Vec<_> = field_idents
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
            pub fn #new_fn_name(#(#ref_node_list),*) -> self::#name_node<T,#variant_marker>{
                let ty = Some(#name_inner::#variant_name {#(#field_idents_assign),*  });
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
                let ty = Some(#name_inner::#variant_name {#(#_new_fn_field_idents_assign),*  });
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
                let ty = Some(#name_inner::#variant_name {#(#field_assignments),*  });
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
        },
        new_fn_name,
        ref_node_list,
        ref_node_list_leave_idents,
    )
}

pub fn new_ph_fns_tt(
    variant: &syn::Variant,
    name_node: &Ident,
    name_inner: &Ident,
    name_counter: &Ident,
) -> (TokenStream, Ident, Vec<TokenStream>, Vec<TokenStream>) {
    let ref_node_list = variant_to_ref_node_list(&variant);
    let ref_node_list_leave_idents = variant_to_ref_node_list_leave_ident(&variant);

    let _new_fn_args = variant_to_sym_list(&variant);
    let field_idents_assign = variant_to_field_ident_with_default(&variant);
    let _new_fn_field_idents_assign = variant_to_typed_assign_node_field_list(&variant);
    let field_idents = variant_to_field_ident(&variant);
    let (variant_marker, variant_name) = variant_marker_name(variant);
    let new_ph_fn_name = format_ident!("new_{}_ph", variant_name.to_string().to_snake_case());
    let _new_fn_name = format_ident!("_new_{}", variant_name.to_string().to_snake_case());
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
                let ty = Some(#name_inner::#variant_name {#(#field_idents_assign),*  });
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
}

pub fn set_fns_tt(variant: &syn::Variant, name_inner: &Ident, name_node: &Ident) -> TokenStream {
    let ref_node_list = variant_to_ref_node_list(&variant);
    let assign_node_field_list = variant_to_assign_node_field_list_without_prefixed_ident(&variant);
    let field_idents = variant_to_field_ident(variant);
    let (variant_marker, variant_name) = variant_marker_name(variant);

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
                                if let Some(#name_inner::#variant_name{ #(#field_idents),*}) = &mut self.node.ty{
                                    *#field_ident = ___sym
                                };
                                T::on_set(self);
                                self
                            }
                        }
                    }
                );
    let sym_list = variants_to_sym_type_list(variant);
    let get_sym_fns = sym_list
        .iter()
        .zip(field_idents.iter())
        .map(|(sym, field_ident)| {
            let get_fn_name = format_ident!("{}_sym", field_ident.to_string());
            quote! {
                pub fn #get_fn_name(&self) -> #sym{
                    if let Some(#name_inner::#variant_name{ #(#field_idents),*}) = &self.node.ty{
                        #field_ident.clone()
                    }else{
                        panic!()
                    }
                }
            }
        });
    let get_mut_sym_fns = sym_list.iter().zip(field_idents.iter()
                    ).map(
                    |(sym,field_ident)|{
                        let get_fn_name = format_ident!("{}_sym_mut",field_ident.to_string());
                        quote! {
                            pub fn #get_fn_name(&mut self) -> &mut #sym{
                                if let Some(#name_inner::#variant_name{ #(#field_idents),*}) = &mut self.node.ty{
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
}

pub fn ctx_insert_fn_ts(variant: &syn::Variant, name_node: &Ident) -> (TokenStream, TokenStream) {
    let valued_ref_node_list: Vec<TokenStream> = variant_to_valued_ref_node_list(&variant);
    let field_idents = variant_to_field_ident(&variant);

    let _new_fn_field_idents_assign = variant_to_typed_assign_node_field_list(&variant);
    let (variant_marker, variant_name) = variant_marker_name(variant);
    let insert_fn_name = format_ident!("insert_{}", variant_name.to_string().to_snake_case());
    let _new_fn_name = format_ident!("_new_{}", variant_name.to_string().to_snake_case());
    let _new_from_term_fn_name =
        format_ident!("new_{}_from_term", variant_name.to_string().to_snake_case());
    let _new_from_term_dyn_fn_name = format_ident!(
        "new_{}_from_term_dyn",
        variant_name.to_string().to_snake_case()
    );

    let field_ty = variant_to_tys(&variant);
    let _field_assignments: Vec<_> = field_idents
        .iter()
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
            fn #insert_fn_name(&mut self, #(#valued_ref_node_list),*) -> #W::Value<self::#name_node<(),#variant_marker>>{
                use #W::Value;
                use #W::ToValue;
                let key = [
                        #(#field_idents.to_value(self).detype()),*
                    ];
                #W::Value::new(self.insert(
                    <#variant_marker as #W::EgglogEnumVariantTy>::TY_NAME,
                    &key
                ))
            }
        },
        quote! {
            #[track_caller]
            fn #insert_fn_name(&mut self, #(#valued_ref_node_list),*) -> #W::Value<self::#name_node<(),#variant_marker>>;
        },
        // insert_fn_name,
        // valued_ref_node_list,
        // ref_node_list_leave_idents,
    )
}
