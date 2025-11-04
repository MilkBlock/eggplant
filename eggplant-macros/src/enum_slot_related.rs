use heck::ToSnakeCase;
use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::Ident;

use crate::*;

pub fn new_fn_without_meta_ts(
    variant: &syn::Variant,
    name_node: &Ident,
    name_inner: &Ident,
    name_counter: &Ident,
) -> (TokenStream, Ident, Vec<TokenStream>, Vec<TokenStream>) {
    let mut ref_node_list: Vec<TokenStream> = variant2ref_node_list(&variant);
    // remove last element which is meta
    ref_node_list.pop();

    // no need to pop
    let complex_ident_list: Vec<TokenStream> = variant2field_list_complex_ident_only(variant);

    let mut ref_node_list_leave_idents = variant2ref_node_list_without_type(&variant);
    ref_node_list_leave_idents.pop();

    let _new_fn_args = variant2sym_list(&variant);
    let field_idents_assign = variant2assign_node_field_list(&variant);
    let _new_fn_field_idents_assign = variant2assign_node_field_typed(&variant);
    let field_idents = variant2field_ident(&variant);
    let (variant_marker, variant_name) = variant2marker_name(variant);
    let new_fn_without_meta_name =
        format_ident!("new_{}_with_meta", variant_name.to_string().to_snake_case());
    let _new_fn_name = format_ident!("_new_{}", variant_name.to_string().to_snake_case());
    let new_from_term_fn_name =
        format_ident!("new_{}_from_term", variant_name.to_string().to_snake_case());
    let new_from_term_dyn_fn_name = format_ident!(
        "new_{}_from_term_dyn",
        variant_name.to_string().to_snake_case()
    );

    let field_ty = variant2tys(&variant);
    let field_assignments: Vec<_> = field_idents
        .into_iter()
        .zip(field_ty.iter())
        .enumerate()
        .map(|(i, (ident, ty))| match BasicOrComplex::from(ty) {
            BasicOrComplex::BaseType | BasicOrComplex::UserDefinedBaseType => {
                quote! {
                    #ident: match term_dag.get(children[#i]) {
                        #E::Term::Lit(lit) => lit.deliteral(),
                        #E::Term::Var(v) => panic!(),
                        #E::Term::App(app,v) => panic!(),
                    }
                }
            }
            BasicOrComplex::UserDefinedContainerType | BasicOrComplex::ComplexType => {
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
            pub fn #new_fn_without_meta_name(#(#ref_node_list),*) -> self::#name_node<T,#variant_marker>{
                use #W::Meta;
                let __meta = SlotMetaBase::Inner{inner: SlotMeta::merge(&mut vec![
                        #(
                            *T::meta_of(#complex_ident_list.cur_sym().erase()).downcast().unwrap()
                        ),*
                    ].into_iter()) };
                let ty = #W::TyPH::Ty(#name_inner::#variant_name {#(#field_idents_assign),*  });
                let node = #W::Node {
                    ty,
                    sym: #name_counter.next_sym(),
                    span:Some(std::panic::Location::caller()),
                    _p:PhantomData,
                    _s:PhantomData,
                    sgl_specific: T::OwnerSpecDataInNode::default()
                };
                let node = #name_node {node};
                T::on_new(&node);
                node
            }
                        /// you should guarantee all the dep nodes has been init and store as (TermId, Sym) in term2sym
            #[track_caller]
            pub fn #new_from_term_fn_name(term_id:#E::TermId, term_dag: &#E::TermDag, term2sym:&mut std::collections::HashMap<#E::TermId, #W::Sym>) -> #name_node<T,#variant_marker>{
                let children = match term_dag.get(term_id){
                    #E::Term::App(app,v) => v,
                    _=> panic!()
                };
                let ty = #W::TyPH::Ty(#name_inner::#variant_name {#(#field_assignments),*  });
                let node = #W::Node {
                    ty,
                    sym: #name_counter.next_sym(),
                    span:Some(std::panic::Location::caller()),
                    _p:PhantomData,
                    _s:PhantomData,
                    sgl_specific: T::OwnerSpecDataInNode::default()
                };
                let node:#name_node<T,#variant_marker> = self::#name_node {node};
                term2sym.insert(term_id, node.cur_sym());
                node
            }
            #[track_caller]
            pub fn #new_from_term_dyn_fn_name(term_id:#E::TermId, term_dag: &#E::TermDag, term2sym:&mut std::collections::HashMap<#E::TermId, #W::Sym>) -> Box<dyn #W::EgglogNode>{
                Box::new(Self::#new_from_term_fn_name(term_id, term_dag, term2sym))
            }

        },
        new_fn_without_meta_name,
        ref_node_list,
        ref_node_list_leave_idents,
    )
}

pub fn ctx_insert_fn_ts_with_pr_without_meta(
    variant: &syn::Variant,
    name_node: &Ident,
) -> (TokenStream, TokenStream, TokenStream, TokenStream) {
    let valued_ref_node_list: Vec<TokenStream> = variant2valued_ref_node_list(&variant);
    let mut valued_ref_node_meta_list: Vec<TokenStream> =
        variant2valued_ref_node_meta_list(&variant);
    // remove last element which is meta
    valued_ref_node_meta_list.pop();

    let complex_field_idents = variant2mapped_ident_type_list_view_container_as_complex(
        variant,
        |_basic, _basic_ty| None,
        |complex, _complex_ty| Some(quote!(#complex)),
    );
    let complex_generic_idents = variant2mapped_ident_type_list_view_container_as_complex(
        variant,
        |_basic, _basic_ty| None,
        |complex, _complex_ty| {
            Some({
                let variant = format_ident!("V_{}", complex);
                quote!( #variant )
            })
        },
    );
    let complex_generic_idents_with_constraint =
        variant2mapped_ident_type_list_view_container_as_complex(
            variant,
            |_basic, _basic_ty| None,
            |complex, _complex_ty| {
                Some({
                    let variant = format_ident!("V_{}", complex);
                    quote!( #variant :#W::EgglogEnumVariantTy)
                })
            },
        );
    let _complex_field_tys = variant2mapped_ident_type_list_view_container_as_complex(
        variant,
        |_basic, _basic_ty| None,
        |_complex, complex_ty| Some(quote!(#complex_ty)),
    );
    let field_idents = variant2field_ident(&variant);
    let func_value_meta_field_idents = complex_field_idents
        .iter()
        .map(|x| format_ident!("func_value_meta_{}", x.to_string()))
        .collect::<Vec<_>>();

    let _new_fn_field_idents_assign = variant2assign_node_field_typed(&variant);
    let (variant_marker, variant_name) = variant2marker_name(variant);
    let insert_fn_name = format_ident!("insert_{}", variant_name.to_string().to_snake_case());
    let _new_fn_name = format_ident!("_new_{}", variant_name.to_string().to_snake_case());

    // MARK: Enum New Fns
    (
        // insert fn
        quote! {
            #[track_caller]
            #[allow(non_camel_case_types)]
            fn #insert_fn_name< #(#complex_generic_idents_with_constraint),* >(&self, #(#valued_ref_node_list),*) -> #W::Value<self::#name_node<(),#variant_marker>>{
                use #W::Value;
                use #W::Insertable;
                let key = [
                        #(#field_idents.to_value(self).erase()),*
                    ];
                #W::Value::new(self.insert(
                    <#variant_marker as #W::EgglogEnumVariantTy>::TY_NAME,
                    &key
                ))
            }
        },
        // insert decl
        quote! {
            #[track_caller]
            #[allow(non_camel_case_types)]
            fn #insert_fn_name< #(#complex_generic_idents_with_constraint),* >(&self, #(#valued_ref_node_list),*) -> #W::Value<self::#name_node<(),#variant_marker>>;
        },
        // pr insert fn
        quote! {
            #[track_caller]
            #[allow(non_camel_case_types)]
            fn #insert_fn_name< #(#complex_generic_idents_with_constraint),* >(&self, #(#valued_ref_node_meta_list),*) -> (#W::Value<self::#name_node<(),#variant_marker>>, SlotMeta){
                use #W::{Meta, EgglogEnumVariantTy, EgglogTy};
                #(
                    let #func_value_meta_field_idents =
                        (#complex_generic_idents::TY_NAME,
                            #complex_field_idents.to_value(&self.ctx).val,
                            #complex_field_idents.meta());
                )*
                let __merged = SlotMeta::merge(&mut vec![ #(#complex_field_idents.meta()),* ].into_iter());
                let __meta = SlotMetaBase::Inner{
                    inner: __merged.clone()
                };
                let __val = self.ctx.#insert_fn_name(#(#field_idents),*);
                PR::on_ctx_insert(
                    vec![#(#func_value_meta_field_idents),*],
                    (<#variant_marker as EgglogEnumVariantTy>::TY_NAME, __val.val , __merged.clone())
                );
                (__val,__merged)
            }
        },
        // pr insert decl
        quote! {
            #[track_caller]
            #[allow(non_camel_case_types)]
            fn #insert_fn_name< #(#complex_generic_idents_with_constraint),* >(&self, #(#valued_ref_node_meta_list),*) -> (#W::Value<self::#name_node<(),#variant_marker>>, SlotMeta);
        },
    )
}
