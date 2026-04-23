use heck::ToSnakeCase;
use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::Ident;

use crate::*;

/// if current node is not a PlaceHolder
pub fn to_term_match_arms_ts(variant: &syn::Variant, name_inner: &Ident) -> TokenStream {
    let variant_idents = variant2field_ident(variant);
    let variant_name = &variant.ident;
    let body = variant2mapped_ident_type_list_view_container_as_complex(
        variant,
        |basic_ident, _| {
            Some(quote! {
                let #basic_ident = term_dag.lit(#E::ast::Literal::from_base(#basic_ident));
            })
        },
        |complex_ident, _| {
            Some(quote! {
                let #complex_ident = sym2term.get(&#complex_ident.erase()).cloned().unwrap();
            })
        },
    );
    quote! {#name_inner::#variant_name {#( #variant_idents ),*  } => {
        #(#body)*
        let term_id =term_dag.app(stringify!(#variant_name).to_string(),vec![#( #variant_idents ),* ]);
        sym2term.insert(self.cur_sym(), term_id);
        term_id
    }}
}

pub fn add_table_fact_match_arms_ts(variant: &syn::Variant, name_inner: &Ident) -> TokenStream {
    let _variant_idents = variant2field_ident(variant);
    let variant_name = &variant.ident;
    // IMPORTANT: treat user-defined container sorts as complex (succ) rather than basic fields.
    // Otherwise, constructor queries will invent fresh placeholder vars for container fields,
    // which prevents users from binding them via `query(&container_var)` and can even make
    // explicit container leaf vars unbound in the rule body.
    let var_names = variant2mapped_ident_type_list_view_container_as_complex(
        variant,
        |basic, _| Some(quote! {format!("{}{}", self.cur_sym(), stringify!(#basic))}),
        |_, _| Some(quote!(succs.next().unwrap().to_string())),
    );
    let sort_names = variant2mapped_ident_type_list_view_container_as_complex(
        variant,
        |_, basic_type| Some(quote!(<#basic_type as EgglogTy>::TY_NAME.to_string())),
        |_, complex_type| Some(quote!(<#complex_type as EgglogTy>::TY_NAME.to_string())),
    );
    let discriminant_enum_name = format_ident!("{}Discriminants", name_inner);

    quote! {
    #discriminant_enum_name::#variant_name => {
        query_builder.add_table_fact(
            stringify!(#variant_name).to_string(),
            vec![
                #((#var_names, #sort_names),)*
                (self.cur_sym().to_string(), <Self as EgglogTy>::TY_NAME.to_string())
            ]
        )
    }}
}
pub fn collect_var_match_arms_ts(variant: &syn::Variant, name_inner: &Ident) -> TokenStream {
    let _variant_idents = variant2field_ident(variant);
    let variant_name = &variant.ident;
    // Same rule as `add_table_fact_match_arms_ts`: container sorts are treated as complex and
    // should not introduce extra "field vars" like `{node_sym}a0`.
    let var_names = variant2mapped_ident_type_list_view_container_as_complex(
        variant,
        |basic, _| Some(quote! {format!("{}{}", self.cur_sym(), stringify!(#basic))}),
        |_, _| None,
    );
    let var_types = variant2mapped_ident_type_list_view_container_as_complex(
        variant,
        |_, basic_type| Some(quote! {#basic_type}),
        |_, _| None,
    );
    let discriminant_enum_name = format_ident!("{}Discriminants", name_inner);

    quote! {
        #discriminant_enum_name::#variant_name => {
            #(vars.push((#var_names, <#var_types as EgglogTy>::TY_NAME.to_string()));)*
        }
    }
}

pub fn collect_binding_name_match_arms_ts(
    variant: &syn::Variant,
    name_inner: &Ident,
) -> TokenStream {
    let variant_name = &variant.ident;
    let binding_pushes =
        variant2mapped_ident_type_list_detailed(variant, |ident, _ty, kind| match kind {
            BasicOrComplex::BaseType | BasicOrComplex::UserDefinedBaseType => Some(quote! {
                names.push(format!("{}{}", self.cur_sym(), stringify!(#ident)));
            }),
            BasicOrComplex::UserDefinedContainerType => Some(quote! {
                names.push(succs.next().unwrap().to_string());
            }),
            BasicOrComplex::ComplexType => None,
        });
    let discriminant_enum_name = format_ident!("{}Discriminants", name_inner);

    quote! {
        #discriminant_enum_name::#variant_name => {
            #(#binding_pushes)*
        }
    }
}

pub fn locate_prev_match_arms_ts(variant: &syn::Variant, name_inner: &Ident) -> TokenStream {
    let variant_idents = variant2field_ident(variant);
    let mapped_variant_idents = variant2mapped_ident_type_list(
        variant,
        |_, _| Some(quote! {}),
        |x, _| Some(quote! {T::set_prev(#x.erase_mut());}),
    );
    let (_variant_marker, variant_name) = variant2marker_name(variant);
    quote! {
        #name_inner::#variant_name {#(#variant_idents),* } => {
            T::set_prev(self.node.sym.erase_mut());
            #(#mapped_variant_idents)*
        }
    }
}

pub fn locate_next_match_arms_ts(variant: &syn::Variant, name_inner: &Ident) -> TokenStream {
    let variant_idents = variant2field_ident(variant);
    let mapped_variant_idents = variant2mapped_ident_type_list(
        variant,
        |_, _| Some(quote! {}),
        |x, _| Some(quote! {T::set_next(#x.erase_mut());}),
    );
    let (_variant_marker, variant_name) = variant2marker_name(variant);
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
    let ref_node_list: Vec<TokenStream> = variant2ref_node_list(&variant);
    let ref_node_list_leave_idents = variant2ref_node_list_without_type(&variant);

    let _new_fn_args = variant2sym_list(&variant);
    let field_idents_assign = variant2assign_node_field_list(&variant);
    let _new_fn_field_idents_assign = variant2assign_node_field_typed(&variant);
    let field_idents = variant2field_ident(&variant);
    let (variant_marker, variant_name) = variant2marker_name(variant);
    let new_fn_name = format_ident!("new_{}", variant_name.to_string().to_snake_case());
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
            pub fn #new_fn_name(#(#ref_node_list),*) -> self::#name_node<T,#variant_marker>{
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
            /// with no side-effect (will not send command to EGraph or change node in WorkAreaGraph)
            #[track_caller]
            pub fn #_new_fn_name(#(#_new_fn_args),*) -> #name_node<T,#variant_marker>{
                let ty = #W::TyPH::Ty(#name_inner::#variant_name {#(#_new_fn_field_idents_assign),*  });
                let node = #W::Node {
                    ty,
                    sym: #name_counter.next_sym(),
                    span:Some(std::panic::Location::caller()),
                    _p:PhantomData,
                    _s:PhantomData,
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
        new_fn_name,
        ref_node_list,
        ref_node_list_leave_idents,
    )
}

pub fn query_fn_ts(
    variant: &syn::Variant,
    name_node: &Ident,
    name_inner: &Ident,
    name_counter: &Ident,
) -> (TokenStream, Ident, Vec<TokenStream>, Vec<TokenStream>) {
    let query_fn_args: Vec<TokenStream> = variant2ref_node_list_except_basic(&variant);
    // IMPORTANT: container sorts must be treated as "complex" here.
    // Otherwise, `query_*` will ignore container inputs and produce fresh placeholder vars,
    // which then makes any explicit container leaf vars in the pattern unbound in the rule body.
    let query_fn_idents: Vec<TokenStream> =
        variant2mapped_ident_type_list_view_container_as_complex(
            &variant,
            |_, _| None,
            |complex, _| Some(quote! {#complex.cur_sym()}),
        );
    let ref_node_list_leave_idents = variant2ident_list_except_basic(&variant);

    let _query_fn_args = variant2sym_list_except_basic(&variant);
    // let field_idents_assign = variant2assign_node_field_list(&variant);
    let _query_fn_field_idents_assign =
        variant2assign_node_field_typed_with_basic_default(&variant);
    // let _field_idents_assign = variant2field_ident_assign_with_basic_default(&variant);
    let (variant_marker, variant_name) = variant2marker_name(variant);
    let query_fn_name = format_ident!("query_{}", variant_name.to_string().to_snake_case());
    let _query_fn_name = format_ident!("_query_{}", variant_name.to_string().to_snake_case());

    // MARK: Enum New Fns
    (
        quote! {
            #[track_caller]
            pub fn #query_fn_name(#(#query_fn_args),*) -> self::#name_node<T,#variant_marker>{
                let ty = #W::TyPH::VarPH(<#name_inner as #EN::IntoDiscriminant>::Discriminant::#variant_name,  vec![#(#query_fn_idents),*]);
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
            /// with no side-effect (will not send command to EGraph or change node in WorkAreaGraph)
            #[track_caller]
            pub fn #_query_fn_name(#(#_query_fn_args),*) -> #name_node<T,#variant_marker>{
                let ty = #W::TyPH::Ty(#name_inner::#variant_name {#(#_query_fn_field_idents_assign),*  });
                let node = #W::Node {
                    ty,
                    sym: #name_counter.next_sym(),
                    span:Some(std::panic::Location::caller()),
                    _p:PhantomData,
                    _s:PhantomData,
                    sgl_specific: T::OwnerSpecDataInNode::default()
                };
                let node = #name_node {node};
                node
            }
        },
        query_fn_name,
        query_fn_args,
        ref_node_list_leave_idents,
    )
}

pub fn query_leaf_fns_tt(
    variant: &syn::Variant,
    name_node: &Ident,
    name_inner: &Ident,
    name_counter: &Ident,
) -> (TokenStream, Ident, Vec<TokenStream>, Vec<TokenStream>) {
    let ref_node_list = variant2ref_node_list(&variant);
    let ref_node_list_leave_idents = variant2ref_node_list_without_type(&variant);

    let _field_idents = variant2field_ident(&variant);
    let (variant_marker, variant_name) = variant2marker_name(variant);
    let query_leaf_fn_name =
        format_ident!("query_{}_leaf", variant_name.to_string().to_snake_case());
    let _query_fn_name = format_ident!("_query_{}", variant_name.to_string().to_snake_case());
    // let new_from_term_fn_name = format_ident!("new_{}_from_term",variant_name.to_string().to_snake_case());
    // let new_from_term_dyn_fn_name = format_ident!("new_{}_from_term_dyn",variant_name.to_string().to_snake_case());
    let _field_ty = variant2tys(&variant);
    // let _field_assignments: Vec<_> = field_idents
    //     .into_iter()
    //     .zip(field_ty.iter())
    //     .enumerate()
    //     .map(|(i, (ident, ty))| {
    //         if BasicOrComplex::from(ty).is_basic() {
    //             quote! {
    //                 #ident: match term_dag.get(children[#i]) {
    //                     #E::Term::Lit(lit) => lit.deliteral(),
    //                     #E::Term::Var(v) => panic!(),
    //                     #E::Term::App(app,v) => panic!(),
    //                 }
    //             }
    //         } else {
    //             quote! {
    //                 #ident: term2sym[&children[#i]].typed()
    //             }
    //         }
    //     })
    //     .collect();

    // MARK: Enum New Fns
    (
        quote! {
            #[track_caller]
            pub fn #query_leaf_fn_name() -> self::#name_node<T,#variant_marker>{
                let ty = #W::TyPH::VarPH(<#name_inner as #EN::IntoDiscriminant>::Discriminant::#variant_name, vec![]);
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
        },
        query_leaf_fn_name,
        ref_node_list,
        ref_node_list_leave_idents,
    )
}
pub fn query_constrain_fns_tt(
    variant: &syn::Variant,
    _name_inner: &Ident,
    name_node: &Ident,
) -> TokenStream {
    let base_types = {
        variant2mapped_ident_type_list_view_container_as_complex(
            variant,
            |_ident, ty| Some(quote! {#ty}),
            |_ident, _ty| None,
        )
    };
    let base_field_idents = variant2mapped_ident_type_list_view_container_as_complex(
        variant,
        |ident, _| Some(quote! {#ident}),
        |_ident, _| None,
    );
    let (variant_marker, _variant_name) = variant2marker_name(variant);

    let handle_fns =
        base_types
            .iter()
            .zip(base_field_idents.iter())
            .map(|(base_type, field_ident)| {
                let constrain_fn_name = format_ident!("{}", field_ident.to_string());
                let handle_fn_name = format_ident!("handle_{}", field_ident.to_string());
                quote! {
                    /// set fn of node, firstly update the sym version and specified field and then informs rx what happen on this node
                    /// rx's behavior depends on whether version control is enabled
                    #[track_caller]
                    pub fn #constrain_fn_name(self, #field_ident:&#base_type) -> Self{
                        use eggplant::wrap::PEq;
                        PR::on_new_constraint(self.#handle_fn_name().eq(#field_ident));
                        self
                    }
                }
            });

    quote! {
        impl<PR: #W::NodeDropperSgl + #W::PatRecSgl> self::#name_node<PR,#variant_marker>{
            #(
                #handle_fns
            )*
        }
    }
}
pub fn handle_getter_fns_tt(
    variant: &syn::Variant,
    _name_inner: &Ident,
    name_node: &Ident,
) -> TokenStream {
    let base_types = {
        variant2mapped_ident_type_list_view_container_as_complex(
            variant,
            |_ident, ty| Some(quote! {#ty}),
            |_ident, _ty| None,
        )
    };
    let base_field_idents = variant2mapped_ident_type_list_view_container_as_complex(
        variant,
        |ident, _| Some(quote! {#ident}),
        |_ident, _| None,
    );
    let (variant_marker, _variant_name) = variant2marker_name(variant);

    let handle_fns = base_types.iter().zip(base_field_idents.iter()).map(
        |(base_type, field_ident)| {
            let base_handle_fn_name = format_ident!("handle_{}", field_ident.to_string());
            quote! {
                /// set fn of node, firstly update the sym version and specified field and then informs rx what happen on this node
                /// rx's behavior depends on whether version control is enabled
                #[track_caller]
                pub fn #base_handle_fn_name(&self) -> #W::HandleToConstrain<#base_type>{
                    if let #W::TyPH::VarPH(_,_) = &self.node.ty{
                        #W::HandleToConstrain{
                            handle: #W::HandleTy::Base {
                                field_name : stringify!(#field_ident),
                                sym: self.node.sym.erase()
                            },
                            _p: PhantomData::<#base_type>
                        }
                    }else {
                        panic!("you can't get basic handle from non-queryed or non-varianted node")
                    }
                }
            }
        },
    );

    quote! {
        impl<PR: #W::NodeDropperSgl + #W::PatRecSgl> self::#name_node<PR,#variant_marker>{
            #(
                #handle_fns
            )*
        }
    }
}

pub fn set_fns_tt(variant: &syn::Variant, name_inner: &Ident, name_node: &Ident) -> TokenStream {
    let ref_node_list = variant2ref_node_list(&variant);
    let assign_node_field_list = variant2assign_node_field_list_without_prefixed_ident(&variant);
    let field_idents = variant2field_ident(variant);
    let (variant_marker, variant_name) = variant2marker_name(variant);

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
                                if let #W::TyPH::Ty(#name_inner::#variant_name{ #(#field_idents),*}) = &mut self.node.ty{
                                    *#field_ident = ___sym
                                };
                                T::on_set(self);
                                self
                            }
                        }
                    }
                );
    let sym_list = variants2sym_type_list(variant);
    let get_sym_fns = sym_list
        .iter()
        .zip(field_idents.iter())
        .map(|(sym, field_ident)| {
            let get_fn_name = format_ident!("{}_sym", field_ident.to_string());
            quote! {
                pub fn #get_fn_name(&self) -> #sym{
                    if let #W::TyPH::Ty(#name_inner::#variant_name{ #(#field_idents),*}) = &self.node.ty{
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
                                if let #W::TyPH::Ty(#name_inner::#variant_name{ #(#field_idents),*}) = &mut self.node.ty{
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
        impl<T: #W::TxSgl + #W::NodeSetterSgl + #W::NonPatRecSgl> self::#name_node<T,#variant_marker>{
            #(
                #set_fns
            )*
        }
    }
}

pub fn ctx_insert_fn_ts(
    variant: &syn::Variant,
    name_node: &Ident,
) -> (TokenStream, TokenStream, TokenStream, TokenStream) {
    let valued_ref_node_list: Vec<TokenStream> = variant2valued_ref_node_list(&variant);
    let field_idents = variant2field_ident(&variant);
    let complex_generic_idents = variant2mapped_ident_type_list_view_container_as_complex(
        variant,
        |_basic, _basic_ty| None,
        |complex, _complex_ty| {
            Some({
                let variant = format_ident!("V_{}", complex);
                quote!( #variant :#W::EgglogEnumVariantTy)
            })
        },
    );

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
            fn #insert_fn_name< #(#complex_generic_idents),* >(&self, #(#valued_ref_node_list),*) -> #W::Value<self::#name_node<(),#variant_marker>>{
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
            fn #insert_fn_name< #(#complex_generic_idents),* >(&self, #(#valued_ref_node_list),*) -> #W::Value<self::#name_node<(),#variant_marker>>;
        },
        // pr insert fn
        quote! {
            #[track_caller]
            #[allow(non_camel_case_types)]
            fn #insert_fn_name< #(#complex_generic_idents),* >(&self, #(#valued_ref_node_list),*) -> #W::Value<self::#name_node<(),#variant_marker>>{
                self.ctx.#insert_fn_name(#(#field_idents),*)
            }
        },
        // pr insert decl
        quote! {
            #[track_caller]
            #[allow(non_camel_case_types)]
            fn #insert_fn_name< #(#complex_generic_idents),* >(&self, #(#valued_ref_node_list),*) -> #W::Value<self::#name_node<(),#variant_marker>>;
        },
    )
}
pub fn ctx_subsume_remove_fn_ts_with_pr(
    variant: &syn::Variant,
    _name_node: &Ident,
) -> (TokenStream, TokenStream, TokenStream, TokenStream) {
    let valued_ref_node_list: Vec<TokenStream> = variant2valued_ref_node_list(&variant);
    let valued_ref_node_meta_list: Vec<TokenStream> = variant2valued_ref_node_meta_list(&variant);
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
    let _func_value_meta_field_idents = complex_field_idents
        .iter()
        .map(|x| format_ident!("func_value_meta_{}", x.to_string()))
        .collect::<Vec<_>>();

    let _new_fn_field_idents_assign = variant2assign_node_field_typed(&variant);
    let (variant_marker, variant_name) = variant2marker_name(variant);
    let subsume_fn_name = format_ident!("subsume_{}", variant_name.to_string().to_snake_case());
    let remove_fn_name = format_ident!("remove_{}", variant_name.to_string().to_snake_case());
    let _new_fn_name = format_ident!("_new_{}", variant_name.to_string().to_snake_case());

    // MARK: Enum New Fns
    (
        // insert fn
        quote! {
            #[track_caller]
            #[allow(non_camel_case_types)]
            fn #subsume_fn_name< #(#complex_generic_idents),* >(&self, #(#valued_ref_node_list),*) {
                use #W::Value;
                use #W::Insertable;
                let key = [
                        #(#field_idents.to_value(self).erase()),*
                    ];
                self.subsume(
                    <#variant_marker as #W::EgglogEnumVariantTy>::TY_NAME,
                    &key
                )
            }
            #[track_caller]
            #[allow(non_camel_case_types)]
            fn #remove_fn_name< #(#complex_generic_idents),* >(&self, #(#valued_ref_node_list),*) {
                use #W::Value;
                use #W::Insertable;
                let key = [
                        #(#field_idents.to_value(self).erase()),*
                    ];
                self.remove(
                    <#variant_marker as #W::EgglogEnumVariantTy>::TY_NAME,
                    &key
                )
            }
        },
        // insert decl
        quote! {
            #[track_caller]
            #[allow(non_camel_case_types)]
            fn #subsume_fn_name< #(#complex_generic_idents),* >(&self, #(#valued_ref_node_list),*) ;
            #[track_caller]
            #[allow(non_camel_case_types)]
            fn #remove_fn_name< #(#complex_generic_idents),* >(&self, #(#valued_ref_node_list),*) ;
        },
        // pr insert fn
        quote! {
            #[track_caller]
            #[allow(non_camel_case_types)]
            fn #subsume_fn_name< #(#complex_generic_idents),* >(&self, #(#valued_ref_node_meta_list),*) {
                 self.ctx.#subsume_fn_name(#(#field_idents),*);
            }
            #[track_caller]
            #[allow(non_camel_case_types)]
            fn #remove_fn_name< #(#complex_generic_idents),* >(&self, #(#valued_ref_node_meta_list),*) {
                 self.ctx.#remove_fn_name(#(#field_idents),*);
            }
        },
        // pr insert decl
        quote! {
            #[track_caller]
            #[allow(non_camel_case_types)]
            fn #subsume_fn_name< #(#complex_generic_idents),* >(&self, #(#valued_ref_node_meta_list),*) ;
            #[track_caller]
            #[allow(non_camel_case_types)]
            fn #remove_fn_name< #(#complex_generic_idents),* >(&self, #(#valued_ref_node_meta_list),*) ;
        },
    )
}

pub fn ctx_insert_fn_ts_with_pr(
    variant: &syn::Variant,
    name_node: &Ident,
) -> (TokenStream, TokenStream, TokenStream, TokenStream) {
    let valued_ref_node_list: Vec<TokenStream> = variant2valued_ref_node_list(&variant);
    let valued_ref_node_meta_list: Vec<TokenStream> = variant2valued_ref_node_meta_list(&variant);
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
            fn #insert_fn_name< #(#complex_generic_idents_with_constraint),* >(&self, #(#valued_ref_node_meta_list),*) -> (#W::Value<self::#name_node<(),#variant_marker>>, PR::MetaTy){
                use #W::{Meta, EgglogEnumVariantTy, EgglogTy};
                #(
                    let #func_value_meta_field_idents =
                        (#complex_generic_idents::TY_NAME,
                            #complex_field_idents.to_value(&self.ctx).val,
                            #complex_field_idents.meta());
                )*
                let __val = self.ctx.#insert_fn_name(#(#field_idents),*);
                let __merged = PR::on_ctx_insert(
                    vec![#(#func_value_meta_field_idents),*],
                    (<#variant_marker as EgglogEnumVariantTy>::TY_NAME, __val.val )
                );
                (__val,__merged)
            }
        },
        // pr insert decl
        quote! {
            #[track_caller]
            #[allow(non_camel_case_types)]
            fn #insert_fn_name< #(#complex_generic_idents_with_constraint),* >(&self, #(#valued_ref_node_meta_list),*) -> (#W::Value<self::#name_node<(),#variant_marker>>, PR::MetaTy);
        },
    )
}

pub fn ctx_set_fn_ts(
    variant: &syn::Variant,
    output: &TokenStream,
    func_name: &Ident,
) -> (TokenStream, TokenStream, TokenStream, TokenStream) {
    let valued_ref_node_list: Vec<TokenStream> = variant2valued_ref_node_list(&variant);
    let field_idents = variant2field_ident(&variant);

    let _new_fn_field_idents_assign = variant2assign_node_field_typed(&variant);
    let mut complex_generic_idents = variant2mapped_ident_type_list_view_container_as_complex(
        variant,
        |_basic, _basic_ty| None,
        |complex, _complex_ty| {
            Some({
                let variant = format_ident!("V_{}", complex);
                quote!( #variant :#W::EgglogEnumVariantTy)
            })
        },
    );
    let (_variant_marker, variant_name) = variant2marker_name(variant);
    let set_fn_name = format_ident!("set_{}", variant_name.to_string().to_snake_case());
    let _new_fn_name = format_ident!("_new_{}", variant_name.to_string().to_snake_case());

    let output_is_basic = BasicOrComplex::from(output).is_basic();
    let output_generic = format_ident!("V_out");
    let output_param_ty = if output_is_basic {
        quote!(impl eggplant::wrap::Insertable<#output>)
    } else {
        complex_generic_idents.push(quote!( #output_generic :#W::EgglogEnumVariantTy));
        quote!(impl eggplant::wrap::Insertable<#output<(), #output_generic>>)
    };

    (
        quote! {
            #[track_caller]
            #[allow(non_camel_case_types)]
            fn #set_fn_name< #(#complex_generic_idents),* >(&self, #(#valued_ref_node_list,)* output:#output_param_ty) {
                use #W::EgglogFunc;
                use #W::Value;
                use #W::Insertable;
                let key = [
                        #(#field_idents.to_value(self).erase(),)* output.to_value(self).erase()
                    ];
                self.insert_func_tbl(
                    #func_name::<()>::FUNC_NAME,
                    &key
                );
            }
        },
        quote! {
            #[track_caller]
            #[allow(non_camel_case_types)]
            fn #set_fn_name< #(#complex_generic_idents),* >(&self, #(#valued_ref_node_list,)*output:#output_param_ty) ;
        },
        // pr insert fn
        quote! {
            #[track_caller]
            #[allow(non_camel_case_types)]
            fn #set_fn_name< #(#complex_generic_idents),* >(&self, #(#valued_ref_node_list,)* output:#output_param_ty){
                self.ctx.#set_fn_name(#(#field_idents,)* output)
            }
        },
        // pr insert decl
        quote! {
            #[track_caller]
            #[allow(non_camel_case_types)]
            fn #set_fn_name< #(#complex_generic_idents),* >(&self, #(#valued_ref_node_list,)* output:#output_param_ty);
        },
    )
}

pub fn ctx_read_fn_ts(
    variant: &syn::Variant,
    output: &TokenStream,
    func_name: &Ident,
) -> (TokenStream, TokenStream, TokenStream, TokenStream) {
    let valued_ref_node_list: Vec<TokenStream> = variant2valued_ref_node_list(&variant);
    let field_idents = variant2field_ident(&variant);

    let complex_generic_idents = variant2mapped_ident_type_list_view_container_as_complex(
        variant,
        |_basic, _basic_ty| None,
        |complex, _complex_ty| {
            Some({
                let variant = format_ident!("V_{}", complex);
                quote!( #variant :#W::EgglogEnumVariantTy)
            })
        },
    );
    let (_variant_marker, variant_name) = variant2marker_name(variant);
    let read_fn_name = format_ident!("read_{}", variant_name.to_string().to_snake_case());
    let read_value_fn_name = format_ident!("{}_value", read_fn_name);
    let try_read_fn_name = format_ident!("try_{}", read_fn_name);
    let try_read_value_fn_name = format_ident!("try_{}", read_value_fn_name);

    let returns_base = matches!(
        BasicOrComplex::from(output),
        BasicOrComplex::BaseType | BasicOrComplex::UserDefinedBaseType
    );

    let (read_fn, read_fn_decl, try_read_fn, try_read_fn_decl) = if returns_base {
        (
            quote! {
                #[track_caller]
                #[allow(non_camel_case_types)]
                fn #read_fn_name< #(#complex_generic_idents),* >(&self, #(#valued_ref_node_list),*) -> #output {
                    use #W::EgglogFunc;
                    use #W::Insertable;
                    let key = [
                        #(#field_idents.to_value(self).erase()),*
                    ];
                    let out = self.lookup_expect(#func_name::<()>::FUNC_NAME, &key);
                    <#output as #W::BoxedValue>::devalue(self, out)
                }
            },
            quote! {
                #[track_caller]
                #[allow(non_camel_case_types)]
                fn #read_fn_name< #(#complex_generic_idents),* >(&self, #(#valued_ref_node_list),*) -> #output;
            },
            quote! {
                #[track_caller]
                #[allow(non_camel_case_types)]
                fn #try_read_fn_name< #(#complex_generic_idents),* >(&self, #(#valued_ref_node_list),*) -> Option<#output> {
                    use #W::EgglogFunc;
                    use #W::Insertable;
                    let key = [
                        #(#field_idents.to_value(self).erase()),*
                    ];
                    let out = self.lookup(#func_name::<()>::FUNC_NAME, &key)?;
                    Some(<#output as #W::BoxedValue>::devalue(self, out))
                }
            },
            quote! {
                #[track_caller]
                #[allow(non_camel_case_types)]
                fn #try_read_fn_name< #(#complex_generic_idents),* >(&self, #(#valued_ref_node_list),*) -> Option<#output>;
            },
        )
    } else {
        (
            quote! {
                #[track_caller]
                #[allow(non_camel_case_types)]
                fn #read_fn_name< #(#complex_generic_idents),* >(&self, #(#valued_ref_node_list),*) -> #W::Value<#output> {
                    use #W::EgglogFunc;
                    use #W::Value;
                    use #W::Insertable;
                    let key = [
                        #(#field_idents.to_value(self).erase()),*
                    ];
                    #W::Value::new(self.lookup_expect(#func_name::<()>::FUNC_NAME, &key))
                }
            },
            quote! {
                #[track_caller]
                #[allow(non_camel_case_types)]
                fn #read_fn_name< #(#complex_generic_idents),* >(&self, #(#valued_ref_node_list),*) -> #W::Value<#output>;
            },
            quote! {
                #[track_caller]
                #[allow(non_camel_case_types)]
                fn #try_read_fn_name< #(#complex_generic_idents),* >(&self, #(#valued_ref_node_list),*) -> Option<#W::Value<#output>> {
                    use #W::EgglogFunc;
                    use #W::Value;
                    use #W::Insertable;
                    let key = [
                        #(#field_idents.to_value(self).erase()),*
                    ];
                    self.lookup(#func_name::<()>::FUNC_NAME, &key).map(#W::Value::new)
                }
            },
            quote! {
                #[track_caller]
                #[allow(non_camel_case_types)]
                fn #try_read_fn_name< #(#complex_generic_idents),* >(&self, #(#valued_ref_node_list),*) -> Option<#W::Value<#output>>;
            },
        )
    };

    let read_value_fn = quote! {
        #[track_caller]
        #[allow(non_camel_case_types)]
        fn #read_value_fn_name< #(#complex_generic_idents),* >(&self, #(#valued_ref_node_list),*) -> #W::Value<#output> {
            use #W::EgglogFunc;
            use #W::Value;
            use #W::Insertable;
            let key = [
                #(#field_idents.to_value(self).erase()),*
            ];
            #W::Value::new(self.lookup_expect(#func_name::<()>::FUNC_NAME, &key))
        }
    };
    let try_read_value_fn = quote! {
        #[track_caller]
        #[allow(non_camel_case_types)]
        fn #try_read_value_fn_name< #(#complex_generic_idents),* >(&self, #(#valued_ref_node_list),*) -> Option<#W::Value<#output>> {
            use #W::EgglogFunc;
            use #W::Value;
            use #W::Insertable;
            let key = [
                #(#field_idents.to_value(self).erase()),*
            ];
            self.lookup(#func_name::<()>::FUNC_NAME, &key).map(#W::Value::new)
        }
    };
    let read_value_decl = quote! {
        #[track_caller]
        #[allow(non_camel_case_types)]
        fn #read_value_fn_name< #(#complex_generic_idents),* >(&self, #(#valued_ref_node_list),*) -> #W::Value<#output>;
    };
    let try_read_value_decl = quote! {
        #[track_caller]
        #[allow(non_camel_case_types)]
        fn #try_read_value_fn_name< #(#complex_generic_idents),* >(&self, #(#valued_ref_node_list),*) -> Option<#W::Value<#output>>;
    };

    let read_fn_pr = if returns_base {
        quote! {
            #[track_caller]
            #[allow(non_camel_case_types)]
            fn #read_fn_name< #(#complex_generic_idents),* >(&self, #(#valued_ref_node_list),*) -> #output {
                self.ctx.#read_fn_name(#(#field_idents),*)
            }
        }
    } else {
        quote! {
            #[track_caller]
            #[allow(non_camel_case_types)]
            fn #read_fn_name< #(#complex_generic_idents),* >(&self, #(#valued_ref_node_list),*) -> #W::Value<#output> {
                self.ctx.#read_fn_name(#(#field_idents),*)
            }
        }
    };
    let try_read_fn_pr = if returns_base {
        quote! {
            #[track_caller]
            #[allow(non_camel_case_types)]
            fn #try_read_fn_name< #(#complex_generic_idents),* >(&self, #(#valued_ref_node_list),*) -> Option<#output> {
                self.ctx.#try_read_fn_name(#(#field_idents),*)
            }
        }
    } else {
        quote! {
            #[track_caller]
            #[allow(non_camel_case_types)]
            fn #try_read_fn_name< #(#complex_generic_idents),* >(&self, #(#valued_ref_node_list),*) -> Option<#W::Value<#output>> {
                self.ctx.#try_read_fn_name(#(#field_idents),*)
            }
        }
    };
    let read_value_fn_pr = quote! {
        #[track_caller]
        #[allow(non_camel_case_types)]
        fn #read_value_fn_name< #(#complex_generic_idents),* >(&self, #(#valued_ref_node_list),*) -> #W::Value<#output> {
            self.ctx.#read_value_fn_name(#(#field_idents),*)
        }
    };
    let try_read_value_fn_pr = quote! {
        #[track_caller]
        #[allow(non_camel_case_types)]
        fn #try_read_value_fn_name< #(#complex_generic_idents),* >(&self, #(#valued_ref_node_list),*) -> Option<#W::Value<#output>> {
            self.ctx.#try_read_value_fn_name(#(#field_idents),*)
        }
    };

    let read_fn_decl_pr = if returns_base {
        quote! {
            #[track_caller]
            #[allow(non_camel_case_types)]
            fn #read_fn_name< #(#complex_generic_idents),* >(&self, #(#valued_ref_node_list),*) -> #output;
        }
    } else {
        quote! {
            #[track_caller]
            #[allow(non_camel_case_types)]
            fn #read_fn_name< #(#complex_generic_idents),* >(&self, #(#valued_ref_node_list),*) -> #W::Value<#output>;
        }
    };
    let try_read_fn_decl_pr = if returns_base {
        quote! {
            #[track_caller]
            #[allow(non_camel_case_types)]
            fn #try_read_fn_name< #(#complex_generic_idents),* >(&self, #(#valued_ref_node_list),*) -> Option<#output>;
        }
    } else {
        quote! {
            #[track_caller]
            #[allow(non_camel_case_types)]
            fn #try_read_fn_name< #(#complex_generic_idents),* >(&self, #(#valued_ref_node_list),*) -> Option<#W::Value<#output>>;
        }
    };
    let read_value_decl_pr = quote! {
        #[track_caller]
        #[allow(non_camel_case_types)]
        fn #read_value_fn_name< #(#complex_generic_idents),* >(&self, #(#valued_ref_node_list),*) -> #W::Value<#output>;
    };
    let try_read_value_decl_pr = quote! {
        #[track_caller]
        #[allow(non_camel_case_types)]
        fn #try_read_value_fn_name< #(#complex_generic_idents),* >(&self, #(#valued_ref_node_list),*) -> Option<#W::Value<#output>>;
    };

    (
        quote! {
            #read_fn
            #read_value_fn
            #try_read_fn
            #try_read_value_fn
        },
        quote! {
            #read_fn_decl
            #read_value_decl
            #try_read_fn_decl
            #try_read_value_decl
        },
        quote! {
            #read_fn_pr
            #read_value_fn_pr
            #try_read_fn_pr
            #try_read_value_fn_pr
        },
        quote! {
            #read_fn_decl_pr
            #read_value_decl_pr
            #try_read_fn_decl_pr
            #try_read_value_decl_pr
        },
    )
}
