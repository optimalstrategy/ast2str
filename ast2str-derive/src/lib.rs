//! A proc macro for pretty-printing ASTs (and nested structures in general).
#![feature(proc_macro_diagnostic)]
extern crate syn;
#[macro_use]
extern crate quote;

mod gen;

use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::ToTokens;
use syn::{spanned::Spanned, Ident, ItemEnum, ItemStruct};

/// Automatically implements the [`AstToStr`] trait for the given struct or enum.
/// Every field of the given item must implement [`AstToStr`] or be annotated with one of the
/// the attributes.
#[proc_macro_derive(
    AstToStr,
    attributes(
        skip, skip_if, forward, debug, display, quoted, callback, default, list, rename, delegate
    )
)]
pub fn derive_ast2str(input: TokenStream) -> TokenStream {
    let item: syn::Item =
        syn::parse(input).expect("This macro can only be used with structs and enums");

    let output = match item {
        syn::Item::Struct(i) => generate_struct_impl(i),
        syn::Item::Enum(i) => generate_enum_impl(i),
        _ => {
            item.span()
                .unwrap()
                .error("This macro can only be used with structs and enums");
            return item.into_token_stream().into();
        }
    };

    match output {
        Ok(ok) => ok.into(),
        Err(e) => e.into_compile_error().into(),
    }
}

/// Generates an [`AstToStr`] impl for the given struct.
fn generate_struct_impl(i: ItemStruct) -> Result<TokenStream2, syn::Error> {
    let name = i.ident;
    let generics = i.generics;

    let fields = match gen::generate_builder_methods(&i.fields, false)? {
        gen::FieldsToBuild::Fields(fields) => fields,
        gen::FieldsToBuild::Forward(fwd) => {
            return Ok(generate_impl_for_stream(
                &name,
                &generics,
                quote! {
                    fn ast_to_str_impl(&self, __symbols: &dyn ::ast2str::ast2str_lib::Symbols) -> String {
                        self.#fwd
                    }
                },
            ));
        }
    };
    let rename_as = gen::extract_rename_ident(&i.attrs).unwrap_or_else(|| name.to_string());

    Ok(generate_impl_for_stream(
        &name,
        &generics,
        quote! {
            #[allow(unused_parens)]
            #[allow(unused_variables)]
            fn ast_to_str_impl(&self, __symbols: &dyn ::ast2str::ast2str_lib::Symbols) -> String {
                use ::ast2str::ast2str_lib::TreeBuilder;
                let mut builder = TreeBuilder::new(#rename_as, __symbols);

                #(builder = #fields;)*

                builder.build()
            }
        },
    ))
}

/// Generates an [`AstToStr`] impl for the given enum.
fn generate_enum_impl(e: ItemEnum) -> Result<TokenStream2, syn::Error> {
    let enum_name = e.ident;
    let rename_as = gen::extract_rename_ident(&e.attrs).unwrap_or_else(|| enum_name.to_string());

    let mut arms = Vec::with_capacity(e.variants.len());
    for var in &e.variants {
        let name = &var.ident;
        let fields = gen::generate_builder_methods(&var.fields, true)?;
        let body = match fields {
            gen::FieldsToBuild::Fields(fields) => {
                quote! {{
                    let mut builder = TreeBuilder::new(concat!(#rename_as, "::", stringify!(#name)), __symbols);

                    #(builder = #fields;)*

                    builder.build()
                }}
            }
            gen::FieldsToBuild::Forward(fwd) => fwd,
        };
        let pattern = {
            let mut field_bindings = vec![];
            let mut index = 0;

            for f in &var.fields {
                match &f.ident {
                    Some(name) => field_bindings.push(quote! { #name }),
                    None => {
                        let name = Ident::new(&format!("operand{}", index), f.span());
                        let syn_index = syn::Index::from(index);
                        field_bindings.push(quote! { #syn_index: #name });
                        index += 1;
                    }
                }
            }

            quote! {
                #name { #(#field_bindings),* }
            }
        };
        arms.push(quote! {
            #pattern => #body
        });
    }

    let impl_body = if !arms.is_empty() {
        quote! {
            match &self {
                #(#arms),*
            }
        }
    } else {
        quote! {
            String::from(stringify!(#enum_name))
        }
    };

    Ok(generate_impl_for_stream(
        &enum_name,
        &e.generics,
        quote! {
            #[allow(unused_parens)]
            #[allow(unused_variables)]
            fn ast_to_str_impl(&self, __symbols: &dyn ::ast2str::ast2str_lib::Symbols) -> String {
                use ::ast2str::ast2str_lib::TreeBuilder;
                use #enum_name::*;
                #impl_body
            }
        },
    ))
}

fn generate_impl_for_stream(
    name: &syn::Ident,
    generics: &syn::Generics,
    body: TokenStream2,
) -> TokenStream2 {
    let parametrized_name = generics.parameterized_name(name);
    let original_generics = generics.provided_generics_without_defaults();
    let where_clauses = generics.provided_where_clauses();

    quote! {
        impl <#(#original_generics),*> ::ast2str::ast2str_lib::AstToStr for #parametrized_name
        where
            #(#where_clauses),*
        {
            #body
        }
    }
}

/// The following trait was adapted from snafu (https://github.com/shepmaster/snafu/blob/main/snafu-derive/src/lib.rs#L1126) by Jake Goulding.
trait GenericAwareness {
    fn generics(&self) -> &syn::Generics;

    fn parameterized_name(&self, name: &syn::Ident) -> TokenStream2 {
        let original_generics = self.provided_generic_names();

        quote! { #name<#(#original_generics,)*> }
    }

    fn provided_generic_types_without_defaults(&self) -> Vec<proc_macro2::TokenStream> {
        use syn::TypeParam;
        self.generics()
            .type_params()
            .map(|t: &TypeParam| {
                let TypeParam {
                    attrs,
                    ident,
                    colon_token,
                    bounds,
                    ..
                } = t;
                quote! {
                    #(#attrs)*
                    #ident
                    #colon_token
                    #bounds
                }
            })
            .collect()
    }

    fn provided_generics_without_defaults(&self) -> Vec<proc_macro2::TokenStream> {
        self.provided_generic_lifetimes()
            .into_iter()
            .chain(self.provided_generic_types_without_defaults().into_iter())
            .collect()
    }

    fn provided_generic_lifetimes(&self) -> Vec<proc_macro2::TokenStream> {
        use syn::{GenericParam, LifetimeDef};

        self.generics()
            .params
            .iter()
            .flat_map(|p| match p {
                GenericParam::Lifetime(LifetimeDef { lifetime, .. }) => Some(quote! { #lifetime }),
                _ => None,
            })
            .collect()
    }

    fn provided_generic_names(&self) -> Vec<proc_macro2::TokenStream> {
        use syn::{ConstParam, GenericParam, LifetimeDef, TypeParam};

        self.generics()
            .params
            .iter()
            .map(|p| match p {
                GenericParam::Type(TypeParam { ident, .. }) => quote! { #ident },
                GenericParam::Lifetime(LifetimeDef { lifetime, .. }) => quote! { #lifetime },
                GenericParam::Const(ConstParam { ident, .. }) => quote! { #ident },
            })
            .collect()
    }

    fn provided_where_clauses(&self) -> Vec<proc_macro2::TokenStream> {
        self.generics()
            .where_clause
            .iter()
            .flat_map(|c| c.predicates.iter().map(|p| quote! { #p }))
            .collect()
    }
}

impl GenericAwareness for syn::Generics {
    fn generics(&self) -> &syn::Generics {
        self
    }
}
