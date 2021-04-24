//! A proc macro for pretty-printing ASTs (and nested structs in general).
//!
//! TODO: docs
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
///
/// # Example
/// ```ignore
/// TODO: docs
/// ```
#[proc_macro_derive(
    AstToStr,
    attributes(skip, forward, debug, display, callback, default, list, rename,)
)]
pub fn derive_ast_printer(input: TokenStream) -> TokenStream {
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
    let fields = match gen::generate_builder_methods(&i.fields, false)? {
        gen::FieldsToBuild::Fields(fields) => fields,
        gen::FieldsToBuild::Forward(fwd) => {
            return Ok(quote! {
                impl ::ast_printer_derive::AstToStr for #name {
                    fn print_ast(&self) -> String {
                        self.#fwd
                    }
                }
            });
        }
    };
    let rename_as = gen::extract_rename_ident(&i.attrs).unwrap_or_else(|| name.clone());
    Ok(quote! {
        impl ::ast_printer_derive::AstToStr for #name {
            #[allow(unused_parens)]
            fn ast_to_str(&self) -> String {
                use ::ast_printer_derive::TreeBuilder;
                let mut builder = TreeBuilder::new(stringify!(#rename_as));

                #(builder = #fields;)*

                builder.build()
            }
       }
    })
}

/// Generates an [`AstToStr`] impl for the given enum.
fn generate_enum_impl(e: ItemEnum) -> Result<TokenStream2, syn::Error> {
    let enum_name = e.ident;
    let rename_as = gen::extract_rename_ident(&e.attrs).unwrap_or_else(|| enum_name.clone());

    let mut arms = Vec::with_capacity(e.variants.len());
    for var in &e.variants {
        let name = &var.ident;
        let fields = gen::generate_builder_methods(&var.fields, true)?;
        let body = match fields {
            gen::FieldsToBuild::Fields(fields) => {
                quote! {{
                    let mut builder = TreeBuilder::new(concat!(stringify!(#rename_as), "::", stringify!(#name)));

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

    Ok(quote! {
        impl ::ast_printer_derive::AstToStr for #enum_name {
            #[allow(unused_parens)]
            fn ast_to_str(&self) -> String {
                use ::ast_printer_derive::TreeBuilder;
                use #enum_name::*;
                match &self {
                    #(#arms),*
                }
            }
        }
    })
}
