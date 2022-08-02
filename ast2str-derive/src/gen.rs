use proc_macro2::TokenStream as TokenStream2;
use quote::ToTokens;
use syn::{spanned::Spanned, Field, Fields, Ident};

/// The possible Ast2Str field attributes.
enum A2SAttribute {
    /// One of the builder attributes.
    Builder(BuilderAttribute),
    /// An alias for the root node.
    Rename(syn::Lit),
}

/// The possible field attributes that get translated into [`TreeBuilder`] methods.
#[derive(Clone)]
pub enum BuilderAttribute {
    /// # None
    /// No configuration supplied. Translates to [`TreeBuilder::field`].
    None,
    /// # `#[forward]`
    /// Forward the implementation to the specified field
    Forward,
    /// # `#[skip]`
    /// Exclude the specified field from the output.
    Skip,
    /// # `#[skip_if = "Option::is_none"]`
    /// Exclude the field from the output if the specified callback returns `true`.
    SkipIf(TokenStream2),
    /// # `#[quoted]`
    /// Wraps the field name with backtickts. Translates to [`TreeBuilder::quoted`].
    Quoted,
    /// # `#[display]`
    /// Formats the field using [`std::fmt::Display`]. Translates to [`TreeBuilder::display`].
    Display,
    /// # `#[debug]`
    /// Formats the field using [`std::fmt::Debug`]. Translates to [`TreeBuilder::debug`].
    Debug,
    /// # `#[callback(name)]` or `#[callback(|x| ...)]`
    /// Formats the field using the given callback. Translates to [`TreeBuilder::display`].
    Callback(TokenStream2),
    /// # `#[delegate = "getter_name"]`
    /// Delegates the formatting of this to the given method on `self`. The method must accept
    /// no arguments and return something that implements `AstToStr`.
    Delegate(syn::Ident),
    /// # `#[default = "None"]`
    /// Formats the field as an option, displaying the supplied default if the value isn't present.
    Option(syn::Lit),
    /// # `#[list]` or `#[list(name)]` or `#[list(|elem| ...)]`
    /// Formats the field as a list, optionally applying the given closure to every element.
    List(Option<TokenStream2>),
}

/// Generates the [`TreeBuilder`] methods needed to stringify the fields.
pub fn generate_builder_methods(
    fields: &Fields,
    is_enum: bool,
) -> Result<FieldsToBuild<TokenStream2>, syn::Error> {
    let mut tokens = vec![];

    let fields = annotate_fields(fields)?;
    let fields = match fields {
        FieldsToBuild::Fields(fields) => fields,
        FieldsToBuild::Forward(field) => {
            let field_name = if let Some(index) = field.index {
                if is_enum {
                    Ident::new(&format!("operand{}", index), field.field.span()).into_token_stream()
                } else {
                    let index = syn::Index::from(index);
                    quote! { #index }
                }
            } else {
                field.field.ident.unwrap().into_token_stream()
            };
            return Ok(FieldsToBuild::Forward(quote! {
                #field_name.ast_to_str_impl(__symbols)
            }));
        }
    };

    for field in fields {
        if matches!(field.attr, BuilderAttribute::Skip) {
            continue;
        }

        let (mut builder_name, name) = match field.index {
            Some(index) => (
                Ident::new(&format!("field{}", index)[..], field.field.span()),
                if is_enum {
                    Ident::new(&format!("operand{}", index), field.field.span()).into_token_stream()
                } else {
                    let index = syn::Index::from(index);
                    quote! { #index }
                },
            ),
            None => (
                field.field.ident.clone().unwrap(),
                field.field.ident.clone().unwrap().into_token_stream(),
            ),
        };

        if let Some(rename_as) = field.rename_as {
            builder_name = match rename_as {
                syn::Lit::Str(value) => syn::Ident::new(&value.value()[..], field.field.span()),
                _ => unreachable!(),
            }
        }

        let accessor = if !is_enum {
            quote! { &self.#name }
        } else {
            quote! { #name }
        };

        match field.attr {
            BuilderAttribute::None => {
                tokens.push(quote! {
                    builder.field(stringify!(#builder_name),  #accessor)
                });
            }
            BuilderAttribute::Quoted => {
                tokens.push(quote! {
                    builder.quoted(stringify!(#builder_name),  #accessor)
                });
            }
            BuilderAttribute::Display => {
                tokens.push(quote! {
                    builder.display(stringify!(#builder_name),  #accessor)
                });
            }
            BuilderAttribute::Debug => {
                tokens.push(quote! {
                    builder.debug(stringify!(#builder_name),  #accessor)
                });
            }
            BuilderAttribute::SkipIf(condition) => {
                tokens.push(quote! {
                    if (#condition)(#accessor) {
                        builder
                    } else {
                        builder.field(stringify!(#builder_name),  #accessor)
                    }
                });
            }
            BuilderAttribute::Callback(cb) => {
                tokens.push(quote! {
                    builder.display(stringify!(#builder_name), (#cb)( #accessor))
                });
            }
            BuilderAttribute::Delegate(method_name) => tokens.push(quote! {
                builder.field(stringify!(#builder_name),  &self.#method_name())
            }),
            BuilderAttribute::Option(default) => {
                tokens.push(quote! {
                    builder.option(stringify!(#builder_name), #default,  #accessor)
                });
            }
            BuilderAttribute::List(Some(cb)) => {
                tokens.push(quote! {
                    builder.list_map(stringify!(#builder_name),  #accessor, #cb)
                });
            }
            BuilderAttribute::List(None) => {
                tokens.push(quote! {
                    builder.list(stringify!(#builder_name),  #accessor)
                });
            }
            BuilderAttribute::Skip | BuilderAttribute::Forward => unreachable!(),
        }
    }

    Ok(FieldsToBuild::Fields(tokens))
}

/// An optionally annotated struct/enum field.
struct AnnotatedField {
    /// The original field AST node.
    pub field: Field,
    /// The name of the field as an index if the given struct is a tuple struct.
    pub index: Option<usize>,
    /// The builder attribute of the field.
    pub attr: BuilderAttribute,
    /// The optional alias for the field.
    pub rename_as: Option<syn::Lit>,
}

/// An enum containing either a single `Forward` value or multiple regular entries.
pub enum FieldsToBuild<T> {
    /// A vec of regular fields.
    Fields(Vec<T>),
    /// A single forwarded field.
    Forward(T),
}

/// Collects the field annotations required to generate the [`TreeBuilder`] API calls.
fn annotate_fields(given_fields: &Fields) -> Result<FieldsToBuild<AnnotatedField>, syn::Error> {
    let mut forward_field = None;
    let mut fields = Vec::with_capacity(given_fields.len());

    let mut name_idx = None;
    for field in given_fields {
        let mut field_attr = BuilderAttribute::None;
        let mut rename_as = None;

        if field.ident.is_none() {
            name_idx = name_idx.or(Some(0)).map(|n| n + 1);
        }

        for attr in &field.attrs {
            let new_attr = match get_attribute(attr)? {
                A2SAttribute::Builder(attr) => attr,
                A2SAttribute::Rename(value) => {
                    rename_as = Some(value);
                    continue;
                }
            };

            if matches!(new_attr, BuilderAttribute::None) {
                continue;
            } else {
                if !matches!(field_attr, BuilderAttribute::None) {
                    return Err(syn::Error::new(
                        field.span(),
                        "Can only have a single formatting attribute",
                    ));
                }
                field_attr = new_attr;
            }

            if let BuilderAttribute::Forward = field_attr {
                if forward_field.is_some() {
                    return Err(syn::Error::new(
                        field.span(),
                        "Can only have one #[forward] attribute",
                    ));
                }

                forward_field = Some(AnnotatedField {
                    field: field.clone(),
                    index: name_idx.map(|n| n - 1),
                    attr: BuilderAttribute::Forward,
                    rename_as: None,
                });
            }
        }

        fields.push(AnnotatedField {
            field: field.clone(),
            index: name_idx.map(|n| n - 1),
            attr: field_attr,
            rename_as,
        });
    }

    Ok(if let Some(field) = forward_field {
        FieldsToBuild::Forward(field)
    } else {
        FieldsToBuild::Fields(fields)
    })
}

/// Find and extracts the identifier supplied by the `#[rename = "..."]` attribute, if any.
pub fn extract_rename_ident(attrs: &[syn::Attribute]) -> Option<String> {
    attrs.iter().find_map(|attr| {
        get_attribute(attr).ok().and_then(|r| match r {
            A2SAttribute::Builder(_) => None,
            A2SAttribute::Rename(name) => match name {
                syn::Lit::Str(s) => Some(s.value()),
                _ => unreachable!(),
            },
        })
    })
}

/// Extracts an Ast2Str attribute value from the given raw attribute and its identifier.
fn get_attribute(attr: &syn::Attribute) -> Result<A2SAttribute, syn::Error> {
    let ident = attr
        .path
        .segments
        .first()
        .expect("Attempted to parse an empty (?) attribute")
        .ident
        .to_string();
    let ba = match &ident[..] {
        "forward" => BuilderAttribute::Forward,
        "skip" => BuilderAttribute::Skip,
        "debug" => BuilderAttribute::Debug,
        "quoted" => BuilderAttribute::Quoted,
        "display" => BuilderAttribute::Display,
        "callback" => BuilderAttribute::Callback(attr.tokens.clone()),
        "skip_if" => BuilderAttribute::SkipIf({
            match parse_key_value_attr(attr)? {
                syn::Lit::Str(s) => match s.value().parse::<TokenStream2>() {
                    Ok(tokens) => tokens,
                    Err(e) => {
                        return Err(syn::Error::new(
                            s.span(),
                            &format!("Failed to parse the skip_if condition: {}", e),
                        ))
                    }
                },
                rest => {
                    return Err(syn::Error::new(
                        rest.span(),
                        "The skip_if condition must be a specified as a string literal.",
                    ))
                }
            }
        }),
        "delegate" => BuilderAttribute::Delegate({
            match parse_key_value_attr(attr)? {
                syn::Lit::Str(s) => Ident::new(&s.value(), s.span()),
                rest => {
                    return Err(syn::Error::new(
                        rest.span(),
                        "The delegate method name must be a string.",
                    ))
                }
            }
        }),
        "default" => BuilderAttribute::Option(parse_key_value_attr(attr)?),
        "rename" => return Ok(A2SAttribute::Rename(parse_key_value_attr(attr)?)),
        "list" => {
            if attr.tokens.is_empty() {
                BuilderAttribute::List(None)
            } else {
                BuilderAttribute::List(Some(attr.tokens.clone()))
            }
        }
        _ => BuilderAttribute::None,
    };
    Ok(A2SAttribute::Builder(ba))
}

/// Extracts the string literal from an attribute in the form `#[name = "value"]`.
fn parse_key_value_attr(attr: &syn::Attribute) -> Result<syn::Lit, syn::Error> {
    Ok(
        match attr
            .parse_meta()
            .expect("Failed to parse the contents of the attribute")
        {
            syn::Meta::NameValue(meta) => match meta.lit {
                syn::Lit::Str(_) => meta.lit,
                _ => {
                    return Err(syn::Error::new(
                        meta.span(),
                        "The default value must be a string literal",
                    ))
                }
            },
            rest => {
                return Err(syn::Error::new(
                    rest.span(),
                    "Only the key-value syntax is supported: `#[default = \"a default value\"]`",
                ))
            }
        },
    )
}
