//! This crate provides helper methods for matching against enum variants, and
//! extracting bindings to each of the fields in the deriving Struct or Enum in
//! a generic way.
//!
//! If you are writing a `#[derive]` which needs to perform some operation on every
//! field, then you have come to the right place!
//!
//! # Example
//!
//! ```
//! extern crate syn;
//! extern crate synstructure;
//! #[macro_use]
//! extern crate quote;
//! use synstructure::{each_field, BindStyle, Options};
//!
//! type TokenStream = String; // XXX: Dummy to not depend on rustc_macro
//!
//! fn sum_fields_derive(input: TokenStream) -> TokenStream {
//!     let source = input.to_string();
//!     let mut ast = syn::parse_macro_input(&source).unwrap();
//!
//!     let match_body = each_field(&mut ast, &Options::new(BindStyle::Ref), |bi| quote! {
//!         sum += #bi as i64;
//!     });
//!
//!     let name = &ast.ident;
//!     let (impl_generics, ty_generics, where_clause) = ast.generics.split_for_impl();
//!     let result = quote! {
//!         // Original struct unmodified
//!         #ast
//!
//!         impl #impl_generics ::sum_fields::SumFields for #name #ty_generics #where_clause {
//!             fn sum_fields(&self) -> i64 {
//!                 let mut sum = 0i64;
//!                 match *self { #match_body }
//!                 sum
//!             }
//!         }
//!     };
//!
//!     result.to_string().parse().unwrap()
//! }
//!
//! fn main() {}
//! ```
//!
//! For more example usage, consider investigating the `abomonation_derive` crate,
//! which makes use of this crate, and is fairly simple.

extern crate syn;
#[macro_use]
extern crate quote;

use syn::{Body, Field, Ident, MacroInput, VariantData};
use quote::{Tokens, ToTokens};

/// The type of binding to use when generating a pattern.
#[derive(Debug, Copy, Clone)]
pub enum BindStyle {
    /// `x`
    Move,
    /// `mut x`
    MoveMut,
    /// `ref x`
    Ref,
    /// `ref mut x`
    RefMut,
}

/// Binding options to use when generating a pattern.
pub struct Options {
    bind_style: BindStyle,
    prefix: String,
}

impl Options {
    /// Use the given style and the prefix "__binding" when generating a pattern.
    pub fn new(bind_style: BindStyle) -> Options {
        Options {
            bind_style: bind_style,
            prefix: "__binding".into(),
        }
    }

    /// Use the given style and prefix when generating a pattern.
    pub fn with_prefix(bind_style: BindStyle, prefix: String) -> Options {
        Options {
            bind_style: bind_style,
            prefix: prefix,
        }
    }

    /// The style to use to generate patterns.
    pub fn bind_style(&self) -> BindStyle {
        self.bind_style
    }

    /// The prefix to use to generate patterns.
    pub fn prefix(&self) -> &str {
        &self.prefix
    }
}

/// Information about a specific binding. This contains both an `Ident`
/// reference to the given field, and the syn `&'a Field` descriptor for that
/// field.
///
/// This type supports `quote::ToTokens`, so can be directly used within the
/// `quote!` macro. It expands to a reference to the matched field.
#[derive(Debug)]
pub struct BindingInfo<'a> {
    pub ident: Ident,
    pub field: &'a mut Field,
}

impl<'a> ToTokens for BindingInfo<'a> {
    fn to_tokens(&self, tokens: &mut Tokens) {
        self.ident.to_tokens(tokens);
    }
}

/// Generate a match pattern for binding to the given VariantData This function
/// returns a tuple of the tokens which make up that match pattern, and a
/// `BindingInfo` object for each of the bindings which were made. The `bind`
/// parameter controls the type of binding which is made.
///
/// The `BindingInfo` object holds a mutable reference into the original
/// `VariantData`, which means that mutations will be reflected in the source
/// object. This can be useful for removing attributes as they are used.
///
/// # Example
///
/// ```
/// extern crate syn;
/// extern crate synstructure;
/// use synstructure::{match_pattern, BindStyle, Options};
///
/// fn main() {
///     let mut ast = syn::parse_macro_input("struct A { a: i32, b: i32 }").unwrap();
///     let vd = if let syn::Body::Struct(ref mut vd) = ast.body {
///         vd
///     } else { unreachable!() };
///
///     let (tokens, bindings) = match_pattern(&ast.ident, vd, &Options::new(BindStyle::Ref));
///     assert_eq!(&tokens.to_string(),
///                "A { a : ref  __binding_0 ,  b : ref  __binding_1 ,  } ");
///     assert_eq!(bindings.len(), 2);
///     assert_eq!(&bindings[0].ident.to_string(), "__binding_0");
///     assert_eq!(&bindings[1].ident.to_string(), "__binding_1");
/// }
/// ```
pub fn match_pattern<'a, N: ToTokens>(name: &N,
                                      vd: &'a mut VariantData,
                                      options: &Options)
                                      -> (Tokens, Vec<BindingInfo<'a>>) {
    let mut t = Tokens::new();
    let mut matches = Vec::new();

    let prefix = match options.bind_style() {
        BindStyle::Move => Tokens::new(),
        BindStyle::MoveMut => quote!(mut),
        BindStyle::Ref => quote!(ref),
        BindStyle::RefMut => quote!(ref mut),
    };

    name.to_tokens(&mut t);
    match *vd {
        VariantData::Unit => {}
        VariantData::Tuple(ref mut fields) => {
            t.append("(");
            for (i, field) in fields.iter_mut().enumerate() {
                let ident: Ident = format!("{}_{}", options.prefix(), i).into();
                quote!(#prefix #ident ,).to_tokens(&mut t);
                matches.push(BindingInfo {
                    ident: ident,
                    field: field,
                });
            }
            t.append(")");
        }
        VariantData::Struct(ref mut fields) => {
            t.append("{");
            for (i, field) in fields.iter_mut().enumerate() {
                let ident: Ident = format!("{}_{}", options.prefix(), i).into();
                {
                    let field_name = field.ident.as_ref().unwrap();
                    quote!(#field_name : #prefix #ident ,).to_tokens(&mut t);
                }
                matches.push(BindingInfo {
                    ident: ident,
                    field: field,
                });
            }
            t.append("}");
        }
    }

    (t, matches)
}

/// This method generates a match branch for each of the substructures of the
/// given `MacroInput`. It will call `func` for each of these substructures,
/// passing in the bindings which were made for each of the fields in the
/// substructure. The return value of `func` is then used as the value of each
/// branch
///
/// The `BindingInfo` object holds a mutable reference into the original
/// `MacroInput`, which means that mutations will be reflected in the source
/// object. This can be useful for removing attributes as they are used.
///
/// # Example
///
/// ```
/// extern crate syn;
/// extern crate synstructure;
/// #[macro_use]
/// extern crate quote;
/// use synstructure::{match_substructs, BindStyle, Options};
///
/// fn main() {
///     let mut ast = syn::parse_macro_input("struct A { a: i32, b: i32 }").unwrap();
///
///     let tokens = match_substructs(&mut ast, &Options::new(BindStyle::Ref), |bindings| {
///         assert_eq!(bindings.len(), 2);
///         assert_eq!(bindings[0].ident.as_ref(), "__binding_0");
///         assert_eq!(bindings[1].ident.as_ref(), "__binding_1");
///         quote!("some_random_string")
///     });
///     let e = concat!("A { a : ref  __binding_0 ,  b : ref  __binding_1 ,  }",
///                     "  => { \"some_random_string\"  }  ");
///     assert_eq!(&tokens.to_string(), e);
/// }
/// ```
pub fn match_substructs<F, T: ToTokens>(input: &mut MacroInput, options: &Options, func: F) -> Tokens
    where F: Fn(Vec<BindingInfo>) -> T
{
    let ident = &input.ident;
    // Generate patterns for matching against all of the variants
    let variants = match input.body {
        Body::Enum(ref mut variants) => {
            variants.iter_mut()
                .map(|variant| {
                    let variant_ident = &variant.ident;
                    match_pattern(&quote!(#ident :: #variant_ident), &mut variant.data, &options)
                })
                .collect()
        }
        Body::Struct(ref mut vd) => vec![match_pattern(&ident, vd, &options)],
    };

    // Now that we have the patterns, generate the actual branches of the match
    // expression
    let mut t = Tokens::new();
    for (pat, bindings) in variants {
        let body = func(bindings);
        quote!(#pat => { #body }).to_tokens(&mut t);
    }

    t
}

/// This method calls `func` once per field in the struct or enum, and generates
/// a series of match branches which will destructure match argument, and run
/// the result of `func` once on each of the bindings.
///
/// The `BindingInfo` object holds a mutable reference into the original
/// `MacroInput`, which means that mutations will be reflected in the source
/// object. This can be useful for removing attributes as they are used.
///
/// # Example
///
/// ```
/// extern crate syn;
/// extern crate synstructure;
/// #[macro_use]
/// extern crate quote;
/// use synstructure::{each_field, BindStyle, Options};
///
/// fn main() {
///     let mut ast = syn::parse_macro_input("struct A { a: i32, b: i32 }").unwrap();
///
///     let tokens = each_field(&mut ast, &Options::new(BindStyle::Ref), |bi| quote! {
///         println!("Saw: {:?}", #bi);
///     });
///     let e = concat!("A { a : ref  __binding_0 ,  b : ref  __binding_1 ,  }  ",
///                     "=> { ",
///                     "{ println ! ( \"Saw: {:?}\" , __binding_0 ) ;  } ",
///                     "{ println ! ( \"Saw: {:?}\" , __binding_1 ) ;  } ",
///                     "( )   }  ");
///     assert_eq!(&tokens.to_string(), e);
/// }
/// ```
pub fn each_field<F, T: ToTokens>(input: &mut MacroInput, options: &Options, func: F) -> Tokens
    where F: Fn(BindingInfo) -> T
{
    match_substructs(input, options, |infos| {
        let mut t = Tokens::new();
        for info in infos {
            t.append("{");
            func(info).to_tokens(&mut t);
            t.append("}");
        }
        quote!(()).to_tokens(&mut t);
        t
    })
}
