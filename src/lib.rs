//! This crate provides helper types for matching against enum variants, and
//! extracting bindings to each of the fields in the deriving Struct or Enum in
//! a generic way.
//!
//! If you are writing a `#[derive]` which needs to perform some operation on
//! every field, then you have come to the right place!
//!
//! # Example: `WalkFields`
//! ```
//! # #[macro_use] extern crate synstructure;
//! # #[macro_use] extern crate quote;
//! /*
//!  * Trait
//!  */
//! pub trait WalkFields: std::any::Any {
//!     fn walk_fields(&self, walk: &mut FnMut(&WalkFields));
//! }
//! impl WalkFields for i32 {
//!     fn walk_fields(&self, walk: &mut FnMut(&WalkFields)) {}
//! }
//!
//! /*
//!  * Derive Implementation
//!  */
//! # decl_derive_mod!(
//! decl_derive!([WalkFields] walkfields_derive(s) {
//!     let body = s.each(|bi| quote!{
//!         walk(#bi)
//!     });
//!
//!     s.bound_impl("::WalkFields", quote!{
//!         fn walk_fields(&self, walk: &mut FnMut(&::WalkFields)) {
//!             match *self { #body }
//!         }
//!     })
//! });
//! # );
//!
//! /*
//!  * Test Case
//!  */
//! fn main() {
//!     test_derive! {
//!         walkfields_derive {
//!             enum A<T> {
//!                 B(i32, T),
//!                 C(i32),
//!             }
//!         }
//!         expands to {
//!             impl<T> ::WalkFields for A<T> where T: ::WalkFields {
//!                 fn walk_fields(&self, walk: &mut FnMut(&::WalkFields)) {
//!                     match *self {
//!                         A::B(ref __binding_0, ref __binding_1,) => {
//!                             { walk(__binding_0) }
//!                             { walk(__binding_1) }
//!                         }
//!                         A::C(ref __binding_0,) => {
//!                             { walk(__binding_0) }
//!                         }
//!                     }
//!                 }
//!             }
//!         }
//!     }
//! }
//! ```
//!
//! # Example: `Interest`
//! ```
//! # #[macro_use] extern crate synstructure;
//! # #[macro_use] extern crate quote;
//! /*
//!  * Trait
//!  */
//! pub trait Interest {
//!     fn interesting(&self) -> bool;
//! }
//! impl Interest for i32 {
//!     fn interesting(&self) -> bool { *self > 0 }
//! }
//!
//! /*
//!  * Derive Implementation
//!  */
//! # decl_derive_mod!(
//! decl_derive!([Interest] interest_derive(mut s) {
//!     let body = s.fold(false, |acc, bi| quote!{
//!         #acc || ::Interest::interesting(#bi)
//!     });
//!
//!     s.bound_impl("::Interest", quote!{
//!         fn interesting(&self) -> bool {
//!             match *self {
//!                 #body
//!             }
//!         }
//!     })
//! });
//! # );
//!
//! /*
//!  * Test Case
//!  */
//! fn main() {
//!     test_derive!{
//!         interest_derive {
//!             enum A<T> {
//!                 B(i32, T),
//!                 C(i32),
//!             }
//!         }
//!         expands to {
//!             impl<T> ::Interest for A<T> where T: ::Interest {
//!                 fn interesting(&self) -> bool {
//!                     match *self {
//!                         A::B(ref __binding_0, ref __binding_1,) => {
//!                             false || ::Interest::interesting(__binding_0) ||
//!                                 ::Interest::interesting(__binding_1)
//!                         }
//!                         A::C(ref __binding_0,) => {
//!                             false || ::Interest::interesting(__binding_0)
//!                         }
//!                     }
//!                 }
//!             }
//!         }
//!     }
//! }
//! ```
//!
//! For more example usage, consider investigating the `abomonation_derive` crate,
//! which makes use of this crate, and is fairly simple.

extern crate syn;
#[macro_use]
extern crate quote;

use syn::{Attribute, Body, ConstExpr, DeriveInput, Field, Ident, Ty, TyParamBound,
          VariantData, WhereBoundPredicate, WherePredicate};
use syn::visit::{self, Visitor};

use quote::{ToTokens, Tokens};

// NOTE: This module has documentation hidden, as it only exports macros (which
// always appear in the root of the crate) and helper methods / re-exports used
// in the implementation of those macros.
#[doc(hidden)]
pub mod macros;

/// The type of binding to use when generating a pattern.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
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

impl ToTokens for BindStyle {
    fn to_tokens(&self, tokens: &mut Tokens) {
        match *self {
            BindStyle::Move => {}
            BindStyle::MoveMut => tokens.append("mut"),
            BindStyle::Ref => tokens.append("ref"),
            BindStyle::RefMut => {
                tokens.append("ref");
                tokens.append("mut");
            }
        }
    }
}

/// Information about a specific binding. This contains both an `Ident`
/// reference to the given field, and the syn `&'a Field` descriptor for that
/// field.
///
/// This type supports `quote::ToTokens`, so can be directly used within the
/// `quote!` macro. It expands to a reference to the matched field.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BindingInfo<'a> {
    /// The name which this BindingInfo will bind to.
    pub binding: Ident,

    /// The type of binding which this BindingInfo will create.
    pub style: BindStyle,

    field: &'a Field,
}

impl<'a> ToTokens for BindingInfo<'a> {
    fn to_tokens(&self, tokens: &mut Tokens) {
        self.binding.to_tokens(tokens);
    }
}

impl<'a> BindingInfo<'a> {
    /// Returns a reference to the underlying `syn` AST node which this
    /// `BindingInfo` references
    pub fn ast(&self) -> &'a Field {
        self.field
    }

    /// Generates the pattern fragment for this field binding.
    ///
    /// # Example
    /// ```
    /// # #[macro_use] extern crate quote;
    /// # extern crate synstructure;
    /// # extern crate syn;
    /// # use synstructure::*;
    /// # fn main() {
    /// let di = syn::parse_derive_input(r#"
    ///     enum A {
    ///         B{ a: i32, b: i32 },
    ///         C(u32),
    ///     }
    /// "#).unwrap();
    /// let s = Structure::new(&di);
    ///
    /// assert_eq!(
    ///     s.variants()[0].bindings()[0].pat(),
    ///     quote!{
    ///         ref __binding_0
    ///     }
    /// );
    /// # }
    /// ```
    pub fn pat(&self) -> Tokens {
        let BindingInfo {
            ref binding,
            ref style,
            ..
        } = *self;
        quote!(#style #binding)
    }
}

/// This type is similar to `syn`'s `Variant` type, however each of the fields
/// are references rather than owned. When this is used as the AST for a real
/// variant, this struct simply borrows the fields of the `syn` `Variant`,
/// however this type may also be used as the sole variant for astruct.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct VariantAst<'a> {
    pub ident: &'a Ident,
    pub attrs: &'a [Attribute],
    pub data: &'a VariantData,
    pub discriminant: &'a Option<ConstExpr>,
}

/// A wrapper around a `syn` `DeriveInput`'s variant which provides utilities
/// for destructuring `Variant`s with `match` expressions.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct VariantInfo<'a> {
    pub prefix: Option<&'a Ident>,
    bindings: Vec<BindingInfo<'a>>,
    omitted_fields: bool,
    ast: VariantAst<'a>,
}

impl<'a> VariantInfo<'a> {
    fn new(ast: VariantAst<'a>, prefix: Option<&'a Ident>) -> Self {
        let bindings = match *ast.data {
            VariantData::Unit => vec![],
            VariantData::Tuple(ref fields) | VariantData::Struct(ref fields) => fields
                .iter()
                .enumerate()
                .map(|(i, field)| {
                    BindingInfo {
                        binding: format!("__binding_{}", i).into(),
                        style: BindStyle::Ref,
                        field: field,
                    }
                })
                .collect::<Vec<_>>(),
        };

        VariantInfo {
            prefix: prefix,
            bindings: bindings,
            omitted_fields: false,
            ast: ast,
        }
    }

    /// Returns a slice of the bindings in this Variant.
    pub fn bindings(&self) -> &[BindingInfo<'a>] {
        &self.bindings
    }

    /// Returns a mut slice of the bindings in this Variant.
    pub fn bindings_mut(&mut self) -> &mut [BindingInfo<'a>] {
        &mut self.bindings
    }

    /// Returns a `VariantAst` object which contains references to the
    /// underlying `syn` AST node which this `Variant` was created from.
    pub fn ast(&self) -> VariantAst<'a> {
        self.ast
    }

    /// True if any bindings were omitted due to a `filter` call.
    pub fn omitted_bindings(&self) -> bool {
        self.omitted_fields
    }

    /// Generates the match-arm pattern which could be used to match against this Variant.
    ///
    /// # Example
    /// ```
    /// # #[macro_use] extern crate quote;
    /// # extern crate synstructure;
    /// # extern crate syn;
    /// # use synstructure::*;
    /// # fn main() {
    /// let di = syn::parse_derive_input(r#"
    ///     enum A {
    ///         B(i32, i32),
    ///         C(u32),
    ///     }
    /// "#).unwrap();
    /// let s = Structure::new(&di);
    ///
    /// assert_eq!(
    ///     s.variants()[0].pat(),
    ///     quote!{
    ///         A::B(ref __binding_0, ref __binding_1,)
    ///     }
    /// );
    /// # }
    /// ```
    pub fn pat(&self) -> Tokens {
        let mut t = Tokens::new();
        if let Some(prefix) = self.prefix {
            prefix.to_tokens(&mut t);
            t.append("::");
        }
        self.ast.ident.to_tokens(&mut t);
        match *self.ast.data {
            VariantData::Unit => {
                assert!(self.bindings.len() == 0);
            }
            VariantData::Tuple(..) => {
                t.append("(");
                for binding in &self.bindings {
                    binding.pat().to_tokens(&mut t);
                    t.append(",");
                }
                if self.omitted_fields {
                    t.append("..");
                }
                t.append(")");
            }
            VariantData::Struct(..) => {
                t.append("{");
                for binding in &self.bindings {
                    binding.field.ident.to_tokens(&mut t);
                    t.append(":");
                    binding.pat().to_tokens(&mut t);
                    t.append(",");
                }
                if self.omitted_fields {
                    t.append("..");
                }
                t.append("}");
            }
        }
        t
    }

    /// Runs the passed-in function once for each bound field, passing in a `BindingInfo`.
    /// and generating a `match` arm which evaluates the returned tokens.
    ///
    /// This method will ignore fields which are ignored through the `filter`
    /// method.
    ///
    /// # Example
    /// ```
    /// # #[macro_use] extern crate quote;
    /// # extern crate synstructure;
    /// # extern crate syn;
    /// # use synstructure::*;
    /// # fn main() {
    /// let di = syn::parse_derive_input(r#"
    ///     enum A {
    ///         B(i32, i32),
    ///         C(u32),
    ///     }
    /// "#).unwrap();
    /// let s = Structure::new(&di);
    ///
    /// assert_eq!(
    ///     s.variants()[0].each(|bi| quote!(println!("{:?}", #bi))),
    ///
    ///     quote!{
    ///         A::B(ref __binding_0, ref __binding_1,) => {
    ///             { println!("{:?}", __binding_0) }
    ///             { println!("{:?}", __binding_1) }
    ///         }
    ///     }
    /// );
    /// # }
    /// ```
    pub fn each<F, R>(&self, mut f: F) -> Tokens
    where
        F: FnMut(&BindingInfo) -> R,
        R: ToTokens,
    {
        let pat = self.pat();
        let mut body = Tokens::new();
        for binding in &self.bindings {
            body.append("{");
            f(binding).to_tokens(&mut body);
            body.append("}");
        }
        quote!(#pat => { #body })
    }

    /// Runs the passed-in function once for each bound field, passing in the
    /// result of the previous call, and a `BindingInfo`. generating a `match`
    /// arm which evaluates to the resulting tokens.
    ///
    /// This method will ignore fields which are ignored through the `filter`
    /// method.
    ///
    /// # Example
    /// ```
    /// # #[macro_use] extern crate quote;
    /// # extern crate synstructure;
    /// # extern crate syn;
    /// # use synstructure::*;
    /// # fn main() {
    /// let di = syn::parse_derive_input(r#"
    ///     enum A {
    ///         B(i32, i32),
    ///         C(u32),
    ///     }
    /// "#).unwrap();
    /// let s = Structure::new(&di);
    ///
    /// assert_eq!(
    ///     s.variants()[0].fold(quote!(0), |acc, bi| quote!(#acc + #bi)),
    ///
    ///     quote!{
    ///         A::B(ref __binding_0, ref __binding_1,) => {
    ///             0 + __binding_0 + __binding_1
    ///         }
    ///     }
    /// );
    /// # }
    /// ```
    pub fn fold<F, I, R>(&self, init: I, mut f: F) -> Tokens
    where
        F: FnMut(Tokens, &BindingInfo) -> R,
        I: ToTokens,
        R: ToTokens,
    {
        let pat = self.pat();
        let body = self.bindings.iter().fold(quote!(#init), |i, bi| {
            let r = f(i, bi);
            quote!(#r)
        });
        quote!(#pat => { #body })
    }

    /// Filter the bindings created by this `Variant` object. This has 2 effects:
    ///
    /// * The bindings will no longer appear in match arms generated by methods
    ///   on this `Variant` or its subobjects.
    ///
    /// * Impl blocks created with the `bound_impl` or `unsafe_bound_impl`
    ///   method only consider type parameters referenced in the types of
    ///   non-filtered fields.
    ///
    /// # Example
    /// ```
    /// # #[macro_use] extern crate quote;
    /// # extern crate synstructure;
    /// # extern crate syn;
    /// # use synstructure::*;
    /// # fn main() {
    /// let di = syn::parse_derive_input(r#"
    ///     enum A {
    ///         B{ a: i32, b: i32 },
    ///         C{ a: u32 },
    ///     }
    /// "#).unwrap();
    /// let mut s = Structure::new(&di);
    ///
    /// s.variants_mut()[0].filter(|bi| {
    ///     bi.ast().ident == Some("b".into())
    /// });
    ///
    /// assert_eq!(
    ///     s.each(|bi| quote!(println!("{:?}", #bi))),
    ///
    ///     quote!{
    ///         A::B{ b: ref __binding_1, .. } => {
    ///             { println!("{:?}", __binding_1) }
    ///         }
    ///         A::C{ a: ref __binding_0, } => {
    ///             { println!("{:?}", __binding_0) }
    ///         }
    ///     }
    /// );
    /// # }
    /// ```
    pub fn filter<F>(&mut self, f: F) -> &mut Self
    where
        F: FnMut(&BindingInfo) -> bool,
    {
        let before_len = self.bindings.len();
        self.bindings.retain(f);
        if self.bindings.len() != before_len {
            self.omitted_fields = true;
        }
        self
    }

    /// Remove the binding at the given index.
    ///
    /// # Panics
    ///
    /// Panics if the index is out of range.
    pub fn remove_binding(&mut self, idx: usize) -> &mut Self {
        self.bindings.remove(idx);
        self.omitted_fields = true;
        self
    }

    /// Updates the `BindStyle` for each of the passed-in fields by calling the
    /// passed-in function for each `BindingInfo`.
    ///
    /// # Example
    /// ```
    /// # #[macro_use] extern crate quote;
    /// # extern crate synstructure;
    /// # extern crate syn;
    /// # use synstructure::*;
    /// # fn main() {
    /// let di = syn::parse_derive_input(r#"
    ///     enum A {
    ///         B(i32, i32),
    ///         C(u32),
    ///     }
    /// "#).unwrap();
    /// let mut s = Structure::new(&di);
    ///
    /// s.variants_mut()[0].bind_with(|bi| BindStyle::RefMut);
    ///
    /// assert_eq!(
    ///     s.each(|bi| quote!(println!("{:?}", #bi))),
    ///
    ///     quote!{
    ///         A::B(ref mut __binding_0, ref mut __binding_1,) => {
    ///             { println!("{:?}", __binding_0) }
    ///             { println!("{:?}", __binding_1) }
    ///         }
    ///         A::C(ref __binding_0,) => {
    ///             { println!("{:?}", __binding_0) }
    ///         }
    ///     }
    /// );
    /// # }
    /// ```
    pub fn bind_with<F>(&mut self, mut f: F) -> &mut Self
    where
        F: FnMut(&BindingInfo) -> BindStyle,
    {
        for binding in &mut self.bindings {
            binding.style = f(&binding);
        }
        self
    }

    /// Updates the binding name for each fo the passed-in fields by calling the
    /// passed-in function for each `BindingInfo`.
    ///
    /// The function will be called with the `BindingInfo` and its index in the
    /// enclosing variant.
    ///
    /// The default name is `__binding_{}` where `{}` is replaced with an
    /// increasing number.
    ///
    /// # Example
    /// ```
    /// # #[macro_use] extern crate quote;
    /// # extern crate synstructure;
    /// # extern crate syn;
    /// # use synstructure::*;
    /// # fn main() {
    /// let di = syn::parse_derive_input(r#"
    ///     enum A {
    ///         B{ a: i32, b: i32 },
    ///         C{ a: u32 },
    ///     }
    /// "#).unwrap();
    /// let mut s = Structure::new(&di);
    ///
    /// s.variants_mut()[0].binding_name(|bi, i| bi.ident.clone().unwrap());
    ///
    /// assert_eq!(
    ///     s.each(|bi| quote!(println!("{:?}", #bi))),
    ///
    ///     quote!{
    ///         A::B{ a: ref a, b: ref b, } => {
    ///             { println!("{:?}", a) }
    ///             { println!("{:?}", b) }
    ///         }
    ///         A::C{ a: ref __binding_0, } => {
    ///             { println!("{:?}", __binding_0) }
    ///         }
    ///     }
    /// );
    /// # }
    /// ```
    pub fn binding_name<F>(&mut self, mut f: F) -> &mut Self
    where
        F: FnMut(&Field, usize) -> Ident,
    {
        for (it, binding) in self.bindings.iter_mut().enumerate() {
            binding.binding = f(binding.field, it);
        }
        self
    }
}

/// A wrapper around a `syn` `DeriveInput` which provides utilities for creating
/// custom derive trait implementations.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Structure<'a> {
    variants: Vec<VariantInfo<'a>>,
    omitted_variants: bool,
    ast: &'a DeriveInput,
}

impl<'a> Structure<'a> {
    /// Create a new `Structure` with the variants and fields from the passed-in
    /// `DeriveInput`.
    pub fn new(ast: &'a DeriveInput) -> Self {
        let variants = match ast.body {
            Body::Enum(ref variants) => variants
                .iter()
                .map(|v| {
                    VariantInfo::new(
                        VariantAst {
                            ident: &v.ident,
                            attrs: &v.attrs,
                            data: &v.data,
                            discriminant: &v.discriminant,
                        },
                        Some(&ast.ident),
                    )
                })
                .collect::<Vec<_>>(),
            Body::Struct(ref vd) => {
                static NONE_DISCRIMINANT: Option<ConstExpr> = None;
                vec![
                    VariantInfo::new(
                        VariantAst {
                            ident: &ast.ident,
                            attrs: &ast.attrs,
                            data: &vd,
                            discriminant: &NONE_DISCRIMINANT,
                        },
                        None,
                    ),
                ]
            }
        };

        Structure {
            variants: variants,
            omitted_variants: false,
            ast: ast,
        }
    }

    /// Returns a slice of the variants in this Structure.
    pub fn variants(&self) -> &[VariantInfo<'a>] {
        &self.variants
    }

    /// Returns a mut slice of the variants in this Structure.
    pub fn variants_mut(&mut self) -> &mut [VariantInfo<'a>] {
        &mut self.variants
    }

    /// Returns a reference to the underlying `syn` AST node which this
    /// `Structure` was created from.
    pub fn ast(&self) -> &'a DeriveInput {
        self.ast
    }

    /// True if any variants were omitted due to a `filter_variants` call.
    pub fn omitted_variants(&self) -> bool {
        self.omitted_variants
    }

    /// Runs the passed-in function once for each bound field, passing in a `BindingInfo`.
    /// and generating `match` arms which evaluate the returned tokens.
    ///
    /// This method will ignore variants or fields which are ignored through the
    /// `filter` and `filter_variant` methods.
    ///
    /// # Example
    /// ```
    /// # #[macro_use] extern crate quote;
    /// # extern crate synstructure;
    /// # extern crate syn;
    /// # use synstructure::*;
    /// # fn main() {
    /// let di = syn::parse_derive_input(r#"
    ///     enum A {
    ///         B(i32, i32),
    ///         C(u32),
    ///     }
    /// "#).unwrap();
    /// let s = Structure::new(&di);
    ///
    /// assert_eq!(
    ///     s.each(|bi| quote!(println!("{:?}", #bi))),
    ///
    ///     quote!{
    ///         A::B(ref __binding_0, ref __binding_1,) => {
    ///             { println!("{:?}", __binding_0) }
    ///             { println!("{:?}", __binding_1) }
    ///         }
    ///         A::C(ref __binding_0,) => {
    ///             { println!("{:?}", __binding_0) }
    ///         }
    ///     }
    /// );
    /// # }
    /// ```
    pub fn each<F, R>(&self, mut f: F) -> Tokens
    where
        F: FnMut(&BindingInfo) -> R,
        R: ToTokens,
    {
        let mut t = Tokens::new();
        for variant in &self.variants {
            variant.each(&mut f).to_tokens(&mut t);
        }
        if self.omitted_variants {
            quote!(_ => {}).to_tokens(&mut t);
        }
        t
    }

    /// Runs the passed-in function once for each bound field, passing in the
    /// result of the previous call, and a `BindingInfo`. generating `match`
    /// arms which evaluate to the resulting tokens.
    ///
    /// This method will ignore variants or fields which are ignored through the
    /// `filter` and `filter_variant` methods.
    ///
    /// If a variant has been ignored, it will return the `init` value.
    ///
    /// # Example
    /// ```
    /// # #[macro_use] extern crate quote;
    /// # extern crate synstructure;
    /// # extern crate syn;
    /// # use synstructure::*;
    /// # fn main() {
    /// let di = syn::parse_derive_input(r#"
    ///     enum A {
    ///         B(i32, i32),
    ///         C(u32),
    ///     }
    /// "#).unwrap();
    /// let s = Structure::new(&di);
    ///
    /// assert_eq!(
    ///     s.fold(quote!(0), |acc, bi| quote!(#acc + #bi)),
    ///
    ///     quote!{
    ///         A::B(ref __binding_0, ref __binding_1,) => {
    ///             0 + __binding_0 + __binding_1
    ///         }
    ///         A::C(ref __binding_0,) => {
    ///             0 + __binding_0
    ///         }
    ///     }
    /// );
    /// # }
    /// ```
    pub fn fold<F, I, R>(&self, init: I, mut f: F) -> Tokens
    where
        F: FnMut(Tokens, &BindingInfo) -> R,
        I: ToTokens,
        R: ToTokens,
    {
        let mut t = Tokens::new();
        for variant in &self.variants {
            variant.fold(&init, &mut f).to_tokens(&mut t);
        }
        if self.omitted_variants {
            quote!(_ => { #init }).to_tokens(&mut t);
        }
        t
    }

    /// Runs the passed-in function once for each variant, passing in a
    /// `VariantInfo`. and generating `match` arms which evaluate the returned
    /// tokens.
    ///
    /// This method will ignore variants and not bind fields which are ignored
    /// through the `filter` and `filter_variant` methods.
    ///
    /// # Example
    /// ```
    /// # #[macro_use] extern crate quote;
    /// # extern crate synstructure;
    /// # extern crate syn;
    /// # use synstructure::*;
    /// # fn main() {
    /// let di = syn::parse_derive_input(r#"
    ///     enum A {
    ///         B(i32, i32),
    ///         C(u32),
    ///     }
    /// "#).unwrap();
    /// let s = Structure::new(&di);
    ///
    /// assert_eq!(
    ///     s.each_variant(|v| {
    ///         let name = &v.ast().ident;
    ///         quote!(println!(stringify!(#name)))
    ///     }),
    ///
    ///     quote!{
    ///         A::B(ref __binding_0, ref __binding_1,) => {
    ///             println!(stringify!(B))
    ///         }
    ///         A::C(ref __binding_0,) => {
    ///             println!(stringify!(C))
    ///         }
    ///     }
    /// );
    /// # }
    /// ```
    pub fn each_variant<F, R>(&self, mut f: F) -> Tokens
    where
        F: FnMut(&VariantInfo) -> R,
        R: ToTokens,
    {
        let mut t = Tokens::new();
        for variant in &self.variants {
            let pat = variant.pat();
            let body = f(variant);
            quote!(#pat => { #body }).to_tokens(&mut t);
        }
        if self.omitted_variants {
            quote!(_ => {}).to_tokens(&mut t);
        }
        t
    }

    /// Filter the bindings created by this `Structure` object. This has 2 effects:
    ///
    /// * The bindings will no longer appear in match arms generated by methods
    ///   on this `Structure` or its subobjects.
    ///
    /// * Impl blocks created with the `bound_impl` or `unsafe_bound_impl`
    ///   method only consider type parameters referenced in the types of
    ///   non-filtered fields.
    ///
    /// # Example
    /// ```
    /// # #[macro_use] extern crate quote;
    /// # extern crate synstructure;
    /// # extern crate syn;
    /// # use synstructure::*;
    /// # fn main() {
    /// let di = syn::parse_derive_input(r#"
    ///     enum A {
    ///         B{ a: i32, b: i32 },
    ///         C{ a: u32 },
    ///     }
    /// "#).unwrap();
    /// let mut s = Structure::new(&di);
    ///
    /// s.filter(|bi| { bi.ast().ident == Some("a".into()) });
    ///
    /// assert_eq!(
    ///     s.each(|bi| quote!(println!("{:?}", #bi))),
    ///
    ///     quote!{
    ///         A::B{ a: ref __binding_0, .. } => {
    ///             { println!("{:?}", __binding_0) }
    ///         }
    ///         A::C{ a: ref __binding_0, } => {
    ///             { println!("{:?}", __binding_0) }
    ///         }
    ///     }
    /// );
    /// # }
    /// ```
    pub fn filter<F>(&mut self, mut f: F) -> &mut Self
    where
        F: FnMut(&BindingInfo) -> bool,
    {
        for variant in &mut self.variants {
            variant.filter(&mut f);
        }
        self
    }

    /// Filter the variants matched by this `Structure` object. This has 2 effects:
    ///
    /// * Match arms destructuring these variants will no longer be generated by
    ///   methods on this `Structure`
    ///
    /// * Impl blocks created with the `bound_impl` or `unsafe_bound_impl`
    ///   method only consider type parameters referenced in the types of
    ///   fields in non-fitered variants.
    ///
    /// # Example
    /// ```
    /// # #[macro_use] extern crate quote;
    /// # extern crate synstructure;
    /// # extern crate syn;
    /// # use synstructure::*;
    /// # fn main() {
    /// let di = syn::parse_derive_input(r#"
    ///     enum A {
    ///         B(i32, i32),
    ///         C(u32),
    ///     }
    /// "#).unwrap();
    /// let mut s = Structure::new(&di);
    ///
    /// s.filter_variants(|v| v.ast().ident != "B");
    ///
    /// assert_eq!(
    ///     s.each(|bi| quote!(println!("{:?}", #bi))),
    ///
    ///     quote!{
    ///         A::C(ref __binding_0,) => {
    ///             { println!("{:?}", __binding_0) }
    ///         }
    ///         _ => {}
    ///     }
    /// );
    /// # }
    /// ```
    pub fn filter_variants<F>(&mut self, f: F) -> &mut Self
    where
        F: FnMut(&VariantInfo) -> bool,
    {
        let before_len = self.variants.len();
        self.variants.retain(f);
        if self.variants.len() != before_len {
            self.omitted_variants = true;
        }
        self
    }

    /// Remove the variant at the given index.
    ///
    /// # Panics
    ///
    /// Panics if the index is out of range.
    pub fn remove_variant(&mut self, idx: usize) -> &mut Self {
        self.variants.remove(idx);
        self.omitted_variants = true;
        self
    }

    /// Updates the `BindStyle` for each of the passed-in fields by calling the
    /// passed-in function for each `BindingInfo`.
    ///
    /// # Example
    /// ```
    /// # #[macro_use] extern crate quote;
    /// # extern crate synstructure;
    /// # extern crate syn;
    /// # use synstructure::*;
    /// # fn main() {
    /// let di = syn::parse_derive_input(r#"
    ///     enum A {
    ///         B(i32, i32),
    ///         C(u32),
    ///     }
    /// "#).unwrap();
    /// let mut s = Structure::new(&di);
    ///
    /// s.bind_with(|bi| BindStyle::RefMut);
    ///
    /// assert_eq!(
    ///     s.each(|bi| quote!(println!("{:?}", #bi))),
    ///
    ///     quote!{
    ///         A::B(ref mut __binding_0, ref mut __binding_1,) => {
    ///             { println!("{:?}", __binding_0) }
    ///             { println!("{:?}", __binding_1) }
    ///         }
    ///         A::C(ref mut __binding_0,) => {
    ///             { println!("{:?}", __binding_0) }
    ///         }
    ///     }
    /// );
    /// # }
    /// ```
    pub fn bind_with<F>(&mut self, mut f: F) -> &mut Self
    where
        F: FnMut(&BindingInfo) -> BindStyle,
    {
        for variant in &mut self.variants {
            variant.bind_with(&mut f);
        }
        self
    }

    /// Updates the binding name for each fo the passed-in fields by calling the
    /// passed-in function for each `BindingInfo`.
    ///
    /// The function will be called with the `BindingInfo` and its index in the
    /// enclosing variant.
    ///
    /// The default name is `__binding_{}` where `{}` is replaced with an
    /// increasing number.
    ///
    /// # Example
    /// ```
    /// # #[macro_use] extern crate quote;
    /// # extern crate synstructure;
    /// # extern crate syn;
    /// # use synstructure::*;
    /// # fn main() {
    /// let di = syn::parse_derive_input(r#"
    ///     enum A {
    ///         B{ a: i32, b: i32 },
    ///         C{ a: u32 },
    ///     }
    /// "#).unwrap();
    /// let mut s = Structure::new(&di);
    ///
    /// s.binding_name(|bi, i| bi.ident.clone().unwrap());
    ///
    /// assert_eq!(
    ///     s.each(|bi| quote!(println!("{:?}", #bi))),
    ///
    ///     quote!{
    ///         A::B{ a: ref a, b: ref b, } => {
    ///             { println!("{:?}", a) }
    ///             { println!("{:?}", b) }
    ///         }
    ///         A::C{ a: ref a, } => {
    ///             { println!("{:?}", a) }
    ///         }
    ///     }
    /// );
    /// # }
    /// ```
    pub fn binding_name<F>(&mut self, mut f: F) -> &mut Self
    where
        F: FnMut(&Field, usize) -> Ident,
    {
        for variant in &mut self.variants {
            variant.binding_name(&mut f);
        }
        self
    }

    /// Returns a list of the type parameters which are refrenced in the types
    /// of non-filtered fields / variants.
    ///
    /// # Caveat
    ///
    /// If the method contains any macros in type position, all parameters will
    /// be considered bound. This is because we cannot determine which type
    /// parameters are bound by type macros.
    ///
    /// # Example
    /// ```
    /// # #[macro_use] extern crate quote;
    /// # extern crate synstructure;
    /// # extern crate syn;
    /// # use synstructure::*;
    /// # fn main() {
    /// let di = syn::parse_derive_input(r#"
    ///     enum A<T, U> {
    ///         B(T, i32),
    ///         C(Option<U>),
    ///     }
    /// "#).unwrap();
    /// let mut s = Structure::new(&di);
    ///
    /// s.filter_variants(|v| v.ast().ident != "C");
    ///
    /// assert_eq!(
    ///     s.referenced_ty_params(),
    ///     &[&(syn::Ident::from("T"))]
    /// );
    /// # }
    /// ```
    pub fn referenced_ty_params(&self) -> Vec<&'a Ident> {
        // Helper type. Discovers all identifiers inside of the visited type,
        // and calls a callback with them.
        struct BoundTypeLocator<'a> {
            result: Vec<&'a Ident>,
            remaining: Vec<&'a Ident>,
        }

        impl<'a> Visitor for BoundTypeLocator<'a> {
            fn visit_ident(&mut self, id: &Ident) {
                // NOTE: We need to borrow result here explicitly otherwise the
                // lambda captures all of `self` mutably.
                let result = &mut self.result;
                self.remaining.retain(|&i| {
                    if i == id {
                        // NOTE: Push `i` rather than `id` otherwise
                        // lifetimes don't work out.
                        result.push(i);
                        false
                    } else {
                        true
                    }
                });
            }

            fn visit_ty(&mut self, ty: &Ty) {
                // If we see a type macro, we can't know what type parameters it
                // might be binding, so we presume that it binds all of them.
                if let Ty::Mac(_) = *ty {
                    self.result.extend(self.remaining.drain(..));
                }
                visit::walk_ty(self, ty);
            }
        }

        let mut btl = BoundTypeLocator {
            result: Vec::new(),
            remaining: self.ast
                .generics
                .ty_params
                .iter()
                .map(|p| &p.ident)
                .collect(),
        };

        for variant in &self.variants {
            for binding in &variant.bindings {
                btl.visit_ty(&binding.field.ty);
            }
        }

        btl.result
    }

    /// Add trait bounds for a trait with the given path for each type parmaeter
    /// referenced in the types of non-filtered fields.
    ///
    /// # Caveat
    ///
    /// If the method contains any macros in type position, all parameters will
    /// be considered bound. This is because we cannot determine which type
    /// parameters are bound by type macros.
    pub fn add_trait_bounds(&self, bound: &TyParamBound, preds: &mut Vec<WherePredicate>) {
        for param in self.referenced_ty_params() {
            preds.push(WherePredicate::BoundPredicate(WhereBoundPredicate {
                bound_lifetimes: vec![],
                bounded_ty: Ty::Path(None, param.clone().into()),
                bounds: vec![bound.clone()],
            }));
        }
    }

    /// Creates an `impl` block with the required generic type fields filled in
    /// to implement the trait `path`.
    ///
    /// This method also adds where clauses to the impl requiring that all
    /// referenced type parmaeters implement the trait `path`.
    ///
    /// # Caveat
    ///
    /// If the method contains any macros in type position, all parameters will
    /// be considered bound. This is because we cannot determine which type
    /// parameters are bound by type macros.
    ///
    /// # Panics
    ///
    /// Panics if the path string parameter is not a valid TyParamBound.
    ///
    /// # Example
    /// ```
    /// # #[macro_use] extern crate quote;
    /// # extern crate synstructure;
    /// # extern crate syn;
    /// # use synstructure::*;
    /// # fn main() {
    /// let di = syn::parse_derive_input(r#"
    ///     enum A<T, U> {
    ///         B(T),
    ///         C(Option<U>),
    ///     }
    /// "#).unwrap();
    /// let mut s = Structure::new(&di);
    ///
    /// s.filter_variants(|v| v.ast().ident != "B");
    ///
    /// assert_eq!(
    ///     s.bound_impl("::krate::Trait", quote!{
    ///         fn a() {}
    ///     }),
    ///     quote!{
    ///         impl<T, U> ::krate::Trait for A<T, U>
    ///             where U: ::krate::Trait
    ///         {
    ///             fn a() {}
    ///         }
    ///     }
    /// );
    /// # }
    /// ```
    pub fn bound_impl<P: AsRef<str>, B: ToTokens>(&self, bound: P, body: B) -> Tokens {
        let name = &self.ast.ident;
        let (impl_generics, ty_generics, where_clause) = self.ast.generics.split_for_impl();

        let bound =
            syn::parse_ty_param_bound(bound.as_ref())
            .expect("`bound` argument must be a valid rust trait bound");

        let mut where_clause = where_clause.clone();
        self.add_trait_bounds(&bound, &mut where_clause.predicates);

        quote! {
            impl #impl_generics #bound for #name #ty_generics #where_clause {
                #body
            }
        }
    }

    /// This method is the same as `bound_impl`, except also includes the
    /// `unsafe` keyword for implementing unsafe traits.
    pub fn unsafe_bound_impl<P: AsRef<str>, B: ToTokens>(&self, path: P, body: B) -> Tokens {
        let safe = self.bound_impl(path, body);
        quote!(unsafe #safe)
    }

    /// This method is like `bound_impl` but doesn't add the additional bounds
    /// to the where clause.
    pub fn unbound_impl<P: AsRef<str>, B: ToTokens>(&self, bound: P, body: B) -> Tokens {
        let name = &self.ast.ident;
        let (impl_generics, ty_generics, where_clause) = self.ast.generics.split_for_impl();

        let bound =
            syn::parse_ty_param_bound(bound.as_ref())
            .expect("`bound` argument must be a valid rust trait bound");

        quote! {
            impl #impl_generics #bound for #name #ty_generics #where_clause {
                #body
            }
        }
    }

    /// This method is the same as `unbound_impl`, except also includes the
    /// `unsafe` keyword for implementing unsafe traits.
    pub fn unsafe_unbound_impl<P: AsRef<str>, B: ToTokens>(&self, path: P, body: B) -> Tokens {
        let safe = self.unbound_impl(path, body);
        quote!(unsafe #safe)
    }
}
