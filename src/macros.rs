//! This module provides two utility macros for testing custom derives. They can
//! be used together to eliminate some of the boilerplate required in order to
//! declare and test custom derive implementations.

// Re-exports used by the decl_derive! and test_derive!
pub use proc_macro2::TokenStream as TokenStream2;
pub use quote::quote;
pub use syn::{parse2, parse_str, DeriveInput};

use dissimilar::Chunk;

#[cfg(all(
    not(all(target_arch = "wasm32", any(target_os = "unknown", target_os = "wasi"))),
    feature = "proc-macro"
))]
pub use proc_macro::TokenStream;
#[cfg(all(
    not(all(target_arch = "wasm32", any(target_os = "unknown", target_os = "wasi"))),
    feature = "proc-macro"
))]
pub use syn::parse;

/// The `decl_derive!` macro declares a custom derive wrapper. It will parse the
/// incoming `TokenStream` into a `synstructure::Structure` object, and pass it
/// into the inner function.
///
/// Your inner function should take a `synstructure::Structure` by value, and
/// return a type implementing `synstructure::MacroResult`, for example:
///
/// ```
/// fn derive_simple(input: synstructure::Structure) -> proc_macro2::TokenStream {
///     unimplemented!()
/// }
///
/// fn derive_result(input: synstructure::Structure)
///     -> syn::Result<proc_macro2::TokenStream>
/// {
///     unimplemented!()
/// }
/// ```
///
/// # Usage
///
/// ### Without Attributes
/// ```
/// fn derive_interesting(_input: synstructure::Structure) -> proc_macro2::TokenStream {
///     quote::quote! { ... }
/// }
///
/// # const _IGNORE: &'static str = stringify! {
/// decl_derive!([Interesting] => derive_interesting);
/// # };
/// ```
///
/// ### With Attributes
/// ```
/// # fn main() {}
/// fn derive_interesting(_input: synstructure::Structure) -> proc_macro2::TokenStream {
///     quote::quote! { ... }
/// }
///
/// # const _IGNORE: &'static str = stringify! {
/// decl_derive!([Interesting, attributes(interesting_ignore)] => derive_interesting);
/// # };
/// ```
///
/// ### Decl Attributes & Doc Comments
/// ```
/// # fn main() {}
/// fn derive_interesting(_input: synstructure::Structure) -> proc_macro2::TokenStream {
///     quote::quote! { ... }
/// }
///
/// # const _IGNORE: &'static str = stringify! {
/// decl_derive! {
///     [Interesting] =>
///     #[allow(some_lint)]
///     /// Documentation Comments
///     derive_interesting
/// }
/// # };
/// ```
///
/// *This macro is available if `synstructure` is built with the `"proc-macro"`
/// feature.*
#[cfg(all(
    not(all(target_arch = "wasm32", any(target_os = "unknown", target_os = "wasi"))),
    feature = "proc-macro"
))]
#[macro_export]
macro_rules! decl_derive {
    // XXX: Switch to using this variant everywhere?
    ([$derives:ident $($derive_t:tt)*] => $(#[$($attrs:tt)*])* $inner:path) => {
        #[proc_macro_derive($derives $($derive_t)*)]
        #[allow(non_snake_case)]
        $(#[$($attrs)*])*
        pub fn $derives(
            i: $crate::macros::TokenStream
        ) -> $crate::macros::TokenStream {
            match $crate::macros::parse::<$crate::macros::DeriveInput>(i) {
                ::core::result::Result::Ok(p) => {
                    match $crate::Structure::try_new(&p) {
                        ::core::result::Result::Ok(s) => $crate::MacroResult::into_stream($inner(s)),
                        ::core::result::Result::Err(e) => {
                            ::core::convert::Into::into(e.to_compile_error())
                        }
                    }
                }
                ::core::result::Result::Err(e) => {
                    ::core::convert::Into::into(e.to_compile_error())
                }
            }
        }
    };
}

/// The `decl_attribute!` macro declares a custom attribute wrapper. It will
/// parse the incoming `TokenStream` into a `synstructure::Structure` object,
/// and pass it into the inner function.
///
/// Your inner function should have the following type:
///
/// ```
/// fn attribute(
///     attr: proc_macro2::TokenStream,
///     structure: synstructure::Structure,
/// ) -> proc_macro2::TokenStream {
///     unimplemented!()
/// }
/// ```
///
/// # Usage
///
/// ```
/// fn attribute_interesting(
///     _attr: proc_macro2::TokenStream,
///     _structure: synstructure::Structure,
/// ) -> proc_macro2::TokenStream {
///     quote::quote! { ... }
/// }
///
/// # const _IGNORE: &'static str = stringify! {
/// decl_attribute!([interesting] => attribute_interesting);
/// # };
/// ```
///
/// *This macro is available if `synstructure` is built with the `"proc-macro"`
/// feature.*
#[cfg(all(
    not(all(target_arch = "wasm32", any(target_os = "unknown", target_os = "wasi"))),
    feature = "proc-macro"
))]
#[macro_export]
macro_rules! decl_attribute {
    ([$attribute:ident] => $(#[$($attrs:tt)*])* $inner:path) => {
        #[proc_macro_attribute]
        $(#[$($attrs)*])*
        pub fn $attribute(
            attr: $crate::macros::TokenStream,
            i: $crate::macros::TokenStream,
        ) -> $crate::macros::TokenStream {
            match $crate::macros::parse::<$crate::macros::DeriveInput>(i) {
                ::core::result::Result::Ok(p) => match $crate::Structure::try_new(&p) {
                    ::core::result::Result::Ok(s) => {
                        $crate::MacroResult::into_stream(
                            $inner(::core::convert::Into::into(attr), s)
                        )
                    }
                    ::core::result::Result::Err(e) => {
                        ::core::convert::Into::into(e.to_compile_error())
                    }
                },
                ::core::result::Result::Err(e) => {
                    ::core::convert::Into::into(e.to_compile_error())
                }
            }
        }
    };
}

/// Run a test on a custom derive. This macro expands both the original struct
/// and the expansion to ensure that they compile correctly, and confirms that
/// feeding the original struct into the named derive will produce the written
/// output.
///
/// You can add `no_build` to the end of the macro invocation to disable
/// checking that the written code compiles. This is useful in contexts where
/// the procedural macro cannot depend on the crate where it is used during
/// tests.
///
/// # Usage
///
/// ```
/// fn test_derive_example(_s: synstructure::Structure)
///     -> Result<proc_macro2::TokenStream, syn::Error>
/// {
///     Ok(quote::quote! { const YOUR_OUTPUT: &'static str = "here"; })
/// }
///
/// fn main() {
///     synstructure::test_derive!{
///         test_derive_example {
///             struct A;
///         }
///         expands to {
///             const YOUR_OUTPUT: &'static str = "here";
///         }
///     }
/// }
/// ```
#[macro_export]
macro_rules! test_derive {
    ($name:path { $($i:tt)* } expands to { $($o:tt)* }) => {
        {
            #[allow(dead_code)]
            fn ensure_compiles() {
                $($i)*
                $($o)*
            }

            $crate::test_derive!($name { $($i)* } expands to { $($o)* } no_build);
        }
    };

    ($name:path { $($i:tt)* } expands to { $($o:tt)* } no_build) => {
        {
            let i = $crate::macros::quote!( $($i)* );
            let parsed = $crate::macros::parse2::<$crate::macros::DeriveInput>(i)
                .expect(::core::concat!(
                    "Failed to parse input to `#[derive(",
                    ::core::stringify!($name),
                    ")]`",
                ));

            let raw_res = $name($crate::Structure::new(&parsed));
            let res = $crate::MacroResult::into_result(raw_res)
                .expect(::core::concat!(
                    "Procedural macro failed for `#[derive(",
                    ::core::stringify!($name),
                    ")]`",
                ));

            let expected_toks = $crate::macros::quote!( $($o)* );
            $crate::macros::assert_eq_streams(expected_toks, res);
        }
    };
}

#[doc(hidden)]
pub fn assert_eq_streams(expect: TokenStream2, res: TokenStream2) {
    fn token_stream_eq(a: TokenStream2, b: TokenStream2) -> bool {
        if a.clone().into_iter().count() != b.clone().into_iter().count() {
            return false;
        }

        a.into_iter()
            .zip(b.into_iter())
            .all(|(a, b)| token_tree_eq(&a, &b))
    }

    fn token_tree_eq(a: &proc_macro2::TokenTree, b: &proc_macro2::TokenTree) -> bool {
        use proc_macro2::TokenTree::*;
        match (a, b) {
            (Group(a), Group(b)) => {
                a.delimiter() == b.delimiter() && token_stream_eq(a.stream(), b.stream())
            }
            (Ident(a), Ident(b)) => format!("{}", a) == format!("{}", b),
            (Punct(a), Punct(b)) => a.as_char() == b.as_char() && a.spacing() == b.spacing(),
            (Literal(a), Literal(b)) => format!("{}", a) == format!("{}", b),
            _ => false,
        }
    }

    if !token_stream_eq(expect.clone(), res.clone()) {
        // NOTE: It may be tempting to want to pretty-print these token streams
        // (or at least the output one) so that the diff is more readable. See
        // below for verbatim code that I (github.com/joshlf) tried.
        // Unfortunately, pretty printing removes some token sequences such as
        // empty type parameter lists (`<>`), empty where clauses (`where` on
        // its own with no clauses), etc. This has the effect of making the
        // diffs wrong since this function performs token-wise comparison, which
        // cares about these token sequences.
        //
        //   let mut expect = expect.to_string();
        //   let mut res = res.to_string();
        //   if let (Ok(e), Ok(r)) = (syn::parse_file(&expect), syn::parse_file(&res)) {
        //       // If we can parse successfully, then pretty-print them, but
        //       // otherwise just use them unmodified. It will still be correct,
        //       // just uglier.
        //       expect = prettyplease::unparse(&e);
        //       res = prettyplease::unparse(&r);
        //   }

        let diff = dissimilar::diff(&expect.to_string(), &res.to_string())
            .into_iter()
            .map(|chunk| {
                let (prefix, chunk) = match chunk {
                    Chunk::Equal(chunk) => (" ", chunk),
                    Chunk::Delete(chunk) => ("-", chunk),
                    Chunk::Insert(chunk) => ("+", chunk),
                };
                [prefix, chunk, "\n"]
            })
            .flatten()
            .collect::<String>();

        panic!(
            "\
test_derive failed:
diff (expected vs got):
```
{}
```\n",
            diff
        );
    }
}
