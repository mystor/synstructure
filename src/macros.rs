//! This module provides two utility macros for testing custom derives. They can
//! be used together to eliminate some of the boilerplate required in order to
//! declare and test custom derive implementations.

// XXX: Stop using `ignore` blocks for examples in this module.

// Re-exports used by the decl_derive! and test_derive!
pub mod syn {
    pub use syn::*; // For syn::parse_derive_input
}

pub mod quote {
    pub use quote::*; // For quote::Tokens
}

/// The `decl_derive!` macro declares a custom derive using synstructure's `Syn`
/// argument. This macro has the internal implementation factored into a
/// separate function such that it can be tested by the `test_derive!` method
/// also provided by this crate.
///
/// # Usage
///
/// ```ignore
/// decl_derive!([Interesting, attributes(interesting_ignore)] derive_interesting(mut structure) {
///     ...body
/// })
/// ```
///
/// This will generate something like:
///
/// ```ignore
/// // The implementation module. This module contains re-exports of some useful
/// // imported crates, as well as the core implementation function. This
/// // function will is separate in this method so that it can be called by
/// // tests generated with the `test_derive!` macro.
/// mod derive_interesting {
///     // The actual implementation of the custom derive. Your code will be
///     // placed in the body of this function.
///     pub fn imp(mut structure: ::synstructure::Structure) -> ::quote::Tokens {
///         ..body
///     }
/// }
///
/// // The publicly exposed procedural macro declaration. Parses the input
/// // tokenstream, a nd parses the tokens returned from `imp` into a TokenStream.
/// #[proc_macro_derive(Interesting, attributes(interesting_ignore))]
/// pub fn derive_interesting(i: ::proc_macro::TokenStream) -> ::proc_macro::TokenStream {
///     let parsed = ::syn::parse_derive_input(&i.to_string())
///         .expect("Failed to parse input to `#[derive(Interesting)]`");
///     derive_interesting::imp(::synstructure::Structure::new(&parsed)).parse()
///         .expect("Failed to parse output from `#[derive(Interesting)]`")
/// }
/// ```
#[macro_export]
macro_rules! decl_derive {
    ([$derives:ident $($derive_t:tt)*] $name:ident($($in:ident)*) $body:expr) => {
        // XXX: Implementation module - contains the implementation of the
        // derive in question. This is used by the testing macros to allow
        // invoking the custom derive without creating TokenStreams.
        mod $name {
            pub extern crate proc_macro;
            pub fn imp($($in)*: $crate::Structure) -> $crate::macros::quote::Tokens {
                $body
            }
        }

        // Wrapper function, parses input and output to custom derive to a
        // useful form.
        #[proc_macro_derive($derives $($derive_t)*)]
        pub fn $name(i: $name::proc_macro::TokenStream) -> $name::proc_macro::TokenStream {
            let parsed = $crate::macros::syn::parse_derive_input(&i.to_string())
                .expect(concat!("Failed to parse input to `#[derive(",
                                stringify!($derives),
                                ")]`"));
            $name::imp($crate::Structure::new(&parsed)).parse()
                .expect(concat!("Failed to parse output from `#[derive(",
                                stringify!($derives),
                                ")]`"))
        }
    }
}

/// Run a test on a custom derive. This macro expands both the original struct
/// and the expansion to ensure that they compile correctly, and confirms that
/// feeding the original struct into the named derive will produce the written
/// output.
///
/// # Usage
///
/// ```ignore
/// test_derive!{
///     super::derive_interesting {
///         struct A {
///             a: i32,
///             b: i32,
///         }
///     }
///     expands to {
///         impl ::Interesting for A {
///             fn is_interesting(&self) -> bool {
///                 match *self {
///                     A { a: ref __binding_0, b: ref __binding_1, } => {
///                         false ||
///                             ::Interesting::is_interesting(__binding_0) ||
///                             ::Interesting::is_interesting(__binding_1)
///                     }
///                 }
///             }
///         }
///     }
/// }
/// ```
#[macro_export]
macro_rules! test_derive {
    ($name:path { $($i:tt)* } expands to { $($o:tt)* }) => {
        {
            use $name as imp_mod;

            #[allow(dead_code)]
            fn ensure_compiles() {
                $($i)*
                $($o)*
            }

            let i = stringify!{ $($i)* };
            let parsed = $crate::macros::syn::parse_derive_input(i)
                .expect(concat!("Failed to parse input to `#[derive(",
                                stringify!($derives),
                                ")]`"));

            let o = stringify!( $($o)* ).replace("\n", " ");
            assert_eq!(
                imp_mod::imp($crate::Structure::new(&parsed)).as_str(),
                o
            )
        }
    }
}
