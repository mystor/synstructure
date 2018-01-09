//! This module provides two utility macros for testing custom derives. They can
//! be used together to eliminate some of the boilerplate required in order to
//! declare and test custom derive implementations.

// XXX: Stop using `ignore` blocks for examples in this module.

// Re-exports used by the decl_derive! and test_derive!
pub mod syn {
    pub use syn::*; // For syn::parse_str and syn::parse
}

pub mod quote {
    pub use quote::*; // For quote::Tokens
}

pub mod proc_macro {
    pub use proc_macro::*; // For proc_macro::TokenStream
}

pub mod proc_macro2 {
    pub use proc_macro2::*; // For proc_macro2::TokenStream
}

/// The `decl_derive!` macro declares a custom derive wrapper. It will parse the
/// incoming TokenStream into a `synstructure::Structure` object, and pass it
/// into the inner function.
///
/// Your inner function should have the following type:
///
/// ```
/// # extern crate quote;
/// # extern crate synstructure;
/// fn derive(input: synstructure::Structure) -> quote::Tokens {
///     // Your body here
/// # unimplemented!()
/// }
/// ```
///
/// # Usage
///
/// ```ignore
/// fn derive_interesting(mut input: synstructure::Structure) -> quote::Tokens {
///     // Your body here
/// }
/// decl_derive!([Interesting, attributes(interesting_ignore)] => derive_interesting);
/// ```
#[macro_export]
macro_rules! decl_derive {
    // XXX: Switch to using this variant everywhere?
    ([$derives:ident $($derive_t:tt)*] => $inner:path) => {
        #[proc_macro_derive($derives $($derive_t)*)]
        #[allow(non_snake_case)]
        pub fn $derives(
            i: $crate::macros::proc_macro::TokenStream
        ) -> $crate::macros::proc_macro::TokenStream
        {
            let parsed = $crate::macros::syn::parse::<$crate::macros::syn::DeriveInput>(i)
                .expect(concat!("Failed to parse input to `#[derive(",
                                stringify!($derives),
                                ")]`"));
            $inner($crate::Structure::new(&parsed)).into()
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
            #[allow(dead_code)]
            fn ensure_compiles() {
                $($i)*
                $($o)*
            }

            test_derive!($name { $($i)* } expands to { $($o)* } no_build);
        }
    };

    ($name:path { $($i:tt)* } expands to { $($o:tt)* } no_build) => {
        {
            let i = stringify!( $($i)* );
            let parsed = $crate::macros::syn::parse_str::<$crate::macros::syn::DeriveInput>(i)
                .expect(concat!("Failed to parse input to `#[derive(",
                                stringify!($name),
                                ")]`"));

            let res = $name($crate::Structure::new(&parsed));
            let expected = stringify!( $($o)* )
                .parse::<$crate::macros::proc_macro2::TokenStream>()
                .expect("output should be a valid TokenStream");
            let mut expected_toks = $crate::macros::quote::Tokens::new();
            expected_toks.append_all(expected);

            assert_eq!(res, expected_toks)
        }
    };
}

/// A helper macro for declaring relatively straightforward derive
/// implementations. It provides mechanisms for operating over structures
/// performing modifications on each field etc.
///
/// This macro is a helper wrapper over decl_derive!.
///
/// # Warning
///
/// This is an unstable experimental macro API, which may be changed or removed
/// in a future version. I'm not yet confident enough that this API is useful
/// enough to warrant its complexity and inclusion in synstructure.
///
/// # Note
///
/// This feature is implemented behind the `simple-derive` feature, and is only
/// avaliable when that feature is enabled.
///
/// # Example
/// ```ignore
/// simple_derive! {
///     // This macro implements the `Interesting` method exported by the `aa`
///     // crate. It will explicitly add an `extern crate` invocation to import the
///     // crate into the expanded context.
///     derive(Interesting) impl ::aa::Interesting {
///         // A "filter" block can be added. It evaluates its body with the (s)
///         // variable bound to a mutable reference to the input `Structure`
///         // object.
///         //
///         // This block can be used to perform general transformations, such as
///         // filtering out fields which should be ignored by all methods and for
///         // the purposes of binding type parameters.
///         filter(s) {
///             s.filter(|bi| bi.ast().ident != Some("a".into()));
///         }
///
///         // This is an implementation of a method in the implemented crate. The
///         // return value should be the series of match patterns to destructure
///         // the `self` argument with.
///         fn is_interesting(&self as s) -> bool {
///             s.fold(false, |acc, bi| {
///                 quote!(#acc || ::aa::Interesting::is_interesting(#bi))
///             })
///         }
///     }
/// }
/// ```
#[cfg(feature = "simple-derive")]
#[macro_export]
macro_rules! simple_derive {
    // entry point
    (
        derive($name:ident) $iname:ident impl $path:path { $($rest:tt)* }
    ) => {
        simple_derive!(@I [$name, $iname, $path] { $($rest)* } [] []);
    };

    // Adding a filter block
    (
        @I $opt:tt {
            filter($s:ident) {
                $($body:tt)*
            }
            $($rest:tt)*
        } [$($done:tt)*] [$($filter:tt)*]
    ) => {
        simple_derive!(
            @I $opt { $($rest)* } [$($done)*] [
                $($filter)*
                [
                    st_name = $s,
                    body = {
                        $($body)*
                    },
                ]
            ]
        );
    };

    // &self bound method
    (
        @I $opt:tt {
            fn $fn_name:ident (&self as $s:ident $($params:tt)*) $(-> $t:ty)* {
                $($body:tt)*
            }
            $($rest:tt)*
        } [$($done:tt)*] [$($filter:tt)*]
    ) => {
        simple_derive!(
            @I $opt { $($rest)* } [
                $($done)*
                [
                    st_name = $s,
                    bind_style = Ref,
                    body = { $($body)* },
                    result = result,
                    expanded = {
                        fn $fn_name(&self $($params)*) $(-> $t)* {
                            match *self { #result }
                        }
                    },
                ]
            ] [$($filter)*]
        );
    };

    // &mut self bound method
    (
        @I $opt:tt {
            fn $fn_name:ident (&mut self as $s:ident $($params:tt)*) $(-> $t:ty)* {
                $($body:tt)*
            }
            $($rest:tt)*
        } [$($done:tt)*] [$($filter:tt)*]
    ) => {
        simple_derive!(
            @I $opt { $($rest)* } [
                $($done)*
                [
                    st_name = $s,
                    bind_style = RefMut,
                    body = { $($body)* },
                    result = result,
                    expanded = {
                        fn $fn_name(&mut self $($params)*) $(-> $t)* {
                            match *self { #result }
                        }
                    },
                ]
            ] [$($filter)*]
        );
    };

    // self bound method
    (
        @I $opt:tt {
            fn $fn_name:ident (self as $s:ident $($params:tt)*) $(-> $t:ty)* {
                $($body:tt)*
            }
            $($rest:tt)*
        } [$($done:tt)*] [$($filter:tt)*]
    ) => {
        simple_derive!(
            @I $opt { $($rest)* } [
                $($done)*
                [
                    st_name = $s,
                    bind_style = Move,
                    body = { $($body)* },
                    result = result,
                    expanded = {
                        fn $fn_name(self $($params)*) $(-> $t)* {
                            match self { #result }
                        }
                    },
                ]
            ] [$($filter)*]
        );
    };

    // XXX: Static methods?

    // codegen after data collection
    (
        @I [$name:ident, $iname:ident, $path:path] {} [$(
            [
                st_name = $st_name:ident,
                bind_style = $bind_style:ident,
                body = $body:tt,
                result = $result:ident,
                expanded = { $($expanded:tt)* },
            ]
        )*] [$(
            [
                st_name = $filter_st_name:ident,
                body = $filter_body:tt,
            ]
        )*]
    ) => {
        decl_derive!([$name] => $iname);
        fn $iname(mut st: $crate::Structure) -> $crate::macros::quote::Tokens {
            let _ = &mut st; // Silence the unused mut warning

            // Filter/transform the `Structure` object before cloning it for
            // individual methods.
            $(
                {
                    let $filter_st_name = &mut st;
                    $filter_body
                }
            )*

            // Clone the `Structure` object and set the correct binding style,
            // then perform method specific expansion.
            $(
                let $result = {
                    let mut $st_name = st.clone();
                    $st_name.bind_with(|_| ::synstructure::BindStyle::$bind_style);
                    let $result = {
                        $body
                    };
                    quote!{ $($expanded)* }
                };
            )*

            st.bound_impl(stringify!($path), quote!{
                $(#$result)*
            })
        }
    }
}
