extern crate syn;
#[macro_use]
extern crate synstructure;
#[macro_use]
extern crate quote;
extern crate proc_macro2;

decl_derive!([Interest] => interest_derive);

fn interest_derive(structure: synstructure::Structure) -> proc_macro2::TokenStream {
    let match_body = structure.fold(quote!(false), |acc, bi| {
        quote! {
            #acc || synstructure_test_traits::Interest::interesting(#bi)
        }
    });
    structure.bound_impl(
        quote!(synstructure_test_traits::Interest),
        quote! {
            fn interesting(&self) -> bool {
                match *self {
                    #match_body
                }
            }
        },
    )
}

#[test]
fn test() {
    test_derive! {
        interest_derive {
            struct A {
                a: i32,
                b: i32,
            }
        }
        expands to {
            #[allow(non_upper_case_globals)]
            #[doc(hidden)]
            const _DERIVE_synstructure_test_traits_Interest_FOR_A: () = {
                extern crate synstructure_test_traits;
                impl synstructure_test_traits::Interest for A {
                    fn interesting(&self) -> bool {
                        match *self {
                            A {
                                a: ref __binding_0,
                                b: ref __binding_1,
                            } => {
                                false ||
                                    synstructure_test_traits::Interest::interesting(__binding_0) ||
                                    synstructure_test_traits::Interest::interesting(__binding_1)
                            }
                        }
                    }
                }
            };
        }
    }
}
