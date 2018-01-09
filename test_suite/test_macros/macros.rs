extern crate syn;
#[macro_use]
extern crate synstructure;
#[macro_use]
extern crate quote;
extern crate proc_macro2;

decl_derive!([Interest] => interest_derive);

fn interest_derive(structure: synstructure::Structure) -> quote::Tokens {
    let match_body = structure.fold(quote!(false), |acc, bi| quote!{
        #acc || example_traits::Interest::interesting(#bi)
    });
    structure.bound_impl(quote!(example_traits::Interest), quote! {
        fn interesting(&self) -> bool {
            match *self {
                #match_body
            }
        }
    })
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
            const _DERIVE_example_traits_Interest_FOR_A: () = {
                extern crate example_traits;
                impl example_traits::Interest for A {
                    fn interesting(&self) -> bool {
                        match *self {
                            A {
                                a: ref __binding_0,
                                b: ref __binding_1,
                            } => {
                                false ||
                                    example_traits::Interest::interesting(__binding_0) ||
                                    example_traits::Interest::interesting(__binding_1)
                            }
                        }
                    }
                }
            };
        }
    }
}
