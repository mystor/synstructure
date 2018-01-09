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
