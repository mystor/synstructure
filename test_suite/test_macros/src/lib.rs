extern crate proc_macro;
extern crate syn;
extern crate synstructure;
#[macro_use]
extern crate quote;

use proc_macro::TokenStream;
use synstructure::Structure;

#[proc_macro_derive(Important)]
pub fn derive_important(input: TokenStream) -> TokenStream {
    let ast = syn::parse_derive_input(&input.to_string()).unwrap();

    let structure = Structure::new(&ast);

    let match_body = structure.fold(quote!(false), |acc, bi| quote!{
        #acc || ::Important::is_important(#bi)
    });
    structure.bound_impl("::Important", quote! {
        fn is_important(&self) -> bool {
            match *self {
                #match_body
            }
        }
    }).parse().unwrap()
}
