# synstructure

> NOTE: What follows is an exerpt from the module level documentation. For full
> details read the docs on [docs.rs](https://docs.rs/synstructure/)
This crate provides helper types for matching against enum variants, and
extracting bindings to each of the fields in the deriving Struct or Enum in
a generic way.

If you are writing a `#[derive]` which needs to perform some operation on
every field, then you have come to the right place!

# Example: `WalkFields`
```rust
/*
 * Trait
 */
pub trait WalkFields: std::any::Any {
    fn walk_fields(&self, walk: &mut FnMut(&WalkFields));
}
impl WalkFields for i32 {
    fn walk_fields(&self, walk: &mut FnMut(&WalkFields)) {}
}

/*
 * Derive Implementation
 */
fn walkfields_derive(s: synstructure::Structure) -> quote::Tokens {
    let body = s.each(|bi| quote!{
        walk(#bi)
    });

    s.bound_impl("::WalkFields", quote!{
        fn walk_fields(&self, walk: &mut FnMut(&::WalkFields)) {
            match *self { #body }
        }
    })
}
decl_derive!([WalkFields] => walkfields_derive);

/*
 * Test Case
 */
fn main() {
    test_derive! {
        walkfields_derive {
            enum A<T> {
                B(i32, T),
                C(i32),
            }
        }
        expands to {
            impl<T> ::WalkFields for A<T> where T: ::WalkFields {
                fn walk_fields(&self, walk: &mut FnMut(&::WalkFields)) {
                    match *self {
                        A::B(ref __binding_0, ref __binding_1,) => {
                            { walk(__binding_0) }
                            { walk(__binding_1) }
                        }
                        A::C(ref __binding_0,) => {
                            { walk(__binding_0) }
                        }
                    }
                }
            }
        }
    }
}
```

# Example: `Interest`
```rust
/*
 * Trait
 */
pub trait Interest {
    fn interesting(&self) -> bool;
}
impl Interest for i32 {
    fn interesting(&self) -> bool { *self > 0 }
}

/*
 * Derive Implementation
 */
fn interest_derive(mut s: synstructure::Structure) -> quote::Tokens {
    let body = s.fold(false, |acc, bi| quote!{
        #acc || ::Interest::interesting(#bi)
    });

    s.bound_impl("::Interest", quote!{
        fn interesting(&self) -> bool {
            match *self {
                #body
            }
        }
    })
}
decl_derive!([Interest] => interest_derive);

/*
 * Test Case
 */
fn main() {
    test_derive!{
        interest_derive {
            enum A<T> {
                B(i32, T),
                C(i32),
            }
        }
        expands to {
            impl<T> ::Interest for A<T> where T: ::Interest {
                fn interesting(&self) -> bool {
                    match *self {
                        A::B(ref __binding_0, ref __binding_1,) => {
                            false || ::Interest::interesting(__binding_0) ||
                                ::Interest::interesting(__binding_1)
                        }
                        A::C(ref __binding_0,) => {
                            false || ::Interest::interesting(__binding_0)
                        }
                    }
                }
            }
        }
    }
}
```

For more example usage, consider investigating the `abomonation_derive` crate,
which makes use of this crate, and is fairly simple.
