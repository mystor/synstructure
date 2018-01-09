//! This crate contains a series of traits which are occasionally referred to in
//! documentation examples. When these examples reference the example_traits
//! crate, they are referencing this crate.

// Used for the WalkFields example in src/lib.rs
pub trait WalkFields: std::any::Any {
    fn walk_fields(&self, walk: &mut FnMut(&WalkFields));
}
impl WalkFields for i32 {
    fn walk_fields(&self, walk: &mut FnMut(&WalkFields)) {}
}

// Used for the Interest example in src/lib.rs
pub trait Interest {
    fn interesting(&self) -> bool;
}
impl Interest for i32 {
    fn interesting(&self) -> bool { *self > 0 }
}
