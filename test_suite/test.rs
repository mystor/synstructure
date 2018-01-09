#![allow(dead_code)]

#[macro_use]
extern crate test_macros;

// Import the trait at a strange path to show that it works when you do this.
extern crate synstructure_test_traits as something_random;
use something_random::Interest as RInterest;

#[derive(Interest)]
struct Test {
    x: i32,
    y: i32,
}

#[derive(Interest)]
struct Unit;

#[derive(Interest)]
struct Generic<T> {
    x: T,
    y: i32,
}

#[derive(Interest)]
struct ExplicitlyBound<T: RInterest> {
    x: T,
    y: i32,
}

#[test]
fn test_important() {
    assert_eq!(Test { x: 1, y: 1 }.interesting(), true);

    assert_eq!(Test { x: 1, y: 0 }.interesting(), true);

    assert_eq!(Test { x: 0, y: 0 }.interesting(), false);

    assert_eq!(Unit.interesting(), false);

    assert_eq!(Generic { x: 0, y: 0 }.interesting(), false);

    assert_eq!(Generic { x: 1, y: 0 }.interesting(), true);

    assert_eq!(Generic { x: 0, y: 1 }.interesting(), true);
}
