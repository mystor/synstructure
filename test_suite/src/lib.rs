#![allow(dead_code)]

#[macro_use]
extern crate test_macros;

trait Important {
    fn is_important(&self) -> bool;
}

impl Important for bool {
    fn is_important(&self) -> bool {
        *self
    }
}

#[derive(Important)]
struct Test {
    x: bool,
    y: bool,
}

#[derive(Important)]
struct Unit;

#[derive(Important)]
struct Generic<T> {
    x: T,
    y: bool,
}

#[derive(Important)]
struct ExplicitlyBound<T: Important> {
    x: T,
    y: bool,
}

#[test]
fn test_important() {
    assert_eq!(Test { x: true, y: true }.is_important(), true);

    assert_eq!(Test { x: true, y: false }.is_important(), true);

    assert_eq!(Test { x: false, y: false }.is_important(), false);

    assert_eq!(Unit.is_important(), false);

    assert_eq!(Generic { x: false, y: false }.is_important(), false);

    assert_eq!(Generic { x: true, y: false }.is_important(), true);

    assert_eq!(Generic { x: false, y: true }.is_important(), true);
}
