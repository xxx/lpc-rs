//! Test-only support for the STM module: shared commit-protocol drivers
//! ([`helpers`]) plus the tests themselves.

mod soak;
mod helpers;

// Re-export the shared drivers so sibling test modules can import them
// with a single `use stm::tests::*`.
use imbl::OrdMap;
pub(crate) use helpers::*;

use crate::interpreter::{
    lpc_ref::LpcRef,
    stm::{Transaction, VarId, Version, snapshot::Snapshot},
};

#[test]
fn multiple_variables_are_isolated() {
    let var_id = VarId::new();
    let mut map = OrdMap::new();
    map.insert(var_id, LpcRef::from(123));

    let snapshot = Snapshot::new(Version::new(), map);
    let mut transaction = Transaction::new(snapshot);
    let var_id2 = VarId::new();
    transaction.write(var_id2, LpcRef::from("foo"));
    assert_eq!(transaction.read(var_id), Some(LpcRef::from(123)));
    assert_eq!(transaction.read(var_id2), Some(LpcRef::from("foo")));
}

#[test]
fn read_sees_previous_writes_before_falling_back_to_state() {
    let var_id = VarId::new();
    let mut map = OrdMap::new();
    map.insert(var_id, LpcRef::from(123));

    let snapshot = Snapshot::new(Version::new(), map);
    let mut transaction = Transaction::new(snapshot);
    assert_eq!(transaction.read(var_id), Some(LpcRef::from(123)));

    let value = LpcRef::from(42.42);
    transaction.write(var_id, value.clone());

    assert_eq!(transaction.read(var_id), Some(value));
}
