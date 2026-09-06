//! Set operators on arrays.

use super::run_nested;
use crate::interpreter::{lpc_ref::LpcRef, stm::TxnHandle};

fn ints(r: &LpcRef, txn: &TxnHandle) -> Vec<i64> {
    r.with_array(txn, |a| {
        a.iter()
            .map(|x| match x {
                LpcRef::Int(i) => i.0,
                other => panic!("expected int, got {other}"),
            })
            .collect()
    })
    .unwrap()
}

#[tokio::test]
async fn and_keeps_the_left_operands_members_found_on_the_right_in_order() {
    let (result, txn) = run_nested(
        "",
        &[],
        "mixed *create() { return ({ ({ 3, 1, 2, 1 }) & ({ 1, 3, 9 }) }); }",
    )
    .await;
    let inner = result.with_array(&txn, |a| a[0].clone()).unwrap();
    assert_eq!(ints(&inner, &txn), vec![3, 1, 1]);
}

#[tokio::test]
async fn and_on_disjoint_arrays_is_empty() {
    let (result, txn) = run_nested(
        "",
        &[],
        "mixed *create() { return ({ ({ 1 }) & ({ 2 }) }); }",
    )
    .await;
    let inner = result.with_array(&txn, |a| a[0].clone()).unwrap();
    assert_eq!(ints(&inner, &txn), Vec::<i64>::new());
}

#[tokio::test]
async fn and_assign_on_an_array_variable() {
    let (result, txn) = run_nested(
        "",
        &[],
        "mixed *create() { int *a = ({ 1, 2, 3 }); a &= ({ 2, 3, 4 }); return ({ a }); }",
    )
    .await;
    let inner = result.with_array(&txn, |a| a[0].clone()).unwrap();
    assert_eq!(ints(&inner, &txn), vec![2, 3]);
}

#[tokio::test]
async fn and_on_object_arrays_compares_objects() {
    let (result, txn) = run_nested(
        "",
        &[],
        "mixed *create() { object me = this_object(); return ({ ({ me }) & ({ me }) }); }",
    )
    .await;
    let inner = result.with_array(&txn, |a| a[0].clone()).unwrap();
    let count = inner.with_array(&txn, |a| a.len()).unwrap();
    assert_eq!(count, 1);
}

#[tokio::test]
async fn or_keeps_the_left_and_appends_the_rights_members_absent_from_it() {
    let (result, txn) = run_nested(
        "",
        &[],
        "mixed *create() { return ({ ({ 1, 2, 3, 3 }) | ({ 1, 4 }), ({ 1 }) | ({ 2, 2 }), ({ 1, 1 }) | ({ 1 }), ({ 1, 2 }) | ({ 2, 1 }) }); }",
    )
    .await;
    let members: Vec<LpcRef> = result
        .with_array(&txn, |a| a.iter().cloned().collect())
        .unwrap();
    assert_eq!(ints(&members[0], &txn), vec![1, 2, 3, 3, 4]);
    assert_eq!(ints(&members[1], &txn), vec![1, 2, 2]);
    assert_eq!(ints(&members[2], &txn), vec![1, 1]);
    assert_eq!(ints(&members[3], &txn), vec![1, 2]);
}

#[tokio::test]
async fn or_assign_on_an_array_variable() {
    let (result, txn) = run_nested(
        "",
        &[],
        "mixed *create() { int *a = ({ 1, 2 }); a |= ({ 2, 3 }); return ({ a }); }",
    )
    .await;
    let inner = result.with_array(&txn, |a| a[0].clone()).unwrap();
    assert_eq!(ints(&inner, &txn), vec![1, 2, 3]);
}
