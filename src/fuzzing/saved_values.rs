use arbitrary::Unstructured;
use indexmap::IndexMap;
use lpc_rs_core::null_mode::NullMode;

use crate::interpreter::{
    lpc_array::LpcArray,
    lpc_mapping::LpcMapping,
    lpc_ref::LpcRef,
    save_format as format,
    stm::{Committer, Transaction, TxnHandle},
};

fn transaction(mode: NullMode) -> TxnHandle {
    TxnHandle::new(Transaction::new(
        Committer::with_null_mode(mode).snapshot_clone(),
    ))
}

fn written(value: &LpcRef, txn: &TxnHandle) -> String {
    let mut text = String::new();
    format::write_value(&mut text, value, txn, 0).expect("generated value must serialize");
    text
}

fn assert_same(left: &LpcRef, right: &LpcRef, txn: &TxnHandle) {
    match (left, right) {
        (LpcRef::Array(_), LpcRef::Array(_)) => {
            left.with_array(txn, |l| {
                right
                    .with_array(txn, |r| {
                        assert_eq!(l.len(), r.len());
                        for (a, b) in l.iter().zip(r.iter()) {
                            assert_same(a, b, txn);
                        }
                    })
                    .unwrap();
            })
            .unwrap();
        }
        (LpcRef::Mapping(_), LpcRef::Mapping(_)) => {
            left.with_mapping(txn, |l| {
                right
                    .with_mapping(txn, |r| {
                        assert_eq!(l.mapping.len(), r.mapping.len());
                        for ((ak, av), (bk, bv)) in l.mapping.iter().zip(r.mapping.iter()) {
                            assert_same(ak, bk, txn);
                            assert_same(av, bv, txn);
                        }
                    })
                    .unwrap();
            })
            .unwrap();
        }
        (LpcRef::Float(l), LpcRef::Float(r)) => {
            assert_eq!(l.0.into_inner().to_bits(), r.0.into_inner().to_bits());
        }
        _ => assert_eq!(left, right),
    }
}

/// Parse malformed values and check canonical stability for persistable results.
pub fn save_format(text: &str) {
    for mode in [NullMode::Zero, NullMode::Distinct] {
        let txn = transaction(mode);
        if let Ok((value, _)) = format::read_value_counted(text, &txn, &|_| None) {
            let mut canonical = String::new();
            if format::write_value(&mut canonical, &value, &txn, 0).is_ok() {
                let (restored, _) = format::read_value_counted(&canonical, &txn, &|_| None)
                    .expect("serialized values must parse");
                assert_eq!(canonical, written(&restored, &txn));
            }
        }
        for line in text.lines() {
            if let Ok((_, value)) = format::split_line(line) {
                let _ = format::read_value_counted(value, &txn, &|_| None);
            }
        }
    }
}

fn generated(
    input: &mut Unstructured<'_>,
    txn: &TxnHandle,
    depth: usize,
    remaining: &mut usize,
) -> arbitrary::Result<LpcRef> {
    if *remaining == 0 {
        return Ok(txn.nil());
    }
    *remaining -= 1;
    let kind = input.int_in_range(0..=if depth == 0 { 4 } else { 6 })?;
    Ok(match kind {
        0 => txn.nil(),
        1 => LpcRef::from(input.arbitrary::<i64>()?),
        2 => {
            let number = input.arbitrary::<f64>()?;
            LpcRef::from(if number.is_finite() { number } else { 0.0 })
        }
        3 | 4 => {
            let len = input.int_in_range(0..=input.len().min(64))?;
            let bytes = input.bytes(len)?;
            if kind == 3 {
                LpcRef::from(String::from_utf8_lossy(bytes).into_owned())
            } else {
                LpcRef::from(bytes.to_vec())
            }
        }
        5 => {
            let len = input.int_in_range(0..=4)?;
            let mut items = Vec::new();
            for _ in 0..len {
                if *remaining == 0 {
                    break;
                }
                items.push(generated(input, txn, depth - 1, remaining)?);
            }
            LpcRef::Array(txn.with(|t| t.mint_array(LpcArray::new(items))))
        }
        _ => {
            let len = input.int_in_range(0..=4)?;
            let mut items = IndexMap::new();
            for index in 0..len {
                if *remaining < 2 {
                    break;
                }
                *remaining -= 1;
                let key = LpcRef::from(format!("key{index}"));
                items.insert(key, generated(input, txn, depth - 1, remaining)?);
            }
            LpcRef::Mapping(txn.with(|t| t.mint_mapping(LpcMapping::new(items))))
        }
    })
}

/// Round-trip bounded value trees in both null modes, comparing container contents.
pub fn save_round_trip(data: &[u8]) {
    for mode in [NullMode::Zero, NullMode::Distinct] {
        let txn = transaction(mode);
        if let Ok(value) = generated(&mut Unstructured::new(data), &txn, 8, &mut 64) {
            let text = written(&value, &txn);
            let (restored, _) = format::read_value_counted(&text, &txn, &|_| None)
                .expect("serialized generated values must parse");
            assert_same(&value, &restored, &txn);
            assert_eq!(text, written(&restored, &txn));
        }
    }
}
