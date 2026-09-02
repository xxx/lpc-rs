//! JSON text to and from LPC values, behind `json_encode` and `json_decode`.

use indexmap::IndexMap;
use lpc_rs_errors::{LpcError, Result};
use serde_json::{Map, Number, Value};

use crate::interpreter::{
    lpc_array::LpcArray, lpc_mapping::LpcMapping, lpc_ref::LpcRef, stm::TxnHandle,
};

/// The deepest nesting either direction accepts, serde_json's own parse limit.
pub(crate) const MAX_DEPTH: usize = 128;

/// Render `value` as compact JSON text.
pub(crate) fn encode(value: &LpcRef, txn: &TxnHandle) -> Result<String> {
    Ok(to_value(value, txn, 0)?.to_string())
}

fn to_value(value: &LpcRef, txn: &TxnHandle, depth: usize) -> Result<Value> {
    if depth > MAX_DEPTH {
        return Err(LpcError::runtime(format!(
            "json_encode: nesting deeper than {MAX_DEPTH} levels"
        )));
    }
    match value {
        LpcRef::Int(x) => Ok(Value::from(x.0)),
        LpcRef::Float(x) => Number::from_f64(x.0.into_inner())
            .map(Value::Number)
            .ok_or_else(|| {
                LpcError::runtime(format!("json_encode: a float must be finite, not {x}"))
            }),
        LpcRef::String(s) => Ok(Value::String(s.to_str().to_owned())),
        LpcRef::Array(_) => {
            let items = value.with_array(txn, |array| {
                array
                    .iter()
                    .map(|item| to_value(item, txn, depth + 1))
                    .collect::<Result<Vec<_>>>()
            })??;
            Ok(Value::Array(items))
        }
        LpcRef::Mapping(_) => {
            let entries = value.with_mapping(txn, |mapping| {
                mapping
                    .mapping
                    .iter()
                    .map(|(key, item)| Ok((key_of(key)?, to_value(item, txn, depth + 1)?)))
                    .collect::<Result<Map<_, _>>>()
            })??;
            Ok(Value::Object(entries))
        }
        LpcRef::Object(object) if object.upgrade().is_none() => Ok(Value::from(0)),
        other => Err(LpcError::runtime(format!(
            "json_encode: cannot encode {}",
            other.type_name()
        ))),
    }
}

/// Parse `text` into an LPC value, minting its arrays and mappings in `txn`.
/// `true` is 1, `false` and `null` are 0; a number written without a
/// fraction or exponent is an int, any other a float.
pub(crate) fn decode(text: &str, txn: &TxnHandle) -> Result<LpcRef> {
    let value: Value =
        serde_json::from_str(text).map_err(|e| LpcError::runtime(format!("json_decode: {e}")))?;
    from_value(value, txn)
}

fn from_value(value: Value, txn: &TxnHandle) -> Result<LpcRef> {
    match value {
        Value::Null | Value::Bool(false) => Ok(LpcRef::from(0)),
        Value::Bool(true) => Ok(LpcRef::from(1)),
        Value::Number(n) => n
            .as_i64()
            .map(LpcRef::from)
            .or_else(|| n.as_f64().map(LpcRef::from))
            .ok_or_else(|| LpcError::runtime(format!("json_decode: {n} is out of range"))),
        Value::String(s) => Ok(LpcRef::from(s)),
        Value::Array(items) => {
            let items = items
                .into_iter()
                .map(|item| from_value(item, txn))
                .collect::<Result<Vec<_>>>()?;
            Ok(LpcRef::Array(
                txn.with(|t| t.mint_array(LpcArray::new(items))),
            ))
        }
        Value::Object(entries) => {
            let entries = entries
                .into_iter()
                .map(|(key, item)| Ok((LpcRef::from(key), from_value(item, txn)?)))
                .collect::<Result<IndexMap<_, _>>>()?;
            Ok(LpcRef::Mapping(
                txn.with(|t| t.mint_mapping(LpcMapping::new(entries))),
            ))
        }
    }
}

/// A JSON object key: strings as they are, numbers as their text.
fn key_of(key: &LpcRef) -> Result<String> {
    match key {
        LpcRef::String(s) => Ok(s.to_str().to_owned()),
        LpcRef::Int(_) | LpcRef::Float(_) => Ok(key.to_string()),
        other => Err(LpcError::runtime(format!(
            "json_encode: a mapping key must be a string or number, not {}",
            other.type_name()
        ))),
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use indexmap::IndexMap;

    use super::*;
    use crate::interpreter::{
        lpc_array::LpcArray, lpc_mapping::LpcMapping, process::Process, program::Program,
    };

    fn array(txn: &TxnHandle, items: Vec<LpcRef>) -> LpcRef {
        LpcRef::Array(txn.with(|t| t.mint_array(LpcArray::new(items))))
    }

    fn mapping(txn: &TxnHandle, pairs: Vec<(LpcRef, LpcRef)>) -> LpcRef {
        let map: IndexMap<LpcRef, LpcRef> = pairs.into_iter().collect();
        LpcRef::Mapping(txn.with(|t| t.mint_mapping(LpcMapping::new(map))))
    }

    fn object() -> (Arc<Process>, LpcRef) {
        let process = Arc::new(Process::new(Program::default()));
        let handle = LpcRef::from(Arc::downgrade(&process));
        (process, handle)
    }

    #[test]
    fn an_int_encodes_as_a_number() {
        let txn = TxnHandle::empty();
        assert_eq!(encode(&LpcRef::from(42), &txn).unwrap(), "42");
    }

    #[test]
    fn a_float_encodes_as_a_number() {
        let txn = TxnHandle::empty();
        assert_eq!(encode(&LpcRef::from(1.5), &txn).unwrap(), "1.5");
    }

    #[test]
    fn a_non_finite_float_is_an_error() {
        let txn = TxnHandle::empty();
        let err = encode(&LpcRef::from(f64::INFINITY), &txn)
            .unwrap_err()
            .to_string();
        assert!(err.contains("json_encode: a float must be finite"), "{err}");
    }

    #[test]
    fn a_string_is_quoted_and_escaped() {
        let txn = TxnHandle::empty();
        assert_eq!(
            encode(&LpcRef::from("he said \"hi\"\n"), &txn).unwrap(),
            r#""he said \"hi\"\n""#
        );
    }

    #[test]
    fn an_array_encodes_element_by_element() {
        let txn = TxnHandle::empty();
        let items = vec![LpcRef::from(1), LpcRef::from("a"), LpcRef::from(2.5)];
        assert_eq!(encode(&array(&txn, items), &txn).unwrap(), r#"[1,"a",2.5]"#);
    }

    #[test]
    fn a_mapping_encodes_as_an_object_in_insertion_order() {
        let txn = TxnHandle::empty();
        let pairs = vec![
            (LpcRef::from("b"), LpcRef::from(1)),
            (LpcRef::from("a"), LpcRef::from(2)),
        ];
        assert_eq!(
            encode(&mapping(&txn, pairs), &txn).unwrap(),
            r#"{"b":1,"a":2}"#
        );
    }

    #[test]
    fn a_number_key_is_stringified() {
        let txn = TxnHandle::empty();
        let pairs = vec![
            (LpcRef::from(1), LpcRef::from("x")),
            (LpcRef::from(1.5), LpcRef::from("y")),
        ];
        assert_eq!(
            encode(&mapping(&txn, pairs), &txn).unwrap(),
            r#"{"1":"x","1.5":"y"}"#
        );
    }

    #[test]
    fn an_object_key_is_an_error() {
        let txn = TxnHandle::empty();
        let (_process, handle) = object();
        let err = encode(&mapping(&txn, vec![(handle, LpcRef::from(1))]), &txn)
            .unwrap_err()
            .to_string();
        assert!(
            err.contains("json_encode: a mapping key must be a string or number, not object"),
            "{err}"
        );
    }

    #[test]
    fn a_live_object_is_an_error() {
        let txn = TxnHandle::empty();
        let (_process, handle) = object();
        let err = encode(&handle, &txn).unwrap_err().to_string();
        assert!(err.contains("json_encode: cannot encode object"), "{err}");
    }

    #[test]
    fn a_destructed_object_encodes_as_0() {
        let txn = TxnHandle::empty();
        let (process, handle) = object();
        drop(process);
        assert_eq!(encode(&handle, &txn).unwrap(), "0");
    }

    #[test]
    fn nesting_beyond_the_cap_is_an_error() {
        let txn = TxnHandle::empty();
        let cyclic = array(&txn, vec![LpcRef::from(0)]);
        cyclic
            .with_array_cow(&txn, |array| {
                array.array[0] = cyclic.clone();
                Ok(())
            })
            .unwrap();
        let err = encode(&cyclic, &txn).unwrap_err().to_string();
        assert!(
            err.contains("json_encode: nesting deeper than 128 levels"),
            "{err}"
        );
    }

    #[test]
    fn a_container_inside_a_container_encodes_recursively() {
        let txn = TxnHandle::empty();
        let tags = array(&txn, vec![LpcRef::from("a")]);
        let pairs = vec![(LpcRef::from("tags"), tags)];
        assert_eq!(
            encode(&mapping(&txn, pairs), &txn).unwrap(),
            r#"{"tags":["a"]}"#
        );
    }

    #[test]
    fn an_object_decodes_to_a_mapping_in_document_order() {
        let txn = TxnHandle::empty();
        let decoded = decode(r#"{"b":1,"a":2}"#, &txn).unwrap();
        let keys = decoded
            .with_mapping(&txn, |m| {
                m.mapping
                    .keys()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
            })
            .unwrap();
        assert_eq!(keys, ["b", "a"]);
    }

    #[test]
    fn an_array_decodes_to_an_array() {
        let txn = TxnHandle::empty();
        let decoded = decode(r#"[1,"a"]"#, &txn).unwrap();
        decoded
            .with_array(&txn, |a| {
                assert_eq!(&**a, [LpcRef::from(1), LpcRef::from("a")])
            })
            .unwrap();
    }

    #[test]
    fn a_number_without_a_fraction_or_exponent_is_an_int() {
        let txn = TxnHandle::empty();
        assert_eq!(decode("42", &txn).unwrap(), LpcRef::from(42));
    }

    #[test]
    fn a_number_with_a_fraction_is_a_float() {
        let txn = TxnHandle::empty();
        assert_eq!(decode("1.0", &txn).unwrap(), LpcRef::from(1.0));
    }

    #[test]
    fn a_number_with_an_exponent_is_a_float() {
        let txn = TxnHandle::empty();
        assert_eq!(decode("1e3", &txn).unwrap(), LpcRef::from(1000.0));
    }

    #[test]
    fn a_number_beyond_int_range_is_a_float() {
        let txn = TxnHandle::empty();
        assert_eq!(
            decode("18446744073709551615", &txn).unwrap(),
            LpcRef::from(18446744073709551615.0)
        );
    }

    #[test]
    fn true_decodes_as_1() {
        let txn = TxnHandle::empty();
        assert_eq!(decode("true", &txn).unwrap(), LpcRef::from(1));
    }

    #[test]
    fn false_and_null_decode_as_0() {
        let txn = TxnHandle::empty();
        let decoded = decode("[false,null]", &txn).unwrap();
        decoded
            .with_array(&txn, |a| {
                assert_eq!(&**a, [LpcRef::from(0), LpcRef::from(0)])
            })
            .unwrap();
    }

    #[test]
    fn a_string_decodes_with_escapes_resolved() {
        let txn = TxnHandle::empty();
        assert_eq!(
            decode(r#""caf\u00e9\n""#, &txn).unwrap(),
            LpcRef::from("caf\u{e9}\n")
        );
    }

    #[test]
    fn malformed_text_is_an_error_naming_the_position() {
        let txn = TxnHandle::empty();
        let err = decode("{", &txn).unwrap_err().to_string();
        assert!(
            err.contains("json_decode:") && err.contains("line 1 column"),
            "{err}"
        );
    }

    #[test]
    fn decoding_beyond_the_cap_is_an_error() {
        let txn = TxnHandle::empty();
        let deep = format!("{}{}", "[".repeat(MAX_DEPTH + 1), "]".repeat(MAX_DEPTH + 1));
        let err = decode(&deep, &txn).unwrap_err().to_string();
        assert!(
            err.contains("json_decode: recursion limit exceeded"),
            "{err}"
        );
    }

    #[test]
    fn a_document_round_trips() {
        let txn = TxnHandle::empty();
        let text = r#"{"hp":12,"name":"bob","tags":["a",1.5],"nested":{"x":0}}"#;
        let decoded = decode(text, &txn).unwrap();
        assert_eq!(encode(&decoded, &txn).unwrap(), text);
    }
}
