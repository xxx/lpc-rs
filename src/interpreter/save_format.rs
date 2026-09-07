//! CD's save-file grammar: `name value` lines, values in the compact literal
//! form `save_object` writes and `restore_object` reads.

#![cfg_attr(
    not(test),
    expect(
        dead_code,
        reason = "the reader and the save/restore efuns call these outside tests"
    )
)]

use std::fmt::Write;

use lpc_rs_errors::{LpcError, Result};

use crate::interpreter::{lpc_ref::LpcRef, stm::TxnHandle};

/// The deepest nesting either direction accepts, the same cap as JSON.
pub(crate) const MAX_DEPTH: usize = 128;

/// `f` as C99 `%a` prints it: `0x1.<hex>p<exp>` with trailing zeros
/// trimmed, `0x0p+0` for zero, `0x0.<hex>p-1022` for a subnormal.
pub(crate) fn hex_float(f: f64) -> String {
    let bits = f.to_bits();
    let sign = if bits >> 63 == 1 { "-" } else { "" };
    let exponent = ((bits >> 52) & 0x7ff) as i32;
    let mantissa = bits & ((1u64 << 52) - 1);
    let (lead, exp) = match exponent {
        0 if mantissa == 0 => return format!("{sign}0x0p+0"),
        0 => ('0', -1022),
        e => ('1', e - 1023),
    };
    let mut digits = format!("{mantissa:013x}");
    while digits.ends_with('0') {
        digits.pop();
    }
    let fraction = if digits.is_empty() {
        String::new()
    } else {
        format!(".{digits}")
    };
    format!("{sign}0x{lead}{fraction}p{exp:+}")
}

/// An error when `depth` has gone past [`MAX_DEPTH`].
fn too_deep(depth: usize) -> Result<()> {
    if depth > MAX_DEPTH {
        return Err(LpcError::runtime(format!(
            "save file: nesting deeper than {MAX_DEPTH} levels"
        )));
    }
    Ok(())
}

/// Append `value` in the compact form. Containers are read through `txn`;
/// a live object is `$created@name$`, a dead one and a function pointer
/// are `0`.
pub(crate) fn write_value(
    out: &mut String,
    value: &LpcRef,
    txn: &TxnHandle,
    depth: usize,
) -> Result<()> {
    too_deep(depth)?;
    match value {
        LpcRef::Int(x) => write!(out, "{}", x.0).expect("String never fails"),
        LpcRef::Float(x) => {
            write!(out, "#{}#", hex_float(x.0.into_inner())).expect("String never fails")
        }
        LpcRef::String(s) => {
            out.push('"');
            for c in s.to_str().chars() {
                match c {
                    '"' => out.push_str("\\\""),
                    '\\' => out.push_str("\\\\"),
                    '\n' => out.push_str("\\n"),
                    c => out.push(c),
                }
            }
            out.push('"');
        }
        LpcRef::Array(_) => {
            out.push_str("({");
            value.with_array(txn, |array| -> Result<()> {
                for item in array.iter() {
                    write_value(out, item, txn, depth + 1)?;
                    out.push(',');
                }
                Ok(())
            })??;
            out.push_str("})");
        }
        LpcRef::Mapping(_) => {
            out.push_str("([");
            value.with_mapping(txn, |mapping| -> Result<()> {
                for (key, item) in mapping.iter() {
                    write_value(out, key, txn, depth + 1)?;
                    out.push(':');
                    write_value(out, item, txn, depth + 1)?;
                    out.push(',');
                }
                Ok(())
            })??;
            out.push_str("])");
        }
        LpcRef::Object(object) => match object.upgrade() {
            Some(process) => write!(out, "${}@{}$", process.created, process.filename())
                .expect("String never fails"),
            None => out.push('0'),
        },
        LpcRef::Function(_) => out.push('0'),
    }
    Ok(())
}

/// Append `name value\n`. A function pointer skips the line and answers
/// `false`; anything else is written and answers `true`.
pub(crate) fn write_line(
    out: &mut String,
    name: &str,
    value: &LpcRef,
    txn: &TxnHandle,
) -> Result<bool> {
    if matches!(value, LpcRef::Function(_)) {
        return Ok(false);
    }
    out.push_str(name);
    out.push(' ');
    write_value(out, value, txn, 0)?;
    out.push('\n');
    Ok(true)
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use indexmap::IndexMap;

    use super::*;
    use crate::interpreter::{
        lpc_array::LpcArray, lpc_mapping::LpcMapping, lpc_ref::LpcRef, process::Process,
        program::Program, stm::TxnHandle,
    };

    fn array(txn: &TxnHandle, items: Vec<LpcRef>) -> LpcRef {
        LpcRef::Array(txn.with(|t| t.mint_array(LpcArray::new(items))))
    }

    fn mapping(txn: &TxnHandle, pairs: Vec<(LpcRef, LpcRef)>) -> LpcRef {
        let map: IndexMap<LpcRef, LpcRef> = pairs.into_iter().collect();
        LpcRef::Mapping(txn.with(|t| t.mint_mapping(LpcMapping::new(map))))
    }

    fn written(value: &LpcRef, txn: &TxnHandle) -> String {
        let mut out = String::new();
        write_value(&mut out, value, txn, 0).unwrap();
        out
    }

    #[test]
    fn hex_float_matches_the_probe_constants() {
        assert_eq!(hex_float(3.25), "0x1.ap+1");
        assert_eq!(hex_float(0.1), "0x1.999999999999ap-4");
        assert_eq!(hex_float(1e20), "0x1.5af1d78b58c4p+66");
        assert_eq!(hex_float(-2.5e-7), "-0x1.0c6f7a0b5ed8dp-22");
        assert_eq!(hex_float(0.0), "0x0p+0");
        assert_eq!(hex_float(-0.0), "-0x0p+0");
        assert_eq!(hex_float(1.0), "0x1p+0");
        assert_eq!(hex_float(2.0), "0x1p+1");
        assert_eq!(hex_float(f64::MIN_POSITIVE / 2.0), "0x0.8p-1022");
    }

    #[test]
    fn scalars_write_in_cd_form() {
        let txn = TxnHandle::empty();
        assert_eq!(written(&LpcRef::from(42), &txn), "42");
        assert_eq!(written(&LpcRef::from(-17), &txn), "-17");
        assert_eq!(written(&LpcRef::from(3.25), &txn), "#0x1.ap+1#");
        assert_eq!(written(&LpcRef::from(""), &txn), "\"\"");
        assert_eq!(
            written(&LpcRef::from("q\"b\\n\nt\tuéé"), &txn),
            "\"q\\\"b\\\\n\\nt\tuéé\""
        );
    }

    #[test]
    fn containers_write_with_a_trailing_comma_per_element() {
        let txn = TxnHandle::empty();
        assert_eq!(written(&array(&txn, vec![]), &txn), "({})");
        assert_eq!(written(&mapping(&txn, vec![]), &txn), "([])");
        let inner = array(&txn, vec![LpcRef::from(3), array(&txn, vec![])]);
        let m = mapping(&txn, vec![(LpcRef::from("k"), LpcRef::from(4))]);
        let na = array(&txn, vec![LpcRef::from(1), LpcRef::from("two"), inner, m]);
        assert_eq!(written(&na, &txn), r#"({1,"two",({3,({}),}),(["k":4,]),})"#);
        let mm = mapping(
            &txn,
            vec![
                (LpcRef::from(1.5), LpcRef::from("f")),
                (LpcRef::from(7), LpcRef::from("i")),
                (LpcRef::from("s"), LpcRef::from(1)),
            ],
        );
        assert_eq!(written(&mm, &txn), r#"([#0x1.8p+0#:"f",7:"i","s":1,])"#);
    }

    #[test]
    fn a_live_object_writes_as_a_reference_and_a_dead_one_as_zero() {
        let txn = TxnHandle::empty();
        let process = Arc::new(Process::new(Program::new("/std/thing.c")));
        let live = LpcRef::from(Arc::downgrade(&process));
        assert_eq!(
            written(&live, &txn),
            format!("${}@/std/thing$", process.created)
        );
        drop(process);
        assert_eq!(written(&live, &txn), "0");
    }

    #[test]
    fn a_nested_function_pointer_writes_as_zero_and_a_top_level_one_skips_the_line() {
        let txn = TxnHandle::empty();
        let fp = crate::test_support::function_pointer_ref();
        assert_eq!(written(&array(&txn, vec![fp.clone()]), &txn), "({0,})");
        let mut out = String::new();
        assert!(!write_line(&mut out, "f", &fp, &txn).unwrap());
        assert_eq!(out, "");
        assert!(write_line(&mut out, "i", &LpcRef::from(6), &txn).unwrap());
        assert_eq!(out, "i 6\n");
    }

    #[test]
    fn nesting_past_the_cap_is_an_error() {
        let txn = TxnHandle::empty();
        let mut value = array(&txn, vec![]);
        for _ in 0..=MAX_DEPTH {
            value = array(&txn, vec![value]);
        }
        let err = write_value(&mut String::new(), &value, &txn, 0)
            .unwrap_err()
            .to_string();
        assert!(err.contains("nesting deeper than 128 levels"), "{err}");
    }
}
