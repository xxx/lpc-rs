//! The `bytes` value's helpers: the `b"..."` notation and the capped join.

use std::fmt::Write;

use bytes::Bytes;
use lpc_rs_errors::{Result, lpc_error};

use crate::string::MAX_STRING_LENGTH;

/// `bytes` as LPC prints it: `b"..."`, printable ASCII as itself, `"` and
/// `\` escaped, every other byte `\xNN`.
pub fn literal(bytes: &[u8]) -> String {
    let mut out = String::with_capacity(bytes.len() + 3);
    out.push_str("b\"");
    for &b in bytes {
        match b {
            b'"' => out.push_str("\\\""),
            b'\\' => out.push_str("\\\\"),
            0x20..=0x7e => out.push(b as char),
            _ => write!(out, "\\x{b:02x}").expect("String never fails"),
        }
    }
    out.push('"');
    out
}

/// `a` then `b` in one allocation, or an error past [`MAX_STRING_LENGTH`].
pub fn concat(a: &[u8], b: &[u8]) -> Result<Bytes> {
    let Some(len) = a
        .len()
        .checked_add(b.len())
        .filter(|l| *l <= MAX_STRING_LENGTH)
    else {
        return Err(lpc_error!("overflow in bytes concatenation"));
    };
    let mut v = Vec::with_capacity(len);
    v.extend_from_slice(a);
    v.extend_from_slice(b);
    Ok(Bytes::from(v))
}

#[cfg(test)]
mod tests {
    use claims::assert_err;

    use super::*;

    #[test]
    fn printable_ascii_prints_as_itself() {
        assert_eq!(literal(b"ab"), r#"b"ab""#);
    }

    #[test]
    fn a_quote_and_a_backslash_are_escaped() {
        assert_eq!(literal(b"\"\\"), r#"b"\"\\""#);
    }

    #[test]
    fn every_other_byte_prints_in_hex() {
        assert_eq!(literal(&[0, 0x7f, 0xff, b'\n']), r#"b"\x00\x7f\xff\x0a""#);
    }

    #[test]
    fn concat_joins_two_short_halves() {
        assert_eq!(concat(b"ab", b"c").unwrap(), Bytes::from_static(b"abc"));
    }

    #[test]
    fn concat_past_the_cap_is_an_error() {
        let half = vec![b'x'; MAX_STRING_LENGTH / 2 + 1];
        assert_err!(concat(&half, &half));
    }
}
