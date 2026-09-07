//! CD's save-file grammar: `name value` lines, values in the compact literal
//! form `save_object` writes and `restore_object` reads.

#![cfg_attr(
    not(test),
    expect(
        dead_code,
        reason = "the reader and the save/restore efuns call these outside tests"
    )
)]

use std::{fmt::Write, sync::Arc};

use indexmap::IndexMap;
use lpc_rs_errors::{LpcError, Result};

use crate::interpreter::{
    lpc_array::LpcArray, lpc_mapping::LpcMapping, lpc_ref::LpcRef, process::Process, stm::TxnHandle,
};

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

/// Answers the live object a `$created@name$` reference names, or `None`;
/// never loads.
pub(crate) type ObjectResolver<'a> = &'a dyn Fn(&str) -> Option<Arc<Process>>;

/// The name and the value text of one `name value` line. A blank line, a
/// separator other than one space, or a name that is not an identifier is
/// an error.
pub(crate) fn split_line(line: &str) -> Result<(&str, &str)> {
    let name_len = line
        .bytes()
        .take_while(|b| b.is_ascii_alphanumeric() || *b == b'_')
        .count();
    let valid_name = name_len > 0 && !line.as_bytes()[0].is_ascii_digit();
    if !valid_name {
        return Err(LpcError::runtime(
            "save file: a line must start with a variable name at byte 0",
        ));
    }
    let rest = &line[name_len..];
    let Some(value) = rest.strip_prefix(' ') else {
        return Err(LpcError::runtime(format!(
            "save file: one space must follow the variable name at byte {name_len}"
        )));
    };
    if value.starts_with(' ') || value.starts_with('\t') || value.is_empty() {
        return Err(LpcError::runtime(format!(
            "save file: no value after `{}` at byte {}",
            &line[..name_len],
            name_len + 1
        )));
    }
    Ok((&line[..name_len], value))
}

/// Parse the whole of `text` as one value, minting containers into `txn`.
/// Spaces after the value are tolerated, anything else is an error.
pub(crate) fn read_value(
    text: &str,
    txn: &TxnHandle,
    resolve: ObjectResolver<'_>,
) -> Result<LpcRef> {
    let mut reader = Reader {
        text: text.as_bytes(),
        at: 0,
        txn,
        resolve,
    };
    let value = reader.value(0)?;
    while reader.peek() == Some(b' ') {
        reader.at += 1;
    }
    if reader.at != reader.text.len() {
        return Err(reader.error("text after the value"));
    }
    Ok(value)
}

struct Reader<'a> {
    text: &'a [u8],
    at: usize,
    txn: &'a TxnHandle,
    resolve: ObjectResolver<'a>,
}

impl Reader<'_> {
    fn peek(&self) -> Option<u8> {
        self.text.get(self.at).copied()
    }

    fn error(&self, what: &str) -> LpcError {
        LpcError::runtime(format!("save file: {what} at byte {}", self.at))
    }

    fn expect(&mut self, s: &str) -> Result<()> {
        if self.text[self.at..].starts_with(s.as_bytes()) {
            self.at += s.len();
            Ok(())
        } else {
            Err(self.error(&format!("expected `{s}`")))
        }
    }

    fn value(&mut self, depth: usize) -> Result<LpcRef> {
        too_deep(depth)?;
        match self.peek() {
            Some(b'"') => self.string(),
            Some(b'#') => self.hex_float(),
            Some(b'$') => self.object(),
            Some(b'(') if self.text[self.at..].starts_with(b"({") => self.array(depth),
            Some(b'(') if self.text[self.at..].starts_with(b"([") => self.mapping(depth),
            Some(b'-') | Some(b'0'..=b'9') => self.number(),
            Some(_) => Err(self.error("not a value")),
            None => Err(self.error("a value is missing")),
        }
    }

    /// A decimal int, clamped on overflow, or a decimal float when a `.`
    /// or an exponent follows the digits.
    fn number(&mut self) -> Result<LpcRef> {
        let start = self.at;
        if self.peek() == Some(b'-') {
            self.at += 1;
        }
        let digits = self.at;
        while matches!(self.peek(), Some(b'0'..=b'9')) {
            self.at += 1;
        }
        if self.at == digits {
            return Err(self.error("expected a digit"));
        }
        let is_float = matches!(self.peek(), Some(b'.') | Some(b'e') | Some(b'E'));
        if is_float {
            if self.peek() == Some(b'.') {
                self.at += 1;
                while matches!(self.peek(), Some(b'0'..=b'9')) {
                    self.at += 1;
                }
            }
            if matches!(self.peek(), Some(b'e') | Some(b'E')) {
                self.at += 1;
                if matches!(self.peek(), Some(b'+') | Some(b'-')) {
                    self.at += 1;
                }
                while matches!(self.peek(), Some(b'0'..=b'9')) {
                    self.at += 1;
                }
            }
            let text = std::str::from_utf8(&self.text[start..self.at]).expect("ASCII");
            return text
                .parse::<f64>()
                .map(LpcRef::from)
                .map_err(|_| self.error("not a float"));
        }
        let text = std::str::from_utf8(&self.text[start..self.at]).expect("ASCII");
        let clamped = match text.parse::<i64>() {
            Ok(n) => n,
            Err(_) if text.starts_with('-') => i64::MIN,
            Err(_) => i64::MAX,
        };
        Ok(LpcRef::from(clamped))
    }

    /// `#[-]0x<hex>[.<hex>]p<exp>#`.
    fn hex_float(&mut self) -> Result<LpcRef> {
        self.expect("#")?;
        let negative = self.peek() == Some(b'-');
        if negative {
            self.at += 1;
        }
        self.expect("0x")?;
        let mut mantissa: u64 = 0;
        let mut frac_digits: i32 = 0;
        let mut seen_dot = false;
        let mut seen_digit = false;
        loop {
            match self.peek() {
                Some(c) if c.is_ascii_hexdigit() => {
                    seen_digit = true;
                    let d = (c as char).to_digit(16).expect("hex digit") as u64;
                    // 52 keeps every folded digit exact in an f64 (13 fraction
                    // hex digits plus the leading one never exceed 2^53 - 1);
                    // an excess digit is skipped along with its frac_digits count.
                    if mantissa >> 52 == 0 {
                        mantissa = mantissa * 16 + d;
                        if seen_dot {
                            frac_digits += 1;
                        }
                    }
                    self.at += 1;
                }
                Some(b'.') if !seen_dot => {
                    seen_dot = true;
                    self.at += 1;
                }
                _ => break,
            }
        }
        if !seen_digit {
            return Err(self.error("expected a hex digit"));
        }
        self.expect("p")?;
        let exp_start = self.at;
        if matches!(self.peek(), Some(b'+') | Some(b'-')) {
            self.at += 1;
        }
        while matches!(self.peek(), Some(b'0'..=b'9')) {
            self.at += 1;
        }
        let exp = std::str::from_utf8(&self.text[exp_start..self.at])
            .expect("ASCII")
            .parse::<i32>()
            .map_err(|_| self.error("expected an exponent"))?;
        self.expect("#")?;
        // Halved: powi(scale) alone can overflow to infinity and invert to 0.
        let scale = (exp - 4 * frac_digits).clamp(-1100, 1100);
        let half = scale / 2;
        let value = (mantissa as f64) * 2f64.powi(half) * 2f64.powi(scale - half);
        Ok(LpcRef::from(if negative { -value } else { value }))
    }

    /// `"..."` with `\n`, `\"` and `\\`; any other escape is its bare
    /// character.
    fn string(&mut self) -> Result<LpcRef> {
        self.expect("\"")?;
        let mut bytes = Vec::new();
        loop {
            match self.peek() {
                None => return Err(self.error("unterminated string")),
                Some(b'"') => {
                    self.at += 1;
                    break;
                }
                Some(b'\\') => {
                    self.at += 1;
                    match self.peek() {
                        Some(b'n') => bytes.push(b'\n'),
                        Some(c) => bytes.push(c),
                        None => return Err(self.error("unterminated string")),
                    }
                    self.at += 1;
                }
                Some(c) => {
                    bytes.push(c);
                    self.at += 1;
                }
            }
        }
        let s = String::from_utf8(bytes).map_err(|_| self.error("not UTF-8"))?;
        Ok(LpcRef::from(s))
    }

    /// `$<created>@<name>$`: the live object when `resolve` finds it and
    /// its creation time matches, else 0.
    fn object(&mut self) -> Result<LpcRef> {
        self.expect("$")?;
        let start = self.at;
        while matches!(self.peek(), Some(b'0'..=b'9')) {
            self.at += 1;
        }
        let created = std::str::from_utf8(&self.text[start..self.at])
            .expect("ASCII")
            .parse::<i64>()
            .map_err(|_| self.error("expected a creation time"))?;
        self.expect("@")?;
        let name_start = self.at;
        while !matches!(self.peek(), Some(b'$') | None) {
            self.at += 1;
        }
        let name = std::str::from_utf8(&self.text[name_start..self.at])
            .map_err(|_| self.error("not UTF-8"))?
            .to_owned();
        self.expect("$")?;
        Ok(match (self.resolve)(&name) {
            Some(process) if process.created == created => LpcRef::from(Arc::downgrade(&process)),
            _ => LpcRef::from(0),
        })
    }

    fn array(&mut self, depth: usize) -> Result<LpcRef> {
        self.expect("({")?;
        let mut items = Vec::new();
        while !self.text[self.at..].starts_with(b"})") {
            items.push(self.value(depth + 1)?);
            self.expect(",")?;
        }
        self.expect("})")?;
        Ok(LpcRef::Array(
            self.txn.with(|t| t.mint_array(LpcArray::new(items))),
        ))
    }

    fn mapping(&mut self, depth: usize) -> Result<LpcRef> {
        self.expect("([")?;
        let mut entries: IndexMap<LpcRef, LpcRef> = IndexMap::new();
        while !self.text[self.at..].starts_with(b"])") {
            let key = self.value(depth + 1)?;
            self.expect(":")?;
            let value = self.value(depth + 1)?;
            self.expect(",")?;
            entries.insert(key, value);
        }
        self.expect("])")?;
        Ok(LpcRef::Mapping(
            self.txn.with(|t| t.mint_mapping(LpcMapping::new(entries))),
        ))
    }
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

    fn read(text: &str, txn: &TxnHandle) -> LpcRef {
        read_value(text, txn, &|_| None).unwrap()
    }

    fn rejects(text: &str) -> String {
        let txn = TxnHandle::empty();
        read_value(text, &txn, &|_| None).unwrap_err().to_string()
    }

    #[test]
    fn scalars_read_back() {
        let txn = TxnHandle::empty();
        assert_eq!(read("42", &txn), LpcRef::from(42));
        assert_eq!(read("-17", &txn), LpcRef::from(-17));
        assert_eq!(read("#0x1.ap+1#", &txn), LpcRef::from(3.25));
        assert_eq!(
            read("#-0x1.0c6f7a0b5ed8dp-22#", &txn),
            LpcRef::from(-2.5e-7)
        );
        assert_eq!(read("#0x0p+0#", &txn), LpcRef::from(0.0));
        assert_eq!(
            read("#0x0.8p-1022#", &txn),
            LpcRef::from(f64::MIN_POSITIVE / 2.0)
        );
        assert_eq!(read("\"\"", &txn), LpcRef::from(""));
        assert_eq!(
            read("\"q\\\"b\\\\n\\nt\tuéé\"", &txn),
            LpcRef::from("q\"b\\n\nt\tuéé")
        );
        // An unknown escape is the bare character.
        assert_eq!(read("\"a\\tb\"", &txn), LpcRef::from("atb"));
        // Trailing spaces after the value are tolerated.
        assert_eq!(read("5  ", &txn), LpcRef::from(5));
    }

    #[test]
    fn ints_clamp_on_overflow() {
        let txn = TxnHandle::empty();
        assert_eq!(read("99999999999999999999", &txn), LpcRef::from(i64::MAX));
        assert_eq!(read("-99999999999999999999", &txn), LpcRef::from(i64::MIN));
    }

    #[test]
    fn a_hex_float_fraction_past_the_mantissa_cap_reads_as_the_truncated_value() {
        let txn = TxnHandle::empty();
        assert_eq!(
            read("#0x1.fffffffffffffffffffffp+0#", &txn),
            read("#0x1.fffffffffffffp+0#", &txn)
        );
    }

    #[test]
    fn a_decimal_float_reads_as_a_float() {
        let txn = TxnHandle::empty();
        assert_eq!(read("3.25", &txn), LpcRef::from(3.25));
        assert_eq!(read("1.5e3", &txn), LpcRef::from(1500.0));
        assert_eq!(read("-0.5", &txn), LpcRef::from(-0.5));
    }

    #[test]
    fn containers_read_back_in_file_order() {
        let txn = TxnHandle::empty();
        let na = read(r#"({1,"two",({3,({}),}),(["k":4,]),})"#, &txn);
        let items = na
            .with_array(&txn, |a| a.iter().cloned().collect::<Vec<_>>())
            .unwrap();
        assert_eq!(items.len(), 4);
        assert_eq!(items[0], LpcRef::from(1));
        assert_eq!(items[1], LpcRef::from("two"));
        let inner = items[2]
            .with_array(&txn, |a| a.iter().cloned().collect::<Vec<_>>())
            .unwrap();
        assert_eq!(inner[0], LpcRef::from(3));
        let keys = items[3]
            .with_mapping(&txn, |m| {
                m.iter().map(|(k, _)| k.clone()).collect::<Vec<_>>()
            })
            .unwrap();
        assert_eq!(keys, vec![LpcRef::from("k")]);
        let sm = read(r#"(["b":1,"c":3,"a":2,])"#, &txn);
        let order = sm
            .with_mapping(&txn, |m| {
                m.iter().map(|(k, _)| k.clone()).collect::<Vec<_>>()
            })
            .unwrap();
        assert_eq!(
            order,
            vec![LpcRef::from("b"), LpcRef::from("c"), LpcRef::from("a")]
        );
        assert_eq!(read("({})", &txn).with_array(&txn, |a| a.len()).unwrap(), 0);
        assert_eq!(
            read("([])", &txn).with_mapping(&txn, |m| m.len()).unwrap(),
            0
        );
    }

    #[test]
    fn a_duplicate_mapping_key_keeps_the_last_value() {
        let txn = TxnHandle::empty();
        let m = read(r#"(["a":1,"a":2,])"#, &txn);
        let entries = m
            .with_mapping(&txn, |m| {
                m.iter()
                    .map(|(k, v)| (k.clone(), v.clone()))
                    .collect::<Vec<_>>()
            })
            .unwrap();
        assert_eq!(entries, vec![(LpcRef::from("a"), LpcRef::from(2))]);
    }

    #[test]
    fn an_object_reference_is_kept_only_when_the_creation_time_matches() {
        let txn = TxnHandle::empty();
        let process = Arc::new(Process::new(Program::new("/std/thing.c")));
        let found = process.clone();
        let resolve = move |name: &str| (name == "/std/thing").then(|| found.clone());
        let same = format!("${}@/std/thing$", process.created);
        let got = read_value(&same, &txn, &resolve).unwrap();
        assert!(matches!(&got, LpcRef::Object(w) if Arc::ptr_eq(&w.upgrade().unwrap(), &process)));
        let stale = format!("${}@/std/thing$", process.created - 1);
        assert_eq!(read_value(&stale, &txn, &resolve).unwrap(), LpcRef::from(0));
        assert_eq!(
            read_value("$5@/std/other$", &txn, &resolve).unwrap(),
            LpcRef::from(0)
        );
    }

    #[test]
    fn the_strict_grammar_rejects_what_cd_rejects() {
        for text in [
            "({1,2})",        // missing trailing comma
            "([ \"a\":1, ])", // spaces inside a mapping
            "({1,2,}) x",     // trailing garbage
            "\"open",         // unterminated string
            "#0x1.ap+1",      // unterminated float
            "#abc#",          // not a hex float
            "$12@",           // unterminated object reference
            "$x@/a$",         // non-numeric creation time
            "",               // nothing
            "(",              // half an opener
            "([1:2])",        // missing trailing comma in a mapping
        ] {
            let err = rejects(text);
            assert!(err.contains("save file:"), "{text:?} -> {err}");
        }
    }

    #[test]
    fn nesting_past_the_cap_is_rejected_on_read() {
        let text = format!(
            "{}{}",
            "({".repeat(MAX_DEPTH + 2),
            "}),".repeat(MAX_DEPTH + 2)
        );
        let err = rejects(&text);
        assert!(err.contains("nesting deeper than 128 levels"), "{err}");
    }

    #[test]
    fn split_line_takes_a_name_one_space_and_the_rest() {
        assert_eq!(
            split_line("hit_points 7220").unwrap(),
            ("hit_points", "7220")
        );
        assert_eq!(split_line(r#"s "a b""#).unwrap(), ("s", r#""a b""#));
        for bad in ["", "a  1", "a\t1", "a", " a 1", "1a 1"] {
            assert!(split_line(bad).is_err(), "{bad:?}");
        }
    }

    /// Every line of CD's literal `xx_save_val.o` (the probe's file) is
    /// unchanged by a read followed by a write.
    #[test]
    fn cd_lines_round_trip_byte_for_byte() {
        let txn = TxnHandle::empty();
        let file = "i 42\n\
                    ni -17\n\
                    fa #0x1.ap+1#\n\
                    fb #0x1.999999999999ap-4#\n\
                    fc #0x1.5af1d78b58c4p+66#\n\
                    fd #-0x1.0c6f7a0b5ed8dp-22#\n\
                    s \"q\\\"b\\\\n\\nt\tuéé\"\n\
                    es \"\"\n\
                    ea ({})\n\
                    na ({1,\"two\",({3,({}),}),([\"k\":4,]),})\n\
                    em ([])\n\
                    sm ([\"b\":1,\"c\":3,\"a\":2,])\n\
                    im ([-3:\"neg\",1:\"one\",2:\"two\",])\n\
                    mm ([#0x1.8p+0#:\"f\",7:\"i\",\"s\":1,])\n\
                    nm ([\"outer\":([\"inner\":({1,2,}),]),])\n\
                    z 0\n\
                    pv 6\n";
        let mut out = String::new();
        for line in file.lines() {
            let (name, text) = split_line(line).unwrap();
            let value = read_value(text, &txn, &|_| None).unwrap();
            assert!(write_line(&mut out, name, &value, &txn).unwrap());
        }
        assert_eq!(out, file);
    }
}
