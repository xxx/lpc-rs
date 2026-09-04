//! One argument rendered under its conversion: text, padded; or the lines
//! of a column or table.

use lpc_rs_core::BaseFloat;
use lpc_rs_errors::Result;

use super::{
    layout::{Field, Kind},
    spec::{Align, Mode, Pad, Sign, Spec},
};
use crate::interpreter::{efun::efun_context::EfunContext, lpc_ref::LpcRef};

/// The type of `value` with its article, for a message.
pub(super) fn described(value: &LpcRef) -> String {
    let name = value.type_name();
    match name.chars().next() {
        Some('a' | 'e' | 'i' | 'o' | 'u') => format!("an {name}"),
        _ => format!("a {name}"),
    }
}

/// `text` in a field of `width` under `align`, filled with `pad`; a text
/// at least as wide as the field is itself.
fn padded(text: &str, width: Option<usize>, align: Align, pad: &Pad) -> String {
    let Some(width) = width else {
        return text.to_owned();
    };
    let len = text.chars().count();
    if len >= width {
        return text.to_owned();
    }
    let fill = width - len;
    let (left, right) = match align {
        Align::Left => (0, fill),
        Align::Right => (fill, 0),
        Align::Center => (fill.div_ceil(2), fill / 2),
    };
    format!("{}{}{}", pad.run(left), text, pad.run(right))
}

/// [`padded`] for a number: zeroes go behind the sign.
fn padded_number(text: &str, width: Option<usize>, align: Align, pad: &Pad) -> String {
    if *pad == Pad::Zero
        && align == Align::Right
        && let Some(rest) = text.strip_prefix(['-', '+', ' '])
        && let Some(width) = width
        && text.chars().count() < width
    {
        let sign = &text[..text.len() - rest.len()];
        return format!("{sign}{}{rest}", "0".repeat(width - text.chars().count()));
    }
    padded(text, width, align, pad)
}

/// `sign`'s prefix for a non-negative number.
fn prefix(sign: Sign, negative: bool) -> &'static str {
    match sign {
        _ if negative => "",
        Sign::None => "",
        Sign::Plus => "+",
        Sign::Space => " ",
    }
}

fn int_text(spec: &Spec, i: i64) -> String {
    let magnitude = i.unsigned_abs();
    let digits = match spec.conversion {
        'b' => format!("{magnitude:b}"),
        'o' => format!("{magnitude:o}"),
        'x' => format!("{magnitude:x}"),
        'X' => format!("{magnitude:X}"),
        _ => magnitude.to_string(),
    };
    let sign = if i < 0 { "-" } else { prefix(spec.sign, false) };
    format!("{sign}{digits}")
}

/// `f` in C's `%e` form: a mantissa with `precision` decimals and a
/// signed, two-digit exponent.
fn exponent_text(f: BaseFloat, precision: usize, upper: bool) -> String {
    let rust = format!("{f:.precision$e}");
    let (mantissa, exponent) = rust.split_once('e').unwrap_or((&rust, "0"));
    let exponent: i32 = exponent.parse().unwrap_or(0);
    let e = if upper { 'E' } else { 'e' };
    let sign = if exponent < 0 { '-' } else { '+' };
    format!("{mantissa}{e}{sign}{:02}", exponent.abs())
}

/// `text` without trailing zeroes in its fraction, and without a bare
/// point; an exponent suffix is kept.
fn trimmed(text: &str) -> String {
    let (number, exponent) = match text.find(['e', 'E']) {
        Some(at) => (&text[..at], &text[at..]),
        None => (text, ""),
    };
    let number = if number.contains('.') {
        number.trim_end_matches('0').trim_end_matches('.')
    } else {
        number
    };
    format!("{number}{exponent}")
}

/// `f` in C's `%g` form: `precision` significant digits, the shorter of
/// the fixed and exponent forms, trailing zeroes dropped.
fn general_text(f: BaseFloat, precision: usize, upper: bool) -> String {
    let significant = precision.max(1);
    if f == 0.0 {
        return "0".to_owned();
    }
    let probe = format!("{f:.*e}", significant - 1);
    let exponent: i32 = probe
        .split_once('e')
        .and_then(|(_, e)| e.parse().ok())
        .unwrap_or(0);
    if exponent < -4 || exponent >= significant as i32 {
        trimmed(&exponent_text(f, significant - 1, upper))
    } else {
        let decimals = (significant as i32 - 1 - exponent).max(0) as usize;
        trimmed(&format!("{f:.decimals$}"))
    }
}

fn float_text(spec: &Spec, f: BaseFloat, precision: Option<usize>) -> String {
    let precision = precision.unwrap_or(6);
    let body = match spec.conversion {
        'e' => exponent_text(f, precision, false),
        'E' => exponent_text(f, precision, true),
        'g' => general_text(f, precision, false),
        'G' => general_text(f, precision, true),
        _ => format!("{f:.precision$}"),
    };
    let negative = body.starts_with('-');
    format!("{}{body}", prefix(spec.sign, negative))
}

/// `text` word-wrapped to `width`: paragraphs split at newlines, words at
/// spaces, a word wider than the line cut into pieces.
fn wrapped(text: &str, width: usize) -> Vec<String> {
    let width = width.max(1);
    let mut lines = Vec::new();
    for paragraph in text.split('\n') {
        let mut line = String::new();
        let mut line_len = 0;
        for word in paragraph.split(' ').filter(|w| !w.is_empty()) {
            let word_len = word.chars().count();
            if line_len > 0 && line_len + 1 + word_len <= width {
                line.push(' ');
                line.push_str(word);
                line_len += 1 + word_len;
                continue;
            }
            if line_len > 0 {
                lines.push(std::mem::take(&mut line));
                line_len = 0;
            }
            if word_len <= width {
                line.push_str(word);
                line_len = word_len;
                continue;
            }
            let pieces: Vec<char> = word.chars().collect();
            for piece in pieces.chunks(width) {
                if piece.len() == width {
                    lines.push(piece.iter().collect());
                } else {
                    line = piece.iter().collect();
                    line_len = piece.len();
                }
            }
        }
        lines.push(line);
    }
    lines
}

/// `words` (one per line of the argument) laid out column-major in
/// `columns` columns of `width / columns`, `ls` style; the last column is
/// never padded.
fn table(words: &[&str], width: usize, columns: Option<usize>, align: Align) -> Vec<String> {
    if words.is_empty() {
        return Vec::new();
    }
    let longest = words.iter().map(|w| w.chars().count()).max().unwrap_or(0);
    let columns = columns
        .unwrap_or_else(|| width / (longest + 2))
        .clamp(1, words.len());
    let column_width = width / columns;
    let rows = words.len().div_ceil(columns);
    (0..rows)
        .map(|row| {
            let mut line = String::new();
            for column in 0..columns {
                let word = words.get(column * rows + row).copied().unwrap_or("");
                if column + 1 == columns {
                    line.push_str(word);
                } else {
                    line.push_str(&padded(word, Some(column_width), align, &Pad::Space));
                }
            }
            line
        })
        .collect()
}

/// Argument `number` (from 1) rendered under `spec`, with its `*` sizes
/// already resolved.
pub(super) fn field<const N: usize>(
    context: &EfunContext<'_, N>,
    spec: &Spec,
    align: Align,
    width: Option<usize>,
    precision: Option<usize>,
    value: &LpcRef,
    number: usize,
) -> Result<Field> {
    let wants = |expected: &str| {
        context.runtime_error(format!(
            "sprintf: argument {number} is {}, %{} wants {expected}",
            described(value),
            spec.conversion
        ))
    };
    let text = match spec.conversion {
        's' => {
            let Some(s) = value.as_str() else {
                return Err(wants("a string"));
            };
            let text: String = match precision {
                Some(p) if spec.mode == Mode::Plain => s.chars().take(p).collect(),
                _ => s.to_owned(),
            };
            match spec.mode {
                Mode::Plain => padded(&text, width, align, &spec.pad),
                Mode::Column => {
                    let Some(width) = width else {
                        return Err(context.runtime_error("sprintf: %= needs a field width"));
                    };
                    let lines = wrapped(&text, precision.unwrap_or(width))
                        .iter()
                        .map(|line| {
                            if line.is_empty() {
                                String::new()
                            } else {
                                padded(line, Some(width), align, &spec.pad)
                            }
                        })
                        .collect();
                    return Ok(Field {
                        lines,
                        kind: Kind::Column {
                            strip: spec.pad == Pad::Space,
                        },
                    });
                }
                Mode::Table => {
                    let Some(width) = width else {
                        return Err(context.runtime_error("sprintf: %# needs a field width"));
                    };
                    let words: Vec<&str> = text
                        .strip_suffix('\n')
                        .unwrap_or(&text)
                        .split('\n')
                        .collect();
                    return Ok(Field {
                        lines: table(&words, width, precision, align),
                        kind: Kind::Table,
                    });
                }
            }
        }
        'd' | 'i' | 'b' | 'o' | 'x' | 'X' => {
            let LpcRef::Int(i) = value else {
                return Err(wants("an int"));
            };
            padded_number(&int_text(spec, i.0), width, align, &spec.pad)
        }
        'e' | 'E' | 'f' | 'g' | 'G' => {
            let f = match value {
                LpcRef::Int(i) => i.0 as BaseFloat,
                LpcRef::Float(f) => f.0.into_inner(),
                _ => return Err(wants("a number")),
            };
            padded_number(&float_text(spec, f, precision), width, align, &spec.pad)
        }
        'c' => {
            let LpcRef::Int(i) = value else {
                return Err(wants("an int"));
            };
            let Some(c) = u32::try_from(i.0).ok().and_then(char::from_u32) else {
                return Err(
                    context.runtime_error(format!("sprintf: %c: {} is not a character", i.0))
                );
            };
            padded(&c.to_string(), width, align, &spec.pad)
        }
        _ => {
            let dumped =
                crate::interpreter::efun::dump::format_ref(value, context, context.txn(), 0, 0)?;
            padded(&dumped, width, align, &spec.pad)
        }
    };
    Ok(Field {
        lines: vec![text],
        kind: Kind::Plain,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn centering_puts_the_odd_space_on_the_left() {
        assert_eq!(padded("123", Some(6), Align::Center, &Pad::Space), "  123 ");
    }

    #[test]
    fn a_zero_pad_goes_behind_the_sign() {
        assert_eq!(
            padded_number("-12", Some(5), Align::Right, &Pad::Zero),
            "-0012"
        );
    }

    #[test]
    fn the_exponent_form_is_signed_and_two_digits_wide() {
        assert_eq!(exponent_text(123.5, 4, false), "1.2350e+02");
        assert_eq!(exponent_text(0.001, 1, true), "1.0E-03");
    }

    #[test]
    fn the_general_form_drops_trailing_zeroes() {
        assert_eq!(general_text(123.5, 6, false), "123.5");
        assert_eq!(general_text(100.0, 6, false), "100");
        assert_eq!(general_text(1e20, 6, false), "1e+20");
    }

    #[test]
    fn wrapping_cuts_a_word_wider_than_the_line() {
        assert_eq!(wrapped("ab sentence", 6), ["ab", "senten", "ce"]);
    }

    #[test]
    fn wrapping_keeps_an_empty_trailing_paragraph() {
        assert_eq!(wrapped("a b\n", 3), ["a b", ""]);
    }

    #[test]
    fn a_table_picks_as_many_columns_as_fit_the_longest_word_plus_two() {
        let rows = table(&["one", "two", "three"], 20, None, Align::Left);
        // 20 / (5 + 2) = 2 columns of 10.
        assert_eq!(rows, ["one       three", "two       "]);
    }
}
