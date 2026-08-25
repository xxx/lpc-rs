//! `sscanf`: a format-driven scan of a string into by-reference variables.

use lpc_rs_core::RegisterSize;
use lpc_rs_errors::Result;

use crate::interpreter::{efun::efun_context::EfunContext, lpc_float::LpcFloat, lpc_ref::LpcRef};

/// One `%` conversion.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Conversion {
    Int,
    Hex,
    Float,
    Str,
}

/// One piece of a format: literal text, a conversion (skipped or assigned),
/// or a literal `%`.
#[derive(Clone, Debug, PartialEq, Eq)]
enum Piece<'f> {
    Literal(&'f str),
    Convert { kind: Conversion, skip: bool },
    Percent,
}

/// Split `fmt` into pieces; the errors are the spec's, `sscanf: ` prefixed by
/// the caller.
fn pieces(fmt: &str) -> std::result::Result<Vec<Piece<'_>>, String> {
    let mut out = Vec::new();
    let mut rest = fmt;
    while !rest.is_empty() {
        let Some(at) = rest.find('%') else {
            out.push(Piece::Literal(rest));
            break;
        };
        if at > 0 {
            out.push(Piece::Literal(&rest[..at]));
        }
        let mut chars = rest[at + 1..].chars();
        let Some(mut c) = chars.next() else {
            return Err("the format cannot end in %".to_owned());
        };
        let skip = c == '*';
        if skip {
            let Some(next) = chars.next() else {
                return Err("the format cannot end in %".to_owned());
            };
            c = next;
        }
        let piece = match c {
            '%' if !skip => Piece::Percent,
            'd' => Piece::Convert {
                kind: Conversion::Int,
                skip,
            },
            'x' => Piece::Convert {
                kind: Conversion::Hex,
                skip,
            },
            'f' => Piece::Convert {
                kind: Conversion::Float,
                skip,
            },
            's' => Piece::Convert {
                kind: Conversion::Str,
                skip,
            },
            other => return Err(format!("%{other} is not a conversion")),
        };
        if let (
            Some(Piece::Convert {
                kind: Conversion::Str,
                ..
            }),
            Piece::Convert {
                kind: Conversion::Str,
                ..
            },
        ) = (out.last(), &piece)
        {
            return Err("two adjacent %s in the format".to_owned());
        }
        out.push(piece);
        rest = chars.as_str();
    }
    Ok(out)
}

/// A scanned value and how much input it consumed.
struct Scanned {
    value: LpcRef,
    len: usize,
}

/// `strtoll`-style: optional leading whitespace, optional sign, digits in
/// `radix` (a `0x` prefix when 16); None when no digit follows or the value
/// does not fit.
fn scan_int(input: &str, radix: u32) -> Option<Scanned> {
    let trimmed = input.trim_start();
    let mut i = input.len() - trimmed.len();
    let mut negative = false;
    if trimmed.starts_with('-') {
        negative = true;
        i += 1;
    } else if trimmed.starts_with('+') {
        i += 1;
    }
    if radix == 16 {
        let after_sign = &input[i..];
        if after_sign.len() >= 3
            && (after_sign.starts_with("0x") || after_sign.starts_with("0X"))
            && after_sign.as_bytes()[2].is_ascii_hexdigit()
        {
            i += 2;
        }
    }
    let digits: usize = input[i..]
        .chars()
        .take_while(|c| c.is_digit(radix))
        .map(char::len_utf8)
        .sum();
    if digits == 0 {
        return None;
    }
    let magnitude = i64::from_str_radix(&input[i..i + digits], radix).ok()?;
    let value = if negative {
        magnitude.checked_neg()?
    } else {
        magnitude
    };
    Some(Scanned {
        value: LpcRef::from(value),
        len: i + digits,
    })
}

/// `strtod`-style: the longest prefix Rust's float parser accepts.
fn scan_float(input: &str) -> Option<Scanned> {
    let trimmed = input.trim_start();
    let lead = input.len() - trimmed.len();
    let mut best = None;
    for end in (1..=trimmed.len()).filter(|&e| trimmed.is_char_boundary(e)) {
        if let Ok(f) = trimmed[..end].parse::<f64>() {
            // A trailing 'e' or '.' parses as part of a longer token only.
            best = Some((f, end));
        }
    }
    let (f, end) = best?;
    Some(Scanned {
        value: LpcRef::from(LpcFloat::from(f)),
        len: lead + end,
    })
}

/// Where a `%s` ends: at the first place the next piece can start. A
/// literal that never appears fails the `%s`; a conversion or `%%` that
/// never appears lets the `%s` take the rest and fails afterwards, as
/// MudOS does.
fn stop_for(next: Option<&Piece<'_>>, input: &str) -> Option<usize> {
    let bytes = input.as_bytes();
    match next {
        None => Some(input.len()),
        Some(Piece::Literal(lit)) => input.find(lit),
        Some(Piece::Percent) => Some(input.find('%').unwrap_or(input.len())),
        Some(Piece::Convert {
            kind: Conversion::Int,
            ..
        }) => Some(
            input
                .find(|c: char| c.is_ascii_digit())
                .unwrap_or(input.len()),
        ),
        Some(Piece::Convert {
            kind: Conversion::Float,
            ..
        }) => Some(
            (0..bytes.len())
                .find(|&i| {
                    bytes[i].is_ascii_digit()
                        || (bytes[i] == b'.' && bytes.get(i + 1).is_some_and(u8::is_ascii_digit))
                })
                .unwrap_or(input.len()),
        ),
        Some(Piece::Convert {
            kind: Conversion::Hex,
            ..
        }) => Some(
            (0..bytes.len())
                .find(|&i| {
                    bytes[i] == b'0'
                        && matches!(bytes.get(i + 1), Some(b'x' | b'X'))
                        && bytes.get(i + 2).is_some_and(u8::is_ascii_hexdigit)
                })
                .unwrap_or(input.len()),
        ),
        // Unreachable: `pieces` refuses adjacent %s.
        Some(Piece::Convert {
            kind: Conversion::Str,
            ..
        }) => None,
    }
}

/// `int sscanf(string str, string fmt, mixed var...)`: the number of
/// conversions matched; variables are written through their cells.
pub async fn sscanf<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let input = context.resolve_local_register(1 as RegisterSize).clone();
    let fmt = context.resolve_local_register(2 as RegisterSize).clone();
    let (Some(input), Some(fmt)) = (input.as_str(), fmt.as_str()) else {
        context.return_efun_result(LpcRef::from(0));
        return Ok(());
    };
    let pieces = pieces(fmt).map_err(|e| context.runtime_error(format!("sscanf: {e}")))?;
    let variables = usize::from(context.frame().called_with_num_args).saturating_sub(2);
    let needed = pieces
        .iter()
        .filter(|p| matches!(p, Piece::Convert { skip: false, .. }))
        .count();
    if needed > variables {
        return Err(context.runtime_error("sscanf: too few arguments for the format"));
    }

    let mut rest = input;
    let mut matched: i64 = 0;
    let mut next_var: RegisterSize = 2;
    let mut writes: Vec<(RegisterSize, LpcRef)> = Vec::new();
    let mut i = 0;
    while i < pieces.len() {
        match &pieces[i] {
            Piece::Literal(lit) => {
                let Some(after) = rest.strip_prefix(lit) else {
                    break;
                };
                rest = after;
            }
            Piece::Percent => {
                let Some(after) = rest.strip_prefix('%') else {
                    break;
                };
                rest = after;
            }
            Piece::Convert { kind, skip } => {
                let scanned = match kind {
                    Conversion::Int => scan_int(rest, 10),
                    Conversion::Hex => scan_int(rest, 16),
                    Conversion::Float => scan_float(rest),
                    Conversion::Str => stop_for(pieces.get(i + 1), rest).map(|end| Scanned {
                        value: LpcRef::from(&rest[..end]),
                        len: end,
                    }),
                };
                let Some(Scanned { value, len }) = scanned else {
                    break;
                };
                rest = &rest[len..];
                matched += 1;
                if !skip {
                    writes.push((next_var, value));
                    next_var += 1;
                }
            }
        }
        i += 1;
    }
    // Every piece matched, input remains, a variable is unused: it takes the rest.
    if i == pieces.len() && !rest.is_empty() && usize::from(next_var) - 2 < variables {
        writes.push((next_var, LpcRef::from(rest)));
        matched += 1;
    }
    for (index, value) in writes {
        context.write_ref(index, value)?;
    }
    context.return_efun_result(LpcRef::from(matched));
    Ok(())
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use crate::{
        interpreter::{lpc_ref::LpcRef, vm::Vm},
        test_support::test_config,
    };

    /// `create()`'s result for a master that returns an array of what it scanned.
    async fn scan(body: &str) -> Vec<LpcRef> {
        let code = format!("mixed *create() {{ {body} }}");
        let vm = Vm::new(test_config());
        let proc = vm
            .initialize_process_from_code("/master.c", &code)
            .await
            .unwrap();
        let result = proc.result().unwrap();
        result
            .with_array(proc.context.txn(), |a| a.iter().cloned().collect())
            .unwrap()
    }

    async fn scan_fails(body: &str) -> String {
        let code = format!("mixed *create() {{ {body} }}");
        let vm = Vm::new(test_config());
        vm.initialize_process_from_code("/master.c", &code)
            .await
            .unwrap_err()
            .to_string()
    }

    #[tokio::test]
    async fn d_and_s_split_a_line() {
        let r = scan(r#"int n; string s; int c = sscanf("get 3 coins", "get %d %s", n, s); return ({ c, n, s });"#).await;
        assert_eq!(
            r,
            vec![LpcRef::from(2), LpcRef::from(3), LpcRef::from("coins")]
        );
    }

    #[tokio::test]
    async fn s_before_a_literal_takes_the_shortest_prefix() {
        let r = scan(r#"string a, b; int c = sscanf("say hi to bob to sam", "say %s to %s", a, b); return ({ c, a, b });"#).await;
        assert_eq!(
            r,
            vec![
                LpcRef::from(2),
                LpcRef::from("hi"),
                LpcRef::from("bob to sam")
            ]
        );
    }

    #[tokio::test]
    async fn s_s_with_a_space_splits_at_the_first_word() {
        let r = scan(r#"string a, b; sscanf("one two three", "%s %s", a, b); return ({ a, b });"#)
            .await;
        assert_eq!(r, vec![LpcRef::from("one"), LpcRef::from("two three")]);
    }

    #[tokio::test]
    async fn s_before_d_stops_at_the_first_digit() {
        let r =
            scan(r#"string a; int n; sscanf("level 42", "%s%d", a, n); return ({ a, n });"#).await;
        assert_eq!(r, vec![LpcRef::from("level "), LpcRef::from(42)]);
    }

    #[tokio::test]
    async fn d_accepts_leading_whitespace_and_a_sign() {
        let r = scan(r#"int n; int c = sscanf("  -17", "%d", n); return ({ c, n });"#).await;
        assert_eq!(r, vec![LpcRef::from(1), LpcRef::from(-17)]);
    }

    #[tokio::test]
    async fn x_reads_hex_with_or_without_a_prefix() {
        let r = scan(r#"int a, b; sscanf("ff 0x10", "%x %x", a, b); return ({ a, b });"#).await;
        assert_eq!(r, vec![LpcRef::from(255), LpcRef::from(16)]);
    }

    #[tokio::test]
    async fn s_before_x_needs_the_prefix() {
        let r = scan(
            r#"string a; int n; int c = sscanf("id 0x1f", "%s%x", a, n); return ({ c, a, n });"#,
        )
        .await;
        assert_eq!(
            r,
            vec![LpcRef::from(2), LpcRef::from("id "), LpcRef::from(31)]
        );
        let r = scan(
            r#"string a; int n; int c = sscanf("id 31", "%s%x", a, n); return ({ c, a, n });"#,
        )
        .await;
        assert_eq!(
            r,
            vec![LpcRef::from(1), LpcRef::from("id 31"), LpcRef::from(0)]
        );
    }

    #[tokio::test]
    async fn f_reads_a_float() {
        let r =
            scan(r#"float f; int c = sscanf("4.43e-2 left", "%f left", f); return ({ c, f });"#)
                .await;
        assert_eq!(r, vec![LpcRef::from(1), LpcRef::from(4.43e-2)]);
    }

    #[tokio::test]
    async fn percent_percent_matches_a_percent() {
        let r =
            scan(r#"int n; int c = sscanf("50% off", "%d%% off", n); return ({ c, n });"#).await;
        assert_eq!(r, vec![LpcRef::from(1), LpcRef::from(50)]);
    }

    #[tokio::test]
    async fn s_before_percent_percent_stops_at_the_percent() {
        let r = scan(r#"string s; int c = sscanf("half%", "%s%%", s); return ({ c, s });"#).await;
        assert_eq!(r, vec![LpcRef::from(1), LpcRef::from("half")]);
    }

    #[tokio::test]
    async fn a_skip_consumes_no_variable_and_counts() {
        let r = scan(
            r#"string s; int c = sscanf("drop 3 coins", "drop %*d %s", s); return ({ c, s });"#,
        )
        .await;
        assert_eq!(r, vec![LpcRef::from(2), LpcRef::from("coins")]);
    }

    #[tokio::test]
    async fn matching_stops_at_the_first_failure_and_keeps_earlier_assignments() {
        let r = scan(r#"int n = -1; string s = "old"; int c = sscanf("7 apples", "%d oranges %s", n, s); return ({ c, n, s });"#).await;
        assert_eq!(
            r,
            vec![LpcRef::from(1), LpcRef::from(7), LpcRef::from("old")]
        );
    }

    #[tokio::test]
    async fn leftover_input_goes_to_the_next_unused_variable() {
        let r = scan(r#"int n; string rest; int c = sscanf("3 red apples", "%d", n, rest); return ({ c, n, rest });"#).await;
        assert_eq!(
            r,
            vec![
                LpcRef::from(2),
                LpcRef::from(3),
                LpcRef::from(" red apples")
            ]
        );
    }

    #[tokio::test]
    async fn a_trailing_s_matches_the_empty_rest() {
        // MudOS: `%s` at the end of the format takes the rest, even "".
        let r = scan(r#"string s = "old"; int c = sscanf("", "%s", s); return ({ c, s });"#).await;
        assert_eq!(r, vec![LpcRef::from(1), LpcRef::from("")]);
    }

    #[tokio::test]
    async fn a_leading_literal_that_fails_matches_nothing() {
        let r =
            scan(r#"string s = "old"; int c = sscanf("", "go %s", s); return ({ c, s });"#).await;
        assert_eq!(r, vec![LpcRef::from(0), LpcRef::from("old")]);
    }

    #[tokio::test]
    async fn a_d_that_does_not_fit_an_int_is_no_match() {
        let r = scan(
            r#"int n = 5; int c = sscanf("99999999999999999999", "%d", n); return ({ c, n });"#,
        )
        .await;
        assert_eq!(r, vec![LpcRef::from(0), LpcRef::from(5)]);
    }

    #[tokio::test]
    async fn literal_text_must_match_exactly() {
        let r = scan(r#"int n; int c = sscanf("Take 3", "take %d", n); return ({ c });"#).await;
        assert_eq!(r, vec![LpcRef::from(0)]);
    }

    #[tokio::test]
    async fn adjacent_s_is_an_error() {
        let err = scan_fails(r#"string a, b; sscanf("x", "%s%s", a, b); return ({});"#).await;
        assert!(
            err.contains("sscanf: two adjacent %s in the format"),
            "{err}"
        );
    }

    #[tokio::test]
    async fn a_format_ending_in_percent_is_an_error() {
        let err = scan_fails(r#"sscanf("x", "abc%"); return ({});"#).await;
        assert!(err.contains("sscanf: the format cannot end in %"), "{err}");
    }

    #[tokio::test]
    async fn an_unknown_conversion_is_an_error() {
        let err = scan_fails(r#"int n; sscanf("x", "%q", n); return ({});"#).await;
        assert!(err.contains("sscanf: %q is not a conversion"), "{err}");
    }

    #[tokio::test]
    async fn too_few_variables_is_an_error() {
        let err = scan_fails(r#"int n; sscanf("1 2", "%d %d", n); return ({});"#).await;
        assert!(
            err.contains("sscanf: too few arguments for the format"),
            "{err}"
        );
    }

    #[tokio::test]
    async fn a_global_variable_receives_the_value() {
        let code = indoc! { r#"
            int n;
            int create() { sscanf("hp 12", "hp %d", n); return n; }
        "# };
        let vm = Vm::new(test_config());
        let proc = vm
            .initialize_process_from_code("/master.c", code)
            .await
            .unwrap();
        assert_eq!(proc.result(), Some(LpcRef::from(12)));
    }
}
