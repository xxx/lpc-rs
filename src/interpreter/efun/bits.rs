//! `set_bit`, `clear_bit` and `test_bit`: bit strings, six bits per
//! character counted up from `' '`, the lowest bits in the first character.

use lpc_rs_errors::Result;

use crate::interpreter::{efun::efun_context::EfunContext, lpc_ref::LpcRef};

/// The highest bit number accepted, CD's; LDMud stops at 6144, FluffOS at 12000.
pub const MAX_BITS: i64 = 49152;

/// The string and the bit number, the number checked against [`MAX_BITS`].
fn arguments<'a, const N: usize>(
    context: &'a EfunContext<'_, N>,
    name: &str,
) -> Result<(&'a str, usize)> {
    let Some(s) = context.arg(0).as_str() else {
        return Err(context.runtime_error(format!(
            "{name}: {} is not a string",
            context.arg(0).type_name()
        )));
    };
    let LpcRef::Int(n) = context.arg(1) else {
        return Err(context.runtime_error(format!(
            "{name}: {} is not an int",
            context.arg(1).type_name()
        )));
    };
    let n = n.0;
    if n < 0 {
        return Err(context.runtime_error(format!("{name}: negative bit number {n}")));
    }
    if n > MAX_BITS {
        return Err(context.runtime_error(format!(
            "{name}: bit number {n} is above the maximum {MAX_BITS}"
        )));
    }
    Ok((s, n as usize))
}

/// The six-bit value of character `i` of `s`; one outside `' '..='_'` is an error.
fn value<const N: usize>(
    context: &EfunContext<'_, N>,
    name: &str,
    s: &str,
    i: usize,
) -> Result<u8> {
    let c = s.as_bytes()[i];
    if !(b' '..=b'_').contains(&c) {
        return Err(context.runtime_error(format!(
            "{name}: character {i} of {s:?} is not a bit pattern"
        )));
    }
    Ok(c - b' ')
}

/// `s` with character `i` replaced by the value `v`.
fn with_value(s: &str, i: usize, v: u8) -> String {
    let mut bytes = s.as_bytes().to_vec();
    bytes[i] = b' ' + v;
    String::from_utf8(bytes).expect("an ASCII byte replaced an ASCII byte")
}

/// `set_bit(s, n)`: `s` with bit `n` set, extended with spaces as needed.
pub fn set_bit<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let (s, n) = arguments(context, "set_bit")?;
    let (i, mask) = (n / 6, 1u8 << (n % 6));
    let result = if i < s.len() {
        let v = value(context, "set_bit", s, i)?;
        with_value(s, i, v | mask)
    } else {
        let mut extended = String::with_capacity(i + 1);
        extended.push_str(s);
        extended.extend(std::iter::repeat_n(' ', i - s.len()));
        extended.push((b' ' + mask) as char);
        extended
    };
    context.return_efun_result(LpcRef::from(result));
    Ok(())
}

/// `clear_bit(s, n)`: `s` with bit `n` cleared; a bit past the end is
/// already clear, so `s` comes back as it is.
pub fn clear_bit<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let (s, n) = arguments(context, "clear_bit")?;
    let (i, mask) = (n / 6, 1u8 << (n % 6));
    let result = if i < s.len() {
        let v = value(context, "clear_bit", s, i)?;
        LpcRef::from(with_value(s, i, v & !mask))
    } else {
        context.arg(0).clone()
    };
    context.return_efun_result(result);
    Ok(())
}

/// `test_bit(s, n)`: 1 when bit `n` of `s` is set, 0 otherwise or past the end.
pub fn test_bit<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let (s, n) = arguments(context, "test_bit")?;
    let (i, mask) = (n / 6, 1u8 << (n % 6));
    let set = i < s.len() && value(context, "test_bit", s, i)? & mask != 0;
    context.return_efun_result(LpcRef::from(i64::from(set)));
    Ok(())
}

#[cfg(test)]
mod tests {
    use crate::{
        interpreter::lpc_ref::LpcRef,
        test_support::{run_prog, strings_of, try_run_prog},
    };

    async fn error_of(expr: &str) -> String {
        let code = format!("mixed create() {{ return {expr}; }}");
        try_run_prog(&code).await.unwrap_err().to_string()
    }

    #[tokio::test]
    async fn set_bit_sets_one_of_six_bits_in_a_character() {
        // "?" is 31; bit 5 makes 63, "_".
        let code = r#"string *create() { return ({ set_bit("?", 5), set_bit("78", 3), set_bit(set_bit("78", 3), 8) }); }"#;
        assert_eq!(strings_of(code).await, ["_", "?8", "?<"]);
    }

    #[tokio::test]
    async fn set_bit_extends_the_string_with_spaces() {
        let code = r#"string *create() { return ({ set_bit("", 6), set_bit("_", 12) }); }"#;
        assert_eq!(strings_of(code).await, [" !", "_ !"]);
    }

    #[tokio::test]
    async fn clear_bit_clears_one_of_six_bits_in_a_character() {
        let code = r#"string *create() { return ({ clear_bit("_", 5), clear_bit(clear_bit("?<", 3), 8) }); }"#;
        assert_eq!(strings_of(code).await, ["?", "78"]);
    }

    #[tokio::test]
    async fn clear_bit_past_the_end_leaves_the_string_as_it_is() {
        let code = r#"string create() { return clear_bit("?", 100); }"#;
        assert_eq!(run_prog(code).await.result(), Some(LpcRef::from("?")));
    }

    #[tokio::test]
    async fn test_bit_reads_one_bit() {
        let code = r#"int *create() { return ({ test_bit("_", 5), test_bit(" ", 3), test_bit("?<", 8), test_bit("?", 100) }); }"#;
        assert_eq!(strings_of(code).await, ["1", "0", "1", "0"]);
    }

    #[tokio::test]
    async fn a_negative_bit_number_is_an_error() {
        let err = error_of(r#"set_bit("?", -1)"#).await;
        assert!(err.contains("set_bit: negative bit number -1"), "{err}");
        let err = error_of(r#"test_bit("?", -1)"#).await;
        assert!(err.contains("test_bit: negative bit number -1"), "{err}");
    }

    #[tokio::test]
    async fn a_bit_number_above_the_maximum_is_an_error() {
        let err = error_of(r#"set_bit("?", 49153)"#).await;
        assert!(
            err.contains("set_bit: bit number 49153 is above the maximum 49152"),
            "{err}"
        );
        let err = error_of(r#"clear_bit("?", 49153)"#).await;
        assert!(
            err.contains("clear_bit: bit number 49153 is above the maximum 49152"),
            "{err}"
        );
    }

    #[tokio::test]
    async fn the_maximum_bit_number_itself_is_allowed() {
        let code = r#"int create() { return test_bit(set_bit("", 49152), 49152); }"#;
        assert_eq!(run_prog(code).await.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn a_character_outside_the_bit_alphabet_is_an_error() {
        let err = error_of(r#"set_bit("?~", 6)"#).await;
        assert!(
            err.contains(r#"set_bit: character 1 of "?~" is not a bit pattern"#),
            "{err}"
        );
        let err = error_of(r#"clear_bit("a", 0)"#).await;
        assert!(
            err.contains(r#"clear_bit: character 0 of "a" is not a bit pattern"#),
            "{err}"
        );
        let err = error_of(r#"test_bit("a", 0)"#).await;
        assert!(
            err.contains(r#"test_bit: character 0 of "a" is not a bit pattern"#),
            "{err}"
        );
    }

    #[tokio::test]
    async fn a_non_string_or_non_int_argument_is_an_error() {
        let code = r#"mixed create() { mixed n = 1; return set_bit(n, 0); }"#;
        let err = try_run_prog(code).await.unwrap_err().to_string();
        assert!(err.contains("set_bit: int is not a string"), "{err}");
        let code = r#"mixed create() { mixed s = "a"; return test_bit("?", s); }"#;
        let err = try_run_prog(code).await.unwrap_err().to_string();
        assert!(err.contains("test_bit: string is not an int"), "{err}");
    }
}
