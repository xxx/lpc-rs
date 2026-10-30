//! `to_int`, `to_float` and `to_string` convert values; LPC casts assert a
//! type without converting the value.

use lpc_rs_core::BaseFloat;
use lpc_rs_core::null_mode::NullMode;
use lpc_rs_errors::Result;

use crate::interpreter::{
    efun::{efun_context::EfunContext, in_game_name},
    lpc_float::LpcFloat,
    lpc_ref::LpcRef,
};

/// The leading integer of `s`: leading whitespace skipped, an optional
/// sign, then digits; 0 with no digits, `Err` with the digits when they
/// pass the int range.
fn leading_int(s: &str) -> std::result::Result<i64, &str> {
    let s = s.trim_start();
    let unsigned = s.strip_prefix(['+', '-']).unwrap_or(s);
    let digits = unsigned.len()
        - unsigned
            .trim_start_matches(|c: char| c.is_ascii_digit())
            .len();
    if digits == 0 {
        return Ok(0);
    }
    let number = &s[..s.len() - unsigned.len() + digits];
    number.parse().map_err(|_| number)
}

/// The leading number of `s`: leading whitespace skipped, an optional sign,
/// digits with an optional fraction and exponent; 0.0 with no digits.
fn leading_float(s: &str) -> BaseFloat {
    let s = s.trim_start();
    let mut end = s.strip_prefix(['+', '-']).map_or(0, |_| 1);
    let digits = |from: usize| {
        s[from..].len()
            - s[from..]
                .trim_start_matches(|c: char| c.is_ascii_digit())
                .len()
    };
    let whole = digits(end);
    end += whole;
    let mut fraction = 0;
    if s[end..].starts_with('.') {
        fraction = digits(end + 1);
        end += 1 + fraction;
    }
    if whole + fraction == 0 {
        return 0.0;
    }
    if let Some(rest) = s[end..].strip_prefix(['e', 'E']) {
        let sign = rest.strip_prefix(['+', '-']).map_or(0, |_| 1);
        let exponent = digits(end + 1 + sign);
        if exponent > 0 {
            end += 1 + sign + exponent;
        }
    }
    s[..end].parse().unwrap_or(0.0)
}

/// The refusal every conversion answers for a value it has no form for.
pub(super) fn cannot_convert<const N: usize>(
    context: &EfunContext<'_, N>,
    name: &str,
    value: &LpcRef,
) -> lpc_rs_errors::LpcError {
    context.runtime_error(format!("{name}: {} cannot be converted", value.type_name()))
}

/// `to_int(x)`: an int as it is, a float truncated toward zero, a string's
/// leading integer, a destructed object as 0 in zero mode.
pub fn to_int<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let result = match context.arg(0) {
        int @ LpcRef::Int(_) => int.clone(),
        LpcRef::Float(f) => LpcRef::from(f.0.into_inner() as i64),
        LpcRef::String(s) => match leading_int(s.to_str()) {
            Ok(i) => LpcRef::from(i),
            Err(digits) => {
                return Err(context.runtime_error(format!("to_int: {digits} is out of range")));
            }
        },
        object @ LpcRef::Object(_)
            if context.txn().null_mode() == NullMode::Zero
                && object.live_object(context.txn()).is_none() =>
        {
            LpcRef::from(0)
        }
        other => return Err(cannot_convert(context, "to_int", other)),
    };
    context.return_efun_result(result);
    Ok(())
}

/// `to_float(x)`: an int widened, a float as it is, a string's leading
/// number, a destructed object as 0.0 in zero mode.
pub fn to_float<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let value = match context.arg(0) {
        LpcRef::Int(i) => i.0 as BaseFloat,
        float @ LpcRef::Float(_) => {
            let result = float.clone();
            context.return_efun_result(result);
            return Ok(());
        }
        LpcRef::String(s) => leading_float(s.to_str()),
        object @ LpcRef::Object(_)
            if context.txn().null_mode() == NullMode::Zero
                && object.live_object(context.txn()).is_none() =>
        {
            0.0
        }
        other => return Err(cannot_convert(context, "to_float", other)),
    };
    context.return_efun_result(LpcRef::Float(LpcFloat::from(value)));
    Ok(())
}

/// `to_string(x)`: a number's text, a string as it is, an object's file
/// name, a destructed object as the selected null's text.
pub fn to_string<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let result = match context.arg(0) {
        number @ (LpcRef::Nil | LpcRef::Int(_) | LpcRef::Float(_)) => {
            LpcRef::from(number.to_string())
        }
        string @ LpcRef::String(_) => string.clone(),
        object @ LpcRef::Object(_) => match object.live_object(context.txn()) {
            Some(process) => LpcRef::from(in_game_name(context, &process)),
            None => LpcRef::from(context.txn().nil().to_string()),
        },
        LpcRef::Bytes(_) => {
            return Err(context
                .runtime_error("to_string: a bytes is converted with to_text(bytes, encoding)"));
        }
        other => return Err(cannot_convert(context, "to_string", other)),
    };
    context.return_efun_result(result);
    Ok(())
}

#[cfg(test)]
mod tests {
    use lpc_rs_core::null_mode::NullMode;
    use lpc_rs_utils::config::ConfigBuilder;

    use crate::{
        interpreter::lpc_ref::LpcRef,
        test_support::{run_prog, try_run_prog, try_run_prog_with_config},
    };

    async fn result_of(code: &str) -> LpcRef {
        run_prog(code).await.result().expect("a result")
    }

    async fn error_of(code: &str) -> String {
        match try_run_prog(code).await {
            Err(error) => error.to_string(),
            Ok(_) => panic!("conversion unexpectedly succeeded"),
        }
    }

    #[tokio::test]
    async fn numeric_conversions_reject_live_objects_in_both_null_modes() {
        for mode in [NullMode::Zero, NullMode::Distinct] {
            for (name, result_type) in [("to_int", "int"), ("to_float", "float")] {
                let config = crate::test_config_builder!()
                    .null_mode(mode)
                    .build()
                    .unwrap();
                let code = format!("{result_type} create() {{ return {name}(this_object()); }}");
                let error = match try_run_prog_with_config(&code, config.into()).await {
                    Err(error) => error.to_string(),
                    Ok(_) => panic!("{name}: a live object was accepted in {mode} mode"),
                };
                assert!(
                    error.contains(&format!("{name}: object cannot be converted")),
                    "{error}"
                );
            }
        }
    }

    #[tokio::test]
    async fn to_int_of_a_string_takes_its_leading_digits() {
        let r = result_of(r#"int create() { return to_int("12abc"); }"#).await;
        assert_eq!(r, LpcRef::from(12));
    }

    #[tokio::test]
    async fn to_int_of_a_string_skips_leading_space_and_takes_a_sign() {
        let r = result_of(r#"int create() { return to_int("  -7x"); }"#).await;
        assert_eq!(r, LpcRef::from(-7));
    }

    #[tokio::test]
    async fn to_int_of_a_string_with_no_digits_is_zero() {
        let r = result_of(r#"int create() { return to_int("abc"); }"#).await;
        assert_eq!(r, LpcRef::from(0));
    }

    #[tokio::test]
    async fn to_int_of_a_string_past_the_int_range_is_an_error() {
        let err = error_of(r#"int create() { return to_int("99999999999999999999"); }"#).await;
        assert!(
            err.contains("to_int: 99999999999999999999 is out of range"),
            "{err}"
        );
    }

    #[tokio::test]
    async fn to_int_truncates_a_float_toward_zero() {
        let r = result_of("int create() { return to_int(-2.7); }").await;
        assert_eq!(r, LpcRef::from(-2));
    }

    #[tokio::test]
    async fn to_int_of_an_int_is_itself() {
        let r = result_of("int create() { return to_int(5); }").await;
        assert_eq!(r, LpcRef::from(5));
    }

    #[tokio::test]
    async fn numeric_conversions_observe_destruction_in_both_null_modes() {
        for mode in [NullMode::Zero, NullMode::Distinct] {
            for (name, result_type, zero) in [
                ("to_int", "int", LpcRef::from(0)),
                ("to_float", "float", LpcRef::from(0.0)),
            ] {
                let config = crate::test_config_builder!()
                    .null_mode(mode)
                    .build()
                    .unwrap();
                let code = format!(
                    r#"
                        {result_type} create() {{
                            object target = clone_object("/clone_target");
                            destruct(target);
                            return {name}(target);
                        }}
                    "#
                );
                let result = try_run_prog_with_config(&code, config.into()).await;
                match mode {
                    NullMode::Zero => assert_eq!(result.unwrap().result(), Some(zero), "{name}"),
                    NullMode::Distinct => {
                        let error = match result {
                            Err(error) => error.to_string(),
                            Ok(_) => {
                                panic!("{name}: a destructed object was accepted in {mode} mode")
                            }
                        };
                        assert!(error.contains("cannot be converted"), "{error}");
                    }
                }
            }
        }
    }

    #[tokio::test]
    async fn to_int_of_an_array_is_an_error() {
        let err = error_of("int create() { return to_int(({ 1 })); }").await;
        assert!(err.contains("to_int: array cannot be converted"), "{err}");
    }

    #[tokio::test]
    async fn to_float_of_an_int_is_that_float() {
        let r = result_of("float create() { return to_float(3); }").await;
        assert_eq!(r, LpcRef::from(3.0));
    }

    #[tokio::test]
    async fn to_float_of_a_string_takes_its_leading_number() {
        for (input, expected) in [
            ("1.5abc", 1.5),
            ("1.25abc", 1.25),
            (".125tail", 0.125),
            ("-12.125tail", -12.125),
            ("12.tail", 12.0),
        ] {
            let code = format!(r#"float create() {{ return to_float("{input}"); }}"#);
            assert_eq!(result_of(&code).await, LpcRef::from(expected), "{input}");
        }
    }

    #[tokio::test]
    async fn to_float_of_a_string_takes_an_exponent() {
        for (input, expected) in [
            ("-2e3x", -2000.0),
            ("2e12tail", 2.0e12),
            ("2e+2tail", 200.0),
            ("5e-1tail", 0.5),
            ("1e", 1.0),
            ("1e+", 1.0),
            ("1e-", 1.0),
        ] {
            let code = format!(r#"float create() {{ return to_float("{input}"); }}"#);
            assert_eq!(result_of(&code).await, LpcRef::from(expected), "{input}");
        }
    }

    #[tokio::test]
    async fn to_float_of_a_string_with_no_number_is_zero() {
        let r = result_of(r#"float create() { return to_float("x"); }"#).await;
        assert_eq!(r, LpcRef::from(0.0));
    }

    #[tokio::test]
    async fn to_float_of_a_float_is_itself() {
        let r = result_of("float create() { return to_float(2.5); }").await;
        assert_eq!(r, LpcRef::from(2.5));
    }

    #[tokio::test]
    async fn to_float_of_a_mapping_is_an_error() {
        let err = error_of("float create() { return to_float(([ ])); }").await;
        assert!(
            err.contains("to_float: mapping cannot be converted"),
            "{err}"
        );
    }

    #[tokio::test]
    async fn to_string_of_an_int_is_its_digits() {
        let r = result_of("string create() { return to_string(42); }").await;
        assert_eq!(r.as_str(), Some("42"));
    }

    #[tokio::test]
    async fn to_string_of_a_float_is_its_text() {
        let r = result_of("string create() { return to_string(1.5); }").await;
        assert_eq!(r.as_str(), Some("1.5"));
    }

    #[tokio::test]
    async fn to_string_of_a_string_is_itself() {
        let r = result_of(r#"string create() { return to_string("hi"); }"#).await;
        assert_eq!(r.as_str(), Some("hi"));
    }

    #[tokio::test]
    async fn to_string_of_an_object_is_its_file_name() {
        let r = result_of("string create() { return to_string(this_object()); }").await;
        assert_eq!(r.as_str(), Some("/my_file"));
    }

    #[tokio::test]
    async fn to_string_of_a_function_is_an_error() {
        let err = error_of("string create() { return to_string((: 1 :)); }").await;
        assert!(
            err.contains("to_string: function cannot be converted"),
            "{err}"
        );
    }
}
