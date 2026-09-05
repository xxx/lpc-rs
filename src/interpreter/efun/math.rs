//! The float math efuns: trig, `exp` / `log` / `pow` / `sqrt`, `floor` /
//! `ceil`. An int argument is promoted; every result is a float.

use lpc_rs_errors::Result;

use crate::interpreter::{efun::efun_context::EfunContext, lpc_ref::LpcRef};

/// A value, or why the argument is outside the function's domain.
type Domain = std::result::Result<f64, String>;

/// Argument `i` as a float, an int promoted; anything else is an error.
fn number<const N: usize>(context: &EfunContext<'_, N>, name: &str, i: usize) -> Result<f64> {
    match context.arg(i) {
        LpcRef::Int(x) => Ok(x.0 as f64),
        LpcRef::Float(x) => Ok(x.0.into_inner()),
        other => {
            Err(context.runtime_error(format!("{name}: {} is not a number", other.type_name())))
        }
    }
}

/// Return `value`; an infinite or NaN result is the overflow error.
fn finish<const N: usize>(context: &mut EfunContext<'_, N>, name: &str, value: f64) -> Result<()> {
    if !value.is_finite() {
        return Err(context.runtime_error(format!("{name}: numeric overflow")));
    }
    context.return_efun_result(LpcRef::from(value));
    Ok(())
}

/// A one-argument efun computed by `f`.
fn unary<const N: usize>(
    context: &mut EfunContext<'_, N>,
    name: &str,
    f: impl FnOnce(f64) -> Domain,
) -> Result<()> {
    let x = number(context, name, 0)?;
    match f(x) {
        Ok(value) => finish(context, name, value),
        Err(why) => Err(context.runtime_error(format!("{name}: {why}"))),
    }
}

/// `x` when it is in `-1..1`, the domain of `asin` and `acos`.
fn unit(x: f64) -> Domain {
    if (-1.0..=1.0).contains(&x) {
        Ok(x)
    } else {
        Err(format!("{x} is out of range -1..1"))
    }
}

/// `sin(x)`: the sine of `x` radians.
pub fn sin<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    unary(context, "sin", |x| Ok(x.sin()))
}

/// `cos(x)`: the cosine of `x` radians.
pub fn cos<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    unary(context, "cos", |x| Ok(x.cos()))
}

/// `tan(x)`: the tangent of `x` radians.
pub fn tan<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    unary(context, "tan", |x| Ok(x.tan()))
}

/// `asin(x)`: the arc sine, in radians; `x` outside `-1..1` is an error.
pub fn asin<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    unary(context, "asin", |x| unit(x).map(f64::asin))
}

/// `acos(x)`: the arc cosine, in radians; `x` outside `-1..1` is an error.
pub fn acos<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    unary(context, "acos", |x| unit(x).map(f64::acos))
}

/// `atan(x)`: the arc tangent, in radians.
pub fn atan<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    unary(context, "atan", |x| Ok(x.atan()))
}

/// `atan2(y, x)`: the angle of the point `(x, y)`, in `-pi..pi`.
pub fn atan2<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let y = number(context, "atan2", 0)?;
    let x = number(context, "atan2", 1)?;
    finish(context, "atan2", y.atan2(x))
}

/// `exp(x)`: e to the power `x`; overflow is an error.
pub fn exp<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    unary(context, "exp", |x| Ok(x.exp()))
}

/// `log(x)`: the natural logarithm; `x <= 0` is an error.
pub fn log<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    unary(context, "log", |x| {
        if x > 0.0 {
            Ok(x.ln())
        } else {
            Err(format!("{x} is not positive"))
        }
    })
}

/// `pow(x, y)`: `x` to the power `y`, always a float. Zero to a negative
/// power, a negative base to a fractional power, and overflow are errors.
pub fn pow<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let x = number(context, "pow", 0)?;
    let y = number(context, "pow", 1)?;
    if x == 0.0 && y < 0.0 {
        return Err(context.runtime_error("pow: 0 raised to a negative power"));
    }
    if x < 0.0 && y.fract() != 0.0 {
        return Err(context.runtime_error(format!("pow: {x} raised to the fractional power {y}")));
    }
    finish(context, "pow", x.powf(y))
}

/// `sqrt(x)`: the square root; a negative `x` is an error.
pub fn sqrt<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    unary(context, "sqrt", |x| {
        if x < 0.0 {
            Err(format!("{x} is negative"))
        } else {
            Ok(x.sqrt())
        }
    })
}

/// `floor(x)`: the largest whole number not above `x`, as a float.
pub fn floor<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    unary(context, "floor", |x| Ok(x.floor()))
}

/// `ceil(x)`: the smallest whole number not below `x`, as a float.
pub fn ceil<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    unary(context, "ceil", |x| Ok(x.ceil()))
}

#[cfg(test)]
mod tests {
    use std::f64::consts::{E, FRAC_PI_2, FRAC_PI_4};

    use crate::{
        interpreter::lpc_ref::LpcRef,
        test_support::{run_prog, try_run_prog},
    };

    async fn float_of(expr: &str) -> f64 {
        let code = format!("float create() {{ return {expr}; }}");
        match run_prog(&code).await.result() {
            Some(LpcRef::Float(f)) => f.0.into_inner(),
            other => panic!("{expr}: a float, actually {other:?}"),
        }
    }

    async fn error_of(expr: &str) -> String {
        let code = format!("mixed create() {{ return {expr}; }}");
        try_run_prog(&code).await.unwrap_err().to_string()
    }

    #[tokio::test]
    async fn the_trig_functions_take_radians() {
        assert_eq!(float_of("sin(0.0)").await, 0.0);
        assert_eq!(float_of("cos(0.0)").await, 1.0);
        assert_eq!(float_of("tan(1.0)").await, 1.0_f64.tan());
        assert_eq!(float_of("asin(1.0)").await, FRAC_PI_2);
        assert_eq!(float_of("acos(1.0)").await, 0.0);
        assert_eq!(float_of("atan(1.0)").await, FRAC_PI_4);
    }

    #[tokio::test]
    async fn an_int_argument_is_promoted_to_float() {
        assert_eq!(float_of("cos(0)").await, 1.0);
        assert_eq!(float_of("sqrt(4)").await, 2.0);
        assert_eq!(float_of("floor(5)").await, 5.0);
        assert_eq!(float_of("ceil(4)").await, 4.0);
        assert_eq!(float_of("exp(0)").await, 1.0);
    }

    #[tokio::test]
    async fn asin_and_acos_outside_the_unit_interval_are_errors() {
        let err = error_of("asin(2.0)").await;
        assert!(err.contains("asin: 2 is out of range -1..1"), "{err}");
        let err = error_of("acos(-1.5)").await;
        assert!(err.contains("acos: -1.5 is out of range -1..1"), "{err}");
    }

    #[tokio::test]
    async fn atan2_takes_y_then_x() {
        assert_eq!(float_of("atan2(1.0, 1.0)").await, FRAC_PI_4);
        assert_eq!(float_of("atan2(1, 0)").await, FRAC_PI_2);
    }

    #[tokio::test]
    async fn exp_of_one_is_e() {
        assert_eq!(float_of("exp(1.0)").await, E);
    }

    #[tokio::test]
    async fn exp_overflow_is_an_error() {
        let err = error_of("exp(1000)").await;
        assert!(err.contains("exp: numeric overflow"), "{err}");
    }

    #[tokio::test]
    async fn log_is_the_natural_logarithm() {
        assert_eq!(float_of("log(1.0)").await, 0.0);
        assert_eq!(float_of("log(exp(2.0))").await, 2.0_f64.exp().ln());
    }

    #[tokio::test]
    async fn log_of_a_non_positive_number_is_an_error() {
        let err = error_of("log(0)").await;
        assert!(err.contains("log: 0 is not positive"), "{err}");
        let err = error_of("log(-1.0)").await;
        assert!(err.contains("log: -1 is not positive"), "{err}");
    }

    #[tokio::test]
    async fn pow_returns_a_float_for_int_arguments() {
        assert_eq!(float_of("pow(2, 3)").await, 8.0);
        assert_eq!(float_of("pow(-2, 3)").await, -8.0);
        assert_eq!(float_of("pow(2.0, 0.5)").await, 2.0_f64.sqrt());
    }

    #[tokio::test]
    async fn pow_of_zero_to_a_negative_power_is_an_error() {
        let err = error_of("pow(0, -1)").await;
        assert!(err.contains("pow: 0 raised to a negative power"), "{err}");
    }

    #[tokio::test]
    async fn pow_of_a_negative_base_to_a_fractional_power_is_an_error() {
        let err = error_of("pow(-8.0, 1.0 / 3.0)").await;
        assert!(
            err.contains("pow: -8 raised to the fractional power"),
            "{err}"
        );
    }

    #[tokio::test]
    async fn pow_overflow_is_an_error() {
        let err = error_of("pow(10.0, 400)").await;
        assert!(err.contains("pow: numeric overflow"), "{err}");
    }

    #[tokio::test]
    async fn sqrt_of_a_negative_number_is_an_error() {
        let err = error_of("sqrt(-1)").await;
        assert!(err.contains("sqrt: -1 is negative"), "{err}");
    }

    #[tokio::test]
    async fn floor_and_ceil_round_toward_the_named_infinity() {
        assert_eq!(float_of("floor(4.5)").await, 4.0);
        assert_eq!(float_of("floor(-4.5)").await, -5.0);
        assert_eq!(float_of("ceil(4.5)").await, 5.0);
        assert_eq!(float_of("ceil(-4.5)").await, -4.0);
    }

    #[tokio::test]
    async fn a_non_number_is_an_error() {
        let code = r#"mixed create() { mixed s = "a"; return sin(s); }"#;
        let err = try_run_prog(code).await.unwrap_err().to_string();
        assert!(err.contains("sin: string is not a number"), "{err}");
        let code = r#"mixed create() { mixed s = "a"; return pow(2, s); }"#;
        let err = try_run_prog(code).await.unwrap_err().to_string();
        assert!(err.contains("pow: string is not a number"), "{err}");
    }
}
