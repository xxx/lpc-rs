use lpc_rs_errors::Result;

use crate::interpreter::{efun::efun_context::EfunContext, lpc_float::LpcFloat, lpc_ref::LpcRef};

/// `abs(x)`: the magnitude of an int or float, as the same type. The most
/// negative int is its own magnitude, as `-` wraps.
pub fn abs<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let result = match context.arg(0) {
        LpcRef::Int(x) => LpcRef::from(x.0.wrapping_abs()),
        LpcRef::Float(x) => LpcRef::Float(LpcFloat::from(x.0.into_inner().abs())),
        other => {
            return Err(
                context.runtime_error(format!("abs: {} is not a number", other.type_name()))
            );
        }
    };
    context.return_efun_result(result);
    Ok(())
}

#[cfg(test)]
mod tests {
    use crate::{
        interpreter::lpc_ref::LpcRef,
        test_support::{run_prog, try_run_prog},
    };

    #[tokio::test]
    async fn abs_of_a_negative_int_is_positive() {
        let code = "int create() { return abs(-5); }";
        assert_eq!(run_prog(code).await.result(), Some(LpcRef::from(5)));
    }

    #[tokio::test]
    async fn abs_of_a_positive_int_is_itself() {
        let code = "int create() { return abs(5); }";
        assert_eq!(run_prog(code).await.result(), Some(LpcRef::from(5)));
    }

    #[tokio::test]
    async fn abs_of_a_negative_float_is_positive() {
        let code = "float create() { return abs(-1.5); }";
        assert_eq!(run_prog(code).await.result(), Some(LpcRef::from(1.5)));
    }

    #[tokio::test]
    async fn abs_of_a_string_is_an_error() {
        let code = r#"mixed create() { mixed s = "a"; return abs(s); }"#;
        let err = try_run_prog(code).await.unwrap_err().to_string();
        assert!(err.contains("abs: string is not a number"), "{err}");
    }
}
