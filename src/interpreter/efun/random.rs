use lpc_rs_errors::Result;

use crate::interpreter::{efun::efun_context::EfunContext, lpc_ref::LpcRef};

/// `random(n)`: a uniform int in `0..n`; `random(0)` is 0. The generator
/// is the thread's, outside any transaction, so a retried attempt draws
/// again.
pub fn random<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let LpcRef::Int(bound) = context.arg(0) else {
        return Err(context.runtime_error(format!(
            "random: {} is not an int",
            context.arg(0).type_name()
        )));
    };
    let result = match bound.0 {
        n if n < 0 => return Err(context.runtime_error(format!("random: negative bound {n}"))),
        0 => 0,
        n => fastrand::i64(0..n),
    };
    context.return_efun_result(LpcRef::from(result));
    Ok(())
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use crate::{
        interpreter::lpc_ref::LpcRef,
        test_support::{run_prog, try_run_prog},
    };

    #[tokio::test]
    async fn every_draw_stays_below_the_bound() {
        let code = indoc! { r#"
            int create() {
                for (int i = 0; i < 200; i++) {
                    int r = random(5);
                    if (r < 0 || r >= 5) return 0;
                }
                return 1;
            }
        "# };
        assert_eq!(run_prog(code).await.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn draws_vary() {
        let code = indoc! { r#"
            int create() {
                int first = random(1000000);
                for (int i = 0; i < 50; i++) {
                    if (random(1000000) != first) return 1;
                }
                return 0;
            }
        "# };
        assert_eq!(run_prog(code).await.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn random_of_zero_is_zero() {
        let code = "int create() { return random(0); }";
        assert_eq!(run_prog(code).await.result(), Some(LpcRef::from(0)));
    }

    #[tokio::test]
    async fn a_negative_bound_is_an_error() {
        let code = "int create() { return random(-3); }";
        let err = try_run_prog(code).await.unwrap_err().to_string();
        assert!(err.contains("random: negative bound -3"), "{err}");
    }
}
