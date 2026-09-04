//! `min` and `max`: the extreme of the arguments, or of one array.

use std::cmp::Ordering;

use lpc_rs_core::{BaseFloat, LpcFloatInner};
use lpc_rs_errors::Result;

use crate::interpreter::{efun::efun_context::EfunContext, lpc_ref::LpcRef};

/// Numeric order across ints and floats; `None` for a non-number.
fn numeric_cmp(a: &LpcRef, b: &LpcRef) -> Option<Ordering> {
    match (a, b) {
        (LpcRef::Int(x), LpcRef::Int(y)) => Some(x.cmp(y)),
        (LpcRef::Float(x), LpcRef::Float(y)) => Some(x.cmp(y)),
        (LpcRef::Int(x), LpcRef::Float(y)) => Some(as_float(x.0).cmp(&y.0)),
        (LpcRef::Float(x), LpcRef::Int(y)) => Some(x.0.cmp(&as_float(y.0))),
        _ => None,
    }
}

fn as_float(i: i64) -> LpcFloatInner {
    LpcFloatInner::from(i as BaseFloat)
}

/// The value the `wins` ordering picks out of the arguments, or out of
/// the one array passed; the winner keeps its own type.
fn extreme<const N: usize>(
    context: &mut EfunContext<'_, N>,
    name: &str,
    wins: Ordering,
) -> Result<()> {
    let values: Vec<LpcRef> = match context.arg(0) {
        array @ LpcRef::Array(_) if context.arg_count() == 1 => {
            array.with_array(context.txn(), |a| a.iter().cloned().collect())?
        }
        _ => (0..context.arg_count())
            .map(|i| context.arg(i).clone())
            .collect(),
    };
    let mut best: Option<&LpcRef> = None;
    for value in &values {
        if !matches!(value, LpcRef::Int(_) | LpcRef::Float(_)) {
            return Err(
                context.runtime_error(format!("{name}: {} is not a number", value.type_name()))
            );
        }
        best = Some(match best {
            Some(current) if numeric_cmp(value, current) != Some(wins) => current,
            _ => value,
        });
    }
    let Some(best) = best else {
        return Err(context.runtime_error(format!("{name}: no values")));
    };
    context.return_efun_result(best.clone());
    Ok(())
}

/// `min(x, ...)` or `min(array)`: the smallest number.
pub fn min<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    extreme(context, "min", Ordering::Less)
}

/// `max(x, ...)` or `max(array)`: the largest number.
pub fn max<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    extreme(context, "max", Ordering::Greater)
}

#[cfg(test)]
mod tests {
    use crate::{
        interpreter::lpc_ref::LpcRef,
        test_support::{run_prog, try_run_prog},
    };

    #[tokio::test]
    async fn min_of_ints_is_the_smallest() {
        let code = "int create() { return min(3, 1, 2); }";
        assert_eq!(run_prog(code).await.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn max_of_ints_is_the_largest() {
        let code = "int create() { return max(3, 1, 2); }";
        assert_eq!(run_prog(code).await.result(), Some(LpcRef::from(3)));
    }

    #[tokio::test]
    async fn min_of_one_array_is_its_smallest() {
        let code = "int create() { return min(({ 3, 1, 2 })); }";
        assert_eq!(run_prog(code).await.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn max_of_one_array_is_its_largest() {
        let code = "float create() { return max(({ 1.5, 7.25, 2.0 })); }";
        assert_eq!(run_prog(code).await.result(), Some(LpcRef::from(7.25)));
    }

    #[tokio::test]
    async fn max_across_ints_and_floats_keeps_the_winner_as_it_was() {
        let code = "mixed create() { return max(1, 2.5, 2); }";
        assert_eq!(run_prog(code).await.result(), Some(LpcRef::from(2.5)));
    }

    #[tokio::test]
    async fn min_across_ints_and_floats_keeps_the_winner_as_it_was() {
        let code = "mixed create() { return min(1, 0.5, 2); }";
        assert_eq!(run_prog(code).await.result(), Some(LpcRef::from(0.5)));
    }

    #[tokio::test]
    async fn min_of_a_single_number_is_that_number() {
        let code = "int create() { return min(4); }";
        assert_eq!(run_prog(code).await.result(), Some(LpcRef::from(4)));
    }

    #[tokio::test]
    async fn min_of_an_empty_array_is_an_error() {
        let code = "mixed create() { return min(({ })); }";
        let err = try_run_prog(code).await.unwrap_err().to_string();
        assert!(err.contains("min: no values"), "{err}");
    }

    #[tokio::test]
    async fn max_of_a_string_is_an_error() {
        let code = r#"mixed create() { mixed s = "a"; return max(s, 1); }"#;
        let err = try_run_prog(code).await.unwrap_err().to_string();
        assert!(err.contains("max: string is not a number"), "{err}");
    }
}
