use lpc_rs_errors::Result;

use crate::interpreter::{efun, efun::efun_context::EfunContext, lpc_ref::LpcRef};

/// `object_time([ob])`: when the object (the caller by default) was
/// created, in seconds since the epoch.
pub async fn object_time<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let Some(process) = efun::arg_or_this_object(context.arg(0), context).await? else {
        return Err(context.runtime_error("object_time: the argument must be an object"));
    };
    context.return_efun_result(LpcRef::from(process.created));
    Ok(())
}

#[cfg(test)]
mod tests {
    use crate::{
        interpreter::lpc_ref::LpcRef,
        test_support::{run_prog, try_run_prog},
    };

    #[tokio::test]
    async fn object_time_is_the_second_the_object_was_created() {
        let before = chrono::Utc::now().timestamp();
        let result = run_prog("int create() { return object_time(); }")
            .await
            .result();
        let after = chrono::Utc::now().timestamp();
        let Some(LpcRef::Int(t)) = result else {
            panic!("an int, actually {result:?}");
        };
        assert!(
            (before..=after).contains(&t.0),
            "{t} not in {before}..={after}"
        );
    }

    #[tokio::test]
    async fn the_argument_names_the_object() {
        let result =
            run_prog("int create() { return object_time(this_object()) == object_time(); }")
                .await
                .result();
        assert_eq!(result, Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn a_non_object_is_an_error() {
        let err = try_run_prog("int create() { mixed x = 1; return object_time(x); }")
            .await
            .expect_err("1 is not an object")
            .to_string();
        assert!(
            err.contains("object_time: the argument must be an object"),
            "{err}"
        );
    }
}
