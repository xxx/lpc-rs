use lpc_rs_errors::Result;

use crate::interpreter::{efun::efun_context::EfunContext, lpc_ref::LpcRef};

/// `time()`: seconds since the epoch. A retried attempt reads the clock
/// again.
pub fn time<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    context.return_efun_result(LpcRef::from(chrono::Utc::now().timestamp()));
    Ok(())
}

#[cfg(test)]
mod tests {
    use std::time::{SystemTime, UNIX_EPOCH};

    use crate::{interpreter::lpc_ref::LpcRef, test_support::run_prog};

    #[tokio::test]
    async fn time_is_the_current_epoch_second() {
        let before = UNIX_EPOCH.elapsed().unwrap().as_secs() as i64;
        let result = run_prog("int create() { return time(); }").await.result();
        let after = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_secs() as i64;
        let Some(LpcRef::Int(t)) = result else {
            panic!("an int, actually {result:?}");
        };
        assert!(
            (before..=after).contains(&t.0),
            "{t} not in {before}..={after}"
        );
    }
}
