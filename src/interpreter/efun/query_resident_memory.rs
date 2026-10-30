use jemalloc_ctl::{epoch, stats};
use lpc_rs_core::LpcIntInner;
use lpc_rs_errors::Result;

use crate::interpreter::{efun::efun_context::EfunContext, lpc_int::LpcInt, lpc_ref::LpcRef};

/// Return jemalloc's resident-memory estimate in bytes.
pub fn query_resident_memory<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    // TODO: once jemalloc is featurized, this needs to be conditional
    let _ = epoch::advance();
    let Ok(bytes) = stats::resident::read() else {
        return Err(context.runtime_error("failed to read resident memory"));
    };

    let int = LpcIntInner::try_from(bytes).unwrap_or(LpcIntInner::MAX);

    let result = LpcRef::Int(LpcInt(int));

    context.return_efun_result(result);

    Ok(())
}

#[cfg(all(test, not(target_env = "msvc")))]
mod tests {
    use super::*;
    use crate::test_support::run_prog;

    #[tokio::test]
    async fn resident_bytes_include_a_live_allocation() {
        let allocation = vec![1u8; 4 * 1024 * 1024];
        let task = run_prog("int create() { return query_resident_memory(); }").await;
        let Some(LpcRef::Int(bytes)) = task.result() else {
            panic!("resident-memory byte count");
        };
        assert!(bytes.0 >= i64::try_from(allocation.len()).unwrap());
        std::hint::black_box(&allocation);
    }
}
