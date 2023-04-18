use lpc_rs_errors::Result;
use jemalloc_ctl::{stats, epoch};
use lpc_rs_core::LpcIntInner;

use crate::interpreter::{efun::efun_context::EfunContext};
use crate::interpreter::lpc_int::LpcInt;
use crate::interpreter::lpc_ref::LpcRef;

/// `query_resident_memory`, an efun for returning the number of bytes of memory in use.
pub async fn query_resident_memory<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
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
