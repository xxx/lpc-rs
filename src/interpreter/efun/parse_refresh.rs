//! `parse_refresh`: accepted, no cached state to invalidate yet.

use lpc_rs_errors::Result;

use crate::interpreter::efun::efun_context::EfunContext;

/// `parse_refresh()`: accepted; nothing is cached across calls.
pub async fn parse_refresh<const N: usize>(_context: &mut EfunContext<'_, N>) -> Result<()> {
    Ok(())
}
