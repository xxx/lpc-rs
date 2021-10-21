use crate::{errors::LpcError, Result};
use crate::interpreter::efun::efun_context::EfunContext;

/// `throw`, intentionally throw an error. Can be caught by `catch`.
pub fn throw(context: &mut EfunContext) -> Result<()> {
    let arg = context.resolve_lpc_ref(1_usize);

    return Err(LpcError::new(format!("{}", arg))
        .with_span(context.frame().current_debug_span()));
}
