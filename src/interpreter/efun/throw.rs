use crate::{errors::LpcError, interpreter::efun::efun_context::EfunContext, Result};

/// `throw`, intentionally throw an error. Can be caught by `catch`.
pub fn throw<const N: usize>(context: &mut EfunContext<N>) -> Result<()> {
    let arg = context.resolve_lpc_ref(1_usize);

    return Err(LpcError::new(format!("{}", arg)).with_span(context.frame().current_debug_span()));
}
