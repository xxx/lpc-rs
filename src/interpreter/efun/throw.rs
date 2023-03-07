use qcell::QCellOwner;
use lpc_rs_errors::{LpcError, Result};

use crate::interpreter::efun::efun_context::EfunContext;
use crate::util::keyable::Keyable;

/// `throw`, intentionally throw an error. Can be caught by `catch`.
pub fn throw<const N: usize>(context: &mut EfunContext<N>, cell_key: &mut QCellOwner) -> Result<()> {
    let arg = context.resolve_local_register(1_usize);

    return Err(
        LpcError::new(arg.with_key(cell_key).to_string())
            .with_span(context.frame().current_debug_span())
    );
}
