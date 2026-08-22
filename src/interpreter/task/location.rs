use std::borrow::Cow;

use lpc_rs_core::register::RegisterVariant;
use lpc_rs_errors::Result;

use crate::interpreter::{call_stack::CallStack, lpc_ref::LpcRef, stm::TxnHandle};

/// Read `location` in the current frame.
#[inline]
pub(crate) fn get_location<'a, const N: usize>(
    stack: &'a CallStack<N>,
    txn: &TxnHandle,
    location: RegisterVariant,
) -> Result<Cow<'a, LpcRef>> {
    Ok(stack.current_frame()?.get_location(txn, location))
}

/// Write `location` in the current frame.
#[inline]
pub(crate) fn set_location<const N: usize>(
    stack: &mut CallStack<N>,
    txn: &TxnHandle,
    location: RegisterVariant,
    lpc_ref: LpcRef,
) -> Result<()> {
    stack
        .current_frame_mut()?
        .set_location(txn, location, lpc_ref);
    Ok(())
}

/// Apply `func` at `location` in the current frame.
#[inline]
pub(crate) fn apply_in_location<F, const N: usize>(
    stack: &mut CallStack<N>,
    txn: &TxnHandle,
    location: RegisterVariant,
    func: F,
) -> Result<()>
where
    F: FnOnce(&mut LpcRef) -> Result<()>,
{
    stack
        .current_frame_mut()?
        .apply_in_location(txn, location, func)
}
