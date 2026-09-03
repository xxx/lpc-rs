use std::borrow::Cow;

use lpc_rs_core::{LpcIntInner, register::RegisterVariant};
use lpc_rs_errors::Result;

use crate::interpreter::{call_stack::CallStack, lpc_ref::LpcRef, stm::TxnHandle};

/// Read `location` in the current frame.
#[inline(always)]
pub(crate) fn get_location<'a, const N: usize>(
    stack: &'a CallStack<N>,
    txn: &TxnHandle,
    location: RegisterVariant,
) -> Result<Cow<'a, LpcRef>> {
    stack.current_frame()?.get_location(txn, location)
}

/// Write `location` in the current frame.
#[inline(always)]
pub(crate) fn set_location<const N: usize>(
    stack: &mut CallStack<N>,
    txn: &TxnHandle,
    location: RegisterVariant,
    lpc_ref: LpcRef,
) -> Result<()> {
    stack
        .current_frame_mut()?
        .set_location(txn, location, lpc_ref)
}

/// Bump the int at `location` in the current frame by `delta`.
#[inline(always)]
pub(crate) fn bump_in_location<const N: usize>(
    stack: &mut CallStack<N>,
    txn: &TxnHandle,
    location: RegisterVariant,
    delta: LpcIntInner,
) -> Result<()> {
    stack
        .current_frame_mut()?
        .bump_in_location(txn, location, delta)
}
