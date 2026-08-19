use std::borrow::Cow;

use lpc_rs_core::register::RegisterVariant;
use lpc_rs_errors::Result;
use tracing::{instrument, trace};

use crate::interpreter::{
    call_frame::CallFrame,
    call_stack::CallStack,
    lpc_ref::{LpcRef, NULL},
    stm::TxnHandle,
};

/// Resolve any type RegisterVariant into an LpcRef, for the current frame
#[inline]
pub(crate) fn get_location<'a, const N: usize>(
    stack: &'a CallStack<N>,
    txn: &TxnHandle,
    location: RegisterVariant,
) -> Result<Cow<'a, LpcRef>> {
    let frame = stack.current_frame()?;

    get_location_in_frame(frame, txn, location)
}

/// Resolve any type RegisterVariant into an LpcRef, for the passed frame
#[instrument(skip(frame))]
#[inline]
pub(crate) fn get_location_in_frame<'a>(
    frame: &'a CallFrame,
    txn: &TxnHandle,
    location: RegisterVariant,
) -> Result<Cow<'a, LpcRef>> {
    match location {
        RegisterVariant::Local(reg) => {
            let registers = &frame.registers;
            Ok(Cow::Borrowed(&registers[reg]))
        }
        RegisterVariant::Global(reg) => {
            let var = frame.process.var_id(reg.into());
            // Read through the transaction.
            Ok(Cow::Owned(
                txn.with(|t| t.read(var).unwrap_or_else(|| NULL.clone())),
            ))
        }
        RegisterVariant::Upvalue(upv) => {
            let upvalue_ptrs = &frame.upvalue_ptrs;
            let reg = upvalue_ptrs[upv.index() as usize];

            // Read through the transaction, exactly like the Global arm
            // above: the slab slot holds only the cell's identity, and an
            // unwritten cell reads NULL.
            let (cell, bank_len) = frame.with_upvalues(|uv| (uv[reg], uv.len()));
            trace!("upvalue data: cell = {:?}, bank len = {}", cell, bank_len);
            Ok(Cow::Owned(
                txn.with(|t| t.read(cell).unwrap_or_else(|| NULL.clone())),
            ))
        }
    }
}

#[inline]
pub(crate) fn set_location<const N: usize>(
    stack: &mut CallStack<N>,
    txn: &TxnHandle,
    location: RegisterVariant,
    lpc_ref: LpcRef,
) -> lpc_rs_errors::Result<()> {
    let frame = stack.current_frame_mut()?;
    frame.set_location(txn, location, lpc_ref);
    Ok(())
}

/// Apply an operation to a location, in-place.
pub(crate) fn apply_in_location<F, const N: usize>(
    stack: &mut CallStack<N>,
    txn: &TxnHandle,
    location: RegisterVariant,
    func: F,
) -> lpc_rs_errors::Result<()>
where
    F: FnOnce(&mut LpcRef) -> Result<()>,
{
    match location {
        RegisterVariant::Local(reg) => {
            let frame = stack.current_frame_mut()?;
            let registers = &mut frame.registers;
            func(&mut registers[reg])
        }
        RegisterVariant::Global(reg) => {
            let frame = stack.current_frame()?;
            // In-txn read-modify-write: the read is tracked, the write
            // lands in the in-flight changeset — atomic per attempt.
            let var = frame.process.var_id(reg.into());
            txn.with(|t| {
                let mut cur = t.read(var).unwrap_or_else(|| NULL.clone());
                func(&mut cur)?;
                t.write(var, cur);
                Ok(())
            })
        }
        RegisterVariant::Upvalue(reg) => {
            let frame = stack.current_frame()?;
            let upvalues = &frame.upvalue_ptrs;
            let idx = upvalues[reg.index() as usize];

            // In-txn read-modify-write through the cell's identity: the
            // read is tracked, the write lands in the in-flight changeset —
            // atomic per attempt, exactly like the Global arm above.
            let cell = frame.with_upvalues(|uv| uv[idx]);
            txn.with(|t| {
                let mut cur = t.read(cell).unwrap_or_else(|| NULL.clone());
                func(&mut cur)?;
                t.write(cell, cur);
                Ok(())
            })
        }
    }
}
