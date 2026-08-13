use std::borrow::Cow;

use lpc_rs_core::register::RegisterVariant;
use lpc_rs_errors::Result;
use tracing::{instrument, trace};

use crate::interpreter::{call_frame::CallFrame, call_stack::CallStack, lpc_ref::LpcRef};

/// Resolve any type RegisterVariant into an LpcRef, for the current frame
#[inline]
pub(crate) fn get_location<const N: usize>(
    stack: &CallStack<N>,
    location: RegisterVariant,
) -> lpc_rs_errors::Result<Cow<'_, LpcRef>> {
    let frame = stack.current_frame()?;

    get_location_in_frame(frame, location)
}

/// Resolve any type RegisterVariant into an LpcRef, for the passed frame
#[instrument(skip(frame))]
#[inline]
pub(crate) fn get_location_in_frame(
    frame: &CallFrame,
    location: RegisterVariant,
) -> Result<Cow<'_, LpcRef>> {
    match location {
        RegisterVariant::Local(reg) => {
            let registers = &frame.registers;
            Ok(Cow::Borrowed(&registers[reg]))
        }
        RegisterVariant::Global(reg) => {
            let proc = &frame.process;
            Ok(Cow::Owned(proc.with_globals(|g| g[reg].clone())))
        }
        RegisterVariant::Upvalue(upv) => {
            let upvalue_ptrs = &frame.upvalue_ptrs;
            let reg = upvalue_ptrs[upv.index() as usize];

            let (val, len) = frame.with_upvalues(|uv| (uv[reg].clone(), uv.len()));
            trace!("upvalue data: idx = {}, len = {}", reg, len);
            Ok(Cow::Owned(val))
        }
    }
}

#[inline]
pub(crate) fn set_location<const N: usize>(
    stack: &mut CallStack<N>,
    location: RegisterVariant,
    lpc_ref: LpcRef,
) -> lpc_rs_errors::Result<()> {
    let frame = stack.current_frame_mut()?;
    frame.set_location(location, lpc_ref);
    Ok(())
}

/// Apply an operation to a location, in-place.
pub(crate) fn apply_in_location<F, const N: usize>(
    stack: &mut CallStack<N>,
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

            let proc = &frame.process;
            proc.with_globals_mut(|g| func(&mut g[reg]))
        }
        RegisterVariant::Upvalue(reg) => {
            let frame = stack.current_frame()?;
            let upvalues = &frame.upvalue_ptrs;
            let idx = upvalues[reg.index() as usize];

            frame.with_upvalues_mut(|uv| func(&mut uv[idx]))
        }
    }
}
