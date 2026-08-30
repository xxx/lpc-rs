//! This module contains all compile-time configuration

use lpc_rs_core::RegisterSize;

/// The maximum size of an execution call stack, in
/// [`CallFrame`](crate::interpreter::call_frame::CallFrame)s.
pub const MAX_CALL_STACK_SIZE: usize = 64;

/// `$64` (or whatever this is set to) is the maximum implicit closure argument
/// allowed, as enough memory slots for all preceding arguments is also
/// allocated. Note that declared arguments can go beyond this number, as it
/// requires intent to do so.
pub const MAX_CLOSURE_ARG_REFERENCE: RegisterSize = 64;

/// The maximum number of queued Tasks that can be waiting to be executed.
/// Any more than that will be dropped.
pub const VM_CHANNEL_CAPACITY: usize = 1024;

/// How deep `command()` may nest: a level costs ~85KB of native stack in a
/// debug build, and 16 fit the 2MiB stack tokio gives a worker thread.
pub const MAX_COMMAND_DEPTH: usize = 16;

/// How many driver tasks may nest on one native stack — a `create()` that
/// clones, a `catch_tell` that writes back, a master hook that re-enters —
/// before the next nesting is a runtime error. Measured 2026-08-30: a debug
/// build on tokio's 2 MiB thread survives 20 nested `catch_tell`s and aborts
/// at 24.
pub const MAX_TASK_CHAIN: u8 = 16;
