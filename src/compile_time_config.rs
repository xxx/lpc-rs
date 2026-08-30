//! This module contains all compile-time configuration

use lpc_rs_core::RegisterSize;

/// Frames one task may hold — every LPC call, `->` included; past it a call
/// is a "stack overflow" runtime error. CD's `MAX_TRACE`.
pub const MAX_CALL_STACK_SIZE: usize = 1024;

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

/// How many driver tasks may nest on one native stack before the next
/// nesting is a runtime error. Measured 2026-08-30, debug build on a 2 MiB
/// thread: `catch_tell` levels abort at 24 and `->` levels (a `Task` each,
/// then) at 50.
pub const MAX_TASK_CHAIN: u8 = 64;

/// The stack of every runtime thread: `MAX_TASK_CHAIN` levels at the ~87 KB
/// a debug `catch_tell` level costs, three times over. Tests take the same
/// through `RUST_MIN_STACK` in `.cargo/config.toml`.
pub const THREAD_STACK: usize = 16 * 1024 * 1024;
