/// This module contains all compile-time configuration

/// The maximum size of an execution call stack, in
/// [`CallFrame`](crate::interpreter::call_frame::CallFrame)s.
pub const MAX_CALL_STACK_SIZE: usize = 64;

/// `$64` (or whatever this is set to) is the maximum implicit closure argument
/// allowed, as enough memory slots for all preceding arguments is also
/// allocated. Note that declared arguments can go beyond this number, as it
/// requires intent to do so.
pub const MAX_CLOSURE_ARG_REFERENCE: usize = 64;

/// The maximum number of queued Tasks that can be waiting to be executed.
/// Any more than that will be dropped.
pub const VM_CHANNEL_CAPACITY: usize = 1024;