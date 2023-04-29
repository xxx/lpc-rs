use lpc_rs_core::RegisterSize;

/// This module contains all compile-time configuration

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

/// How long can a chain of object clones continue, before we error out? This is
/// specifically for chains of clones, where one object clones another object,
/// which clones a third, etc.
/// One object that clones 20 objects in an array, for example,
/// will _not_ be affected by this.
pub const MAX_CLONE_CHAIN: u8 = 20;

