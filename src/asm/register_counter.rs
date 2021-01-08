use crate::asm::register::Register;
use std::sync::atomic::{AtomicUsize, Ordering};

// initialize to 0, so the first register is 1. r0 is reserved for function return values.
static REGISTER_COUNTER: AtomicUsize = AtomicUsize::new(0);

/// Reset the counter to 0.
pub fn reset() {
    REGISTER_COUNTER.store(0, Ordering::SeqCst);
}

/// Increment the counter, and return the next register.
pub fn next() -> Register {
    let counter = REGISTER_COUNTER.fetch_add(1, Ordering::SeqCst);
    Register(counter + 1)
}

/// Return the current register. This is intended for testing and debugging.
/// Typical use should always use next().
pub fn value() -> Register {
    let counter = REGISTER_COUNTER.load(Ordering::SeqCst);
    Register(counter)
}
