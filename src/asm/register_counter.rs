use crate::asm::register::Register;
use std::sync::atomic::{AtomicUsize, Ordering};

static REGISTER_COUNTER: AtomicUsize = AtomicUsize::new(0);

#[derive(Debug)]
pub struct RegisterCounter;

impl RegisterCounter {
    /// Reset the counter to 0.
    pub fn reset() {
        REGISTER_COUNTER.store(0, Ordering::SeqCst);
    }

    /// Return the current register
    pub fn value() -> Register {
        let counter = REGISTER_COUNTER.load(Ordering::SeqCst);
        Register(format!("r{}", counter))
    }

    /// Increment the counter, and return the next register.
    pub fn next() -> Register {
        let counter = REGISTER_COUNTER.fetch_add(1, Ordering::SeqCst);
        Register(format!("r{}", counter + 1))
    }
}