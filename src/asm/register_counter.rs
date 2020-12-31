use crate::asm::register::Register;
use std::sync::atomic::{AtomicUsize, Ordering};

static REGISTER_COUNTER: AtomicUsize = AtomicUsize::new(0);

pub struct RegisterCounter;

impl RegisterCounter {
    pub fn reset() {
        REGISTER_COUNTER.store(0, Ordering::SeqCst);
    }

    pub fn next() -> Register {
        let counter = REGISTER_COUNTER.fetch_add(1, Ordering::SeqCst);
        Register(format!("r{}", counter + 1))
    }
}