use crate::asm::register::Register;
use std::sync::atomic::{AtomicUsize, Ordering};

// initialize to 0, so the first register is 1. r0 is reserved for function return values.
static REGISTER_COUNTER: AtomicUsize = AtomicUsize::new(0);

#[derive(Debug, Copy, Clone, Default)]
pub struct RegisterCounter {
    count: usize
}

impl RegisterCounter {
    /// Reset the counter to 0.
    pub fn reset(&mut self) {
        self.count = 0;
    }

    /// Increment the counter, and return the next register.
    pub fn next(&mut self) -> Register {
        self.count += 1;
        Register(self.count)
    }

    /// Return the current register. This is intended for testing and debugging.
    /// Typical use should always use next().
    pub fn value(&self) -> Register {
        Register(self.count)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_next_increments_and_returns() {
        let mut counter: RegisterCounter = Default::default();

        assert_eq!(counter.next(), Register(1));
        assert_eq!(counter.next(), Register(2));
        assert_eq!(counter.next(), Register(3));
    }

    #[test]
    fn test_value_returns_without_increment() {
        let mut counter: RegisterCounter = Default::default();

        assert_eq!(counter.value(), Register(0));
        counter.next();
        counter.next();
        counter.next();
        assert_eq!(counter.value(), Register(3));
    }

    #[test]
    fn test_reset_resets_the_value() {
        let mut counter: RegisterCounter = Default::default();

        assert_eq!(counter.value(), Register(0));
        counter.next();
        counter.next();
        counter.next();

        counter.reset();

        assert_eq!(counter.value(), Register(0));
    }
}
