use crate::asm::register::Register;

#[derive(Debug, Copy, Clone, Default)]
pub struct RegisterCounter {
    count: usize
}

impl RegisterCounter {
    /// Reset the counter to 0.
    pub fn reset(&mut self) {
        self.count = 0;
    }

    /// Return the current register. This is intended for testing and debugging.
    /// Typical use should always use next().
    pub fn value(&self) -> Register {
        Register(self.count)
    }
}

impl Iterator for RegisterCounter {
    type Item = Register;

    fn next(&mut self) -> Option<Register> {
        self.count += 1;
        Some(Register(self.count))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_next_increments_and_returns() {
        let mut counter: RegisterCounter = Default::default();

        assert_eq!(counter.next(), Some(Register(1)));
        assert_eq!(counter.next(), Some(Register(2)));
        assert_eq!(counter.next(), Some(Register(3)));
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
