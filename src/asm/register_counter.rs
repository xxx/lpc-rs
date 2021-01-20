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

    /// Return the current register.
    pub fn value(&self) -> Register {
        Register(self.count)
    }

    /// move the counter back by one, for use after loops where next() is called.
    pub fn go_back(&mut self) {
        self.count -= 1;
    }

    /// Return the current internal count
    pub fn get_count(&self) -> usize {
        self.count
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
        let mut counter = RegisterCounter::default();

        assert_eq!(counter.next(), Some(Register(1)));
        assert_eq!(counter.next(), Some(Register(2)));
        assert_eq!(counter.next(), Some(Register(3)));
    }

    #[test]
    fn test_value_returns_without_increment() {
        let mut counter = RegisterCounter::default();

        assert_eq!(counter.value(), Register(0));
        counter.next();
        counter.next();
        counter.next();
        assert_eq!(counter.value(), Register(3));
    }

    #[test]
    fn test_reset_resets_the_value() {
        let mut counter = RegisterCounter::default();

        assert_eq!(counter.value(), Register(0));
        counter.next();
        counter.next();
        counter.next();

        counter.reset();

        assert_eq!(counter.value(), Register(0));
    }

    #[test]
    fn test_go_back_decrements_the_count() {
        let mut counter = RegisterCounter::default();

        assert_eq!(counter.value(), Register(0));
        counter.next();
        counter.next();
        counter.next();

        counter.go_back();

        assert_eq!(counter.value(), Register(2));
    }
}
