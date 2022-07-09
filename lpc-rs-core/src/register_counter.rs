use crate::register::Register;

/// A [`Register`]-aware counter, used during code generation.
#[derive(Debug, Clone, Default)]
pub struct RegisterCounter {
    count: usize,
    stack: Vec<usize>,
    // hacky, but beats allowing isize, enums, etc.
    start_at_zero: bool,
    emitted_zero: bool,
}

impl RegisterCounter {
    // create a new counter
    pub fn new() -> Self {
        Self::default()
    }

    /// Reset the counter to 0.
    pub fn reset(&mut self) {
        self.count = 0;
    }

    pub fn start_at_zero(&mut self, value: bool) {
        self.start_at_zero = value;
    }

    /// Return the current register.
    pub fn current(&self) -> Register {
        Register(self.count)
    }

    /// Move the counter back by one, for use after loops where next()
    /// is called., but the result is not used.
    pub fn go_back(&mut self) {
        self.count -= 1;
    }

    /// Get the current counter value
    pub fn as_usize(&self) -> usize {
        self.count
    }

    /// Set a new value, and store the old one for `pop`ping.
    /// # Returns
    /// The previous value
    pub fn push(&mut self, new_val: usize) -> usize {
        self.stack.push(self.count);
        let ret = self.count;
        self.count = new_val;
        ret
    }

    /// Pop a value, if there is one, and set the current `count` back to it.
    /// # Returns
    /// The popped value if there is one, else the current `count`
    pub fn pop(&mut self) -> usize {
        if let Some(x) = self.stack.pop() {
            self.count = x;

            return x;
        }

        self.count
    }

    /// Set a new value on the counter
    pub fn set(&mut self, new_val: usize) {
        if self.start_at_zero {
            self.emitted_zero = new_val != 0;
        }
        self.count = new_val;
    }
}

impl Iterator for RegisterCounter {
    type Item = Register;

    fn next(&mut self) -> Option<Register> {
        if self.start_at_zero && !self.emitted_zero {
            self.emitted_zero = true;
            return Some(Register(0));
        }

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

        let mut counter = RegisterCounter::default();
        counter.start_at_zero(true);

        assert_eq!(counter.next(), Some(Register(0)));
        assert_eq!(counter.next(), Some(Register(1)));
        assert_eq!(counter.next(), Some(Register(2)));
    }

    #[test]
    fn test_value_returns_without_increment() {
        let mut counter = RegisterCounter::default();

        assert_eq!(counter.current(), Register(0));
        counter.next();
        counter.next();
        counter.next();
        assert_eq!(counter.current(), Register(3));
    }

    #[test]
    fn test_reset_resets_the_value() {
        let mut counter = RegisterCounter::default();

        assert_eq!(counter.current(), Register(0));
        counter.next();
        counter.next();
        counter.next();

        counter.reset();

        assert_eq!(counter.current(), Register(0));
    }

    #[test]
    fn test_go_back_decrements_the_count() {
        let mut counter = RegisterCounter::default();

        assert_eq!(counter.current(), Register(0));
        counter.next();
        counter.next();
        counter.next();

        counter.go_back();

        assert_eq!(counter.current(), Register(2));
    }

    #[test]
    fn test_set_updates_the_count() {
        let mut counter = RegisterCounter::default();

        assert_eq!(counter.current(), Register(0));
        assert_eq!(counter.next(), Some(Register(1)));

        counter.set(5);

        assert_eq!(counter.current(), Register(5));
    }

    #[test]
    fn test_set_with_start_at_zero() {
        let mut counter = RegisterCounter::default();
        counter.start_at_zero(true);

        assert_eq!(counter.current(), Register(0));

        assert_eq!(counter.next(), Some(Register(0)));
        assert_eq!(counter.next(), Some(Register(1)));
        assert_eq!(counter.next(), Some(Register(2)));

        counter.set(5);
        assert_eq!(counter.next(), Some(Register(6)));

        counter.set(0);

        assert_eq!(counter.next(), Some(Register(0)));
    }
}