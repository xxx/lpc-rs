use crate::core::register::Register;

/// A [`Register`]-aware counter, used during code generation.
#[derive(Debug, Clone, Default)]
pub struct RegisterCounter {
    count: usize,
    stack: Vec<usize>,
}

impl RegisterCounter {
    /// Reset the counter to 0.
    pub fn reset(&mut self) {
        self.count = 0;
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
}
