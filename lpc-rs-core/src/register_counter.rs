use crate::register::Register;

/// A [`Register`]-aware counter, used during code generation.
#[derive(Debug, Clone, Default)]
pub struct RegisterCounter {
    count: usize,
    stack: Vec<(usize, bool)>,
    // hacky, but beats allowing isize
    start_at_zero: bool,
    emitted_zero: bool,
}

impl RegisterCounter {
    // create a new counter
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }

    /// Reset the counter to 0.
    #[inline]
    pub fn reset(&mut self) {
        self.count = 0;
    }

    /// Set whether the first register emitted via `next()` is Register(0) or Register(1).
    #[inline]
    pub fn start_at_zero(&mut self, value: bool) {
        self.start_at_zero = value;
    }

    /// Return the current register.
    #[inline]
    pub fn current(&self) -> Register {
        Register(self.count)
    }

    /// Move the counter back by one, for use after loops where next()
    /// is called., but the result is not used.
    #[inline]
    pub fn go_back(&mut self) {
        self.count -= 1;
    }

    /// Get the current counter value
    #[inline]
    pub fn as_usize(&self) -> usize {
        self.count
    }

    /// Return the number of registers that have been emitted, taking start_at_zero into account.
    pub fn number_emitted(&self) -> usize {
        if self.start_at_zero && self.emitted_zero {
            return self.count + 1;
        }

        return self.count;
    }

    /// Set a new value, and store the old one for `pop`ping.
    /// # Returns
    /// The previous value
    pub fn push(&mut self, new_val: usize) -> usize {
        self.stack.push((self.count, self.emitted_zero));
        let ret = self.count;
        self.set(new_val);
        ret
    }

    /// Pop a value, if there is one, and set the current `count` back to it.
    /// # Returns
    /// The popped value if there is one, else the current `count`
    pub fn pop(&mut self) -> usize {
        if let Some(x) = self.stack.pop() {
            (self.count, self.emitted_zero) = x;

            return x.0;
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

        // TODO: rewrite this so the count is incremented after the register is captured
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

        assert_eq!(counter.next(), Some(Register(1)));

        counter.push(0);
        assert_eq!(counter.next(), Some(Register(0)));

        counter.pop();
        assert_eq!(counter.next(), Some(Register(2)));

        counter.push(4);
        assert_eq!(counter.next(), Some(Register(5)));
    }

    #[test]
    fn test_number_emitted() {
        let mut counter = RegisterCounter::default();
        counter.start_at_zero(true);
        assert_eq!(counter.number_emitted(), 0);
        assert_eq!(counter.next().unwrap(), Register(0));
        assert_eq!(counter.number_emitted(), 1);
        assert_eq!(counter.next().unwrap(), Register(1));
        assert_eq!(counter.number_emitted(), 2);

        let mut counter = RegisterCounter::default();
        counter.start_at_zero(false);
        assert_eq!(counter.number_emitted(), 0);
        assert_eq!(counter.next().unwrap(), Register(1));
        assert_eq!(counter.number_emitted(), 1);
        assert_eq!(counter.next().unwrap(), Register(2));
        assert_eq!(counter.number_emitted(), 2);
    }
}
