use crate::{register::Register, RegisterSize};

/// A [`Register`]-aware counter, used during code generation.
#[derive(Debug, Clone, Default)]
pub struct RegisterCounter {
    base_count: RegisterSize,
    count: RegisterSize,
    stack: Vec<RegisterSize>,
}

impl RegisterCounter {
    /// create a new counter
    #[inline]
    pub fn new(base_count: RegisterSize) -> Self {
        Self {
            base_count,
            count: base_count,
            stack: vec![],
        }
    }

    /// Return the number of registers that have been emitted.
    pub fn number_emitted(&self) -> RegisterSize {
        self.count - self.base_count
    }

    /// Set a new value, and store the old one for `pop`ping.
    /// # Returns
    /// The previous value
    pub fn push(&mut self) -> RegisterSize {
        self.stack.push(self.count);
        let ret = self.count;
        self.set(self.base_count);
        ret
    }

    /// Pop a value, if there is one, and set the current `count` back to it.
    /// # Returns
    /// The popped value if there is one, else the current `count`
    pub fn pop(&mut self) -> RegisterSize {
        if let Some(x) = self.stack.pop() {
            self.count = x;
        }

        self.count
    }

    #[inline]
    /// Set a new value on the counter.
    /// Will use the original base_value if the new value is less than it.
    pub fn set(&mut self, new_val: RegisterSize) {
        let new = if new_val < self.base_count {
            self.base_count
        } else {
            new_val
        };

        self.count = new;
    }
}

impl Iterator for RegisterCounter {
    type Item = Register;

    fn next(&mut self) -> Option<Register> {
        let c = self.count;
        self.count += 1;
        Some(Register(c))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn next_increments_and_returns() {
        let mut counter = RegisterCounter::default();

        assert_eq!(counter.next(), Some(Register(0)));
        assert_eq!(counter.next(), Some(Register(1)));
        assert_eq!(counter.next(), Some(Register(2)));

        let mut counter = RegisterCounter::new(5);

        assert_eq!(counter.next(), Some(Register(5)));
        assert_eq!(counter.next(), Some(Register(6)));
        assert_eq!(counter.next(), Some(Register(7)));
    }

    #[test]
    fn set_updates_the_count() {
        let mut counter = RegisterCounter::default();

        assert_eq!(counter.count, 0);
        assert_eq!(counter.next(), Some(Register(0)));

        counter.set(5);

        assert_eq!(counter.count, 5);
        assert_eq!(counter.next(), Some(Register(5)));
    }

    #[test]
    fn number_emitted() {
        let mut counter = RegisterCounter::default();
        assert_eq!(counter.number_emitted(), 0);
        assert_eq!(counter.next().unwrap(), Register(0));
        assert_eq!(counter.number_emitted(), 1);
        assert_eq!(counter.next().unwrap(), Register(1));
        assert_eq!(counter.number_emitted(), 2);
    }
}
