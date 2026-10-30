use crate::{RegisterSize, register::Register};

/// A [`Register`]-aware counter, used during code generation.
/// Iteration stops when advancing the count would exceed [`RegisterSize`].
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
        self.count = self.count.checked_add(1)?;
        Some(Register(c))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn iteration_stops_before_the_register_count_overflows() {
        for base in [0, 1, 5] {
            let mut counter = RegisterCounter::new(base);
            for index in base..RegisterSize::MAX {
                assert_eq!(counter.next(), Some(Register(index)));
            }

            assert_eq!(counter.next(), None);
            assert_eq!(counter.next(), None);
            assert_eq!(counter.number_emitted(), RegisterSize::MAX - base);
        }
    }

    #[test]
    fn nested_scopes_restore_an_exhausted_counter() {
        let mut counter = RegisterCounter::new(1);
        counter.set(RegisterSize::MAX);
        assert_eq!(counter.next(), None);

        assert_eq!(counter.push(), RegisterSize::MAX);
        assert_eq!(counter.next(), Some(Register(1)));
        assert_eq!(counter.pop(), RegisterSize::MAX);
        assert_eq!(counter.next(), None);
    }

    #[test]
    fn setting_a_lower_count_resumes_allocation_after_exhaustion() {
        let mut counter = RegisterCounter::new(1);
        counter.set(RegisterSize::MAX);
        assert_eq!(counter.next(), None);

        counter.set(2);

        assert_eq!(counter.next(), Some(Register(2)));
    }

    #[test]
    fn a_counter_with_no_remaining_capacity_never_allocates() {
        let mut counter = RegisterCounter::new(RegisterSize::MAX);

        assert_eq!(counter.next(), None);
        assert_eq!(counter.number_emitted(), 0);
    }

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

    #[test]
    fn number_emitted_excludes_reserved_registers() {
        let mut counter = RegisterCounter::new(5);
        assert_eq!(counter.number_emitted(), 0);

        counter.next();
        counter.next();

        assert_eq!(counter.number_emitted(), 2);
    }

    #[test]
    fn set_clamps_values_below_the_base() {
        let mut counter = RegisterCounter::new(5);
        counter.set(2);

        assert_eq!(counter.next(), Some(Register(5)));
    }

    #[test]
    fn set_accepts_the_base_after_emitting_registers() {
        let mut counter = RegisterCounter::new(5);
        counter.next();
        counter.set(5);

        assert_eq!(counter.next(), Some(Register(5)));
    }

    #[test]
    fn set_accepts_values_above_the_base() {
        let mut counter = RegisterCounter::new(5);
        counter.set(8);

        assert_eq!(counter.next(), Some(Register(8)));
    }

    #[test]
    fn push_returns_the_previous_count() {
        let mut counter = RegisterCounter::new(5);
        counter.set(8);

        assert_eq!(counter.push(), 8);
    }

    #[test]
    fn push_starts_a_new_scope_at_the_base() {
        let mut counter = RegisterCounter::new(5);
        counter.set(8);
        counter.push();

        assert_eq!(counter.number_emitted(), 0);
        assert_eq!(counter.next(), Some(Register(5)));
    }

    #[test]
    fn pop_restores_nested_scopes_in_reverse_order() {
        let mut counter = RegisterCounter::new(5);
        counter.set(8);
        counter.push();
        counter.set(11);
        counter.push();
        counter.set(14);

        assert_eq!(counter.pop(), 11);
        assert_eq!(counter.next(), Some(Register(11)));
        assert_eq!(counter.pop(), 8);
        assert_eq!(counter.next(), Some(Register(8)));
        assert_eq!(counter.pop(), 9);
    }

    #[test]
    fn pop_without_a_saved_scope_keeps_the_current_count() {
        let mut counter = RegisterCounter::new(5);
        counter.set(8);

        assert_eq!(counter.pop(), 8);
        assert_eq!(counter.next(), Some(Register(8)));
    }
}
