use std::{
    fmt,
    fmt::{Display, Formatter},
    ops::Add,
};

/// Really just a `pc` index in the vm.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Address(pub usize);

impl Display for Address {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:04x}", self.0)
    }
}

impl From<usize> for Address {
    #[inline]
    fn from(value: usize) -> Self {
        Self(value)
    }
}

impl From<Address> for usize {
    #[inline]
    fn from(value: Address) -> Self {
        value.0
    }
}

impl PartialEq<usize> for Address {
    #[inline]
    fn eq(&self, other: &usize) -> bool {
        self.0 == *other
    }
}

impl Add<usize> for Address {
    type Output = Self;

    #[inline]
    fn add(self, rhs: usize) -> Self::Output {
        Self(self.0 + rhs)
    }
}

pub type Label = String;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn conversions_and_equality_preserve_the_full_program_counter() {
        for index in [0, 1, 255, 65_536, usize::MAX] {
            let address = Address::from(index);

            assert_eq!(address.0, index);
            assert_eq!(usize::from(address), index);
            assert_eq!(address, index);
            assert_ne!(address, index.wrapping_add(1));
        }
    }

    #[test]
    fn offsets_advance_by_the_requested_instruction_count() {
        for (start, offset, expected) in [
            (0, 0, 0),
            (0, 2, 2),
            (17, 25, 42),
            (usize::MAX - 7, 7, usize::MAX),
        ] {
            assert_eq!(Address(start) + offset, Address(expected));
        }
    }

    #[test]
    fn hexadecimal_display_pads_small_addresses_without_truncating_large_ones() {
        for (index, expected) in [
            (0, "0000"),
            (15, "000f"),
            (65_535, "ffff"),
            (65_536, "10000"),
        ] {
            assert_eq!(Address(index).to_string(), expected);
        }
    }
}
