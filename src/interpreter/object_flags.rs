use std::sync::atomic::{AtomicU8, Ordering};

/// Flags for a [`Process`](crate::interpreter::process::Process).
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum ObjectFlags {
    /// Is this process a clone?
    Clone = 0b0000_0001,

    /// Have commands been enabled on this object?
    CommandsEnabled = 0b0000_0010,
}

/// A [`Process`](crate::interpreter::process::Process)'s flags, set and
/// cleared atomically.
#[derive(Debug, Default)]
pub struct AtomicFlags {
    flags: AtomicU8,
}

impl AtomicFlags {
    /// Set `flag`; returns the previous bits.
    #[inline]
    pub fn set(&self, flag: ObjectFlags) -> u8 {
        self.flags.fetch_or(flag as u8, Ordering::Relaxed)
    }

    /// Clear `flag`; returns the previous bits.
    #[inline]
    pub fn clear(&self, flag: ObjectFlags) -> u8 {
        self.flags.fetch_and(!(flag as u8), Ordering::Relaxed)
    }

    #[inline]
    pub fn test(&self, flag: ObjectFlags) -> bool {
        (self.flags.load(Ordering::Relaxed) & flag as u8) != 0
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_atomic_flags() {
        let flags = AtomicFlags::default();
        assert!(!flags.test(ObjectFlags::Clone));
        assert!(!flags.test(ObjectFlags::CommandsEnabled));
        assert_eq!(flags.set(ObjectFlags::CommandsEnabled), 0);
        assert!(!flags.test(ObjectFlags::Clone));
        assert!(flags.test(ObjectFlags::CommandsEnabled));
        assert_eq!(
            flags.set(ObjectFlags::Clone),
            ObjectFlags::CommandsEnabled as u8
        );
        assert!(flags.test(ObjectFlags::Clone));
        assert!(flags.test(ObjectFlags::CommandsEnabled));
        assert_eq!(
            flags.clear(ObjectFlags::CommandsEnabled),
            ObjectFlags::Clone as u8 | ObjectFlags::CommandsEnabled as u8
        );
        assert!(flags.test(ObjectFlags::Clone));
        assert!(!flags.test(ObjectFlags::CommandsEnabled));
        assert_eq!(flags.clear(ObjectFlags::Clone), ObjectFlags::Clone as u8);
        assert!(!flags.test(ObjectFlags::Clone));
        assert!(!flags.test(ObjectFlags::CommandsEnabled));
    }
}
