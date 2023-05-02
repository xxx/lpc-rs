use std::{
    ops::{BitAnd, BitOr},
    sync::atomic::{AtomicU8, Ordering},
};

/// Flags for a [`Process`](crate::interpreter::process::Process).
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum ObjectFlags {
    /// Has this process been initialized?
    Initialized = 0b0000_0001,

    /// Has this process been destructed?
    Destructed = 0b0000_0010,

    /// Is this process a clone?
    Clone = 0b0000_0100,

    /// Have commands been enabled on this object?
    CommandsEnabled = 0b0000_1000,
}

impl From<ObjectFlags> for u8 {
    fn from(flags: ObjectFlags) -> u8 {
        flags as u8
    }
}

impl BitOr for ObjectFlags {
    type Output = u8;

    fn bitor(self, rhs: Self) -> Self::Output {
        (self as u8) | (rhs as u8)
    }
}

impl BitAnd for ObjectFlags {
    type Output = u8;

    fn bitand(self, rhs: Self) -> Self::Output {
        (self as u8) & (rhs as u8)
    }
}

impl PartialEq<u8> for ObjectFlags {
    fn eq(&self, other: &u8) -> bool {
        *self as u8 == *other
    }
}

impl PartialEq<ObjectFlags> for u8 {
    fn eq(&self, other: &ObjectFlags) -> bool {
        *self == *other as u8
    }
}

/// A bitfield where the bits can be set and cleared atomically.
#[derive(Debug)]
pub struct AtomicFlags<T> {
    flags: AtomicU8,
    _marker: std::marker::PhantomData<T>,
}

impl<T> AtomicFlags<T>
where
    T: Into<u8>,
{
    pub fn new() -> Self {
        Self::default()
    }

    #[inline]
    pub fn set<U>(&self, flag: U) -> u8
    where
        U: Into<u8>,
    {
        self.flags.fetch_or(flag.into(), Ordering::Relaxed)
    }

    #[inline]
    pub fn clear<U>(&self, flag: U) -> u8
    where
        U: Into<u8>,
    {
        self.flags.fetch_and(!flag.into(), Ordering::Relaxed)
    }

    #[inline]
    pub fn test<U>(&self, flag: U) -> bool
    where
        U: Into<u8>,
    {
        (self.flags.load(Ordering::Relaxed) & flag.into()) != 0
    }
}

impl<T> Default for AtomicFlags<T> {
    fn default() -> Self {
        Self {
            flags: AtomicU8::new(0),
            _marker: std::marker::PhantomData,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_atomic_flags() {
        let flags = AtomicFlags::<ObjectFlags>::new();
        assert!(!flags.test(ObjectFlags::Initialized));
        assert!(!flags.test(ObjectFlags::Destructed));
        assert!(!flags.test(ObjectFlags::Clone));
        assert_eq!(flags.set(ObjectFlags::Initialized), 0);
        assert!(flags.test(ObjectFlags::Initialized));
        assert!(!flags.test(ObjectFlags::Destructed));
        assert!(!flags.test(ObjectFlags::Clone));
        assert_eq!(flags.set(ObjectFlags::Destructed), 1);
        assert!(flags.test(ObjectFlags::Initialized));
        assert!(flags.test(ObjectFlags::Destructed));
        assert!(!flags.test(ObjectFlags::Clone));
        assert_eq!(
            flags.clear(ObjectFlags::Initialized),
            ObjectFlags::Initialized | ObjectFlags::Destructed
        );
        assert!(!flags.test(ObjectFlags::Initialized));
        assert!(flags.test(ObjectFlags::Destructed));
        assert!(!flags.test(ObjectFlags::Clone));
        assert_eq!(
            flags.clear(ObjectFlags::Destructed),
            ObjectFlags::Destructed
        );
        assert!(!flags.test(ObjectFlags::Initialized));
        assert!(!flags.test(ObjectFlags::Destructed));
        assert!(!flags.test(ObjectFlags::Clone));
        assert_eq!(flags.set(ObjectFlags::Clone), 0);
        assert!(!flags.test(ObjectFlags::Initialized));
        assert!(!flags.test(ObjectFlags::Destructed));
        assert!(flags.test(ObjectFlags::Clone));
        assert_eq!(flags.clear(ObjectFlags::Clone), ObjectFlags::Clone);
        assert!(!flags.test(ObjectFlags::Initialized));
        assert!(!flags.test(ObjectFlags::Destructed));
        assert!(!flags.test(ObjectFlags::Clone));

        flags.set(ObjectFlags::Clone | ObjectFlags::Initialized);
        assert!(flags.test(ObjectFlags::Clone));
    }
}
