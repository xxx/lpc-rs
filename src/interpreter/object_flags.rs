use std::ops::{BitAnd, BitOr};
use std::sync::atomic::{AtomicU8, Ordering};

/// Flags for a [`Process`]
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum ObjectFlags {
    /// Has this process been initialized?
    INITIALIZED = 0b0000_0001,

    /// Has this process been destructed?
    DESTRUCTED = 0b0000_0010,

    /// Is this process a clone?
    CLONE = 0b0000_0100,
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
    pub fn set(&self, flag: T) -> u8 {
        self.flags.fetch_or(flag.into(), Ordering::Relaxed)
    }

    #[inline]
    pub fn clear(&self, flag: T) -> u8 {
        self.flags.fetch_and(!flag.into(), Ordering::Relaxed)
    }

    #[inline]
    pub fn test(&self, flag: T) -> bool {
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
        assert_eq!(flags.test(ObjectFlags::INITIALIZED), false);
        assert_eq!(flags.test(ObjectFlags::DESTRUCTED), false);
        assert_eq!(flags.test(ObjectFlags::CLONE), false);
        assert_eq!(flags.set(ObjectFlags::INITIALIZED), 0);
        assert_eq!(flags.test(ObjectFlags::INITIALIZED), true);
        assert_eq!(flags.test(ObjectFlags::DESTRUCTED), false);
        assert_eq!(flags.test(ObjectFlags::CLONE), false);
        assert_eq!(flags.set(ObjectFlags::DESTRUCTED), 1);
        assert_eq!(flags.test(ObjectFlags::INITIALIZED), true);
        assert_eq!(flags.test(ObjectFlags::DESTRUCTED), true);
        assert_eq!(flags.test(ObjectFlags::CLONE), false);
        assert_eq!(flags.clear(ObjectFlags::INITIALIZED), ObjectFlags::INITIALIZED | ObjectFlags::DESTRUCTED);
        assert_eq!(flags.test(ObjectFlags::INITIALIZED), false);
        assert_eq!(flags.test(ObjectFlags::DESTRUCTED), true);
        assert_eq!(flags.test(ObjectFlags::CLONE), false);
        assert_eq!(flags.clear(ObjectFlags::DESTRUCTED), ObjectFlags::DESTRUCTED);
        assert_eq!(flags.test(ObjectFlags::INITIALIZED), false);
        assert_eq!(flags.test(ObjectFlags::DESTRUCTED), false);
        assert_eq!(flags.test(ObjectFlags::CLONE), false);
        assert_eq!(flags.set(ObjectFlags::CLONE), 0);
        assert_eq!(flags.test(ObjectFlags::INITIALIZED), false);
        assert_eq!(flags.test(ObjectFlags::DESTRUCTED), false);
        assert_eq!(flags.test(ObjectFlags::CLONE), true);
        assert_eq!(flags.clear(ObjectFlags::CLONE), ObjectFlags::CLONE);
        assert_eq!(flags.test(ObjectFlags::INITIALIZED), false);
        assert_eq!(flags.test(ObjectFlags::DESTRUCTED), false);
        assert_eq!(flags.test(ObjectFlags::CLONE), false);
    }
}