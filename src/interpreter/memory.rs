use crate::{
    interpreter::{lpc_ref::LpcRef, lpc_value::LpcValue},
    value_to_ref,
};
use refpool::{Pool, PoolRef};
use std::{borrow::Cow, cell::RefCell};

/// The initial size (in cells) of system memory
const MEMORY_SIZE: usize = 100_000;

/// Encapsulate the shared VM memory. All tasks share the same pool.
#[derive(Debug, Clone)]
pub struct Memory {
    /// Where things are actually stored. Only reference types use any space from this pool.
    pool: Pool<RefCell<LpcValue>>,
}

impl Memory {
    /// Create a new [`Memory`], with space for `size` values.
    /// In practice, only reference types use the memory pool.
    pub fn new(size: usize) -> Self {
        Self {
            pool: Pool::new(size),
        }
    }

    /// Convert an [`LpcValue`] to an [`LpcRef`], allocating from the pool if necessary
    pub fn value_to_ref(&self, value: LpcValue) -> LpcRef {
        value_to_ref!(value, self.pool)
    }
}

impl Default for Memory {
    fn default() -> Self {
        Self {
            pool: Pool::new(MEMORY_SIZE),
        }
    }
}

impl<'pool> From<&'pool Memory> for Cow<'pool, Memory> {
    fn from(memory: &'pool Memory) -> Self {
        Cow::Borrowed(memory)
    }
}

impl<'pool> From<Memory> for Cow<'pool, Memory> {
    fn from(memory: Memory) -> Self {
        Cow::Owned(memory)
    }
}
