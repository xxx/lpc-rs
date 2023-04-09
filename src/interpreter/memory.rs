use std::cell::RefCell;

use shared_arena::{Arena, SharedArena};
use parking_lot::RwLock;

use crate::{
    interpreter::{lpc_ref::LpcRef, lpc_value::LpcValue},
    value_to_ref,
};

/// The initial size (in cells) of system memory
const MEMORY_SIZE: usize = 100_000;

/// Encapsulate the shared VM heap. All tasks share the same pool.
#[derive(Debug)]
pub struct Memory {
    /// Where things are actually stored. Only reference types use any space
    /// from this pool.
    pool: SharedArena<RwLock<LpcValue>>,
}

impl Memory {
    /// Create a new [`Memory`], with space for `size` values.
    /// In practice, only reference types use the memory pool.
    pub fn new(size: usize) -> Self {
        Self {
            pool: SharedArena::with_capacity(size),
        }
    }

    /// Convert an [`LpcValue`] to an [`LpcRef`], allocating from the pool if
    /// necessary
    pub fn value_to_ref(&self, value: LpcValue) -> LpcRef {
        value_to_ref!(value, self.pool)
    }
}

impl Default for Memory {
    fn default() -> Self {
        Self::new(MEMORY_SIZE)
    }
}
