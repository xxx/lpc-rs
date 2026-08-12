use std::sync::{Arc, Weak};

use parking_lot::RwLock;

use crate::interpreter::{
    function_type::function_ptr::FunctionPtr, into_lpc_ref::IntoLpcRef, lpc_array::LpcArray,
    lpc_mapping::LpcMapping, lpc_ref::LpcRef, lpc_string::LpcString, process::Process,
};

/// The initial size (in cells) of system memory. This number is divided evenly
/// between the different types. This is just the initial allocation - more will
/// be allocated as necessary.
const MEMORY_SIZE: usize = 5_000;

/// Encapsulate the shared VM heap.
#[derive(Debug)]
pub struct Heap;

impl Heap {
    /// Create a new [`Heap`], with `size` slots for _each_ type.
    pub fn new(size: usize) -> Self {
        Self
    }

    /// Allocate a new [`LpcRef`]
    #[inline]
    pub fn alloc<T: IntoLpcRef>(&self, value: T) -> LpcRef {
        value.into_lpc_ref(self)
    }

    /// Allocate a new [`LpcString`]
    #[inline]
    pub fn alloc_string(&self, string: LpcString) -> LpcRef {
        let arc = Arc::new(RwLock::new(string));
        LpcRef::String(arc)
    }

    /// Allocate a new [`LpcArray`]
    #[inline]
    pub fn alloc_array(&self, array: LpcArray) -> LpcRef {
        let arc = Arc::new(RwLock::new(array));
        LpcRef::Array(arc)
    }

    /// Allocate a new [`LpcMapping`]
    #[inline]
    pub fn alloc_mapping(&self, mapping: LpcMapping) -> LpcRef {
        let arc = Arc::new(RwLock::new(mapping));
        LpcRef::Mapping(arc)
    }

    /// Allocate a new [`Process`]
    #[inline]
    pub fn alloc_process(&self, process: Weak<Process>) -> LpcRef {
        LpcRef::Object(process)
    }

    /// Allocate a new [`FunctionPtr`]
    #[inline]
    pub fn alloc_function(&self, function: FunctionPtr) -> LpcRef {
        let ptr = self.alloc_function_arc(function);
        LpcRef::Function(ptr)
    }

    /// Wrap a [`FunctionPtr`] directly with an [`Arc`].
    #[inline]
    pub fn alloc_function_arc(&self, function: FunctionPtr) -> Arc<FunctionPtr> {
        Arc::new(function)
    }
}

impl Default for Heap {
    fn default() -> Self {
        Self::new(MEMORY_SIZE / 4)
    }
}
