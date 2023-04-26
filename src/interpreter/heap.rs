use std::sync::Weak;

use parking_lot::RwLock;
use shared_arena::SharedArena;

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
pub struct Heap {
    /// The string arena
    string_pool: SharedArena<RwLock<LpcString>>,

    /// The array arena
    array_pool: SharedArena<RwLock<LpcArray>>,

    /// The mapping arena
    mapping_pool: SharedArena<RwLock<LpcMapping>>,

    /// The object arena
    object_pool: SharedArena<Weak<Process>>,

    /// The function arena
    function_pool: SharedArena<RwLock<FunctionPtr>>,
}

impl Heap {
    /// Create a new [`Heap`], with `size` slots for _each_ type.
    pub fn new(size: usize) -> Self {
        Self {
            string_pool: SharedArena::with_capacity(size),
            array_pool: SharedArena::with_capacity(size),
            mapping_pool: SharedArena::with_capacity(size),
            object_pool: SharedArena::with_capacity(size),
            function_pool: SharedArena::with_capacity(size),
        }
    }

    /// Allocate a new [`LpcRef`]
    #[inline]
    pub fn alloc<T: IntoLpcRef>(&self, value: T) -> LpcRef {
        value.into_lpc_ref(self)
    }

    /// Allocate a new [`LpcString`]
    #[inline]
    pub fn alloc_string(&self, string: LpcString) -> LpcRef {
        let arc = self.string_pool.alloc_arc(RwLock::new(string));
        LpcRef::String(arc)
    }

    /// Allocate a new [`LpcArray`]
    #[inline]
    pub fn alloc_array(&self, array: LpcArray) -> LpcRef {
        let arc = self.array_pool.alloc_arc(RwLock::new(array));
        LpcRef::Array(arc)
    }

    /// Allocate a new [`LpcMapping`]
    #[inline]
    pub fn alloc_mapping(&self, mapping: LpcMapping) -> LpcRef {
        let arc = self.mapping_pool.alloc_arc(RwLock::new(mapping));
        LpcRef::Mapping(arc)
    }

    /// Allocate a new [`Process`]
    #[inline]
    pub fn alloc_process(&self, process: Weak<Process>) -> LpcRef {
        let arc = self.object_pool.alloc_arc(process);
        LpcRef::Object(arc)
    }

    /// Allocate a new [`FunctionPtr`]
    #[inline]
    pub fn alloc_function(&self, function: FunctionPtr) -> LpcRef {
        let ptr = self.function_pool.alloc_arc(RwLock::new(function));
        LpcRef::Function(ptr)
    }
}

impl Default for Heap {
    fn default() -> Self {
        Self::new(MEMORY_SIZE / 5)
    }
}