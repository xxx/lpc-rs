use std::sync::Weak;

use parking_lot::RwLock;
use shared_arena::SharedArena;

use crate::interpreter::{
    function_type::function_ptr::FunctionPtr, into_lpc_ref::IntoLpcRef, lpc_array::LpcArray,
    lpc_mapping::LpcMapping, lpc_ref::LpcRef, lpc_string::LpcString, process::Process,
};

/// The initial size (in cells) of system memory
const MEMORY_SIZE: usize = 100_000;

/// Encapsulate the shared VM heap. All tasks share the same pool.
#[derive(Debug)]
pub struct Memory {
    /// The string arena
    string_pool: SharedArena<RwLock<LpcString>>,

    /// The array arena
    array_pool: SharedArena<RwLock<LpcArray>>,

    /// The mapping arena
    mapping_pool: SharedArena<RwLock<LpcMapping>>,

    /// The object arena
    object_pool: SharedArena<Weak<RwLock<Process>>>,

    /// The function arena
    function_pool: SharedArena<RwLock<FunctionPtr>>,
}

impl Memory {
    /// Create a new [`Memory`], with `size` slots for _each_ type.
    pub fn new(size: usize) -> Self {
        Self {
            string_pool: SharedArena::with_capacity(size),
            array_pool: SharedArena::with_capacity(size),
            mapping_pool: SharedArena::with_capacity(size),
            object_pool: SharedArena::with_capacity(size),
            function_pool: SharedArena::with_capacity(size),
        }
    }

    /// Convert a value to an [`LpcRef`]
    pub fn value_to_ref<V>(&self, value: V) -> LpcRef
    where
        V: IntoLpcRef,
    {
        value.into_lpc_ref(self)
    }

    /// Allocate a new [`LpcString`]
    pub fn alloc_string(&self, string: LpcString) -> LpcRef {
        let arc = self.string_pool.alloc_arc(RwLock::new(string));
        LpcRef::String(arc)
    }

    /// Allocate a new [`LpcArray`]
    pub fn alloc_array(&self, array: LpcArray) -> LpcRef {
        let arc = self.array_pool.alloc_arc(RwLock::new(array));
        LpcRef::Array(arc)
    }

    /// Allocate a new [`LpcMapping`]
    pub fn alloc_mapping(&self, mapping: LpcMapping) -> LpcRef {
        let arc = self.mapping_pool.alloc_arc(RwLock::new(mapping));
        LpcRef::Mapping(arc)
    }

    /// Allocate a new [`Process`]
    pub fn alloc_process(&self, process: Weak<RwLock<Process>>) -> LpcRef {
        let arc = self.object_pool.alloc_arc(process);
        LpcRef::Object(arc)
    }

    /// Allocate a new [`FunctionPtr`]
    pub fn alloc_function(&self, function: FunctionPtr) -> LpcRef {
        let ptr = self.function_pool.alloc_arc(RwLock::new(function));
        LpcRef::Function(ptr)
    }
}

impl Default for Memory {
    fn default() -> Self {
        Self::new(MEMORY_SIZE / 5)
    }
}
