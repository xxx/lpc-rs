use std::sync::Weak;

use parking_lot::RwLock;

use crate::interpreter::{heap::Heap, lpc_ref::LpcRef, process::Process};

pub trait IntoLpcRef {
    /// Consume myself and turn into an [`LpcRef`].
    fn into_lpc_ref(self, memory: &Heap) -> LpcRef;
}

impl IntoLpcRef for Weak<Process> {
    fn into_lpc_ref(self, memory: &Heap) -> LpcRef {
        memory.alloc_process(self)
    }
}
