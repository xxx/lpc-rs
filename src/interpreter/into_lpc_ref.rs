use std::sync::Weak;

use parking_lot::RwLock;

use crate::interpreter::{lpc_ref::LpcRef, memory::Memory, process::Process};

pub trait IntoLpcRef {
    /// Consume myself and turn into an [`LpcRef`].
    fn into_lpc_ref(self, memory: &Memory) -> LpcRef;
}

impl IntoLpcRef for Weak<RwLock<Process>> {
    fn into_lpc_ref(self, memory: &Memory) -> LpcRef {
        memory.alloc_process(self)
    }
}
