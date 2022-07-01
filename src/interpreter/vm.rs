use crate::interpreter::{call_frame::CallFrame, memory::Memory, object_space::ObjectSpace};

pub struct Vm {
    /// The call stack
    pub stack: Vec<CallFrame>,

    /// Our object space, which stores all of the system objects (masters and clones)
    pub object_space: ObjectSpace,

    /// Shared VM memory
    pub memory: Memory,
}
