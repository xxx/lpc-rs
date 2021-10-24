use crate::interpreter::{memory::Memory, object_space::ObjectSpace, stack_frame::StackFrame};

pub struct Vm {
    /// The call stack
    pub stack: Vec<StackFrame>,

    /// Our object space, which stores all of the system objects (masters and clones)
    pub object_space: ObjectSpace,

    /// Shared VM memory
    pub memory: Memory,
}
