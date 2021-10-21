use crate::interpreter::stack_frame::StackFrame;
use crate::interpreter::object_space::ObjectSpace;
use crate::interpreter::memory::Memory;

pub struct Vm {
    /// The call stack
    pub stack: Vec<StackFrame>,

    /// Our object space, which stores all of the system objects (masters and clones)
    pub object_space: ObjectSpace,

    /// Shared VM memory
    pub memory: Memory,
}