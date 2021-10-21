pub mod call_stack;
pub mod efun;
pub mod task_context;
pub mod function_evaluator;
pub mod function_type;
pub mod instruction_counter;
pub mod lpc_ref;
pub mod lpc_value;
pub mod memory;
pub mod object_space;
pub mod pragma_flags;
pub mod process;
pub mod program;
pub mod stack_frame;
pub mod vm;

pub const MAX_CALL_STACK_SIZE: usize = 64;
