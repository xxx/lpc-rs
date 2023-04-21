pub mod bank;
pub mod call_frame;
pub mod call_outs;
pub mod call_stack;
pub mod efun;
pub mod function_type;
pub mod gc;
pub mod heap;
pub mod into_lpc_ref;
pub mod lpc_array;
pub mod lpc_float;
pub mod lpc_int;
pub mod lpc_mapping;
pub mod lpc_ref;
pub mod lpc_string;
pub mod object_space;
pub mod process;
pub mod program;
pub mod task;
pub mod task_context;
pub mod vm;


// Applies - functions in LPC objects that are called directly by the driver at various times.
pub const CONNECT: &str = "connect";
pub const LOGON: &str = "logon";