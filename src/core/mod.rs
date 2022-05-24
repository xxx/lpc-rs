pub mod call_namespace;
pub mod function_arity;
pub mod lpc_type;
pub mod lpc_type_union;
pub mod register;

/// Name of the user-overridable initializer function for objects
pub const CREATE_FUNCTION: &str = "create";

/// Name of the function for initialization of a program's global variables.
/// Note, this name cannot be parsed, so the user is unable to override it.
pub const INIT_PROGRAM: &str = "init-program";

/// Reserved efun inherit namespace
pub const EFUN: &str = "efun";
