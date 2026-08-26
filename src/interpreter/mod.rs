pub mod bank;
pub mod call_frame;
pub mod call_outs;
pub mod call_stack;
pub mod efun;
pub mod function_type;

pub mod lpc_array;
pub mod lpc_float;
pub mod lpc_int;
pub mod lpc_mapping;
pub mod lpc_ref;
pub mod lpc_string;
pub mod object_space;
#[cfg(feature = "opcode-profile")]
pub mod opcode_profile;
pub mod process;
pub mod program;
pub(crate) mod stm;
pub mod task;
pub mod task_context;
#[cfg(test)]
mod tests;
pub mod vm;

/// Sync "read the latest committed world" API for non-transactional
/// readers (test/debug/tooling).
pub use stm::CommittedReader;
pub use stm::{AttemptTelemetrySnapshot, CommitterStats};
pub use stm::{GcRefused, GcReport};

// Applies - functions in LPC objects that are called directly by the driver at various times.
pub const CATCH_TELL: &str = "catch_tell";
pub const COMMAND_NOT_FOUND: &str = "command_not_found";
pub const CONNECT: &str = "connect";
pub const ERROR_HANDLER: &str = "error_handler";
pub const ID: &str = "id";
pub const INIT: &str = "init";
pub const LOGON: &str = "logon";
pub const PARSE_COMMAND_ADJECTIV_ID_LIST: &str = "parse_command_adjectiv_id_list";
pub const PARSE_COMMAND_ALL_WORD: &str = "parse_command_all_word";
pub const PARSE_COMMAND_ID_LIST: &str = "parse_command_id_list";
pub const PARSE_COMMAND_NUMERAL: &str = "parse_command_numeral";
pub const PARSE_COMMAND_PLURALIZE: &str = "parse_command_pluralize";
pub const PARSE_COMMAND_PLURAL_ID_LIST: &str = "parse_command_plural_id_list";
pub const PARSE_COMMAND_PREPOS_LIST: &str = "parse_command_prepos_list";
pub const PROCESS_INPUT: &str = "process_input";
pub const SHUTDOWN: &str = "shutdown";
