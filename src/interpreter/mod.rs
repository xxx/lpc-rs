pub(crate) mod apply;
pub mod bank;
pub mod call_frame;
pub mod call_outs;
pub mod call_stack;
pub(crate) mod compile_gate;
pub mod efun;
pub mod function_type;
pub(crate) mod json;

pub mod lpc_array;
pub mod lpc_float;
pub mod lpc_int;
pub mod lpc_mapping;
pub mod lpc_ref;
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
/// Living apply: receives in-game output addressed to the living (`doc/apply/living/catch_tell.md`).
pub const CATCH_TELL: &str = "catch_tell";
/// Master apply: called with the living and its line when no rule handled it (`doc/apply/master/command_not_found.md`).
pub const COMMAND_NOT_FOUND: &str = "command_not_found";
/// Master apply: whether the calling object may `exec` (`doc/apply/master/valid_exec.md`).
pub const VALID_EXEC: &str = "valid_exec";
/// Master apply: whether the calling code may read a lib file (`doc/apply/master/valid_read.md`).
pub const VALID_READ: &str = "valid_read";
/// Master apply: whether the calling code may write a lib file (`doc/apply/master/valid_write.md`).
pub const VALID_WRITE: &str = "valid_write";
/// Master apply: whether the calling code may compile a file into an object (`doc/apply/master/valid_load.md`).
pub const VALID_LOAD: &str = "valid_load";
/// Master apply: names the blueprint for a path that has no source file (`doc/apply/master/compile_object.md`).
pub const COMPILE_OBJECT: &str = "compile_object";
/// Master apply: whether a program being compiled may inherit another (`doc/apply/master/valid_inherit.md`).
pub const VALID_INHERIT: &str = "valid_inherit";
/// Master apply: returns the object a new connection becomes (`doc/apply/master/connect.md`).
pub const CONNECT: &str = "connect";
/// Master apply: receives each uncaught runtime error (`doc/apply/master/error_handler.md`).
pub const ERROR_HANDLER: &str = "error_handler";
/// Master apply: receives each warning a successful compile raised (`doc/apply/master/warning_handler.md`).
pub const WARNING_HANDLER: &str = "warning_handler";
/// Master apply: the MSSP variables a client is told (`doc/apply/master/get_mud_stats.md`).
pub const GET_MUD_STATS: &str = "get_mud_stats";
/// Body apply: a GMCP message from the client (`doc/apply/special/gmcp.md`).
pub const GMCP: &str = "gmcp";
/// Object apply: the noun-matching fallback when `parse_command_id_list` is undefined (`doc/apply/object/id.md`).
pub const ID: &str = "id";
/// Object apply: called on each object that comes into a living's presence (`doc/apply/object/init.md`).
pub const INIT: &str = "init";
/// Object apply: whether a container's contents can be reached by the parser package (`doc/apply/object/inventory_accessible.md`).
pub const INVENTORY_ACCESSIBLE: &str = "inventory_accessible";
/// Object apply: whether a container's contents are in the parser package's scope (`doc/apply/object/inventory_visible.md`).
pub const INVENTORY_VISIBLE: &str = "inventory_visible";
/// Object apply: called in the body `connect` returned (`doc/apply/master/connect.md`).
pub const LOGON: &str = "logon";
/// Body apply: the client went away without logging out (`doc/apply/special/net_dead.md`).
pub const NET_DEAD: &str = "net_dead";
/// Object/master apply: the adjectives a phrase may put before the object's ids (`doc/apply/object/parse_command_adjectiv_id_list.md`).
pub const PARSE_COMMAND_ADJECTIV_ID_LIST: &str = "parse_command_adjectiv_id_list";
/// Master apply: the word meaning every match (`doc/apply/master/parse_command_all_word.md`).
pub const PARSE_COMMAND_ALL_WORD: &str = "parse_command_all_word";
/// Object/master apply: the singular nouns naming the object (`doc/apply/object/parse_command_id_list.md`).
pub const PARSE_COMMAND_ID_LIST: &str = "parse_command_id_list";
/// Master apply: the number a non-digit word stands for (`doc/apply/master/parse_command_numeral.md`).
pub const PARSE_COMMAND_NUMERAL: &str = "parse_command_numeral";
/// Master apply: derives plural nouns from an object's singular ids (`doc/apply/master/parse_command_pluralize.md`).
pub const PARSE_COMMAND_PLURALIZE: &str = "parse_command_pluralize";
/// Object/master apply: the plural nouns naming the object (`doc/apply/object/parse_command_plural_id_list.md`).
pub const PARSE_COMMAND_PLURAL_ID_LIST: &str = "parse_command_plural_id_list";
/// Master apply: the prepositions `%p` recognises when a rule gives none (`doc/apply/master/parse_command_prepos_list.md`).
pub const PARSE_COMMAND_PREPOS_LIST: &str = "parse_command_prepos_list";
/// Master apply: the livings `LIV`/`LVS` may name beyond the scope (`doc/apply/master/parse_command_users.md`).
pub const PARSE_COMMAND_USERS: &str = "parse_command_users";
/// Master apply: the message for a parser-package failure (`doc/apply/master/parser_error_message.md`).
pub const PARSER_ERROR_MESSAGE: &str = "parser_error_message";
/// Living apply: sees each command line before dispatch (`doc/apply/living/process_input.md`).
pub const PROCESS_INPUT: &str = "process_input";
/// Master apply: called as the driver shuts down (`doc/apply/master/shutdown.md`).
pub const SHUTDOWN: &str = "shutdown";
/// Body apply: the window size the client reported (`doc/apply/special/window_size.md`).
pub const WINDOW_SIZE: &str = "window_size";
/// Body apply: the prompt after a command (`doc/apply/special/write_prompt.md`).
pub const WRITE_PROMPT: &str = "write_prompt";
