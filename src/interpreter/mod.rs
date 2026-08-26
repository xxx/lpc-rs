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
/// Living apply: receives in-game output addressed to the living (`doc/apply/living/catch_tell.md`).
pub const CATCH_TELL: &str = "catch_tell";
/// Master apply: called with the living and its line when no rule handled it (`doc/apply/master/command_not_found.md`).
pub const COMMAND_NOT_FOUND: &str = "command_not_found";
/// Master apply: returns the object a new connection becomes (`doc/apply/master/connect.md`).
pub const CONNECT: &str = "connect";
/// Master apply: receives each uncaught runtime error (`doc/apply/master/error_handler.md`).
pub const ERROR_HANDLER: &str = "error_handler";
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
