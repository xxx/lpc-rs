//! The command system: one grammar engine that every parsing frontend
//! compiles into. The registry, dispatch, presence, the add_action
//! frontend, and the noun resolver exist and are wired together; other
//! frontends follow.

pub(crate) mod command_task;
pub mod dispatch;
pub mod frontend;
pub mod grammar;
#[expect(dead_code, reason = "parser::run is wired to dispatch in a later task")]
pub(crate) mod parser;
pub mod presence;
pub mod registry;
pub(crate) mod resolve;
