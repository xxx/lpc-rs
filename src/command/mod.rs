//! The command system: one grammar engine that every parsing frontend
//! compiles into. The registry, dispatch, presence, scope, the add_action
//! frontend, and the noun resolver exist and are wired together; other
//! frontends follow.

pub(crate) mod command_task;
pub mod dispatch;
pub mod frontend;
pub mod grammar;
pub(crate) mod memo;
pub(crate) mod parser;
pub mod presence;
pub mod registry;
pub(crate) mod resolve;
pub mod scope;
pub(crate) mod trial;
