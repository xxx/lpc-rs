//! The command system: one grammar engine that every parsing frontend
//! compiles into. The registry, dispatch, presence, and the add_action
//! frontend now exist; the resolver and the other frontends follow.

pub(crate) mod command_task;
pub mod dispatch;
pub mod frontend;
pub mod grammar;
pub mod presence;
pub mod registry;
pub(crate) mod resolve;
