//! The command system: one grammar engine that every parsing frontend
//! compiles into. Only the engine exists yet; the rule registry, dispatch,
//! noun resolver, and the frontends build on it.

pub mod dispatch;
pub mod frontend;
pub mod grammar;
pub mod presence;
pub mod registry;
