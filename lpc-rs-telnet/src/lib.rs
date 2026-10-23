//! The telnet session for lpc-rs: one connection's protocol state, with no socket.
//!
//! Bytes from the client go into [`Session::feed`] and come out as [`Event`]s;
//! what the driver wants to say goes in as an [`Op`] and comes out of
//! [`Session::drain_output`] as bytes. Negotiation is answered inside, from a
//! fixed policy: the caller never sees a DO or a WILL. Text is decoded lazily
//! by [`Session::next_event`], so [`Session::set_input_mode`] can change how
//! remaining bytes are delivered after each callback.
#![forbid(unsafe_code)]
#![warn(missing_docs)]

mod opt;
mod parser;
mod session;
mod table;
mod terminal;
mod wire;

pub use opt::Opt;
pub use session::{Event, InputMode, MAX_LINE, Op, Session, Stats};
pub use terminal::{ColourDepth, TerminalInfo};
