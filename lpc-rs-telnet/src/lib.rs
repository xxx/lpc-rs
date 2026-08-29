//! The telnet session for lpc-rs: one connection's protocol state, with no socket.
//!
//! Bytes from the client go into [`Session::feed`] and come out as [`Event`]s;
//! what the driver wants to say goes in as an [`Op`] and comes out of
//! [`Session::drain_output`] as bytes. Negotiation is answered inside, from a
//! fixed policy: the caller never sees a DO or a WILL.
#![forbid(unsafe_code)]
#![warn(missing_docs)]

mod opt;

pub use opt::Opt;
