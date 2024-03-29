use lpc_rs_core::{BaseFloat, LpcIntInner};
use lpc_rs_errors::span::Span;

/// Some small wrappers to store both a [`Span`] and a value for compatibility
/// with [Logos](logos), which only allows a single field in token defs.

#[derive(Debug, Clone, PartialEq)]
pub struct FloatToken(pub Span, pub BaseFloat);

#[derive(Debug, Clone, PartialEq)]
pub struct IntToken(pub Span, pub LpcIntInner);

#[derive(Debug, Clone, PartialEq)]
pub struct StringToken(pub Span, pub String);
