use crate::parser::span::Span;
use crate::LpcInt;

/// Some small wrappers to store both a [`Span`] and a value for compatibility with [Logos](logos),
/// which only allows a single field in token defs.

#[derive(Debug, Clone, PartialEq)]
pub struct FloatToken(pub Span, pub f64);

#[derive(Debug, Clone, PartialEq)]
pub struct IntToken(pub Span, pub LpcInt);

#[derive(Debug, Clone, PartialEq)]
pub struct StringToken(pub Span, pub String);
