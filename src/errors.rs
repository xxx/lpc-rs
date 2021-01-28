use std::fmt::{Display, Formatter};
use crate::semantic::symbol::Symbol;
use std::fmt;
use crate::parser::span::Span;

/// Error for duplicate var definitions in a single local scope.
#[derive(Debug, Clone)]
pub struct VarRedefinitionError<'a> {
    /// Reference to the original symbol
    pub symbol: &'a Symbol,
    /// The span of the *re*definition
    pub span: Option<Span>
}

impl Display for VarRedefinitionError<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "Redefinition of `{}`", self.symbol.name)
    }
}