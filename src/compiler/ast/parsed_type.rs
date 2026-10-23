use std::fmt;

use lpc_rs_core::lpc_type::LpcType;
use lpc_rs_errors::{Result, lpc_bug, span::Span};
use ustr::Ustr;

/// Class names remain unresolved until inherited definitions are available.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Hash)]
pub enum ParsedType {
    Resolved(LpcType),
    Class { name: Ustr, array: bool, span: Span },
}

impl ParsedType {
    pub fn resolved(self) -> Result<LpcType> {
        match self {
            Self::Resolved(type_) => Ok(type_),
            Self::Class { name, span, .. } => {
                Err(lpc_bug!(Some(span), "unresolved class type `{name}`"))
            }
        }
    }

    pub fn as_array(self, array: bool) -> Self {
        match self {
            Self::Resolved(type_) => Self::Resolved(type_.as_array(array)),
            Self::Class { name, span, .. } => Self::Class { name, array, span },
        }
    }

    pub fn is_array(self) -> bool {
        match self {
            Self::Resolved(type_) => type_.is_array(),
            Self::Class { array, .. } => array,
        }
    }
}

impl From<LpcType> for ParsedType {
    fn from(type_: LpcType) -> Self {
        Self::Resolved(type_)
    }
}

impl PartialEq<LpcType> for ParsedType {
    fn eq(&self, other: &LpcType) -> bool {
        matches!(self, Self::Resolved(type_) if type_ == other)
    }
}

impl fmt::Display for ParsedType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Resolved(type_) => type_.fmt(f),
            Self::Class { name, array, .. } => {
                write!(f, "class {name}{}", if *array { " *" } else { "" })
            }
        }
    }
}
