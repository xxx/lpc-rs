use std::fmt::{Display, Formatter};

use serde::{Deserialize, Serialize};
use ustr::Ustr;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize, Default)]
pub enum CallNamespace {
    #[default]
    Local,
    Parent,
    Named(Ustr),
}

impl CallNamespace {
    pub fn as_str(&self) -> &str {
        match self {
            CallNamespace::Local => "",
            CallNamespace::Parent => "::",
            CallNamespace::Named(name) => &*name,
        }
    }
}

impl Display for CallNamespace {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            CallNamespace::Local => write!(f, ""),
            CallNamespace::Parent => write!(f, "::"),
            CallNamespace::Named(name) => write!(f, "{name}::"),
        }
    }
}
