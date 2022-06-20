use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum CallNamespace {
    Local,
    Parent,
    Named(String),
}

impl CallNamespace {
    pub fn as_str(&self) -> &str {
        match self {
            CallNamespace::Local => "",
            CallNamespace::Parent => "::",
            CallNamespace::Named(name) => name,
        }
    }
}

impl Default for CallNamespace {
    fn default() -> Self {
        CallNamespace::Local
    }
}

impl Display for CallNamespace {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            CallNamespace::Local => write!(f, ""),
            CallNamespace::Parent => write!(f, "::"),
            CallNamespace::Named(name) => write!(f, "{}::", name),
        }
    }
}
