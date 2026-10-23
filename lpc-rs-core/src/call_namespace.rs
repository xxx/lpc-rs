use std::fmt::{Display, Formatter};

use ustr::Ustr;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
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
            CallNamespace::Named(name) => name,
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lookup_and_display_distinguish_local_parent_and_named_namespaces() {
        for (namespace, lookup, display) in [
            (CallNamespace::Local, "", ""),
            (CallNamespace::Parent, "::", "::"),
            (CallNamespace::Named("base".into()), "base", "base::"),
        ] {
            assert_eq!(namespace.as_str(), lookup);
            assert_eq!(namespace.to_string(), display);
        }
    }
}
