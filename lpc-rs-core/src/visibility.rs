use std::fmt::{Display, Formatter};

use modular_bitfield::prelude::*;

#[derive(Specifier, Debug, Copy, Clone, Eq, Hash, PartialOrd, PartialEq)]
#[bits = 2]
#[derive(Default)]
pub enum Visibility {
    #[default]
    Public,
    Private,
    Protected,
}

impl Visibility {
    /// Restrict an inherited declaration without making it more visible.
    pub fn restricted_by(self, inheritance: Self) -> Self {
        match (self, inheritance) {
            (Self::Private, _) | (_, Self::Private) => Self::Private,
            (Self::Protected, _) | (_, Self::Protected) => Self::Protected,
            _ => Self::Public,
        }
    }
}

impl Display for Visibility {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Visibility::Public => "public",
            Visibility::Private => "private",
            Visibility::Protected => "protected",
        };

        write!(f, "{s}")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn inheritance_keeps_the_more_restrictive_visibility() {
        use Visibility::{Private, Protected, Public};

        for (declared, inheritance, expected) in [
            (Public, Public, Public),
            (Public, Protected, Protected),
            (Public, Private, Private),
            (Protected, Public, Protected),
            (Protected, Protected, Protected),
            (Protected, Private, Private),
            (Private, Public, Private),
            (Private, Protected, Private),
            (Private, Private, Private),
        ] {
            assert_eq!(
                declared.restricted_by(inheritance),
                expected,
                "{declared} inherited as {inheritance}"
            );
        }
    }

    #[test]
    fn visibility_names_are_lpc_modifiers() {
        for (visibility, expected) in [
            (Visibility::Public, "public"),
            (Visibility::Protected, "protected"),
            (Visibility::Private, "private"),
        ] {
            assert_eq!(visibility.to_string(), expected);
        }
    }
}
