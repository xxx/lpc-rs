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
