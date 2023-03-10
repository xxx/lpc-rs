use std::fmt::{Display, Formatter};

use modular_bitfield::prelude::*;
use serde::{Deserialize, Serialize};

#[derive(
    BitfieldSpecifier, Debug, Copy, Clone, Eq, PartialOrd, PartialEq, Serialize, Deserialize,
)]
#[bits = 2]
#[derive(Default)]
pub enum Visibility {
    #[default]
    Public,
    Private,
    Protected,
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


