use std::fmt::{Display, Formatter};

use modular_bitfield::prelude::*;
use serde::{Deserialize, Serialize};

use crate::visibility::Visibility;

/// A struct to keep track of the various boolean flags that can be set
/// on global variables, like `public` and `private`.
#[bitfield(filled = false)]
#[derive(Debug, Copy, Clone, Eq, PartialOrd, PartialEq, Default, Hash, Serialize, Deserialize)]
pub struct GlobalVarFlags {
    #[bits = 2]
    pub visibility: Visibility,
    pub is_static: bool,
}

impl Display for GlobalVarFlags {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut s = self.visibility().to_string();
        if self.is_static() {
            s.push_str(" static");
        }
        write!(f, "{s}")
    }
}

impl GlobalVarFlags {
    pub fn validate<'a>(strs: &'_ [&'a str]) -> Vec<&'a str> {
        let mut invalid = vec![];
        for s in strs.iter() {
            match *s {
                "public" | "private" | "protected" | "static" => {}
                _ => {
                    invalid.push(*s);
                }
            }
        }

        invalid
    }
}

impl<T> From<Vec<T>> for GlobalVarFlags
where
    T: AsRef<str>,
{
    fn from(vec: Vec<T>) -> Self {
        let mut flags = Self::default();
        for s in vec {
            match s.as_ref() {
                "public" => {
                    flags.set_visibility(Visibility::Public);
                }
                "private" => {
                    flags.set_visibility(Visibility::Private);
                }
                "protected" => {
                    // Protected variables are the same as public,
                    // unless and until we support classes.
                    flags.set_visibility(Visibility::Public);
                }
                "static" => {
                    flags.set_is_static(true);
                }
                _ => {}
            }
        }

        flags
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_from() {
        let vec = vec!["public", "private"];
        let flags = GlobalVarFlags::from(vec);
        assert_eq!(flags.visibility(), Visibility::Private);

        let vec = vec!["protected", "static"];
        let flags = GlobalVarFlags::from(vec);
        assert_eq!(flags.visibility(), Visibility::Public);
        assert!(flags.is_static());

        let vec: Vec<&'static str> = vec![];
        let flags = GlobalVarFlags::from(vec);
        assert_eq!(flags.visibility(), Visibility::Public);
    }
}
