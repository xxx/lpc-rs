use crate::visibility::Visibility;
use modular_bitfield::prelude::*;
use serde::{Deserialize, Serialize};

/// A struct to keep track of the various boolean flags that can be set
/// on functions, like `varargs` and `static`, as well as whether the function
/// takes ellipsis args.
#[bitfield(filled = false)]
#[derive(Debug, Copy, Clone, Eq, PartialOrd, PartialEq, Default, Serialize, Deserialize)]
pub struct FunctionFlags {
    pub ellipsis: bool,
    pub varargs: bool,
    pub nomask: bool,
    #[bits = 2]
    pub visibility: Visibility,
}

impl FunctionFlags {
    /// Is the function public?
    #[inline]
    pub fn public(&self) -> bool {
        self.visibility() == Visibility::Public
    }

    /// Is the function private?
    #[inline]
    pub fn private(&self) -> bool {
        self.visibility() == Visibility::Private
    }

    /// Is the function protected?
    #[inline]
    pub fn protected(&self) -> bool {
        self.visibility() == Visibility::Protected
    }
}

impl<T> From<Vec<T>> for FunctionFlags
where
    T: AsRef<str>,
{
    fn from(vec: Vec<T>) -> Self {
        let mut flags = Self::default();
        for s in vec {
            match s.as_ref() {
                "varargs" => {
                    flags.set_varargs(true);
                }
                "nomask" => {
                    flags.set_nomask(true);
                }
                "public" => {
                    flags.set_visibility(Visibility::Public);
                }
                "private" => {
                    flags.set_visibility(Visibility::Private);
                }
                "protected" => {
                    flags.set_visibility(Visibility::Protected);
                }
                "static" => {
                    flags.set_visibility(Visibility::Protected);
                }
                _ => {}
            }
        }

        flags
    }
}
