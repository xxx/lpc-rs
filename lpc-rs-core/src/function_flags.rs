use modular_bitfield::prelude::*;
use serde::{Deserialize, Serialize};

use crate::{mangle::Mangle, visibility::Visibility};

/// A struct to keep track of the various boolean flags that can be set
/// on functions, like `varargs` and `static`, as well as whether the function
/// takes ellipsis args.
#[bitfield(filled = false)]
#[derive(Debug, Copy, Clone, Eq, PartialOrd, PartialEq, Hash, Default, Serialize, Deserialize)]
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

impl Mangle for FunctionFlags {
    fn mangle(&self) -> String {
        let mut result = String::new();

        match self.visibility() {
            Visibility::Public => {
                result.push_str("pb");
            }
            Visibility::Private => {
                result.push_str("pv");
            }
            Visibility::Protected => {
                result.push_str("pt");
            }
        }

        let mut flag_str = String::new();

        if self.ellipsis() {
            flag_str.push('e');
        }

        if self.varargs() {
            flag_str.push('v');
        }

        if self.nomask() {
            flag_str.push('n');
        }

        if !flag_str.is_empty() {
            result.push('_');
            result.push_str(&flag_str);
        }

        result
    }
}

impl<T> From<&[T]> for FunctionFlags
where
    T: AsRef<str>,
{
    fn from(vec: &[T]) -> Self {
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
