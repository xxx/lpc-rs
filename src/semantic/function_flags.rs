use modular_bitfield::prelude::*;

/// A struct to keep track of the various boolean flags that can be set
/// on functions, like `varargs` and `static`, as well as whether the function
/// takes ellipsis args.
#[bitfield(filled = false)]
#[derive(Debug, Copy, Clone, Eq, PartialOrd, PartialEq, Default, Serialize, Deserialize)]
pub struct FunctionFlags {
    pub ellipsis: bool,
    pub varargs: bool,
    pub public: bool,
    pub private: bool,
    pub protected: bool,
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
                "public" => {
                    flags.set_public(true);
                }
                "private" => {
                    flags.set_private(true);
                }
                "protected" => {
                    flags.set_protected(true);
                }
                "static" => {
                    flags.set_protected(true);
                }
                _ => {}
            }
        }

        flags
    }
}
