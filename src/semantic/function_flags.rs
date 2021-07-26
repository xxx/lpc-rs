use modular_bitfield::prelude::*;

// A struct to keep track of the various boolean flags that can be set
// on functions, like `varargs` and `static`, as well as whether the function
// takes ellipsis args.

#[bitfield(filled = false)]
#[derive(Debug, Copy, Clone, Eq, PartialOrd, PartialEq, Default)]
pub struct FunctionFlags {
    pub ellipsis: bool,
    pub varargs: bool,
}
