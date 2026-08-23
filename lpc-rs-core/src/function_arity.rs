use derive_builder::Builder;

use crate::RegisterSize;

/// A struct to hold data about a function's expected arity at call time.
#[derive(Debug, Copy, Clone, Default, PartialEq, Eq, Builder)]
pub struct FunctionArity {
    /// The number of explicitly-specified parameters
    /// For partial applications, this is the arity of the underlying function,
    /// without taking partial parameters into account.
    #[builder(default)]
    pub num_args: RegisterSize,

    /// The number of arguments that defaults were specified for
    #[builder(default)]
    pub num_default_args: RegisterSize,
}

impl FunctionArity {
    /// create a new [`FunctionArity`] with the passed arity
    pub fn new(num_args: RegisterSize) -> Self {
        Self {
            num_args,
            ..Default::default()
        }
    }
}
