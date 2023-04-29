use derive_builder::Builder;
use serde::{Deserialize, Serialize};

use crate::RegisterSize;

/// A struct to hold data about a function's expected arity at call time.
#[derive(Debug, Copy, Clone, Default, PartialEq, Eq, Serialize, Deserialize, Builder)]
pub struct FunctionArity {
    /// The number of explicitly-specified parameters
    /// For partial applications, this is the arity of the underlying function,
    /// without taking partial parameters into account.
    #[builder(default)]
    pub num_args: RegisterSize,

    /// The number of arguments that defaults were specified for
    #[builder(default)]
    pub num_default_args: RegisterSize,

    /// Has an ellipsis arg been declared for this function?
    #[builder(default)]
    pub ellipsis: bool,

    /// Is the function `varargs`?
    #[builder(default)]
    pub varargs: bool,
}

impl FunctionArity {
    /// create a new [`FunctionArity`] with the passed arity
    pub fn new(num_args: RegisterSize) -> Self {
        Self {
            num_args,
            ..Default::default()
        }
    }

    /// Is the passed length valid for this arity?
    /// This takes `varargs` and ellipsis args into account.
    #[inline]
    pub fn is_valid(&self, len: usize) -> bool {
        if len > RegisterSize::MAX as usize {
            return false;
        }

        let len = len as RegisterSize;

        match (self.varargs, self.ellipsis) {
            (true, true) => true,
            (true, false) => len <= self.num_args,
            (false, true) => len >= self.net_args(),
            (false, false) => {
                let range = (self.num_args - self.num_default_args)..=self.num_args;
                range.contains(&len)
            }
        }
    }

    #[inline]
    fn net_args(&self) -> RegisterSize {
        self.num_args - self.num_default_args
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    mod test_function_arity {
        use super::*;

        #[test]
        fn test_is_valid() {
            let arity = FunctionArity {
                num_args: 5,
                num_default_args: 3,
                varargs: false,
                ellipsis: false,
            };

            assert!(!arity.is_valid(0));
            assert!(!arity.is_valid(1));
            assert!(arity.is_valid(2));
            assert!(arity.is_valid(3));
            assert!(arity.is_valid(4));
            assert!(arity.is_valid(5));
            assert!(!arity.is_valid(6));

            let arity = FunctionArity {
                num_args: 5,
                num_default_args: 3,
                varargs: true,
                ellipsis: false,
            };

            assert!(arity.is_valid(0));
            assert!(arity.is_valid(1));
            assert!(arity.is_valid(2));
            assert!(arity.is_valid(3));
            assert!(arity.is_valid(4));
            assert!(arity.is_valid(5));
            assert!(!arity.is_valid(6));

            let arity = FunctionArity {
                num_args: 5,
                num_default_args: 3,
                varargs: false,
                ellipsis: true,
            };

            assert!(!arity.is_valid(0));
            assert!(!arity.is_valid(1));
            assert!(arity.is_valid(2));
            assert!(arity.is_valid(3));
            assert!(arity.is_valid(4));
            assert!(arity.is_valid(5));
            assert!(arity.is_valid(6));

            let arity = FunctionArity {
                num_args: 5,
                num_default_args: 3,
                varargs: true,
                ellipsis: true,
            };

            assert!(arity.is_valid(0));
            assert!(arity.is_valid(1));
            assert!(arity.is_valid(2));
            assert!(arity.is_valid(3));
            assert!(arity.is_valid(4));
            assert!(arity.is_valid(5));
            assert!(arity.is_valid(6));
        }
    }
}
