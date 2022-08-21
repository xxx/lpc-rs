use std::fmt::{Display, Formatter};

use serde::{Deserialize, Serialize};

use crate::register::RegisterVariant;

/// An enum to handle function names that are either vars or literal names.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum FunctionName {
    /// The name is a variable, that needs to be resolved at runtime.
    Var(RegisterVariant),
    /// The name is a literal function name, so call it directly.
    Literal(String),
}

impl Display for FunctionName {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            FunctionName::Var(reg) => write!(f, "var({})", reg),
            FunctionName::Literal(name) => write!(f, "{}", name),
        }
    }
}

/// The possible receivers.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum FunctionReceiver {
    /// The receiver is the object that defines the function.
    Local,

    /// The receiver is the Process stored in the [`RegisterVariant`].
    Var(RegisterVariant),

    /// The receiver will be filled-in at call time, with the first argument
    /// passed to the call. i.e. the `&->foo()` syntax
    Argument,
}

/// Used as the target that's stored for a function pointer
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum FunctionTarget {
    /// The call will be to an efun
    Efun(String),

    /// The call will be to an lfun defined in some object
    Local(FunctionName, FunctionReceiver),
}

impl Display for FunctionTarget {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            FunctionTarget::Efun(name) => write!(f, "{}", name),
            FunctionTarget::Local(name, receiver) => match receiver {
                FunctionReceiver::Local => write!(f, "{}", name),
                FunctionReceiver::Var(reg) => write!(f, "var({})->{}", reg, name),
                FunctionReceiver::Argument => write!(f, "&->{}", name),
            },
        }
    }
}
