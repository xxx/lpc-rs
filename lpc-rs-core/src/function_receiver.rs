use std::fmt::{Display, Formatter};

use serde::{Deserialize, Serialize};

use crate::register::RegisterVariant;

/// The possible receivers.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum FunctionReceiver {
    /// The receiver is the object that defines the function.
    Local,

    /// The receiver is the Process stored in the [`RegisterVariant`].
    Var(RegisterVariant),

    /// The receiver will be filled-in at call time, with the first argument
    /// passed to the call. i.e. the `&->foo()` syntax
    Dynamic,

    /// The function is an Efun, and doesn't have a receiver.
    Efun,

    /// The function is a SimulEfun, and doesn't have a receiver.
    SimulEfun,
}

impl Display for FunctionReceiver {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            FunctionReceiver::Local => write!(f, "local"),
            FunctionReceiver::Var(reg) => write!(f, "var({reg})"),
            FunctionReceiver::Dynamic => write!(f, "dynamic"),
            FunctionReceiver::Efun => write!(f, "efun"),
            FunctionReceiver::SimulEfun => write!(f, "simul_efun"),
        }
    }
}
