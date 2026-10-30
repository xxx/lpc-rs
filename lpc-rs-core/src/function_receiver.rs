use std::fmt::{Display, Formatter};

use crate::register::RegisterVariant;

/// The possible receivers.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FunctionReceiver {
    /// The receiver is the object that defines the function.
    Local,

    /// The receiver is the Process stored in the [`RegisterVariant`].
    Var(RegisterVariant),

    /// The receiver is a function value read from the [`RegisterVariant`];
    /// the new pointer is built over it rather than by name.
    Value(RegisterVariant),

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
            FunctionReceiver::Value(reg) => write!(f, "value({reg})"),
            FunctionReceiver::Dynamic => write!(f, "dynamic"),
            FunctionReceiver::Efun => write!(f, "efun"),
            FunctionReceiver::SimulEfun => write!(f, "simul_efun"),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::register::Register;

    use super::*;

    #[test]
    fn display_distinguishes_receiver_roles_and_operand_banks() {
        for (receiver, expected) in [
            (FunctionReceiver::Local, "local"),
            (FunctionReceiver::Dynamic, "dynamic"),
            (FunctionReceiver::Efun, "efun"),
            (FunctionReceiver::SimulEfun, "simul_efun"),
            (FunctionReceiver::Var(Register(3).as_local()), "var(r3)"),
            (FunctionReceiver::Var(Register(7).as_global()), "var(g7)"),
            (FunctionReceiver::Value(Register(5).as_local()), "value(r5)"),
            (
                FunctionReceiver::Value(Register(7).as_upvalue()),
                "value(u7)",
            ),
        ] {
            assert_eq!(receiver.to_string(), expected);
        }
    }
}
