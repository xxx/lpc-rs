use std::{
    fmt::{Display, Formatter},
    ops::Deref,
    sync::{Arc, Weak},
};

use educe::Educe;
use lpc_rs_core::RegisterSize;
use ustr::Ustr;

use crate::interpreter::process::Process;
use crate::interpreter::program::Function;

/// Different ways to store a function address, for handling at runtime.
#[derive(Educe, Clone)]
#[educe(Debug)]
pub enum FunctionAddress {
    /// The function being called is located in an object.
    Local(Weak<Process>, LocalFunction),

    /// The receiver isn't known until called (i.e. the `&->foo()` syntax)
    Dynamic(Ustr),

    /// The function being called is an efun, and requires the name.
    Efun(Ustr),

    /// The function being called is a simulated efun, and requires the name.
    SimulEfun(Ustr),
}

/// A pointer's binding survives its weak receiver for structural address comparison.
#[derive(Debug, Clone)]
pub struct LocalFunction {
    pub function: Function,
    globals: RegisterSize,
    aliases: Arc<[RegisterSize]>,
}

impl Deref for LocalFunction {
    type Target = Function;
    fn deref(&self) -> &Function {
        &self.function
    }
}

impl Display for LocalFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.function.fmt(f)
    }
}

impl PartialEq for LocalFunction {
    fn eq(&self, other: &Self) -> bool {
        self.function.projected(self.globals, &self.aliases)
            == other.function.projected(other.globals, &other.aliases)
    }
}

impl Eq for LocalFunction {}

impl FunctionAddress {
    /// Bind local code without retaining the receiver or its unrelated functions.
    pub fn local(receiver: &Arc<Process>, function: Function) -> Self {
        Self::Local(
            Arc::downgrade(receiver),
            LocalFunction {
                function,
                globals: receiver.program.num_globals,
                aliases: receiver.program.global_views.clone(),
            },
        )
    }

    /// Get the name of the function being called.
    /// Will return the variable name in those cases.
    #[inline]
    pub fn function_name(&self) -> &str {
        match self {
            FunctionAddress::Local(_, x) => x.name(),
            FunctionAddress::Dynamic(x)
            | FunctionAddress::Efun(x)
            | FunctionAddress::SimulEfun(x) => x,
        }
    }
}

impl PartialEq for FunctionAddress {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (FunctionAddress::Local(_, x), FunctionAddress::Local(_, y)) => x == y,
            (FunctionAddress::Dynamic(x), FunctionAddress::Dynamic(y)) => x == y,
            (FunctionAddress::Efun(x), FunctionAddress::Efun(y)) => x == y,
            (FunctionAddress::SimulEfun(x), FunctionAddress::SimulEfun(y)) => x == y,
            _ => false,
        }
    }
}

impl Eq for FunctionAddress {}

impl Display for FunctionAddress {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            FunctionAddress::Local(owner, x) => {
                if let Some(owner) = owner.upgrade() {
                    write!(f, "{}::{}", owner, x)
                } else {
                    write!(f, "<destructed>::{}", x)
                }
            }
            FunctionAddress::Dynamic(x) => write!(f, "dynamic::{x}"),
            FunctionAddress::Efun(x) => write!(f, "efun::{x}"),
            FunctionAddress::SimulEfun(x) => write!(f, "simul_efun::{x}"),
        }
    }
}
