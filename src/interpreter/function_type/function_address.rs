use std::{
    fmt::{Display, Formatter},
    sync::Arc,
};

use educe::Educe;
use lpc_rs_function_support::program_function::ProgramFunction;
use parking_lot::RwLock;
use ustr::Ustr;

use crate::interpreter::process::Process;

/// Different ways to store a function address, for handling at runtime.
#[derive(Educe, Clone)]
#[educe(Debug)]
pub enum FunctionAddress {
    /// The function being called is located in an object.
    Local(Arc<RwLock<Process>>, Arc<ProgramFunction>),

    /// The receiver isn't known until called (i.e. the `&->foo()` syntax)
    Dynamic(Ustr),

    /// The function being called is an efun, and requires the name.
    Efun(Ustr),

    /// The function being called is a simulated efun, and requires the name.
    SimulEfun(Ustr),
}

impl FunctionAddress {
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
                write!(f, "{}::{}", owner.read(), x)
            }
            FunctionAddress::Dynamic(x) => write!(f, "dynamic::{x}"),
            FunctionAddress::Efun(x) => write!(f, "efun::{x}"),
            FunctionAddress::SimulEfun(x) => write!(f, "simul_efun::{x}"),
        }
    }
}
