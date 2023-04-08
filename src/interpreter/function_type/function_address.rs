use std::{
    cmp::Ordering,
    fmt::{Display, Formatter},
    hash::Hasher,
    rc::Rc,
    sync::Arc,
};

use educe::Educe;
use lpc_rs_function_support::program_function::ProgramFunction;
use qcell::{QCell, QCellOwner};
use ustr::Ustr;

use crate::{
    interpreter::process::Process,
    util::{keyable::Keyable, qcell_process_debug},
};

/// Different ways to store a function address, for handling at runtime.
#[derive(Educe, Clone)]
#[educe(Debug)]
pub enum FunctionAddress {
    /// The function being called is located in an object.
    Local(
        #[educe(Debug(method = "qcell_process_debug"))] Rc<QCell<Process>>,
        Arc<ProgramFunction>,
    ),

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
            FunctionAddress::Local(_owner, _x) => {
                // write!(f, "{}::{}", owner.borrow(), x)
                write!(f, "<QCell local>")
            }
            FunctionAddress::Dynamic(x) => write!(f, "dynamic::{x}"),
            FunctionAddress::Efun(x) => write!(f, "efun::{x}"),
            FunctionAddress::SimulEfun(x) => write!(f, "simul_efun::{x}"),
        }
    }
}

impl<'a> Keyable<'a> for FunctionAddress {
    fn keyable_debug(&self, f: &mut Formatter<'_>, cell_key: &QCellOwner) -> std::fmt::Result {
        match self {
            FunctionAddress::Local(process, function) => {
                write!(f, "{:?}::{:?}", process.ro(cell_key), function)
            }
            FunctionAddress::Dynamic(name) => write!(f, "dynamic::{:?}", name),
            FunctionAddress::Efun(name) => write!(f, "efun::{:?}", name),
            FunctionAddress::SimulEfun(name) => write!(f, "simul_efun::{:?}", name),
        }
    }

    fn keyable_display(&self, f: &mut Formatter<'_>, cell_key: &QCellOwner) -> std::fmt::Result {
        match self {
            FunctionAddress::Local(process, function) => {
                write!(f, "{}::{}", process.ro(cell_key), function)
            }
            FunctionAddress::Dynamic(name) => write!(f, "dynamic::{name}"),
            FunctionAddress::Efun(name) => write!(f, "efun::{name}"),
            FunctionAddress::SimulEfun(name) => write!(f, "simul_efun::{name}"),
        }
    }

    fn keyable_hash<H: Hasher>(&self, _state: &mut H, _cell_key: &QCellOwner) {
        unimplemented!()
        // match self {
        //     FunctionAddress::Local(process, function) => {
        //         process.ro(cell_key).hash(state);
        //         function.hash(state);
        //     }
        //     FunctionAddress::Dynamic(name) => {
        //         name.hash(state);
        //     }
        //     FunctionAddress::Efun(name) => {
        //         name.hash(state);
        //     }
        // }
    }

    fn keyable_eq(&self, other: &Self, cell_key: &QCellOwner) -> bool {
        match (self, other) {
            (
                FunctionAddress::Local(process, function),
                FunctionAddress::Local(other_process, other_function),
            ) => process.ro(cell_key) == other_process.ro(cell_key) && function == other_function,
            (FunctionAddress::Dynamic(name), FunctionAddress::Dynamic(other_name)) => {
                name == other_name
            }
            (FunctionAddress::Efun(name), FunctionAddress::Efun(other_name)) => name == other_name,
            (FunctionAddress::SimulEfun(name), FunctionAddress::SimulEfun(other_name)) => {
                name == other_name
            }
            _ => false,
        }
    }

    fn keyable_partial_cmp(&self, _other: &Self, _cell_key: &QCellOwner) -> Option<Ordering> {
        unimplemented!()
        // match (self, other) {
        //     (FunctionAddress::Local(process, function), FunctionAddress::Local(other_process, other_function)) => {
        //         process.ro(cell_key).partial_cmp(&other_process.ro(cell_key)).map(|x| x.then(function.cmp(other_function)))
        //     }
        //     (FunctionAddress::Dynamic(name), FunctionAddress::Dynamic(other_name)) => {
        //         name.partial_cmp(other_name)
        //     }
        //     (FunctionAddress::Efun(name), FunctionAddress::Efun(other_name)) => {
        //         name.partial_cmp(other_name)
        //     }
        //     _ => None,
        // }
    }
}
