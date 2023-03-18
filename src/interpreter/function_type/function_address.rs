use std::cmp::Ordering;
use educe::Educe;
use lpc_rs_core::function_flags::FunctionFlags;
use std::rc::Rc;
use qcell::{QCell, QCellOwner};
use lpc_rs_function_support::program_function::ProgramFunction;
use std::fmt::{Display, Formatter};
use std::hash::{Hash, Hasher};
use crate::interpreter::efun::EFUN_PROTOTYPES;
use crate::interpreter::process::Process;
use crate::util::keyable::Keyable;
use crate::util::qcell_debug;


/// Different ways to store a function address, for handling at runtime.
/// This is the run-time equivalent of
/// [`FunctionTarget`](lpc_rs_core::function::FunctionTarget).
#[derive(Educe, Clone)]
#[educe(Debug)]
pub enum FunctionAddress {
    /// The function being called is located in an object.
    Local(
        #[educe(Debug(method = "qcell_debug"))] Rc<QCell<Process>>,
        Rc<ProgramFunction>,
    ),

    /// The receiver isn't known until runtime (i.e. the `&->foo()` syntax)
    Dynamic(String),

    /// The function being called is an efun, and requires the name.
    Efun(String),
}

impl FunctionAddress {
    /// Get the name of the function being called.
    /// Will return the variable name in those cases.
    pub fn function_name(&self) -> &str {
        match self {
            FunctionAddress::Local(_, x) => x.name(),
            FunctionAddress::Dynamic(x) | FunctionAddress::Efun(x) => x,
        }
    }

    /// Get the flags for the function this address represents
    pub fn flags(&self) -> FunctionFlags {
        match self {
            FunctionAddress::Local(_, x) => x.prototype.flags,
            FunctionAddress::Dynamic(_) => FunctionFlags::default(),
            FunctionAddress::Efun(x) => EFUN_PROTOTYPES
                .get(x.as_str())
                .map(|x| x.flags)
                .unwrap_or_default(),
        }
    }
}

impl PartialEq for FunctionAddress {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (FunctionAddress::Local(_, x), FunctionAddress::Local(_, y)) => x == y,
            (FunctionAddress::Dynamic(x), FunctionAddress::Dynamic(y)) => x == y,
            (FunctionAddress::Efun(x), FunctionAddress::Efun(y)) => x == y,
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
        }
    }

    fn keyable_display(&self, f: &mut Formatter<'_>, cell_key: &QCellOwner) -> std::fmt::Result {
        match self {
            FunctionAddress::Local(process, function) => {
                write!(f, "{}::{}", process.ro(cell_key), function)
            }
            FunctionAddress::Dynamic(name) => write!(f, "dynamic::{name}"),
            FunctionAddress::Efun(name) => write!(f, "efun::{name}"),
        }
    }

    fn keyable_hash<H: Hasher>(&self, state: &mut H, cell_key: &QCellOwner) {
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
            (FunctionAddress::Local(process, function), FunctionAddress::Local(other_process, other_function)) => {
                process.ro(cell_key) == other_process.ro(cell_key) && function == other_function
            }
            (FunctionAddress::Dynamic(name), FunctionAddress::Dynamic(other_name)) => {
                name == other_name
            }
            (FunctionAddress::Efun(name), FunctionAddress::Efun(other_name)) => {
                name == other_name
            }
            _ => false,
        }
    }

    fn keyable_partial_cmp(&self, other: &Self, cell_key: &QCellOwner) -> Option<Ordering> {
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