use crate::interpreter::process::Process;
use std::rc::Rc;

use crate::{asm::register::Register, interpreter::lpc_ref::LpcRef};

use crate::semantic::program_function::ProgramFunction;
use std::fmt::{Display, Formatter};

/// An enum to handle function names that are either vars or literal names.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum FunctionName {
    /// The name is a variable, that needs to be resolved at runtime.
    Var(Register),
    /// The name is a literal function name, so call it directly.
    Literal(String),
}

/// The possible receivers.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum FunctionReceiver {
    /// The receiver is the Process stored in the [`Register`].
    Value(Register),

    /// There is no receiver.
    None,

    /// The receiver will be filled-in at call time, with the first argument passed to the call.
    Argument,
}

/// Used as the target that's stored for a function pointer
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum FunctionTarget {
    /// The call will be to an efun
    Efun(FunctionName),

    /// The call will be to a function that's in the same object as the pointer.
    Local(FunctionName),

    /// The call will be a call_other in another object.
    CallOther(FunctionName, FunctionReceiver),
}

/// Different ways to store a function address, for handling at runtime.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FunctionAddress {
    /// The function being called is local to the object the pointer is declared in.
    Local(Rc<ProgramFunction>),
    // Local(Address),
    /// The function being called is located in another object.
    Remote(Rc<Process>, Rc<ProgramFunction>),
    // Remote(Rc<Process>, Address),
    /// The function being called is an efun, and requires the name.
    Efun(String),
}

// pub struct Closure {
//     owner: Rc<Process>,
//     address: Address,
//     frame: Rc<StackFrame>,
// }
//

/// A pointer to a function, created with the `&` syntax.
/// Partially-applied functions
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionPtr {
    // TODO: owner and address can be replaced by Rc<FunctionSymbol>
    /// The object that this pointer was declared in.
    pub owner: Rc<Process>,

    /// Address of the function, in either the receiver or owner
    pub address: FunctionAddress,

    /// Arguments to be passed to the call. `None` arguments in this vector
    /// are expected to be filled at call time, in the case of pointers that
    /// are partially-applied.
    pub args: Vec<Option<LpcRef>>,
}

/// `function` type variations
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LpcFunction {
    FunctionPtr(FunctionPtr),
}

impl LpcFunction {
    /// How many arguments do we expect to be called with at runtime?
    pub fn arity(&self) -> usize {
        match self {
            LpcFunction::FunctionPtr(x) => x.args.iter().filter(|x| x.is_none()).count(),
        }
    }
}

impl Display for LpcFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}
