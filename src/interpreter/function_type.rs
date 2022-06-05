use crate::interpreter::process::Process;
use std::{cell::RefCell, rc::Rc};

use crate::interpreter::lpc_ref::LpcRef;

use crate::{
    core::{function_arity::FunctionArity, register::Register},
    interpreter::efun::EFUN_PROTOTYPES,
    semantic::{function_flags::FunctionFlags, program_function::ProgramFunction},
};
use delegate::delegate;
use std::fmt::{Display, Formatter};

/// used for local Debug implementations, to avoid stack overflow when dumping function pointers
fn owner_name(owner: &Rc<Process>, f: &mut Formatter) -> std::fmt::Result {
    f.write_str(&*owner.filename())
}

/// used for local Debug implementations, to avoid stack overflow when dumping function pointers
fn borrowed_owner_name(owner: &Rc<RefCell<Process>>, f: &mut Formatter) -> std::fmt::Result {
    f.write_str(&*owner.borrow().filename())
}

/// An enum to handle function names that are either vars or literal names.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum FunctionName {
    /// The name is a variable, that needs to be resolved at runtime.
    Var(Register),
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

    /// The receiver is the Process stored in the [`Register`].
    Var(Register),

    /// The receiver will be filled-in at call time, with the first argument passed to the call.
    /// i.e. the `&->foo()` syntax
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

/// Different ways to store a function address, for handling at runtime.
/// This is the run-time equivalent of [`FunctionTarget`]
#[derive(Educe, Clone, PartialEq, Eq)]
#[educe(Debug)]
pub enum FunctionAddress {
    /// The function being called is located in an object.
    Local(
        #[educe(Debug(method = "borrowed_owner_name"))] Rc<RefCell<Process>>,
        Rc<ProgramFunction>,
    ),

    /// The function being called is an efun, and requires the name.
    Efun(String),
}

impl FunctionAddress {
    /// Get the name of the function being called.
    /// Will return the variable name in those cases.
    pub fn function_name(&self) -> &str {
        match self {
            FunctionAddress::Local(_, x) => x.name(),
            FunctionAddress::Efun(x) => x,
        }
    }

    /// Get the flags for the function this address represents
    pub fn flags(&self) -> FunctionFlags {
        match self {
            FunctionAddress::Local(_, x) => x.prototype.flags,
            FunctionAddress::Efun(x) => match EFUN_PROTOTYPES.get(x.as_str()) {
                Some(prototype) => prototype.flags,
                None => FunctionFlags::default(),
            },
        }
    }
}

// pub struct Closure {
//     owner: Rc<Process>,
//     address: Address,
//     frame: Rc<StackFrame>,
// }
//

/// A pointer to a function, created with the `&` syntax.
/// Partially-applied functions
#[derive(Educe, Clone, PartialEq, Eq)]
#[educe(Debug)]
pub struct FunctionPtr {
    /// The object that this pointer was declared in.
    #[educe(Debug(method = "owner_name"))]
    pub owner: Rc<Process>,

    /// Address of the function, in either the receiver or owner
    pub address: FunctionAddress,

    /// The arity of the function being pointed to
    pub arity: FunctionArity,

    /// Arguments to be passed to the call. `None` arguments in this vector
    /// are expected to be filled at call time, in the case of pointers that
    /// are partially-applied.
    pub partial_args: Vec<Option<LpcRef>>,

    /// Does this pointer use `call_other`?
    pub call_other: bool,
}

impl FunctionPtr {
    /// Get the name of the function being called.
    /// Will return the variable name in those cases.
    pub fn name(&self) -> &str {
        self.address.function_name()
    }

    delegate! {
        to self.address {
            /// retrieve the flags for the function
            pub fn flags(&self) -> FunctionFlags;
        }
    }
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
            LpcFunction::FunctionPtr(x) => x.partial_args.iter().filter(|x| x.is_none()).count(),
        }
    }

    /// Get the flags for the function
    pub fn flags(&self) -> FunctionFlags {
        match self {
            LpcFunction::FunctionPtr(x) => x.flags(),
        }
    }
}

impl Display for LpcFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
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
