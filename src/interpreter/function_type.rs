use crate::interpreter::process::Process;
use std::{cell::RefCell, rc::Rc};

use crate::{asm::register::Register, interpreter::lpc_ref::LpcRef};

use crate::semantic::program_function::ProgramFunction;
use std::fmt::{Display, Formatter};
use delegate::delegate;
use crate::interpreter::efun::EFUN_PROTOTYPES;
use crate::semantic::function_flags::FunctionFlags;
use crate::semantic::function_prototype::FunctionPrototype;

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
            FunctionAddress::Local(_, x) => &x.name,
            FunctionAddress::Efun(x) => x,
        }
    }

    /// Get the flags for the function this address represents
    pub fn flags(&self) -> FunctionFlags {
        match self {
            FunctionAddress::Local(_, x) => x.flags,
            FunctionAddress::Efun(x) => {
                match EFUN_PROTOTYPES.get(x.as_str()) {
                    Some(prototype) => prototype.flags,
                    None => FunctionFlags::default()
                }
            }
        }
    }
}

/// A struct to hold data about a function's expected arity at call time.
#[derive(Debug, Copy, Clone, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct FunctionArity {
    /// The number of explicitly-specified parameters
    /// For partial applications, this is the arity of the underlying function,
    /// without taking partial parameters into account.
    pub num_args: usize,

    /// The number of arguments that defaults were specified for
    pub num_default_args: usize,

    /// Has an ellipsis arg been declared for this function?
    pub ellipsis: bool,

    /// Is the function `varargs`?
    pub varargs: bool,
}

impl FunctionArity {
    /// create a new [`FunctionArity`] with the passed arity
    pub fn new(num_args: usize) -> Self {
        Self {
            num_args,
            ..Default::default()
        }
    }

    /// Is the passed length valid for this arity?
    /// This takes `varargs` and ellipsis args into account.
    #[inline]
    pub fn is_valid(&self, len: usize) -> bool {
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
    fn net_args(&self) -> usize {
        self.num_args - self.num_default_args
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
