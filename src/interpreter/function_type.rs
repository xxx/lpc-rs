use std::{
    cell::RefCell,
    fmt::{Display, Formatter},
    rc::Rc,
};

use delegate::delegate;
use educe::Educe;
use lpc_rs_core::{function_arity::FunctionArity, function_flags::FunctionFlags};
use lpc_rs_core::register::Register;
use lpc_rs_function_support::program_function::ProgramFunction;

use crate::interpreter::{efun::EFUN_PROTOTYPES, lpc_ref::LpcRef, process::Process};

/// used for local Debug implementations, to avoid stack overflow when dumping
/// function pointers
fn owner_name(owner: &Rc<Process>, f: &mut Formatter) -> std::fmt::Result {
    f.write_str(&*owner.filename())
}

/// used for local Debug implementations, to avoid stack overflow when dumping
/// function pointers
fn borrowed_owner_name(owner: &Rc<RefCell<Process>>, f: &mut Formatter) -> std::fmt::Result {
    f.write_str(&*owner.borrow().filename())
}

/// Different ways to store a function address, for handling at runtime.
/// This is the run-time equivalent of
/// [`FunctionTarget`](lpc_rs_core::function::FunctionTarget).
#[derive(Educe, Clone, PartialEq, Eq)]
#[educe(Debug)]
pub enum FunctionAddress {
    /// The function being called is located in an object.
    Local(
        #[educe(Debug(method = "borrowed_owner_name"))] Rc<RefCell<Process>>,
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UpvalueMapping {
    // when this function is called, which of the CallFrame's upvalues does this get stored in?
    pub frame_location: Register,
    // What is the index in the [`Process`]-level `upvalues` array (which contains the actual data)?
    pub upvalue_location: Register
}

/// A pointer to a function, created with the `&` syntax.
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

    /// Track the (runtime) indexes for both the function, and into the
    /// Process-level upvalued data, for closed-over variables in this function
    pub captured_upvalues: Vec<UpvalueMapping>,
}

impl FunctionPtr {
    /// Get the name of the function being called.
    /// Will return the variable name in those cases.
    pub fn name(&self) -> &str {
        self.address.function_name()
    }

    /// How many arguments do we expect to be called with at runtime?
    pub fn arity(&self) -> usize {
        self.partial_args.iter().filter(|x| x.is_none()).count()
    }

    delegate! {
        to self.address {
            /// retrieve the flags for the function
            pub fn flags(&self) -> FunctionFlags;
        }
    }
}

impl Display for FunctionPtr {
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
