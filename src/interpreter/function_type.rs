use std::{
    fmt::{Display, Formatter},
    rc::Rc,
};

use delegate::delegate;
use educe::Educe;
use itertools::Itertools;
use lpc_rs_core::{
    function_arity::FunctionArity, function_flags::FunctionFlags, register::Register,
};
use lpc_rs_function_support::program_function::ProgramFunction;
use qcell::QCell;

use crate::{
    interpreter::{efun::EFUN_PROTOTYPES, lpc_ref::LpcRef, process::Process},
    util::qcell_debug,
};

/// used for local Debug implementations, to avoid stack overflow when dumping
/// function pointers
fn borrowed_owner_name(_owner: &Rc<QCell<Process>>, f: &mut Formatter) -> std::fmt::Result {
    // f.write_str(&owner.borrow().filename())
    f.write_str("<Local Owner QCell>")
}

/// Different ways to store a function address, for handling at runtime.
/// This is the run-time equivalent of
/// [`FunctionTarget`](lpc_rs_core::function::FunctionTarget).
#[derive(Educe, Clone)]
#[educe(Debug)]
pub enum FunctionAddress {
    /// The function being called is located in an object.
    Local(
        #[educe(Debug(method = "borrowed_owner_name"))] Rc<QCell<Process>>,
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

/// A pointer to a function, created with the `&` syntax.
#[derive(Educe, Clone)]
#[educe(Debug)]
pub struct FunctionPtr {
    /// The object that this pointer was declared in.
    #[educe(Debug(method = "borrowed_owner_name"))]
    pub owner: Rc<QCell<Process>>,

    /// Address of the function, in either the receiver or owner
    #[educe(Debug(method = "qcell_debug"))]
    pub address: FunctionAddress,

    /// The arity of the function being pointed to
    pub arity: FunctionArity,

    /// Arguments to be passed to the call. `None` arguments in this vector
    /// are expected to be filled at call time, in the case of pointers that
    /// are partially-applied.
    #[educe(Debug(method = "qcell_debug"))]
    pub partial_args: Vec<Option<LpcRef>>,

    /// Does this pointer use `call_other`?
    pub call_other: bool,

    /// The variables that I need from the environment, at the time this
    /// [`FunctionPtr`] ss created.
    pub upvalues: Vec<Register>,
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

impl PartialEq for FunctionPtr {
    fn eq(&self, other: &Self) -> bool {
        // TODO handle owner somehow
        self.address == other.address
            && self.arity == other.arity
            && self.partial_args == other.partial_args
            && self.call_other == other.call_other
            && self.upvalues == other.upvalues
    }
}

impl Eq for FunctionPtr {}

impl Display for FunctionPtr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut s = String::new();

        s.push_str("FunctionPtr { ");
        s.push_str("owner: <QCell data>");
        s.push_str(&format!("address: {}, ", self.address));

        let partial_args = &self
            .partial_args
            .iter()
            .map(|arg| match arg {
                Some(_a) => "<QCell LpcRef>".to_string(),
                None => "<None>".to_string(),
            })
            .join(", ");
        s.push_str(&format!("partial_args: [{partial_args}], "));
        s.push_str(&format!(
            "upvalues: [{}]",
            self.upvalues.iter().map(|x| format!("{x}")).join(", ")
        ));
        s.push('}');

        write!(f, "{s}")
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
