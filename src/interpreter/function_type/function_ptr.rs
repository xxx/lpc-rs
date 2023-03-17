use educe::Educe;
use std::rc::Weak;
use qcell::{QCell, QCellOwner};
use lpc_rs_core::function_arity::FunctionArity;
use lpc_rs_core::register::Register;
use std::fmt::{Display, Formatter};
use bit_set::BitSet;
use std::collections::HashSet;
use delegate::delegate;
use itertools::Itertools;
use crate::interpreter::function_type::FunctionAddress;
use crate::interpreter::gc::unique_id::{GcMark, UniqueId};
use crate::interpreter::lpc_ref::LpcRef;
use crate::interpreter::process::Process;
use crate::util::qcell_debug;
use lpc_rs_core::function_flags::FunctionFlags;


/// A pointer to a function, created with the `&` syntax.
#[derive(Educe, Clone)]
#[educe(Debug)]
pub struct FunctionPtr {
    /// The object that this pointer was declared in.
    #[educe(Debug(method = "qcell_debug"))]
    pub owner: Weak<QCell<Process>>,

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

    /// A globally-unique ID for this function pointer, used for GC purposes.
    pub unique_id: UniqueId,
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

impl GcMark for FunctionPtr {
    fn mark(
        &self,
        marked: &mut BitSet,
        processed: &mut HashSet<UniqueId>,
        _cell_key: &QCellOwner,
    ) -> lpc_rs_errors::Result<()> {
        if !processed.insert(self.unique_id) {
            return Ok(());
        }

        marked.extend(self.upvalues.iter().copied().map(Register::index));

        Ok(())
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
