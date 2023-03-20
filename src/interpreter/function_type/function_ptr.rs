use std::{
    cmp::Ordering,
    collections::HashSet,
    fmt::{Display, Formatter},
    hash::Hasher,
    rc::Weak,
};

use bit_set::BitSet;
use delegate::delegate;
use educe::Educe;
use itertools::Itertools;
use lpc_rs_core::{
    function_arity::FunctionArity, function_flags::FunctionFlags, register::Register,
};
use qcell::{QCell, QCellOwner};
use tracing::{instrument, trace};

use crate::{
    interpreter::{
        function_type::function_address::FunctionAddress,
        gc::{mark::Mark, unique_id::UniqueId},
        lpc_ref::LpcRef,
        process::Process,
    },
    util::{keyable::Keyable, qcell_process_debug},
};

/// A pointer to a function, created with the `&` syntax.
#[derive(Educe, Clone)]
#[educe(Debug)]
pub struct FunctionPtr {
    /// The object that this pointer was declared in.
    #[educe(Debug(method = "qcell_process_debug"))]
    pub owner: Weak<QCell<Process>>,

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

    /// The variables that I need from the environment, at the time this
    /// [`FunctionPtr`] ss created.
    pub upvalue_ptrs: Vec<Register>,

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

impl Mark for FunctionPtr {
    #[instrument(skip(self, _cell_key))]
    fn mark(
        &self,
        marked: &mut BitSet,
        processed: &mut BitSet,
        _cell_key: &QCellOwner,
    ) -> lpc_rs_errors::Result<()> {
        trace!("marking function ptr");

        if !processed.insert(*self.unique_id.as_ref()) {
            return Ok(());
        }

        trace!("marking upvalue ptrs: {:?}", &self.upvalue_ptrs);

        marked.extend(self.upvalue_ptrs.iter().copied().map(Register::index));

        Ok(())
    }
}

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
            self.upvalue_ptrs.iter().map(|x| format!("{x}")).join(", ")
        ));
        s.push('}');

        write!(f, "{s}")
    }
}

impl<'a> Keyable<'a> for FunctionPtr {
    fn keyable_debug(&self, f: &mut Formatter<'_>, cell_key: &QCellOwner) -> std::fmt::Result {
        write!(f, "FunctionPtr {{ ")?;
        match self.owner.upgrade() {
            Some(owner) => write!(f, "owner: {:?}, ", owner.ro(cell_key))?,
            None => write!(f, "owner: <dropped>, ")?,
        }
        write!(f, "address: {:?}, ", self.address.with_key(cell_key))?;
        write!(f, "partial_args: [")?;
        for arg in &self.partial_args {
            match arg {
                Some(arg) => write!(f, "{:?}, ", arg.with_key(cell_key))?,
                None => write!(f, "<None>, ")?,
            }
        }
        write!(f, "], ")?;
        write!(f, "upvalue_ptrs: {:?}", &self.upvalue_ptrs)?;
        write!(f, "unique_id: {:?}, ", self.unique_id)?;
        write!(f, "}}")
    }

    fn keyable_display(&self, f: &mut Formatter<'_>, cell_key: &QCellOwner) -> std::fmt::Result {
        write!(f, "{}", self.address.with_key(cell_key))?;
        write!(f, "(")?;
        for arg in &self.partial_args {
            match arg {
                Some(arg) => write!(f, "{}, ", arg.with_key(cell_key))?,
                None => write!(f, ", ")?,
            }
        }
        write!(f, ")")
    }

    fn keyable_hash<H: Hasher>(&self, _state: &mut H, _cell_key: &QCellOwner) {
        todo!()
    }

    fn keyable_eq(&self, _other: &Self, _cell_key: &QCellOwner) -> bool {
        todo!()
    }

    fn keyable_partial_cmp(&self, _other: &Self, _cell_key: &QCellOwner) -> Option<Ordering> {
        todo!()
    }
}
