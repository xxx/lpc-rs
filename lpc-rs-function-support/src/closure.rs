use serde::{Deserialize, Serialize};

use crate::program_function::ProgramFunction;

// TODO: can we just use FunctionPtr for these as well?
//       Seems like it only needs an optional upvalues array
//       seems like FunctionAddress::Local handles it

/// A closure (i.e. a function that is bound to a particular environment).
/// This is the run-time representation, which includes its upvalue bindings, and
/// is contained within an LpcRef
#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub struct Closure {
    /// Our underlying function
    pub function: ProgramFunction,

    /// The indexes into the Process-level upvalues vector, that links all of the
    /// closed-over variables.
    pub upvalue_indexes: Vec<usize>,
}

impl Closure {
    /// Create a new closure from a function, with empty upvalues.
    pub fn new(function: ProgramFunction) -> Self {
        Self {
            function,
            upvalue_indexes: vec![],
        }
    }
}
