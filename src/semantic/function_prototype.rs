use crate::semantic::lpc_type::LPCVarType;

/// A representation of a function prototype, used to allow forward references.
#[derive(Debug, Clone)]
pub struct FunctionPrototype {
    /// The name of the function
    pub name: String,

    /// The number of arguments this function accepts.
    /// Varargs are handled elsewhere and are ignored in this count.
    pub num_args: usize,

    /// Vector of argument types, used for type checking calls.
    pub arg_types: Vec<LPCVarType>,
}
