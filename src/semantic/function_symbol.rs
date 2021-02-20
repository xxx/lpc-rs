/// A representation of a function symbol, used during
/// semantic checks and codegen.
#[derive(Debug, Clone, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct FunctionSymbol {
    /// Yep, the name of the function
    pub name: String,

    /// The number of arguments this function accepts.
    /// Varargs are handled elsewhere and are ignored in this count.
    pub num_args: usize,

    /// The number of non-argument, non-return-value locals. Used for register allocation.
    pub num_locals: usize,

    /// The address of this function in memory.
    pub address: usize,
}
