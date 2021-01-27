/// A representation of a function symbol, used during
/// semantic checks and codegen.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct FunctionSymbol {
    pub name: String,
    pub num_args: usize,
    /// The number of non-argument, non-return-value locals. Used for register allocation.
    pub num_locals: usize,
    /// The address of this function in memory.
    pub address: usize
}