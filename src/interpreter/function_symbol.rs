#[derive(Debug, Clone, Eq, PartialEq, Hash)]
/// A representation of a function symbol, used during
/// semantic checks and codegen.
pub struct FunctionSymbol {
    pub name: String,
    pub num_args: usize,
    pub num_locals: usize,
    /// The address of this function in memory.
    pub address: usize
}