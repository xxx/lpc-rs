/// A representation of a function symbol, used during
/// semantic checks and codegen.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct FunctionSymbol {
    pub name: String,
    pub num_args: usize,
    pub num_locals: usize,
    /// The address of this function in memory.
    pub address: usize
}