#[derive(Debug, Clone)]
pub struct FunctionSymbol {
    pub name: String,
    pub num_args: usize,
    pub num_locals: usize,
    pub address: usize
}