use crate::interpreter::function_symbol::FunctionSymbol;

#[derive(Debug, Clone)]
pub struct StackFrame {
    pub symbol: FunctionSymbol,
    pub return_address: usize,
    pub registers: Vec<i64>
}

impl StackFrame {
    pub fn new(symbol: FunctionSymbol, return_address: usize) -> Self {
        // +1 for r0 (where return value is stored)
        let reg_len = symbol.num_args + symbol.num_locals + 1;
        Self {
            symbol,
            return_address,
            registers: vec![0; reg_len]
        }
    }
}