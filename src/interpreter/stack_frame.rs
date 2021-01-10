use crate::interpreter::function_symbol::FunctionSymbol;

#[derive(Debug, Clone)]
struct StackFrame {
    symbol: FunctionSymbol,
    return_address: usize,
    registers: Vec<i64>
}

impl StackFrame {
    fn new(symbol: FunctionSymbol, return_address: usize) -> Self {
        // +1 for r0 (where return value is stored)
        let reg_len = symbol.num_args + symbol.num_locals + 1;
        Self {
            symbol,
            return_address,
            registers: vec![0; reg_len]
        }
    }
}