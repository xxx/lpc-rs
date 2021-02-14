use crate::{interpreter::lpc_var::LPCVar, semantic::function_symbol::FunctionSymbol};

/// A representation of a function call's context.
#[derive(Debug, Clone)]
pub struct StackFrame {
    /// The function symbol that this frame represents a call to
    pub symbol: FunctionSymbol,
    /// Where we return to after we return from this function.
    pub return_address: usize,
    /// Our registers. By convention, `registers[0]` is for the return value of the call.
    pub registers: Vec<LPCVar>,
}

impl StackFrame {
    /// Create a new StackFrame instance
    ///
    /// # Arguments
    ///
    /// * `symbol` - The symbol representing the function being called
    /// * `return_address` - Where to return to after we return from this frame's function.
    pub fn new(symbol: FunctionSymbol, return_address: usize) -> Self {
        // +1 for r0 (where return value is stored)
        let reg_len = symbol.num_args + symbol.num_locals + 1;
        Self {
            symbol,
            return_address,
            registers: vec![LPCVar::Int(0); reg_len],
        }
    }
}
