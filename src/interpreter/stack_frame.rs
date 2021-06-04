use crate::{interpreter::lpc_ref::LpcRef, semantic::function_symbol::FunctionSymbol};

/// A representation of a function call's context.
#[derive(Debug, Clone)]
pub struct StackFrame {
    /// The function symbol that this frame represents a call to
    pub symbol: FunctionSymbol,
    /// Where we return to after we return from this function.
    pub return_address: usize,
    /// Our registers. By convention, `registers[0]` is for the return value of the call.
    pub registers: Vec<LpcRef>,
}

impl StackFrame {
    /// Create a new [`StackFrame`] instance
    ///
    /// # Arguments
    ///
    /// * `symbol` - The symbol representing the function being called
    /// * `return_address` - Where to return to after we return from this frame's function.
    pub fn new(symbol: FunctionSymbol, return_address: usize) -> Self {
        // add +1 for r0 (where return value is stored)
        let reg_len = symbol.num_args + symbol.num_locals + 1;

        Self {
            symbol,
            return_address,
            registers: vec![LpcRef::Int(0); reg_len],
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new_sets_up_registers() {
        let fs = FunctionSymbol {
            name: "my_function".to_string(),
            num_args: 4,
            num_locals: 7,
            address: 123,
        };

        let frame = StackFrame::new(fs, 5);

        assert_eq!(frame.registers.len(), 12);
        assert!(frame.registers.iter().all(|r| r == &LpcRef::Int(0)));
    }
}
