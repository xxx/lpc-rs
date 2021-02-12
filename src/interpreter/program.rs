use std::collections::HashMap;
use crate::interpreter::constant_pool::ConstantPool;
use crate::asm::instruction::Instruction;
use crate::semantic::function_symbol::FunctionSymbol;
use crate::parser::span::Span;

#[derive(Debug, Default)]
pub struct Program {
    /// The actual program to execute
    pub instructions: Vec<Instruction>,

    /// The path to the file that this program was compiled from. Used for error messaging.
    pub filename: String,

    /// Code spans corresponding to instructions, for use in error messages
    pub debug_spans: Vec<Option<Span>>,

    /// jump labels
    pub labels: HashMap<String, usize>,

    /// function mapping of name to Symbol
    pub functions: HashMap<String, FunctionSymbol>,

    /// All non-int constants are stored in the pool.
    pub constants: ConstantPool,

    /// How many globals does this program need storage for?
    pub num_globals: usize
}