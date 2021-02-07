use std::collections::HashMap;
use crate::interpreter::constant_pool::ConstantPool;
use crate::asm::instruction::Instruction;
use crate::semantic::function_symbol::FunctionSymbol;
use crate::parser::span::Span;

#[derive(Debug)]
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
    pub constants: ConstantPool
}

impl Default for Program {
    fn default() -> Self {
        Self {
            instructions: vec![],
            debug_spans: vec![],
            filename: String::new(),
            labels: HashMap::new(),
            functions: HashMap::new(),
            constants: ConstantPool::default()
        }
    }
}