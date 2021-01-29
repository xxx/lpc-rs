use crate::interpreter::constant_pool::ConstantPool;
use crate::asm::instruction::Instruction;
use std::collections::HashMap;
use crate::interpreter::function_symbol::FunctionSymbol;

#[derive(Debug)]
pub struct Program {
    /// The actual program to execute
    pub instructions: Vec<Instruction>,

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
            labels: HashMap::new(),
            functions: HashMap::new(),
            constants: ConstantPool::default()
        }
    }
}