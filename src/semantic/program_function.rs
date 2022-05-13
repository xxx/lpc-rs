use crate::{
    asm::instruction::{Address, Instruction},
    interpreter::function_type::FunctionArity,
    parser::span::Span,
    semantic::{function_prototype::FunctionPrototype, lpc_type::LpcType},
};
use multimap::MultiMap;
use std::{borrow::Cow, collections::HashMap};

/// A [`Program`] function, which stores its actual code, along with
/// metadata for type checking, etc.
#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub struct ProgramFunction {
    /// My prototype from compilation
    pub prototype: FunctionPrototype,

    /// The number of non-argument, non-return-value locals. Used for register allocation.
    pub num_locals: usize,

    /// The actual instructions of this function
    pub instructions: Vec<Instruction>,

    /// Code spans corresponding to instructions, for use in error messages
    pub debug_spans: Vec<Option<Span>>,

    /// Map of labels, to their respective addresses
    pub labels: HashMap<String, Address>,
}

impl ProgramFunction {
    /// Is this function public?
    #[inline]
    pub fn public(&self) -> bool {
        self.prototype.flags.public()
    }

    #[inline]
    pub fn name(&self) -> &Cow<'static, str> {
        &self.prototype.name
    }

    #[inline]
    pub fn return_type(&self) -> LpcType {
        self.prototype.return_type
    }

    #[inline]
    pub fn arity(&self) -> FunctionArity {
        self.prototype.arity
    }

    pub fn new(prototype: FunctionPrototype, num_locals: usize) -> Self {
        Self {
            prototype,
            num_locals,
            instructions: Vec::new(),
            debug_spans: Vec::new(),
            labels: HashMap::new(),
        }
    }

    /// Push and [`Instruction`] and corresponding [`Span`] into this function's code.
    pub fn push_instruction(&mut self, instruction: Instruction, debug_span: Option<Span>) {
        self.instructions.push(instruction);
        self.debug_spans.push(debug_span);
    }

    /// Insert a label at the specified address into this function
    pub fn insert_label<T>(&mut self, label: T, address: Address)
    where
        T: Into<String>,
    {
        self.labels.insert(label.into(), address);
    }

    /// Get a listing of this function's instructions, for use in debugging.
    pub fn listing(&self) -> Vec<String> {
        let mut v = Vec::new();

        v.push(format!(
            "fn {} num_args={} num_locals={}:",
            self.prototype.name, self.prototype.arity.num_args, self.num_locals
        ));

        // use MultiMap as multiple labels can be at the same address
        let labels_by_pc = self
            .labels
            .values()
            .zip(self.labels.keys())
            .collect::<MultiMap<_, _>>();

        for (counter, instruction) in self.instructions.iter().enumerate() {
            if let Some(vec) = labels_by_pc.get_vec(&counter) {
                for label in vec {
                    v.push(format!("{}:", label));
                }
            }

            v.push(format!("    {}", instruction));
        }

        v
    }
}

impl AsRef<FunctionPrototype> for ProgramFunction {
    fn as_ref(&self) -> &FunctionPrototype {
        &self.prototype
    }
}
