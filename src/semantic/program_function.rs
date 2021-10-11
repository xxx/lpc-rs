use crate::asm::instruction::{Instruction, Address};
use crate::parser::span::Span;
use std::collections::HashMap;
use multimap::MultiMap;

/// A [`Program`] function, which stores its actual code, along with
/// metadata for type checking, etc.
#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub struct ProgramFunction {
    /// Yep, the name of the function
    pub name: String,

    /// The number of arguments this function accepts.
    /// Varargs are handled elsewhere and are ignored in this count.
    pub num_args: usize,

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
    pub fn new<T>(name: T, num_args: usize, num_locals: usize) -> Self
    where
        T: Into<String>
    {
        Self {
            name: name.into(),
            num_args,
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
        T: Into<String>
    {
        self.labels.insert(label.into(), address);
    }

    /// Get a listing of this function's instructions, for use in debugging.
    pub fn listing(&self) -> Vec<String> {
        let mut v = Vec::new();

        v.push(format!(
            "fn {} num_args={} num_locals={}:",
            self.name, self.num_args, self.num_locals
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
