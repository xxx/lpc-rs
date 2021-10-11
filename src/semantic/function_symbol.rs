use crate::asm::instruction::{Instruction, Address};
use crate::parser::span::Span;
use std::collections::HashMap;
use multimap::MultiMap;

/// A representation of a function symbol, used during
/// semantic checks and codegen.
#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub struct FunctionSymbol {
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

impl FunctionSymbol {
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

    pub fn push_instruction(&mut self, instruction: Instruction, debug_span: Option<Span>) {
        self.instructions.push(instruction);
        self.debug_spans.push(debug_span);
    }

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
