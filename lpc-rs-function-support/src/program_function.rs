use std::{
    borrow::Cow,
    collections::HashMap,
    fmt::{Display, Formatter},
    rc::Rc,
};

use derive_builder::Builder;
use lpc_rs_asm::instruction::Instruction;
use lpc_rs_core::{
    function_arity::FunctionArity, lpc_type::LpcType, mangle::Mangle, register::RegisterVariant,
};
use lpc_rs_errors::span::Span;
use multimap::MultiMap;
use once_cell::sync::OnceCell;
use serde::{Deserialize, Serialize};
use tracing::trace;
use lpc_rs_asm::jump_location::Address;

use crate::{function_prototype::FunctionPrototype, symbol::Symbol};

/// A `Program` function, which stores its actual code, along with
/// metadata for type checking, etc.
///
/// Note that closures also use this structure. By convention, they are named
/// `closure-<id>`, which is unparseable, and cannot conflict with user-defined
/// functions They otherwise act as normal functions, with the exception of
/// upvalue access.
#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize, Builder)]
#[builder(build_fn(error = "lpc_rs_errors::LpcError"))]
pub struct ProgramFunction {
    /// My prototype from compilation
    pub prototype: FunctionPrototype,

    /// The number of non-argument, non-return-value locals.
    /// Used for register allocation.
    #[builder(default)]
    pub num_locals: usize,

    /// How many of my locals are actually upvalues?
    /// Note that this is just the count of captured variables, not
    /// vars that are captured from elsewhere.
    #[builder(default)]
    pub num_upvalues: usize,

    /// The actual instructions of this function
    #[builder(default)]
    pub instructions: Vec<Instruction>,

    /// Code spans corresponding to instructions, for use in error messages
    #[builder(default)]
    pub debug_spans: Vec<Option<Span>>,

    /// Map of labels, to their respective addresses
    #[builder(default)]
    pub labels: HashMap<String, Address>,

    /// List of local variables declared within this function
    #[builder(default)]
    pub local_variables: Vec<Symbol>,

    /// Track the location of where my arguments are expected
    #[builder(default)]
    pub arg_locations: Vec<RegisterVariant>,

    /// Interned strings. These are stored by our containing [`Program`] and
    /// shared among all functions in the program.
    #[builder(default)]
    #[serde(serialize_with = "lpc_rs_core::serialize::serialize_once_cell")]
    #[serde(deserialize_with = "lpc_rs_core::serialize::deserialize_once_cell")]
    pub strings: OnceCell<Rc<Vec<String>>>,
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
            num_upvalues: 0,
            instructions: vec![],
            debug_spans: vec![],
            labels: HashMap::new(),
            local_variables: vec![],
            arg_locations: vec![],
            strings: OnceCell::new(),
        }
    }

    /// Push an [`Instruction`] and corresponding [`Span`] into this function's code.
    #[inline]
    pub fn push_instruction(&mut self, instruction: Instruction, debug_span: Option<Span>) {
        trace!(instruction = %instruction, "pushing instruction");
        self.instructions.push(instruction);
        self.debug_spans.push(debug_span);
    }

    /// Insert a label at the specified address into this function
    #[inline]
    pub fn insert_label<T>(&mut self, label: T, address: Address)
    where
        T: Into<String>,
    {
        self.labels.insert(label.into(), address);
    }

    /// Get a listing of this function's instructions, for use in debugging.
    pub fn listing(&self) -> Vec<String> {
        let mut v = vec![];

        v.push(format!(
            "fn {} ({}) num_args={} num_locals={} num_upvalues={}:",
            self.prototype.name,
            self.mangle(),
            self.prototype.arity.num_args,
            self.num_locals,
            self.num_upvalues
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
                    v.push(format!("  {label}:"));
                }
            }

            v.push(format!(
                "    {:04x}  {instruction}",
                counter,
                instruction = instruction
            ));
        }

        v
    }

    /// Is this function a closure?
    #[inline]
    pub fn is_closure(&self) -> bool {
        self.prototype.name.starts_with("closure-")
    }
}

impl Mangle for ProgramFunction {
    #[inline]
    fn mangle(&self) -> String {
        self.prototype.mangle()
    }
}

impl Display for ProgramFunction {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.prototype.name)
    }
}

impl AsRef<FunctionPrototype> for ProgramFunction {
    #[inline]
    fn as_ref(&self) -> &FunctionPrototype {
        &self.prototype
    }
}

impl AsRef<FunctionPrototype> for Rc<ProgramFunction> {
    #[inline]
    fn as_ref(&self) -> &FunctionPrototype {
        &self.prototype
    }
}
