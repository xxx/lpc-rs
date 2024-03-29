use std::{
    borrow::Cow,
    collections::HashMap,
    fmt::{Display, Formatter},
    sync::Arc,
};

use derive_builder::Builder;
use lpc_rs_asm::{
    address::{Address, Label},
    instruction::Instruction,
};
use lpc_rs_core::{
    function_arity::FunctionArity, lpc_type::LpcType, mangle::Mangle, register::RegisterVariant,
    RegisterSize,
};
use lpc_rs_errors::span::Span;
use multimap::MultiMap;
use once_cell::sync::OnceCell;
use serde::{Deserialize, Serialize};
use string_interner::StringInterner;
use tracing::trace;

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
    pub num_locals: RegisterSize,

    /// How many of my locals are actually upvalues?
    /// Note that this is just the count of captured variables, not
    /// vars that are captured from elsewhere.
    #[builder(default)]
    pub num_upvalues: RegisterSize,

    /// The actual instructions of this function
    #[builder(default)]
    pub instructions: Vec<Instruction>,

    /// Code spans corresponding to instructions, for use in error messages
    #[builder(default)]
    pub debug_spans: Vec<Option<Span>>,

    /// Map of labels, to their respective addresses.
    /// Unused at runtime, so can be set to `None` to save space.
    #[builder(default, setter(strip_option))]
    pub labels: Option<HashMap<Label, Address>>,

    /// List of local variables declared within this function
    #[builder(default)]
    pub local_variables: Vec<Symbol>,

    /// Track the location of where my arguments are expected
    #[builder(default)]
    pub arg_locations: Vec<RegisterVariant>,

    /// Interned strings. These are stored by our containing `Program` and
    /// shared among all functions in the program.
    #[builder(default)]
    #[serde(serialize_with = "lpc_rs_core::serialize::serialize_once_cell")]
    #[serde(deserialize_with = "lpc_rs_core::serialize::deserialize_once_cell")]
    pub strings: OnceCell<Arc<StringInterner>>,
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

    pub fn new(prototype: FunctionPrototype, num_locals: RegisterSize) -> Self {
        Self {
            prototype,
            num_locals,
            num_upvalues: 0,
            instructions: vec![],
            debug_spans: vec![],
            labels: Some(HashMap::new()),
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
        if let Some(ref mut labels) = &mut self.labels {
            labels.insert(label.into(), address);
        }
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
            .as_ref()
            .map(|labels| {
                labels
                    .values()
                    .zip(labels.keys())
                    .collect::<MultiMap<_, _>>()
            })
            .unwrap_or_else(MultiMap::new);

        for (counter, instruction) in self.instructions.iter().enumerate() {
            if let Some(vec) = labels_by_pc.get_vec(&Address(counter)) {
                for label in vec {
                    v.push(format!("  {label}:"));
                }
            }

            v.push(format!("    {:04x}  {}", counter, instruction));
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

impl AsRef<FunctionPrototype> for Arc<ProgramFunction> {
    #[inline]
    fn as_ref(&self) -> &FunctionPrototype {
        &self.prototype
    }
}
