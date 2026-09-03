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
    RegisterSize, function_arity::FunctionArity, lpc_type::LpcType, mangle::Mangle,
    register::RegisterVariant,
};
use lpc_rs_errors::span::Span;
use multimap::MultiMap;
use tracing::trace;

use crate::{
    constant::LpcConstant,
    function_prototype::{FunctionKind, FunctionPrototype},
    symbol::Symbol,
};

/// A `Program` function, which stores its actual code, along with
/// metadata for type checking, etc.
///
/// Note that closures also use this structure. By convention, they are named
/// `closure-<id>`, which is unparseable, and cannot conflict with user-defined
/// functions They otherwise act as normal functions, with the exception of
/// upvalue access.
#[derive(Debug, Clone, Eq, PartialEq, Builder)]
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

    /// The literals this function reads through `Constant` operands, in
    /// pool order.
    #[builder(default)]
    pub constants: Vec<LpcConstant>,
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
            constants: vec![],
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
        if let Some(labels) = &mut self.labels {
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

        for (index, constant) in self.constants.iter().enumerate() {
            v.push(format!("    k{index} = {constant}"));
        }

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
            .unwrap_or_default();

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
        self.prototype.kind == FunctionKind::Closure
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

#[cfg(test)]
mod tests {
    use lpc_rs_core::{lpc_path::LpcPath, lpc_type::LpcType};

    use lpc_rs_utils::lpc_string::LpcString;
    use ustr::ustr;

    use super::*;
    use crate::{constant::LpcConstant, function_prototype::FunctionPrototypeBuilder};

    fn function(kind: FunctionKind) -> ProgramFunction {
        let prototype = FunctionPrototypeBuilder::default()
            .name("closure-0")
            .filename(Arc::new(LpcPath::default()))
            .return_type(LpcType::Mixed(false))
            .kind(kind)
            .build()
            .unwrap();
        ProgramFunction::new(prototype, 0)
    }

    #[test]
    fn a_new_function_has_an_empty_constant_pool() {
        assert!(function(FunctionKind::Local).constants.is_empty());
    }

    #[test]
    fn the_listing_names_each_constant() {
        let mut func = function(FunctionKind::Local);
        func.constants = vec![
            LpcConstant::Int(5),
            LpcConstant::Float(1.5.into()),
            LpcConstant::String(Arc::new(LpcString::Static(ustr("a")))),
        ];

        let listing = func.listing();

        assert_eq!(
            &listing[1..4],
            ["    k0 = 5", "    k1 = 1.5", "    k2 = \"a\""]
        );
    }

    #[test]
    fn a_closure_is_known_by_its_kind_not_its_name() {
        assert!(function(FunctionKind::Closure).is_closure());
        assert!(!function(FunctionKind::Local).is_closure());
    }
}
