use std::{
    borrow::Cow,
    cell::RefCell,
    collections::HashMap,
    fmt::{Display, Formatter},
    sync::Arc,
};

use derive_builder::Builder;
use lpc_rs_asm::{
    address::{Address, Label},
    instruction::{Arg, ArgList, Instruction},
};
use lpc_rs_core::{
    RegisterSize, function_arity::FunctionArity, lpc_type::LpcType, mangle::Mangle,
    register::RegisterVariant,
};
use lpc_rs_errors::span::Span;
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
    /// The argument lists this function's calls read, one per call site,
    /// named by `ArgList` operands.
    #[builder(default)]
    pub arg_lists: Vec<Vec<Arg>>,
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
            arg_lists: vec![],
        }
    }

    /// The arguments the call with `list` reads.
    #[inline]
    pub fn args(&self, list: ArgList) -> &[Arg] {
        &self.arg_lists[usize::from(list.0)]
    }

    /// Every register `instruction` names: its own operands in order, then
    /// its argument list's.
    pub fn operand_registers(&self, instruction: &Instruction) -> Vec<RegisterVariant> {
        let seen = RefCell::new(Vec::new());
        let _ = instruction.map_registers(|register| {
            seen.borrow_mut().push(register);
            register
        });
        let mut registers = seen.into_inner();
        if let Some(list) = instruction.arg_list() {
            registers.extend(self.args(list).iter().map(|arg| arg.register()));
        }
        registers
    }

    /// Every register in the instructions and their argument lists passed
    /// through `f`.
    pub fn rename_registers<F>(&mut self, f: F)
    where
        F: Fn(RegisterVariant) -> RegisterVariant,
    {
        for instruction in &mut self.instructions {
            *instruction = instruction.map_registers(&f);
        }
        for arg in self.arg_lists.iter_mut().flatten() {
            *arg = arg.map_register(&f);
        }
    }

    /// The instruction at `at` and its argument list, registers passed
    /// through `f`.
    pub fn rename_registers_at<F>(&mut self, at: usize, f: F)
    where
        F: Fn(RegisterVariant) -> RegisterVariant,
    {
        let instruction = self.instructions[at].map_registers(&f);
        if let Some(list) = instruction.arg_list() {
            for arg in &mut self.arg_lists[usize::from(list.0)] {
                *arg = arg.map_register(&f);
            }
        }
        self.instructions[at] = instruction;
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
        for (index, list) in self.arg_lists.iter().enumerate() {
            let args: Vec<String> = list.iter().map(ToString::to_string).collect();
            v.push(format!("    a{index} = ({})", args.join(", ")));
        }

        // Several labels can share an address.
        let mut labels_by_pc: HashMap<Address, Vec<&Label>> = HashMap::new();
        for (label, address) in self.labels.iter().flatten() {
            labels_by_pc.entry(*address).or_default().push(label);
        }

        for (counter, instruction) in self.instructions.iter().enumerate() {
            if let Some(labels) = labels_by_pc.get(&Address(counter)) {
                for label in labels {
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
    use lpc_rs_core::{lpc_path::LpcPath, lpc_type::LpcType, register::Register};

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

    fn local(i: RegisterSize) -> RegisterVariant {
        RegisterVariant::Local(Register(i))
    }

    #[test]
    fn a_call_reads_its_list() {
        let mut func = function(FunctionKind::Local);
        func.arg_lists
            .push(vec![Arg::Value(local(2)), Arg::Ref(local(3))]);
        assert_eq!(
            func.args(ArgList(0)),
            &[Arg::Value(local(2)), Arg::Ref(local(3))]
        );
    }

    #[test]
    fn a_register_rename_reaches_the_lists() {
        let mut func = function(FunctionKind::Local);
        func.instructions = vec![Instruction::Call(ustr("f"), ArgList(0))];
        func.arg_lists.push(vec![Arg::Value(local(2))]);
        func.rename_registers(|r| if r == local(2) { local(5) } else { r });
        assert_eq!(func.arg_lists, vec![vec![Arg::Value(local(5))]]);
    }

    #[test]
    fn a_rename_at_one_call_reaches_only_its_list() {
        let mut func = function(FunctionKind::Local);
        func.instructions = vec![
            Instruction::Call(ustr("f"), ArgList(0)),
            Instruction::Call(ustr("g"), ArgList(1)),
        ];
        func.arg_lists = vec![vec![Arg::Value(local(2))], vec![Arg::Value(local(2))]];
        func.rename_registers_at(1, |r| if r == local(2) { local(0) } else { r });
        assert_eq!(
            func.arg_lists,
            vec![vec![Arg::Value(local(2))], vec![Arg::Value(local(0))]]
        );
    }

    #[test]
    fn a_calls_operands_end_with_its_list() {
        let mut func = function(FunctionKind::Local);
        func.arg_lists
            .push(vec![Arg::Value(local(2)), Arg::Ref(local(3))]);
        let call = Instruction::CallOther(local(1), local(4), ArgList(0));
        assert_eq!(
            func.operand_registers(&call),
            vec![local(1), local(4), local(2), local(3)]
        );
    }

    #[test]
    fn the_listing_prints_each_list_after_the_constants() {
        let mut func = function(FunctionKind::Local);
        func.constants = vec![LpcConstant::Int(1)];
        func.arg_lists
            .push(vec![Arg::Value(local(2)), Arg::Ref(local(3))]);
        func.instructions = vec![Instruction::Call(ustr("f"), ArgList(0))];
        let listing = func.listing();
        assert_eq!(
            &listing[1..],
            [
                "    k0 = 1",
                "    a0 = (r2, ref r3)",
                "    0000  call f, a0"
            ]
        );
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
