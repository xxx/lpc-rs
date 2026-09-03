use std::{collections::HashMap, sync::Arc};

use async_recursion::async_recursion;
use async_trait::async_trait;
use bit_set::BitSet;
use if_chain::if_chain;
use indexmap::IndexMap;
use lpc_rs_asm::{
    address::{Address, Label},
    instruction::Instruction,
};
use lpc_rs_core::{
    CREATE_FUNCTION, INIT_GLOBALS, INIT_PROGRAM, LpcIntInner, RegisterSize, ScopeId,
    call_namespace::CallNamespace,
    function_flags::FunctionFlags,
    function_receiver::FunctionReceiver,
    lpc_path::LpcPath,
    lpc_type::LpcType,
    mangle::Mangle,
    register::{Register, RegisterVariant},
    register_counter::RegisterCounter,
};
use lpc_rs_errors::{LpcError, Result, lpc_bug, lpc_error, lpc_warning, span::Span};
use lpc_rs_function_support::{
    constant::LpcConstant,
    function_prototype::{FunctionKind, FunctionPrototypeBuilder},
    program_function::ProgramFunction,
    symbol::Symbol,
};
use lpc_rs_utils::{lpc_string::LpcString, string::closure_arg_number};
use tracing::{instrument, trace};
use tree_walker::{Pass, TreeWalker};
use ustr::{Ustr, ustr};

use crate::{
    compiler::{
        ast::{
            array_node::ArrayNode,
            assignment_node::AssignmentNode,
            ast_node::{AstNode, AstNodeTrait, SpannedNode},
            binary_op_node::{BinaryOpNode, BinaryOperation},
            break_node::BreakNode,
            call_node::{CallChain, CallNode},
            closure_node::ClosureNode,
            continue_node::ContinueNode,
            decl_node::DeclNode,
            do_while_node::DoWhileNode,
            expression_node::ExpressionNode,
            float_node::FloatNode,
            for_each_node::{FOREACH_INDEX, FOREACH_LENGTH, ForEachInit, ForEachNode},
            for_node::ForNode,
            function_def_node::{ARGV, FunctionDefNode},
            function_ptr_node::{FunctionPtrNode, FunctionPtrReceiver},
            if_node::IfNode,
            int_node::IntNode,
            label_node::LabelNode,
            mapping_node::MappingNode,
            program_node::ProgramNode,
            range_node::RangeNode,
            ref_node::RefNode,
            return_node::ReturnNode,
            string_node::StringNode,
            switch_node::SwitchNode,
            ternary_node::TernaryNode,
            unary_op_node::{UnaryOpNode, UnaryOperation},
            var_init_node::VarInitNode,
            var_node::VarNode,
            while_node::WhileNode,
        },
        callee::Callee,
        codegen::{tree_walker, tree_walker::ContextHolder},
        compilation_context::CompilationContext,
        diagnostics::Diagnostics,
    },
    interpreter::{
        efun::{CALL_OTHER, CATCH, EFUN_PROTOTYPES, SIZEOF},
        program::{Program, Region},
    },
};

macro_rules! push_instruction {
    ($slf:expr, $inst:expr, $span:expr) => {
        $slf.function_stack
            .last_mut()
            .unwrap()
            .push_instruction($inst, $span);
    };
}

/// Partition on whether the value is stored in registers or memory, to help
/// select instructions. tl;dr - Value types use `Register`, while reference
/// types use `Memory`.
#[derive(Debug)]
enum OperationType {
    Register,
    Memory,
}

/// Where `break` and `continue` jump inside the innermost loop or `switch`.
#[derive(Debug)]
struct JumpTarget {
    pub break_target: Label,
    /// `None` inside a `switch` that no loop encloses.
    pub continue_target: Option<Label>,
}

impl JumpTarget {
    fn new(break_target: Label, continue_target: Label) -> Self {
        Self {
            break_target,
            continue_target: Some(continue_target),
        }
    }
}

/// How the pool tells literals apart: floats by their bits, so `0.0` and
/// `-0.0` are two entries.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum ConstantKey {
    Int(LpcIntInner),
    Float(u64),
    String(Ustr),
}

impl From<&LpcConstant> for ConstantKey {
    fn from(constant: &LpcConstant) -> Self {
        match constant {
            LpcConstant::Int(x) => Self::Int(*x),
            LpcConstant::Float(x) => Self::Float(x.into_inner().to_bits()),
            LpcConstant::String(x) => Self::String(ustr(x.to_str())),
        }
    }
}

/// Which truth value of a condition takes the jump.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum JumpWhen {
    True,
    False,
}

impl JumpWhen {
    fn flipped(self) -> Self {
        match self {
            Self::True => Self::False,
            Self::False => Self::True,
        }
    }
}

/// Something to store switch case statements
#[derive(Hash, Debug, Clone, Eq, PartialOrd, PartialEq)]
struct SwitchCase(Option<ExpressionNode>);
impl SwitchCase {
    #[inline]
    pub fn is_default(&self) -> bool {
        self.0.is_none()
    }
}

/// A call argument as codegen pushes it: a value register, or the cell a
/// `ref` argument names.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ArgOperand {
    Value(RegisterVariant),
    Ref(RegisterVariant),
}

impl ArgOperand {
    /// The value register, for a position semantic checks already guarantee
    /// never receives a `ref` (`sizeof`'s argument, `call_other`'s receiver
    /// and name).
    fn expect_value(self, span: Option<Span>) -> Result<RegisterVariant> {
        match self {
            ArgOperand::Value(r) => Ok(r),
            ArgOperand::Ref(_) => Err(lpc_bug!(
                span,
                "a `ref` argument reached a value-only position"
            )),
        }
    }
}

/// A tree walker that generates assembly language instructions based on an AST.
#[derive(Debug)]
pub struct CodegenWalker {
    /// Keep track of the current function being generated (including global
    /// initialization)
    function_stack: Vec<ProgramFunction>,

    /// Stack of HashMaps, with keys being Labels, and values being a set of indices of
    /// [`Instruction`]s that need to be patched once the labels have known [`Address`]es.
    backpatch_maps: Vec<HashMap<Label, BitSet>>,

    /// The pool index of each literal an open function has interned so
    /// far, one map per function on the stack, created on first use.
    constant_keys: Vec<HashMap<ConstantKey, RegisterSize>>,

    /// Counter for labels, as they need to be unique.
    label_count: usize,

    /// Function table. The keys are the mangled names of the functions.
    pub functions: HashMap<String, Arc<ProgramFunction>>,

    /// The initialization function for the program, which sets up global variables.
    initializer: Option<Arc<ProgramFunction>>,

    /// The mangled name of this program's own global initializer.
    init_globals: Ustr,

    /// Track where the result of a child branch is
    current_result: RegisterVariant,

    /// Internal counter to track which registers are used.
    register_counter: RegisterCounter,

    /// Counter for tracking globals
    global_counter: RegisterCounter,

    /// Compilation context
    context: CompilationContext,

    /// Labels where jumps at any particular time need to go to.
    jump_targets: Vec<JumpTarget>,

    /// Mapping of `switch` cases to the address of the first instruction for a match
    case_addresses: Vec<Vec<(SwitchCase, Address)>>,

    /// Because Ranges have two results, we store both locations when we `visit_range`.
    visit_range_results: Option<(Option<RegisterVariant>, Option<RegisterVariant>)>,

    /// Track the final locations of closure arguments, so that deeply nested
    /// `$1`-type variables can resolve to the correct location.
    closure_arg_locations: Vec<Vec<RegisterVariant>>,
}

impl CodegenWalker {
    /// Create a new [`CodegenWalker`] that consumes the passed scopes
    ///
    /// # Arguments
    /// `context` - The [`CompilationContext`] state that this tree walker will
    /// use for its internal workings.
    pub fn new(context: CompilationContext) -> Self {
        let num_globals = context.num_globals;

        let mut result = Self {
            context,
            ..Self::default()
        };

        result.global_counter.set(num_globals);

        result.setup_init();

        result
    }

    /// Start this program's own global initializer, for the globals declared
    /// here; `init-program` runs the inherited blocks' initializers itself.
    #[instrument(skip_all)]
    pub fn setup_init(&mut self) {
        let prototype = FunctionPrototypeBuilder::default()
            .name(INIT_GLOBALS)
            .filename(self.context.filename.clone())
            .return_type(LpcType::Void)
            .flags(FunctionFlags::from(&["private"][..]))
            .build()
            .expect("Failed to build init prototype");

        self.init_globals = ustr(&prototype.mangle());
        self.function_stack.push(ProgramFunction::new(prototype, 0));
    }

    /// The function that initializes a new object: each inherited block's
    /// globals in layout order, this program's own, then `create()` if any
    /// program in the chain defines it.
    async fn build_initializer(&mut self, init_globals: Ustr) -> Result<ProgramFunction> {
        let prototype = FunctionPrototypeBuilder::default()
            .name(INIT_PROGRAM)
            .filename(self.context.filename.clone())
            .return_type(LpcType::Void)
            .build()
            .expect("Failed to build initializer prototype");

        self.function_stack.push(ProgramFunction::new(prototype, 0));
        self.backpatch_maps.push(HashMap::new());
        self.register_counter.push();

        let inits: Vec<Ustr> = self.context.layout.iter().map(|r| r.init).collect();
        for init in inits.into_iter().chain(std::iter::once(init_globals)) {
            push_instruction!(self, Instruction::Call(init), None);
        }

        if self
            .context
            .lookup_function(CREATE_FUNCTION, &CallNamespace::Local)
            .is_some()
        {
            let mut call = CallNode {
                chain: CallChain::Root {
                    receiver: None,
                    name: ustr(CREATE_FUNCTION),
                    namespace: CallNamespace::Local,
                },
                arguments: vec![],
                span: None,
            };
            call.visit(self).await?;
        }

        let mut ret = ReturnNode {
            value: None,
            span: None,
        };
        ret.visit(self).await?;

        let func = self.finalize_function(0, None, None)?;
        self.register_counter.pop();

        Ok(func)
    }

    /// Consume this walker and convert it into a [`Program`]
    pub fn into_program(mut self) -> Result<Program> {
        // These are expected and assumed to be in 1:1 correspondence at runtime
        self.ensure_sync()?;

        // get a combined hashmap of all inherited global variables
        let inherits = std::mem::take(&mut self.context.inherits);
        let mut global_variables = inherits.into_iter().map(|i| i.global_variables).fold(
            HashMap::new(),
            |mut acc, vars| {
                acc.extend(*vars);
                acc
            },
        );
        self.context.scopes.goto_root();
        global_variables.extend(std::mem::take(
            &mut self.context.scopes.current_mut().unwrap().symbols,
        ));

        let functions: IndexMap<_, _, ahash::RandomState> = self
            .context
            .inherited_functions
            .into_iter()
            .chain(self.functions)
            .collect();

        // Note that due to name clashes, only the latest seen version of a function is included,
        // but that should be fine, as they are inserted in the order they are processed.
        let unmangled_functions = functions
            .values()
            .filter(|f| !f.is_closure())
            .map(|f| (f.prototype.name.to_string(), f.clone()))
            .collect::<IndexMap<_, _, ahash::RandomState>>();

        let num_globals = self.global_counter.number_emitted();

        let filename = Arc::new(LpcPath::InGame(
            self.context
                .filename
                .as_in_game(self.context.config.lib_dir.as_str())
                .into_owned(),
        ));

        let own = Region {
            filename: Arc::clone(&filename),
            base: self.context.num_globals,
            count: num_globals - self.context.num_globals,
            init: self.init_globals,
        };
        let layout = std::mem::take(&mut self.context.layout)
            .into_iter()
            .chain(std::iter::once(own))
            .collect();

        Ok(Program {
            filename,
            functions: Box::new(functions),
            initializer: self.initializer,
            unmangled_functions: Box::new(unmangled_functions),
            global_variables: Box::new(global_variables),
            num_globals,
            layout,
            pragmas: self.context.pragmas,
        })
    }

    fn ensure_sync(&self) -> Result<()> {
        for func in self
            .functions
            .values()
            .map(|f| f.as_ref())
            .chain(self.initializer.as_deref())
        {
            let a = func.instructions.len();
            let b = func.debug_spans.len();
            if a != b {
                return Err(LpcError::bug(format!(
                    concat!(
                        "Instructions (length {}) and `debug_spans` (length {}) for ",
                        "function `{}` are out of sync. This would be catastrophic at ",
                        "runtime, and indicates a major bug in the code generator."
                    ),
                    a,
                    b,
                    &func.name()
                )));
            }
        }

        Ok(())
    }

    /// helper to choose operation instructions
    fn to_operation_type(&self, node: &ExpressionNode) -> OperationType {
        match node {
            ExpressionNode::Int(_) | ExpressionNode::Float(_) => OperationType::Register,

            ExpressionNode::String(_)
            | ExpressionNode::Array(_)
            | ExpressionNode::Mapping(_)
            | ExpressionNode::Closure(_)
            | ExpressionNode::CommaExpression(_)
            | ExpressionNode::Range(_)
            | ExpressionNode::FunctionPtr(_) => OperationType::Memory,
            ExpressionNode::Assignment(node) => self.to_operation_type(&node.lhs),
            ExpressionNode::Call(node) => {
                if_chain! {
                    if let CallChain::Root { name, namespace, .. } = &node.chain;
                    if let Some(func) = self.context.lookup_function_complete(name, namespace);
                    if let LpcType::Int(_) | LpcType::Float(_) = func.prototype().return_type;
                    then {
                        OperationType::Register
                    } else {
                        OperationType::Memory
                    }
                }
            }
            ExpressionNode::BinaryOp(node) => {
                let left_type = self.to_operation_type(&node.l);
                let right_type = self.to_operation_type(&node.r);
                match (left_type, right_type) {
                    (OperationType::Register, OperationType::Register) => OperationType::Register,
                    _ => OperationType::Memory,
                }
            }
            ExpressionNode::Ternary(node) => {
                let body_type = self.to_operation_type(&node.body);
                let else_type = self.to_operation_type(&node.else_clause);
                match (body_type, else_type) {
                    (OperationType::Register, OperationType::Register) => OperationType::Register,
                    _ => OperationType::Memory,
                }
            }
            ExpressionNode::UnaryOp(node) => {
                let expr_type = self.to_operation_type(&node.expr);

                if matches!(expr_type, OperationType::Register) {
                    OperationType::Register
                } else {
                    OperationType::Memory
                }
            }
            ExpressionNode::Var(VarNode { name, .. })
            | ExpressionNode::Ref(RefNode { name, .. }) => {
                match self.context.lookup_var(name) {
                    Some(Symbol { type_: ty, .. }) => match ty {
                        LpcType::Int(false) => OperationType::Register,
                        LpcType::Float(false) => OperationType::Register,
                        _ => OperationType::Memory,
                    },
                    None => OperationType::Memory, // arbitrary - doing this instead of panicking
                }
            }
        }
    }

    /// The main switch to determine which instruction we select for a binary
    /// operation
    fn choose_op_instruction(
        &self,
        node: &BinaryOpNode,
        reg_left: RegisterVariant,
        reg_right: RegisterVariant,
        reg_result: RegisterVariant,
    ) -> Instruction {
        match node.op {
            BinaryOperation::Add => self.choose_num_or_mixed(
                node,
                || Instruction::IAdd(reg_left, reg_right, reg_result),
                || Instruction::MAdd(reg_left, reg_right, reg_result),
            ),
            BinaryOperation::Sub => self.choose_num_or_mixed(
                node,
                || Instruction::ISub(reg_left, reg_right, reg_result),
                || Instruction::MSub(reg_left, reg_right, reg_result),
            ),
            BinaryOperation::Mul => self.choose_num_or_mixed(
                node,
                || Instruction::IMul(reg_left, reg_right, reg_result),
                || Instruction::MMul(reg_left, reg_right, reg_result),
            ),
            BinaryOperation::Div => Instruction::IDiv(reg_left, reg_right, reg_result),
            BinaryOperation::Mod => Instruction::IMod(reg_left, reg_right, reg_result),
            BinaryOperation::Index => Instruction::Load(reg_left, reg_right, reg_result),
            BinaryOperation::AndAnd => {
                unimplemented!("The short-circuiting behavior requires multiple instructions")
            }
            BinaryOperation::OrOr => {
                unimplemented!("The short-circuiting behavior requires multiple instructions")
            }
            BinaryOperation::And => Instruction::And(reg_left, reg_right, reg_result),
            BinaryOperation::Or => Instruction::Or(reg_left, reg_right, reg_result),
            BinaryOperation::Xor => Instruction::Xor(reg_left, reg_right, reg_result),
            BinaryOperation::EqEq => Instruction::EqEq(reg_left, reg_right, reg_result),
            BinaryOperation::NotEq => Instruction::NotEq(reg_left, reg_right, reg_result),
            BinaryOperation::Lt => Instruction::Lt(reg_left, reg_right, reg_result),
            BinaryOperation::Lte => Instruction::Lte(reg_left, reg_right, reg_result),
            BinaryOperation::Gt => Instruction::Gt(reg_left, reg_right, reg_result),
            BinaryOperation::Gte => Instruction::Gte(reg_left, reg_right, reg_result),
            BinaryOperation::Shl => Instruction::Shl(reg_left, reg_right, reg_result),
            BinaryOperation::Shr => Instruction::Shr(reg_left, reg_right, reg_result),
            BinaryOperation::Compose => unimplemented!(
                "Composition takes multiple instructions, so this is done elsewhere."
            ),
        }
    }

    /// Allows for recursive determination of typed binary operator
    /// instructions, allowing choice between a numeric (i.e. held in
    /// registers) and mixed (i.e. tracked via references) Switching on the
    /// instructions lets us avoid some value lookups at runtime.
    fn choose_num_or_mixed<F, G>(&self, node: &BinaryOpNode, a: F, b: G) -> Instruction
    where
        F: Fn() -> Instruction,
        G: Fn() -> Instruction,
    {
        let left_type = self.to_operation_type(&node.l);
        let right_type = self.to_operation_type(&node.r);

        match (left_type, right_type) {
            (OperationType::Register, OperationType::Register) => a(),
            _ => b(),
        }
    }

    /// A special case for function def parameters, where we don't want to
    /// generate code for default arguments - we just want to have it on
    /// hand to refer to when we generate code for calls.
    async fn visit_parameter(&mut self, node: &VarInitNode) -> Result<RegisterVariant> {
        let loc = self.assign_sym_location(&node.name)?;

        if let Some(sym) = self.context.lookup_var(node.name) {
            let func = self.function_stack.last_mut().unwrap();
            func.local_variables.push(sym.clone())
        }

        Ok(loc)
    }

    /// The location a read of `name` resolves to — a variable read inside its
    /// own initializer is given one here.
    fn location_of(&mut self, name: &str) -> Result<RegisterVariant> {
        match self.context.lookup_var(name).and_then(|sym| sym.location) {
            Some(loc) => Ok(loc),
            None => self.assign_sym_location(name),
        }
    }

    /// The location of `name` in the current scope: a captured symbol keeps
    /// the cell the scope walker laid out, any other takes the next free register.
    fn assign_sym_location(&mut self, name: &str) -> Result<RegisterVariant> {
        let Some(sym) = self.context.lookup_var_mut(name) else {
            return Ok(RegisterVariant::Local(Register(0)));
        };

        if sym.upvalue {
            return sym
                .location
                .ok_or_else(|| lpc_bug!("captured `{}` was never given a cell", name));
        }

        let current_register = if sym.is_global() {
            self.global_counter.next().unwrap().as_global()
        } else {
            self.register_counter.next().unwrap().as_local()
        };

        trace!("Assigning location {} to {}", current_register, sym);

        sym.location = Some(current_register);

        Ok(current_register)
    }

    /// Emit the instruction(s) to take the range of an array or string
    /// # Arguments
    /// `reference` - The [`Register`] holding the reference to the ref we're
    /// taking a slice from. `node` - A reference to the [`RangeNode`] that
    /// holds the range of the slice we're taking.
    async fn emit_range(&mut self, reference: RegisterVariant, node: &mut RangeNode) -> Result<()> {
        let first_index = if let Some(expr) = &mut *node.l {
            expr.visit(self).await?;
            self.current_result
        } else {
            // Default to 0. No instruction needed as the value in registers defaults to int
            // 0.
            self.register_counter.next().unwrap().as_local()
        };

        let second_index = if let Some(expr) = &mut *node.r {
            expr.visit(self).await?;
            self.current_result
        } else {
            // A missing range end means just go to the end of the array.
            self.constant(LpcConstant::Int(-1), node.span)?
        };

        let result = self.register_counter.next().unwrap().as_local();
        self.current_result = result;
        push_instruction!(
            self,
            Instruction::Range(reference, first_index, second_index, result,),
            node.span
        );

        Ok(())
    }

    /// Emit a numbered label with prefix `T`, tracking the current count.
    fn new_label<T>(&mut self, prefix: T) -> String
    where
        T: AsRef<str>,
    {
        let r = format!("{}_{}", prefix.as_ref(), self.label_count);
        self.label_count += 1;
        r
    }

    /// Backpatch the instructions of the passed function, based on the map of
    /// labels and addresses.
    fn backpatch(
        backpatch_map: &HashMap<Label, BitSet>,
        function: &mut ProgramFunction,
    ) -> Result<()> {
        let Some(labels) = &function.labels else {
            return Err(lpc_bug!(
                "No labels found in function `{}`",
                function.name()
            ));
        };

        for (label, addresses) in backpatch_map {
            let Some(label_address) = labels.get(label) else {
                return Err(lpc_bug!(
                    "Label `{}` not found in function `{}`",
                    label,
                    function.name()
                ));
            };

            for address in addresses {
                let Some(instruction) = function.instructions.get_mut(address) else {
                    return Err(lpc_bug!(
                        "Instruction at address {} not found in function `{}`",
                        address,
                        function.name()
                    ));
                };
                instruction.backpatch(*label_address)?
            }
        }

        Ok(())
    }

    /// Append a backpatch entry to the current backpatch map.
    ///
    /// # Arguments
    /// `label` - The label, whose address we're going to insert back into the instructions.
    /// `instruction_address` - The address of the instruction that needs to be backpatched.
    ///
    /// # Errors
    /// If there is no backpatch map to append to, an error is returned.
    ///
    /// # Panics
    /// If the instruction address is already in the backpatch map for the given label.
    fn schedule_backpatch(&mut self, label: &Label, instruction_address: Address) -> Result<()> {
        let Some(backpatch_map) = self.backpatch_maps.last_mut() else {
            return Err(lpc_bug!("No backpatch map found to append to"));
        };

        if let Some(bitset) = backpatch_map.get_mut(label) {
            debug_assert!(
                !bitset.contains(instruction_address.0),
                "Backpatching the same instruction twice"
            );
            bitset.insert(instruction_address.0);
        } else {
            let mut bitset = BitSet::new();
            bitset.insert(instruction_address.0);
            backpatch_map.insert(label.to_string(), bitset);
        }

        Ok(())
    }

    // special case for `catch()`
    async fn emit_catch(&mut self, node: &mut CallNode) -> Result<()> {
        let result_register = self.register_counter.next().unwrap().as_local();
        let label = self.new_label("catch_end");

        let start_address = self.current_address();
        push_instruction!(
            self,
            Instruction::CatchStart(result_register, Address(0)),
            node.span
        );

        for argument in &mut node.arguments {
            argument.visit(self).await?;
        }

        // get the address of the `catch_end` pseudo-instruction, so we can jump to a
        // location that is both guaranteed to have an instruction, as well as
        // clean up the handled catch point
        let label_address = self.current_address();
        self.insert_label(label, label_address);

        // backpatch the `catch_start` instruction with the address of the `catchend`
        let instructions = &mut self.function_stack.last_mut().unwrap().instructions;
        let _ = std::mem::replace(
            &mut instructions[start_address.0],
            Instruction::CatchStart(result_register, label_address),
        );

        push_instruction!(self, Instruction::CatchEnd, node.span);

        self.current_result = result_register;

        Ok(())
    }

    #[inline]
    /// Get the current address of the current function.
    /// This is actually the *next* address that a pushed instruction will be
    /// stored at.
    /// tl;dr This returns the length of the current function's `instructions` vector.
    /// The operand of `constant` in the current function's pool, interned
    /// on first use.
    fn constant(&mut self, constant: LpcConstant, span: Option<Span>) -> Result<RegisterVariant> {
        let key = ConstantKey::from(&constant);
        let depth = self.function_stack.len();
        let Some(function) = self.function_stack.last_mut() else {
            return Err(lpc_bug!(span, "no open function to intern a constant into"));
        };
        self.constant_keys.resize_with(depth, HashMap::new);
        let keys = self.constant_keys.last_mut().unwrap();
        if let Some(&index) = keys.get(&key) {
            return Ok(Register(index).as_constant());
        }

        let pool = &mut function.constants;
        let Ok(index) = RegisterSize::try_from(pool.len()) else {
            return Err(lpc_error!(
                span,
                "too many constants in one function (the limit is {})",
                usize::from(RegisterSize::MAX) + 1
            ));
        };
        pool.push(constant);
        keys.insert(key, index);
        Ok(Register(index).as_constant())
    }

    /// The condition form of an expression: code that jumps to `label` when
    /// the expression's truth is `when` and falls through otherwise, leaving
    /// no value behind. `&&`, `||`, `!`, and integer literals become jumps
    /// alone; anything else is visited for its value and tested once.
    #[async_recursion]
    async fn emit_condition(
        &mut self,
        node: &mut ExpressionNode,
        when: JumpWhen,
        label: &Label,
    ) -> Result<()> {
        let span = node.span();

        match node {
            ExpressionNode::Int(IntNode { value, .. }) => {
                if (*value != 0) == (when == JumpWhen::True) {
                    self.schedule_backpatch(label, self.current_address())?;
                    push_instruction!(self, Instruction::Jmp(Address(0)), span);
                }
            }
            ExpressionNode::UnaryOp(UnaryOpNode {
                op: UnaryOperation::Bang,
                expr,
                ..
            }) => {
                self.emit_condition(expr, when.flipped(), label).await?;
            }
            ExpressionNode::BinaryOp(BinaryOpNode {
                op: op @ (BinaryOperation::AndAnd | BinaryOperation::OrOr),
                l,
                r,
                ..
            }) => {
                // The polarity a single operand can decide on its own.
                let direct = if *op == BinaryOperation::AndAnd {
                    JumpWhen::False
                } else {
                    JumpWhen::True
                };

                if when == direct {
                    self.emit_condition(l, when, label).await?;
                    self.emit_condition(r, when, label).await?;
                } else {
                    let skip_label = self.new_label("condition-skip");
                    self.emit_condition(l, direct, &skip_label).await?;
                    self.emit_condition(r, when, label).await?;
                    self.insert_label(skip_label, self.current_address());
                }
            }
            _ => {
                node.visit(self).await?;
                self.schedule_backpatch(label, self.current_address())?;
                let instruction = match when {
                    JumpWhen::True => Instruction::Jnz(self.current_result, Address(0)),
                    JumpWhen::False => Instruction::Jz(self.current_result, Address(0)),
                };
                push_instruction!(self, instruction, span);
            }
        }

        Ok(())
    }

    fn current_address(&self) -> Address {
        let a = match self.function_stack.last() {
            Some(x) => x.instructions.len(),
            None => 0,
        };

        Address::from(a)
    }

    #[inline]
    fn insert_label<T>(&mut self, label: T, address: Address)
    where
        T: Into<String>,
    {
        self.function_stack
            .last_mut()
            .unwrap()
            .insert_label(label, address);
    }

    // Get a reference to the current [`CompilationContext`]
    pub fn context(&self) -> &CompilationContext {
        &self.context
    }

    pub fn context_mut(&mut self) -> &mut CompilationContext {
        &mut self.context
    }

    fn setup_populate_defaults(
        &mut self,
        span: Option<Span>,
        num_default_args: RegisterSize,
    ) -> Option<Address> {
        if num_default_args == 0 {
            return None;
        }

        let address = Some(self.current_address());

        let instruction = Instruction::PopulateDefaults;
        push_instruction!(self, instruction, span);

        // these addresses are backpatched later, once we have them.
        for _ in 0..num_default_args {
            let instruction = Instruction::Jmp(Address(0));
            push_instruction!(self, instruction, span);
        }

        address
    }

    fn setup_populate_argv(
        &mut self,
        ellipsis: bool,
        span: Option<Span>,
        passed_param_count: RegisterSize,
    ) -> Result<Option<Address>> {
        if ellipsis {
            let argv_location = self.assign_sym_location(ARGV)?;

            if let Some(sym) = self.context.lookup_var(ARGV)
                && let Some(func) = self.function_stack.last_mut()
            {
                func.local_variables.push(sym.clone())
            }

            // We don't set `argv_location` as `self.current_result`, because it's
            // being assigned implicitly, and doesn't need to be made available
            // to more complex expressions. Expressions that use `argv` explicitly
            // are handled elsewhere, as any other expr would be.

            let result = Some(self.current_address());

            // The number of locals isn't known yet, so just set it to zero for now.
            // This gets backpatched after the function body is generated.
            let instruction = Instruction::PopulateArgv(argv_location, passed_param_count, 0);
            push_instruction!(self, instruction, span);

            Ok(result)
        } else {
            Ok(None)
        }
    }

    async fn init_default_params(
        &mut self,
        parameters: &mut [VarInitNode],
        declared_arg_locations: &[RegisterVariant],
        span: Option<Span>,
        populate_defaults_index: Address,
    ) -> Result<()> {
        let mut default_init_addresses = vec![];

        for (idx, parameter) in parameters.iter_mut().enumerate() {
            if let Some(value) = &mut parameter.value {
                default_init_addresses.push(self.current_address());

                // generate code for only the value, then copy by hand, because we
                // pre-generated locations of the parameters above.
                value.visit(self).await?;
                let instruction =
                    Instruction::Copy(self.current_result, declared_arg_locations[idx]);
                push_instruction!(self, instruction, span);
            }
        }

        // backpatch the the correct init addresses for the PopulateDefaults call.
        let sym = self.function_stack.last_mut().unwrap();
        let instruction = &sym.instructions[populate_defaults_index.0];
        if matches!(instruction, Instruction::PopulateDefaults) {
            let idx = populate_defaults_index.0;
            for i in 1..=default_init_addresses.len() {
                debug_assert!(
                    matches!(sym.instructions[idx + i], Instruction::Jmp(_)),
                    "Expected a Jmp instruction for argument default {}.",
                    i
                );
                sym.instructions[idx + i] = Instruction::Jmp(default_init_addresses[i - 1]);
            }
        } else {
            return Err(lpc_bug!(span, "Invalid populate_defaults_index"));
        }

        // jump back to the function now that defaults are populated.
        let instruction =
            Instruction::Jmp(populate_defaults_index + 1 + default_init_addresses.len());
        push_instruction!(self, instruction, span);

        Ok(())
    }

    /// Seal the function on top of the stack: set its locals count (a
    /// captured parameter holds no register, so the count can fall short of
    /// `num_args`), backpatch argv and labels, and coalesce copies.
    fn finalize_function(
        &mut self,
        num_args: RegisterSize,
        populate_argv_index: Option<Address>,
        span: Option<Span>,
    ) -> Result<ProgramFunction> {
        let backpatch_map = self.backpatch_maps.pop().unwrap();
        let mut func = self.function_stack.pop().unwrap();
        self.constant_keys.truncate(self.function_stack.len());
        func.num_locals = self
            .register_counter
            .number_emitted()
            .saturating_sub(num_args);
        if let Some(idx) = populate_argv_index {
            Self::backpatch_populate_argv(&mut func, idx, span)?;
        }
        Self::backpatch(&backpatch_map, &mut func)?;
        super::coalesce::coalesce(&mut func);
        Ok(func)
    }

    fn backpatch_populate_argv(
        func: &mut ProgramFunction,
        populate_argv_address: Address,
        span: Option<Span>,
    ) -> Result<()> {
        let instruction = &func.instructions[populate_argv_address.0];

        if let Instruction::PopulateArgv(loc, num_args, _) = instruction {
            let new_instruction = Instruction::PopulateArgv(*loc, *num_args, func.num_locals);
            func.instructions[populate_argv_address.0] = new_instruction;

            Ok(())
        } else {
            Err(lpc_error!(span, "Invalid populate_argv_index"))
        }
    }

    async fn visit_parameters(&mut self, nodes: &[VarInitNode]) -> Result<Vec<RegisterVariant>> {
        let mut result = Vec::with_capacity(nodes.len());

        for node in nodes {
            result.push(self.visit_parameter(node).await?);
        }

        Ok(result)
    }
}

impl ContextHolder for CodegenWalker {
    fn into_context(self) -> CompilationContext {
        self.context
    }
}

impl Pass for CodegenWalker {
    fn new(context: CompilationContext) -> Self {
        CodegenWalker::new(context)
    }

    fn diagnostics_mut(&mut self) -> &mut Diagnostics {
        &mut self.context.diagnostics
    }
}

#[async_trait]
impl TreeWalker for CodegenWalker {
    fn enter_scope(&mut self, scope_id: &mut Option<ScopeId>) {
        self.context.scopes.goto(*scope_id);
    }

    fn exit_scope(&mut self) {
        self.context.scopes.pop();
    }

    #[instrument(skip_all)]
    async fn visit_array(&mut self, node: &mut ArrayNode) -> Result<()> {
        let mut items = Vec::with_capacity(node.value.len());
        for member in &mut node.value {
            let _ = member.visit(self).await;
            items.push(self.current_result);
        }

        let register = self.register_counter.next().unwrap().as_local();
        self.current_result = register;
        for item in items.iter() {
            let instruction = Instruction::PushArrayItem(*item);
            push_instruction!(self, instruction, node.span);
        }
        push_instruction!(self, Instruction::AConst(register), node.span);

        Ok(())
    }

    #[instrument(skip_all)]
    async fn visit_assignment(&mut self, node: &mut AssignmentNode) -> Result<()> {
        node.rhs.visit(self).await?;
        let rhs_result = self.current_result;
        let lhs = &mut *node.lhs;

        match lhs {
            ExpressionNode::Var(_) => {
                lhs.visit(self).await?;
                let lhs_result = self.current_result;
                trace!("assignment: lhs: {}, rhs: {}", lhs_result, rhs_result);

                let assign = Instruction::Copy(rhs_result, lhs_result);

                push_instruction!(self, assign, node.span);

                self.current_result = lhs_result;
            }
            ExpressionNode::BinaryOp(BinaryOpNode {
                op: BinaryOperation::Index,
                l,
                r,
                ..
            }) => {
                l.visit(self).await?;
                let var_result = self.current_result;
                r.visit(self).await?;
                let index_result = self.current_result;

                let store = Instruction::Store(rhs_result, var_result, index_result);

                push_instruction!(self, store, node.span);

                self.current_result = rhs_result;
            }
            x => {
                return Err(lpc_error!(
                    node.span,
                    "Attempt to assign to an invalid lvalue: `{}`",
                    x
                ));
            }
        }

        Ok(())
    }

    #[instrument(skip_all)]
    async fn visit_binary_op(&mut self, node: &mut BinaryOpNode) -> Result<()> {
        if node.op == BinaryOperation::AndAnd {
            // A loop re-enters with the last iteration's value still in the
            // result register.
            let end_label = self.new_label("andand-end");
            let reg_result = self.register_counter.next().unwrap().as_local();
            let zero = self.constant(LpcConstant::Int(0), node.span)?;
            push_instruction!(self, Instruction::Copy(zero, reg_result), node.span);

            self.emit_condition(&mut node.l, JumpWhen::False, &end_label)
                .await?;

            node.r.visit(self).await?;
            let instruction = Instruction::Copy(self.current_result, reg_result);
            push_instruction!(self, instruction, node.span);

            self.insert_label(end_label, self.current_address());
            self.current_result = reg_result;

            return Ok(());
        }

        node.l.visit(self).await?;
        let reg_left = self.current_result;

        // special handling for ops that require more than a single instruction
        match node.op {
            BinaryOperation::Index => {
                // Ranges need special handling that complicates this function otherwise, due to
                // the visit to node.r needing to handle multiple results.
                if let ExpressionNode::Range(range_node) = &mut *node.r {
                    self.emit_range(reg_left, range_node).await?;
                    return Ok(());
                }
            }
            BinaryOperation::OrOr => {
                // Handle short-circuit behavior
                let end_label = self.new_label("oror-end");

                let reg_result = self.register_counter.next().unwrap().as_local();
                let instruction = Instruction::Copy(reg_left, reg_result);
                push_instruction!(self, instruction, node.span);

                self.schedule_backpatch(&end_label, self.current_address())?;
                let instruction = Instruction::Jnz(reg_result, Address(0));
                push_instruction!(self, instruction, node.span);

                node.r.visit(self).await?;
                let reg_right = self.current_result;
                let instruction = Instruction::Copy(reg_right, reg_result);
                push_instruction!(self, instruction, node.span);

                self.insert_label(end_label, self.current_address());
                self.current_result = reg_result;

                return Ok(());
            }
            BinaryOperation::Compose => {
                // This literally just sets up a call to the compose() efun, and
                // puts the result of it into a register.
                node.r.visit(self).await?;
                let reg_right = self.current_result;
                push_instruction!(self, Instruction::PushArg(reg_left), node.span);
                push_instruction!(self, Instruction::PushArg(reg_right), node.span);
                push_instruction!(
                    self,
                    Instruction::CallEfun(u8::try_from(
                        EFUN_PROTOTYPES.get_index_of("compose").unwrap()
                    )?),
                    node.span
                );

                let reg_result = self.register_counter.next().unwrap().as_local();
                let instruction =
                    Instruction::Copy(RegisterVariant::Local(Register(0)), reg_result);
                push_instruction!(self, instruction, node.span);

                self.current_result = reg_result;

                return Ok(());
            }
            _ => { /* fallthrough */ }
        }

        node.r.visit(self).await?;
        let reg_right = self.current_result;

        let reg_result = self.register_counter.next().unwrap().as_local();
        self.current_result = reg_result;

        let instruction = self.choose_op_instruction(node, reg_left, reg_right, reg_result);
        push_instruction!(self, instruction, node.span);

        Ok(())
    }

    #[instrument(skip_all)]
    async fn visit_break(&mut self, node: &mut BreakNode) -> Result<()> {
        if let Some(JumpTarget { break_target, .. }) = self.jump_targets.last() {
            self.schedule_backpatch(&break_target.clone(), self.current_address())?;
            let instruction = Instruction::Jmp(0.into());
            push_instruction!(self, instruction, node.span);
            return Ok(());
        }

        Err(lpc_bug!(
            node.span,
            "`break` statement without a jump target?"
        ))
    }

    #[instrument(skip_all)]
    async fn visit_call_root(&mut self, node: &mut CallNode) -> Result<()> {
        let node_span = node.span();
        let CallChain::Root {
            ref mut receiver,
            ref name,
            ref namespace,
        } = node.chain
        else {
            return Err(lpc_bug!(node.span, "Invalid call chain"));
        };
        let has_receiver = receiver.is_some();

        if name.as_str() == CATCH {
            return self.emit_catch(node).await;
        }

        let argument_len = node.arguments.len();

        // Resolved before the arguments are visited: an implicit efun lvalue
        // is pushed as a cell, never evaluated.
        // A function-typed variable answers a bare name only.
        let calls_variable = namespace == &CallNamespace::Local
            && self
                .context
                .lookup_var(name)
                .is_some_and(|v| v.type_.matches_type(LpcType::Function(false)));
        let implicit_refs: Vec<bool> = if receiver.is_none() && !calls_variable {
            self.context
                .lookup_function_complete(name, namespace)
                .map(|f| {
                    let proto = f.as_ref();
                    (0..argument_len).map(|i| proto.is_ref_param(i)).collect()
                })
                .unwrap_or_default()
        } else {
            Vec::new()
        };

        // Visited before the pushes: a receiver that is itself a call would
        // consume the staged arguments.
        let receiver_result = match receiver {
            Some(rcvr) => {
                rcvr.visit(self).await?;
                Some(self.current_result)
            }
            None => None,
        };

        let mut arg_results: Vec<ArgOperand> = Vec::with_capacity(argument_len);
        for (index, argument) in node.arguments.iter_mut().enumerate() {
            let by_ref = match argument {
                ExpressionNode::Ref(r) => Some((r.name, r.span)),
                ExpressionNode::Var(v) if implicit_refs.get(index).copied().unwrap_or(false) => {
                    Some((v.name, v.span))
                }
                _ => None,
            };
            match by_ref {
                Some((var_name, span)) => {
                    let loc = self.location_of(&var_name)?;
                    if matches!(loc, RegisterVariant::Local(_)) {
                        return Err(lpc_bug!(
                            span,
                            "`ref {}` resolved to a register, not a cell",
                            var_name
                        ));
                    }
                    arg_results.push(ArgOperand::Ref(loc));
                }
                None => {
                    argument.visit(self).await?;
                    arg_results.push(ArgOperand::Value(self.current_result));
                }
            }
        }

        if name.as_str() == SIZEOF {
            let result = self.register_counter.next().unwrap().as_local();
            // `sizeof`'s arity is checked by the semantic walker, fatal before codegen runs.
            let arg = arg_results.first().unwrap().expect_value(node.span)?;
            let instruction = Instruction::Sizeof(arg, result);
            push_instruction!(self, instruction, node.span);
            self.current_result = result;

            return Ok(());
        }

        let instruction = {
            // populate the args vector
            for result in &arg_results {
                push_instruction!(
                    self,
                    match *result {
                        ArgOperand::Value(r) => Instruction::PushArg(r),
                        ArgOperand::Ref(r) => Instruction::PushRef(r),
                    },
                    node.span
                );
            }

            if let Some(receiver_result) = receiver_result {
                let name = LpcString::Static(*name);
                let name_register =
                    self.constant(LpcConstant::String(Arc::new(name)), node.span)?;

                Instruction::CallOther(receiver_result, name_register)
            } else if name.as_str() == CALL_OTHER {
                debug_assert!(
                    arg_results.len() >= 2,
                    "CallOther requires at least 2 arguments, for the receiver and function name"
                );
                let receiver = arg_results[0].expect_value(node.span)?;
                let name_index = arg_results[1].expect_value(node.span)?;

                Instruction::CallOther(receiver, name_index)
            } else {
                if_chain! {
                    if calls_variable;
                    then {
                        Instruction::CallFp(self.location_of(name)?)
                    } else {
                        let Some(func) =
                            self.context.lookup_function_complete(name, namespace) else {
                            return Err(lpc_bug!(
                                node.span,
                                "Cannot find function during code gen: {}",
                                name
                            ));
                        };

                        match func {
                            Callee::Local(prototype) => {
                                if prototype.kind == FunctionKind::Closure {
                                    return Err(lpc_bug!(
                                        node.span,
                                        "closure `{}` called by name",
                                        name
                                    ));
                                }
                                Instruction::Call(ustr(&prototype.mangle()))
                            }
                            Callee::SimulEfun(_) => Instruction::CallSimulEfun(*name),
                            Callee::Efun(_) => {
                                let idx = EFUN_PROTOTYPES.get_index_of(name.as_str()).unwrap();
                                Instruction::CallEfun(u8::try_from(idx)?)
                            }
                        }
                    }
                }
            }
        };

        push_instruction!(self, instruction, node.span);

        let push_copy = |walker: &mut Self| {
            let next_register = walker.register_counter.next().unwrap().as_local();

            push_instruction!(
                walker,
                Instruction::Copy(Register(0).as_local(), next_register),
                node_span
            );

            walker.current_result = next_register;
        };

        // Take care of the result after the call returns.
        if let Some(func) = self.context.lookup_function_complete(name, namespace) {
            if func.as_ref().return_type == LpcType::Void {
                self.current_result = Register(0).as_local();
            } else {
                push_copy(self);
            }
        } else if let Some(Symbol {
            type_: LpcType::Function(false) | LpcType::Mixed(false),
            ..
        }) = self.context.lookup_var(name)
        {
            push_copy(self);
        } else if has_receiver
            || matches!(
                self.context.scopes.lookup(name),
                Some(Symbol {
                    type_: LpcType::Function(false),
                    ..
                })
            )
        {
            push_copy(self);
        } else {
            return Err(lpc_bug!(
                node.span,
                "Unable to find the return type for `{}`. This is a weird issue that indicates \
                something very broken in the semantic checks, or that I'm not looking hard enough.",
                name
            ));
        }

        Ok(())
    }

    #[instrument(skip_all)]
    async fn visit_call_chain(&mut self, node: &mut CallNode) -> Result<()> {
        let CallChain::Node(chain_node) = &mut node.chain else {
            return Err(lpc_bug!(node.span, "Invalid call chain"));
        };

        chain_node.visit(self).await?;
        let fp_loc = self.current_result;

        let argument_len = node.arguments.len();
        let mut arg_results = Vec::with_capacity(argument_len);

        for argument in &mut node.arguments {
            argument.visit(self).await?;
            arg_results.push(self.current_result);
        }

        // populate the args vector
        for result in &arg_results {
            push_instruction!(self, Instruction::PushArg(*result), node.span);
        }

        push_instruction!(self, Instruction::CallFp(fp_loc), node.span);

        let next_register = self.register_counter.next().unwrap().as_local();

        push_instruction!(
            self,
            Instruction::Copy(Register(0).as_local(), next_register),
            node.span()
        );

        self.current_result = next_register;

        Ok(())
    }

    #[instrument(skip_all)]
    async fn visit_closure(&mut self, node: &mut ClosureNode) -> Result<()> {
        let Some(prototype) = self.context.function_prototypes.get(&*node.name) else {
            return Err(lpc_error!(
                node.span,
                "closure prototype for {} not found",
                node.name
            ));
        };

        let arity = prototype.arity;
        let num_args = arity.num_args;
        let num_default_args = arity.num_default_args;

        let func = ProgramFunction::new(prototype.clone(), 0);
        debug_assert!(func.labels.is_some(), "labels are expected for codegen");

        self.function_stack.push(func);
        self.backpatch_maps.push(HashMap::new());

        let Some(scope_id) = node.scope_id else {
            return Err(lpc_error!(
                node.span,
                "closure scope for {} not found",
                node.name
            ));
        };

        let len = self.current_address();

        self.register_counter.push();

        let declared_arg_locations = if let Some(parameters) = &node.parameters {
            self.visit_parameters(parameters).await?
        } else {
            Vec::new()
        };
        let declared_arg_count = RegisterSize::try_from(declared_arg_locations.len())?;

        self.closure_arg_locations.push(declared_arg_locations);

        let populate_defaults_index = self.setup_populate_defaults(node.span, num_default_args);

        let populate_argv_index =
            self.setup_populate_argv(node.flags.ellipsis(), node.span, declared_arg_count)?;

        let start_label = self.new_label("closure-body-start");
        self.insert_label(&start_label, self.current_address());

        for expression in &mut node.body {
            expression.visit(self).await?;
        }

        // return the current result if there is no explicit return.
        {
            let sym = self.function_stack.last_mut().unwrap();
            if sym.instructions.len() == len.0
                || (!sym.instructions.is_empty()
                    && *sym.instructions.last().unwrap() != Instruction::Ret)
            {
                let target = RegisterVariant::Local(Register(0));

                if self.current_result != target {
                    sym.push_instruction(Instruction::Copy(self.current_result, target), node.span);
                }

                sym.push_instruction(Instruction::Ret, node.span);
            }
        }

        let declared_arg_locations = self.closure_arg_locations.pop().unwrap();

        if num_default_args > 0
            && let Some(parameters) = &mut node.parameters
        {
            debug_assert!(populate_defaults_index.is_some());

            self.init_default_params(
                parameters,
                &declared_arg_locations,
                node.span,
                populate_defaults_index.unwrap(),
            )
            .await?;
        }

        {
            let func = self.function_stack.last_mut().unwrap();
            func.num_upvalues = self.context.scopes.get(scope_id).unwrap().num_upvalues;
            func.arg_locations = declared_arg_locations;
        }
        let func = self.finalize_function(num_args, populate_argv_index, node.span)?;

        // The mangled name carries the file, so inherited code keeps its own closures.
        let name = ustr(&func.mangle());

        self.functions.insert(func.mangle(), func.into());

        self.register_counter.pop();

        // At this point, the closure has been generated and stored.
        // We just need to store a reference to it in the current result.
        let location = self.register_counter.next().unwrap().as_local();
        self.current_result = location;

        // closures are just pointers to functions
        let instruction = Instruction::FunctionPtrConst {
            location,
            receiver: FunctionReceiver::Local,
            name,
        };

        push_instruction!(self, instruction, node.span);

        Ok(())
    }

    #[instrument(skip_all)]
    async fn visit_continue(&mut self, node: &mut ContinueNode) -> Result<()> {
        if let Some(JumpTarget {
            continue_target: Some(continue_target),
            ..
        }) = self.jump_targets.last()
        {
            self.schedule_backpatch(&continue_target.clone(), self.current_address())?;
            let instruction = Instruction::Jmp(Address(0));
            push_instruction!(self, instruction, node.span);
            return Ok(());
        }

        Err(lpc_error!(
            node.span,
            "`continue` statement without a jump target?"
        ))
    }

    #[instrument(skip_all)]
    async fn visit_decl(&mut self, node: &mut DeclNode) -> Result<()> {
        for init in &mut node.initializations {
            self.visit_var_init(init).await?;
        }

        Ok(())
    }

    #[instrument(skip_all)]
    async fn visit_do_while(&mut self, node: &mut DoWhileNode) -> Result<()> {
        let start_label = self.new_label("do-while-start");
        let end_label = self.new_label("do-while-end");
        let continue_label = self.new_label("do-while-continue");
        let jump_target = JumpTarget::new(end_label.clone(), continue_label.clone());
        self.jump_targets.push(jump_target);

        let start_addr = self.current_address();
        self.insert_label(start_label.clone(), start_addr);

        node.body.visit(self).await?;

        let continue_addr = self.current_address();
        self.insert_label(continue_label, continue_addr);

        self.emit_condition(&mut node.condition, JumpWhen::True, &start_label)
            .await?;
        let end_addr = self.current_address();
        self.insert_label(end_label, end_addr);

        self.jump_targets.pop();
        Ok(())
    }

    #[instrument(skip_all)]
    async fn visit_float(&mut self, node: &mut FloatNode) -> Result<()> {
        self.current_result = self.constant(LpcConstant::Float(node.value), node.span)?;

        Ok(())
    }

    #[instrument(skip_all)]
    async fn visit_for(&mut self, node: &mut ForNode) -> Result<()> {
        if let Some(i) = &mut *node.initializer {
            i.visit(self).await?;
        }

        let start_label = self.new_label("for-start");
        let end_label = self.new_label("for-end");
        let continue_label = self.new_label("for-continue");
        let jump_target = JumpTarget::new(end_label.clone(), continue_label.clone());
        self.jump_targets.push(jump_target);
        let start_addr = self.current_address();
        self.insert_label(start_label.clone(), start_addr);

        if let Some(cond) = &mut node.condition {
            self.emit_condition(cond, JumpWhen::False, &end_label)
                .await?;
        }

        node.body.visit(self).await?;

        let continue_addr = self.current_address();
        self.insert_label(continue_label, continue_addr);

        if let Some(i) = &mut node.incrementer {
            i.visit(self).await?;
        }

        // go back to the start of the loop
        self.schedule_backpatch(&start_label, self.current_address())?;
        let instruction = Instruction::Jmp(Address(0));
        push_instruction!(self, instruction, node.span);

        let addr = self.current_address();
        self.insert_label(end_label, addr);

        self.jump_targets.pop();
        Ok(())
    }

    #[instrument(skip_all)]
    async fn visit_foreach(&mut self, node: &mut ForEachNode) -> Result<()> {
        node.collection.visit(self).await?;
        let collection_location = self.current_result;

        let index_location = self.assign_sym_location(FOREACH_INDEX)?;
        let length_location = self.assign_sym_location(FOREACH_LENGTH)?;

        let instruction = Instruction::Sizeof(collection_location, length_location);
        push_instruction!(self, instruction, node.span);

        let locations = match &mut node.initializer {
            ForEachInit::Array(node) | ForEachInit::String(node) => {
                node.visit(self).await?;

                vec![self.current_result]
            }
            ForEachInit::Mapping { key, value } => {
                key.visit(self).await?;
                let key_result = self.current_result;
                value.visit(self).await?;
                vec![key_result, self.current_result]
            }
        };

        let start_label = self.new_label("foreach-start");
        let end_label = self.new_label("foreach-end");
        let continue_label = self.new_label("foreach-continue");
        let jump_target = JumpTarget::new(end_label.clone(), continue_label.clone());
        self.jump_targets.push(jump_target);
        let start_addr = self.current_address();
        self.insert_label(start_label.clone(), start_addr);

        let eqeq_result = self.register_counter.next().unwrap().as_local();
        let instruction = Instruction::EqEq(index_location, length_location, eqeq_result);
        push_instruction!(self, instruction, node.span);

        self.schedule_backpatch(&end_label, self.current_address())?;
        let instruction = Instruction::Jnz(eqeq_result, Address(0));
        push_instruction!(self, instruction, node.span);

        // assign next element(s) to the locations
        match &node.initializer {
            ForEachInit::Array(node) | ForEachInit::String(node) => {
                debug_assert!(locations.len() == 1);

                let instruction =
                    Instruction::Load(collection_location, index_location, locations[0]);
                push_instruction!(self, instruction, node.span);
            }
            ForEachInit::Mapping { key, value } => {
                debug_assert!(locations.len() == 2);

                let instruction =
                    Instruction::LoadMappingKey(collection_location, index_location, locations[0]);
                push_instruction!(self, instruction, key.span());

                let instruction =
                    Instruction::Load(collection_location, locations[0], locations[1]);
                push_instruction!(self, instruction, value.span());
            }
        }

        node.body.visit(self).await?;

        let continue_addr = self.current_address();
        self.insert_label(continue_label, continue_addr);

        let instruction = Instruction::Inc(index_location);
        push_instruction!(self, instruction, node.span);

        // go back to the start of the loop
        self.schedule_backpatch(&start_label, self.current_address())?;
        let instruction = Instruction::Jmp(Address(0));
        push_instruction!(self, instruction, node.span);

        let addr = self.current_address();
        self.insert_label(end_label, addr);

        self.jump_targets.pop();
        Ok(())
    }

    #[instrument(skip_all)]
    async fn visit_function_def(&mut self, node: &mut FunctionDefNode) -> Result<()> {
        // Note we don't look to inherited files at all for this -
        // We're generating code for a function defined _in this object_
        let prototype = match self.context.function_prototypes.get(&*node.name) {
            Some(p) => p,
            None => {
                return Err(lpc_error!(
                    node.span,
                    "function prototype for {} not found",
                    node.name
                ));
            }
        };

        let arity = prototype.arity;
        let num_args = arity.num_args;
        let num_default_args = arity.num_default_args;

        let sym = ProgramFunction::new(prototype.clone(), 0);
        debug_assert!(sym.labels.is_some(), "labels are expected for codegen");

        self.function_stack.push(sym);
        self.backpatch_maps.push(HashMap::new());

        let len = self.current_address();
        self.register_counter.push();

        self.context.scopes.goto_function(&node.name)?;
        let declared_arg_count = RegisterSize::try_from(node.parameters.len())?;
        let declared_arg_locations = self.visit_parameters(&node.parameters).await?;

        let populate_defaults_index = self.setup_populate_defaults(node.span, num_default_args);

        let populate_argv_index =
            self.setup_populate_argv(node.flags.ellipsis(), node.span, declared_arg_count)?;

        let start_label = self.new_label("function-body-start");
        self.insert_label(&start_label, self.current_address());

        for expression in &mut node.body {
            expression.visit(self).await?;
        }

        // insert a final return if one isn't already there.
        {
            let sym = self.function_stack.last_mut().unwrap();
            if sym.instructions.len() == len.0
                || (!sym.instructions.is_empty()
                    && *sym.instructions.last().unwrap() != Instruction::Ret)
            {
                if sym.return_type() != LpcType::Void {
                    self.context.diagnostics.record(lpc_warning!(
                        node.span,
                        "non-void function does not return a value. defaulting to 0."
                    ));
                }
                sym.push_instruction(Instruction::Ret, node.span);
            }
        }

        debug_assert_eq!(declared_arg_count as usize, declared_arg_locations.len());

        if num_default_args > 0 {
            // always set when num_default_args > 0
            debug_assert!(populate_defaults_index.is_some());

            self.init_default_params(
                &mut node.parameters,
                &declared_arg_locations,
                node.span,
                populate_defaults_index.unwrap(),
            )
            .await?;
        }

        self.context.scopes.pop();
        {
            let func = self.function_stack.last_mut().unwrap();
            func.num_upvalues = self
                .context
                .scopes
                .function_scope(&node.name)
                .map_or(0, |scope| scope.num_upvalues);
            func.arg_locations = declared_arg_locations;
        }
        let func = self.finalize_function(num_args, populate_argv_index, node.span)?;
        self.functions.insert(func.mangle(), func.into());

        self.register_counter.pop();

        Ok(())
    }

    #[instrument(skip_all)]
    async fn visit_function_ptr(&mut self, node: &mut FunctionPtrNode) -> Result<()> {
        let mut applied_arguments = vec![];
        if let Some(args) = &mut node.arguments {
            for argument in args {
                if let Some(n) = argument {
                    n.visit(self).await?;
                    applied_arguments.push(Some(self.current_result));
                } else {
                    applied_arguments.push(None);
                }
            }
        }

        let receiver = if let Some(rcvr) = &mut node.receiver {
            // remote receiver, i.e. `call_other`
            match rcvr {
                FunctionPtrReceiver::Static(rcvr_node) => {
                    rcvr_node.visit(self).await?;
                    FunctionReceiver::Var(self.current_result)
                }

                // `&` used as the receiver
                FunctionPtrReceiver::Dynamic => FunctionReceiver::Dynamic,
            }
        } else {
            match self
                .context
                .lookup_function_complete(node.name, &CallNamespace::Local)
            {
                Some(Callee::Local(_)) => FunctionReceiver::Local,
                Some(Callee::SimulEfun(_)) => FunctionReceiver::SimulEfun,
                Some(Callee::Efun(_)) => FunctionReceiver::Efun,
                None => {
                    return Err(lpc_error!(
                        node.span,
                        "unknown call in function pointer: `{}`",
                        node.name
                    ));
                }
            }
        };

        // prepare the partially-applied arguments
        for a in &applied_arguments {
            let instruction = Instruction::PushPartialArg(*a);
            push_instruction!(self, instruction, node.span);
        }

        let location = self.register_counter.next().unwrap().as_local();
        self.current_result = location;

        let instruction = Instruction::FunctionPtrConst {
            location,
            name: node.name,
            receiver,
        };

        push_instruction!(self, instruction, node.span);

        Ok(())
    }

    #[instrument(skip_all)]
    async fn visit_if(&mut self, node: &mut IfNode) -> Result<()> {
        let else_label = self.new_label("if-else");
        let end_label = self.new_label("if-end");

        self.emit_condition(&mut node.condition, JumpWhen::False, &else_label)
            .await?;

        node.body.visit(self).await?;

        if node.else_clause.is_some() {
            self.schedule_backpatch(&end_label, self.current_address())?;
            let instruction = Instruction::Jmp(Address(0));
            push_instruction!(self, instruction, node.span);
        }

        let addr = self.current_address();
        self.insert_label(else_label, addr);

        // Generate the else clause code if necessary
        if let Some(n) = &mut *node.else_clause {
            n.visit(self).await?;

            let addr = self.current_address();
            self.insert_label(end_label, addr);
        }

        Ok(())
    }

    #[instrument(skip_all)]
    async fn visit_int(&mut self, node: &mut IntNode) -> Result<()> {
        self.current_result = self.constant(LpcConstant::Int(node.value), node.span)?;

        Ok(())
    }

    #[instrument(skip_all)]
    async fn visit_label(&mut self, node: &mut LabelNode) -> Result<()> {
        let address = self.current_address();
        match self.case_addresses.last_mut() {
            Some(x) => {
                // track address of where this label will point
                let case = SwitchCase(node.case.clone());
                x.push((case, address));
                Ok(())
            }
            None => Err(lpc_error!(
                node.span,
                "Found a label in the code generator, but nowhere to store the address?",
            )),
        }
    }

    #[instrument(skip_all)]
    async fn visit_mapping(&mut self, node: &mut MappingNode) -> Result<()> {
        let mut items = Vec::with_capacity(node.value.len() * 2);

        for (key, value) in &mut node.value {
            key.visit(self).await?;
            items.push(self.current_result);

            value.visit(self).await?;
            items.push(self.current_result);
        }

        for item in items {
            // Just let the `array_items` vector do double duty.
            push_instruction!(self, Instruction::PushArrayItem(item), node.span);
        }

        let register = self.register_counter.next().unwrap().as_local();
        self.current_result = register;
        push_instruction!(self, Instruction::MapConst(register), node.span);

        Ok(())
    }

    #[instrument(skip_all)]
    async fn visit_program(&mut self, program: &mut ProgramNode) -> Result<()> {
        self.context.scopes.goto_root();
        self.backpatch_maps.push(HashMap::new());

        // Partition global variable initializations vs everything else
        let (global_init, functions): (Vec<&mut AstNode>, Vec<&mut AstNode>) = program
            .body
            .iter_mut()
            .partition(|x| matches!(**x, AstNode::Decl(_)));

        // Hoist all global variables, and initialize them at the very start
        // of the program (i.e. at the time it's cloned)
        for node in global_init {
            node.visit(self).await?;
        }

        let mut ret = ReturnNode {
            value: None,
            span: None,
        };
        ret.visit(self).await?;

        let init_globals = self.finalize_function(0, None, None)?;
        debug_assert!(init_globals.name() == INIT_GLOBALS);
        let init_globals_name = ustr(&init_globals.mangle());
        debug_assert_eq!(init_globals_name, self.init_globals);
        self.functions
            .insert(init_globals.mangle(), init_globals.into());

        for node in functions {
            node.visit(self).await?;
        }

        self.initializer = Some(Arc::new(self.build_initializer(init_globals_name).await?));

        self.context.scopes.pop();

        Ok(())
    }

    #[instrument(skip_all)]
    async fn visit_range(&mut self, node: &mut RangeNode) -> Result<()> {
        let mut result_left: Option<RegisterVariant> = None;
        let mut result_right: Option<RegisterVariant> = None;
        if let Some(expr) = &mut *node.l {
            expr.visit(self).await?;
            result_left = Some(self.current_result);
        }

        if let Some(expr) = &mut *node.r {
            expr.visit(self).await?;
            result_right = Some(self.current_result);
        }

        self.visit_range_results = Some((result_left, result_right));

        Ok(())
    }

    /// `visit_call_root` resolves every `ref` argument itself, before
    /// visiting it; reaching here means one slipped through outside a call.
    #[instrument(skip_all)]
    async fn visit_ref(&mut self, node: &mut RefNode) -> Result<()> {
        Err(lpc_bug!(node.span, "`ref` outside an argument list"))
    }

    #[instrument(skip_all)]
    async fn visit_return(&mut self, node: &mut ReturnNode) -> Result<()> {
        if let Some(expression) = &mut node.value {
            expression.visit(self).await?;
            let copy = Instruction::Copy(self.current_result, Register(0).as_local());
            push_instruction!(self, copy, expression.span());
        }

        push_instruction!(self, Instruction::Ret, node.span);

        Ok(())
    }

    #[instrument(skip_all)]
    async fn visit_string(&mut self, node: &mut StringNode) -> Result<()> {
        let string = LpcString::Static(ustr(&node.value));
        self.current_result = self.constant(LpcConstant::String(Arc::new(string)), node.span)?;

        Ok(())
    }

    #[instrument(skip_all)]
    async fn visit_switch(&mut self, node: &mut SwitchNode) -> Result<()> {
        node.expression.visit(self).await?;
        let expr_result = self.current_result;

        let test_label = self.new_label("switch-test");
        self.schedule_backpatch(&test_label, self.current_address())?;
        let instruction = Instruction::Jmp(Address(0));
        push_instruction!(self, instruction, node.span);

        let end_label = self.new_label("switch-end");
        // `continue` inside a case belongs to the enclosing loop.
        let continue_target = self
            .jump_targets
            .last()
            .and_then(|target| target.continue_target.clone());
        self.jump_targets.push(JumpTarget {
            break_target: end_label.clone(),
            continue_target,
        });
        let addresses = vec![];
        self.case_addresses.push(addresses);

        node.body.visit(self).await?;

        // skip over the tests that we're about to generate.
        let instruction = Instruction::Jmp(Address(0));
        // skip this jump if the final case statement ended with its own `break`.
        if self
            .function_stack
            .last()
            .unwrap()
            .instructions
            .last()
            .unwrap()
            != &instruction
        {
            self.schedule_backpatch(&end_label, self.current_address())?;
            push_instruction!(self, instruction, node.span);
        }

        // generate all the tests for matching the case statements
        let test_address = self.current_address();
        self.insert_label(test_label, test_address);

        let mut case_addresses = self.case_addresses.pop().unwrap();
        // move the default case to the end, so we check it last when generating code.
        if let Some(idx) = case_addresses.iter().position(|i| i.0.is_default()) {
            let last_idx = case_addresses.len() - 1;
            case_addresses.swap(idx, last_idx);
        }

        for case_address in case_addresses {
            match case_address.0.0 {
                Some(mut case_expr) => {
                    case_expr.visit(self).await?;
                    let test_result = self.register_counter.next().unwrap().as_local();

                    if let ExpressionNode::Range(range_node) = case_expr {
                        let (range_left, range_right) = self.visit_range_results.unwrap();

                        // An open end of the range is always satisfied.
                        let gte_result = if let Some(left_reg) = range_left {
                            let result = self.register_counter.next().unwrap().as_local();
                            let instruction = Instruction::Gte(expr_result, left_reg, result);
                            push_instruction!(self, instruction, range_node.span);
                            result
                        } else {
                            self.constant(LpcConstant::Int(1), range_node.span)?
                        };

                        let lte_result = if let Some(right_reg) = range_right {
                            let result = self.register_counter.next().unwrap().as_local();
                            let instruction = Instruction::Lte(expr_result, right_reg, result);
                            push_instruction!(self, instruction, range_node.span);
                            result
                        } else {
                            self.constant(LpcConstant::Int(1), range_node.span)?
                        };

                        // & the results to see if we're in the range
                        let instruction = Instruction::And(gte_result, lte_result, test_result);
                        push_instruction!(self, instruction, node.span);
                    } else {
                        let instruction =
                            Instruction::EqEq(expr_result, self.current_result, test_result);
                        push_instruction!(self, instruction, node.span);
                    }

                    let case_label = self.new_label("switch-case");
                    self.schedule_backpatch(&case_label, self.current_address())?;
                    let instruction = Instruction::Jnz(test_result, Address(0));
                    push_instruction!(self, instruction, node.span);
                    self.insert_label(case_label, case_address.1);
                }
                None => {
                    let default_label = self.new_label("switch-default");
                    self.schedule_backpatch(&default_label, self.current_address())?;
                    let instruction = Instruction::Jmp(Address(0));
                    push_instruction!(self, instruction, node.span);
                    self.insert_label(default_label, case_address.1);
                }
            }
        }

        let end_address = self.current_address();
        self.insert_label(end_label, end_address);

        self.jump_targets.pop();

        Ok(())
    }

    #[instrument(skip_all)]
    async fn visit_ternary(&mut self, node: &mut TernaryNode) -> Result<()> {
        let result_reg = self.register_counter.next().unwrap().as_local();
        let else_label = self.new_label("ternary-else");
        let end_label = self.new_label("ternary-end");

        self.emit_condition(&mut node.condition, JumpWhen::False, &else_label)
            .await?;

        node.body.visit(self).await?;
        push_instruction!(
            self,
            Instruction::Copy(self.current_result, result_reg),
            node.span
        );

        self.schedule_backpatch(&end_label, self.current_address())?;
        let instruction = Instruction::Jmp(Address(0));
        push_instruction!(self, instruction, node.span);

        let else_addr = self.current_address();
        self.insert_label(else_label, else_addr);

        node.else_clause.visit(self).await?;
        push_instruction!(
            self,
            Instruction::Copy(self.current_result, result_reg),
            node.span
        );

        let end_addr = self.current_address();
        self.insert_label(end_label, end_addr);

        self.current_result = result_reg;
        Ok(())
    }

    #[instrument(skip_all)]
    async fn visit_unary_op(&mut self, node: &mut UnaryOpNode) -> Result<()> {
        node.expr.visit(self).await?;
        let location = self.current_result;

        self.current_result = match node.op {
            UnaryOperation::Negate => {
                let minus_one = self.constant(LpcConstant::Int(-1), node.span)?;
                let reg_result = self.register_counter.next().unwrap().as_local();

                let instruction = Instruction::MMul(location, minus_one, reg_result);
                push_instruction!(self, instruction, node.span);

                reg_result
            }
            UnaryOperation::Inc | UnaryOperation::Dec => {
                let instruction = if node.op == UnaryOperation::Inc {
                    Instruction::Inc(location)
                } else {
                    Instruction::Dec(location)
                };

                if node.is_post {
                    // TODO: only copy if pre-operation value is needed elsewhere
                    let temp = self.register_counter.next().unwrap().as_local();
                    let copy = Instruction::Copy(location, temp);
                    push_instruction!(self, copy, node.span);
                    push_instruction!(self, instruction, node.span);
                    temp
                } else {
                    push_instruction!(self, instruction, node.span);
                    location
                }
            }
            UnaryOperation::Bang => {
                let reg_result = self.register_counter.next().unwrap().as_local();

                let instruction = Instruction::Not(location, reg_result);
                push_instruction!(self, instruction, node.span);

                reg_result
            }
            UnaryOperation::BitwiseNot => {
                let reg_result = self.register_counter.next().unwrap().as_local();

                let instruction = Instruction::BitwiseNot(location, reg_result);
                push_instruction!(self, instruction, node.span);

                reg_result
            }
        };

        Ok(())
    }

    #[instrument(skip_all)]
    async fn visit_var(&mut self, node: &mut VarNode) -> Result<()> {
        if node.is_closure_arg_var() {
            let idx = closure_arg_number(node.name)?;
            let Some(loc) = self
                .closure_arg_locations
                .last()
                .and_then(|locs| locs.get((idx - 1) as usize))
                .copied()
            else {
                return Err(lpc_bug!(
                    node.span,
                    "positional `{}` is not a parameter of its closure",
                    node.name
                ));
            };
            self.current_result = loc;

            return Ok(());
        }

        if node.function_name {
            let mut fptr_node = FunctionPtrNode {
                receiver: None,
                arguments: None,
                name: node.name,
                span: node.span,
            };

            return self.visit_function_ptr(&mut fptr_node).await;
        }

        if self.context.lookup_var(node.name).is_none() {
            return Err(lpc_error!(
                node.span,
                "Unable to find symbol `{}`",
                node.name
            ));
        }

        self.current_result = self.location_of(&node.name)?;

        Ok(())
    }

    #[instrument(skip_all)]
    async fn visit_var_init(&mut self, node: &mut VarInitNode) -> Result<()> {
        let symbol = self.context.lookup_var(node.name);

        let Some(sym) = symbol else {
            return Err(lpc_error!(
                node.span,
                "Missing symbol, that somehow passed semantic checks?: {}",
                node.name
            ));
        };

        let global = sym.is_global();
        let upvalue = sym.upvalue;

        // Before the initializer runs, so a closure inside it captures the new cell.
        if upvalue {
            let Some(cell) = sym.location else {
                return Err(lpc_bug!(
                    node.span,
                    "captured `{}` was never given a cell",
                    node.name
                ));
            };
            push_instruction!(self, Instruction::NewUpvalue(cell), node.span());
        }

        let current_register = if let Some(expression) = &mut node.value {
            expression.visit(self).await?;
            // A read of the variable inside the initializer already gave it a location.
            let assigned = self
                .context
                .lookup_var(node.name)
                .and_then(|sym| sym.location);

            // TODO: This whole thing sucks. We'd rather have the `expression.visit()` call
            //       above put the result into the correct location directly.
            if let Some(next_register) = assigned {
                trace!("Copying into {:?}", next_register);
                push_instruction!(
                    self,
                    Instruction::Copy(self.current_result, next_register),
                    node.span()
                );
                next_register
            } else if global {
                let next_register = self.global_counter.next().unwrap().as_global();

                trace!("Copying global to {:?}", next_register);
                push_instruction!(
                    self,
                    Instruction::Copy(self.current_result, next_register),
                    node.span()
                );

                next_register
            } else if upvalue {
                return Err(lpc_bug!(
                    node.span,
                    "captured `{}` was never given a cell",
                    node.name
                ));
            } else if matches!(expression, ExpressionNode::Var(_))
                || matches!(self.current_result, RegisterVariant::Constant(_))
            {
                // Copy to a new register so the new var isn't literally
                // sharing a register with the old one, or living in the pool.
                let next_register = self.register_counter.next().unwrap().as_local();
                trace!("Copying var to {:?}", next_register);
                push_instruction!(
                    self,
                    Instruction::Copy(self.current_result, next_register),
                    node.span()
                );
                next_register
            } else {
                trace!("Not copying the result");
                self.current_result
            }
        } else {
            trace!("No value, defaulting to NULL");
            self.assign_sym_location(&node.name)?
        };

        self.current_result = current_register;

        if let Some(sym) = self.context.lookup_var_mut(node.name) {
            sym.location = Some(current_register);

            if let Some(func) = self.function_stack.last_mut() {
                func.local_variables.push(sym.clone())
            }
        }

        Ok(())
    }

    #[instrument(skip_all)]
    async fn visit_while(&mut self, node: &mut WhileNode) -> Result<()> {
        let start_label = self.new_label("while-start");
        let end_label = self.new_label("while-end");
        self.jump_targets
            .push(JumpTarget::new(end_label.clone(), start_label.clone()));
        let start_addr = self.current_address();
        self.insert_label(start_label.clone(), start_addr);

        self.emit_condition(&mut node.condition, JumpWhen::False, &end_label)
            .await?;

        node.body.visit(self).await?;

        // go back to the start of the loop
        self.schedule_backpatch(&start_label, self.current_address())?;
        let instruction = Instruction::Jmp(Address(0));
        push_instruction!(self, instruction, node.span);

        let addr = self.current_address();
        self.insert_label(end_label, addr);

        self.jump_targets.pop();
        Ok(())
    }
}

impl Default for CodegenWalker {
    fn default() -> Self {
        // The local counter starts at 1, as r0 is reserved for return values.
        let register_counter = RegisterCounter::new(1);
        let global_counter = RegisterCounter::new(0);

        Self {
            function_stack: vec![],
            backpatch_maps: vec![],
            constant_keys: vec![],
            label_count: 0,
            functions: Default::default(),
            initializer: None,
            init_globals: Ustr::default(),
            current_result: RegisterVariant::Local(Register(0)),
            register_counter,
            global_counter,
            context: Default::default(),
            jump_targets: vec![],
            case_addresses: vec![],
            visit_range_results: None,
            closure_arg_locations: vec![],
        }
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use claims::assert_some;
    use factori::create;
    use lpc_rs_asm::instruction::Instruction::*;
    use lpc_rs_core::{lpc_path::LpcPath, lpc_type::LpcType};
    use lpc_rs_errors::{Result, span::Span};

    use super::*;
    use crate::test_support::CompileThrough;
    use crate::{
        compiler::{
            ast::{
                ast_node::AstNode, block_node::BlockNode,
                comma_expression_node::CommaExpressionNode, expression_node::ExpressionNode,
            },
            codegen::{
                codegen_walker::CodegenWalker, function_prototype_walker::FunctionPrototypeWalker,
                scope_walker::ScopeWalker,
            },
            lexer::LexWrapper,
        },
        interpreter::{process::Process, program::Program},
        lpc_parser,
        test_support::factories::*,
    };

    const LIB_DIR: &str = "./tests/fixtures/code";

    fn default_walker() -> CodegenWalker {
        let mut walker = CodegenWalker::default();
        walker.setup_init();

        // Both functions below are planted with the default kind: the
        // instruction follows from where the name is found, not from the kind.
        let path = LpcPath::new_in_game("/secure/simul_efuns", "/", LIB_DIR);
        let mut prog = Program::new(path);
        prog.functions.insert(
            "simul_efun".into(),
            ProgramFunction::new(
                FunctionPrototypeBuilder::default()
                    .name("simul_efun")
                    .filename(Arc::new("/secure/simul_efuns".into()))
                    .return_type(LpcType::Void)
                    .build()
                    .unwrap(),
                0,
            )
            .into(),
        );
        let process = Process::new(prog);
        walker.context.simul_efuns = Some(process.into());

        walker.context.function_prototypes.insert(
            "local_function".into(),
            FunctionPrototypeBuilder::default()
                .name("local_function")
                .filename(Arc::new("/test/local.c".into()))
                .return_type(LpcType::Void)
                .build()
                .unwrap(),
        );

        walker
    }

    async fn walk_prog(prog: &str) -> CodegenWalker {
        walk_code(prog).await.expect("failed to walk.")
    }

    async fn walk_code(code: &str) -> Result<CodegenWalker> {
        CodegenWalker::compile_through(code).await
    }

    fn walker_function_instructions<T>(walker: &mut CodegenWalker, name: T) -> Vec<Instruction>
    where
        T: AsRef<str>,
    {
        let function = walker
            .functions
            .values()
            .find(|f| f.name() == name.as_ref())
            .unwrap();
        function.instructions.clone()
    }

    fn walker_init_instructions(walker: &mut CodegenWalker) -> Vec<Instruction> {
        walker.function_stack.last().unwrap().instructions.clone()
    }

    /// The instructions of the program's own global initializer.
    async fn generate_init_instructions(prog: &str) -> Vec<Instruction> {
        let walker = walk_prog(prog).await;
        walker
            .functions
            .values()
            .find(|f| f.name() == INIT_GLOBALS)
            .unwrap()
            .instructions
            .clone()
    }

    fn find_function<'a, K, S>(
        map: &'a IndexMap<K, Arc<ProgramFunction>, S>,
        name: &str,
    ) -> Option<&'a Arc<ProgramFunction>> {
        map.values().find(|f| f.name() == name)
    }

    #[tokio::test]
    async fn test_visit_array_populates_the_instructions() {
        let mut walker = default_walker();

        let mut arr = ArrayNode::new(vec![
            ExpressionNode::from(123),
            ExpressionNode::from("foo"),
            ExpressionNode::from(vec![ExpressionNode::from(666)]),
        ]);

        let _ = walker.visit_array(&mut arr).await;

        let expected = vec![
            PushArrayItem(RegisterVariant::Constant(Register(2))),
            AConst(RegisterVariant::Local(Register(1))),
            PushArrayItem(RegisterVariant::Constant(Register(0))),
            PushArrayItem(RegisterVariant::Constant(Register(1))),
            PushArrayItem(RegisterVariant::Local(Register(1))),
            AConst(RegisterVariant::Local(Register(2))),
        ];

        assert_eq!(walker_init_instructions(&mut walker), expected);
    }

    mod test_visit_assignment {
        use super::*;

        #[tokio::test]
        async fn test_populates_the_instructions_for_globals() {
            let mut context = CompilationContext::default();
            context.scopes.push_new();
            let mut walker = CodegenWalker::new(context);

            let sym = Symbol {
                name: "marf".to_string(),
                type_: LpcType::Int(false),
                location: Some(RegisterVariant::Global(Register(666))),
                ..Default::default()
            };
            insert_symbol(&mut walker, sym);

            let mut node = AssignmentNode {
                lhs: Box::new(ExpressionNode::Var(VarNode {
                    name: ustr("marf"),
                    span: None,
                    global: true,
                    function_name: false,
                })),
                rhs: Box::new(ExpressionNode::Int(IntNode::new(-12))),
                span: None,
            };

            let _ = walker.visit_assignment(&mut node).await;
            assert_eq!(
                walker_init_instructions(&mut walker),
                [Copy(
                    RegisterVariant::Constant(Register(0)),
                    RegisterVariant::Global(Register(666))
                )]
            );
        }

        #[tokio::test]
        async fn test_populates_the_instructions_for_locals() {
            let mut context = CompilationContext::default();
            context.scopes.push_new();
            let local_id = context.scopes.push_new();
            let mut walker = CodegenWalker::new(context);

            let sym = Symbol {
                name: "marf".to_string(),
                type_: LpcType::Int(false),
                location: Some(RegisterVariant::Local(Register(666))),
                scope_id: Some(local_id),
                ..Default::default()
            };

            insert_symbol(&mut walker, sym);

            let mut node = AssignmentNode {
                lhs: Box::new(ExpressionNode::Var(VarNode::new("marf"))),
                rhs: Box::new(ExpressionNode::Int(IntNode::new(-12))),
                span: None,
            };

            let _ = walker.visit_assignment(&mut node).await;
            assert_eq!(
                walker_init_instructions(&mut walker),
                [Copy(
                    RegisterVariant::Constant(Register(0)),
                    RegisterVariant::Local(Register(666))
                )]
            );
        }

        #[tokio::test]
        async fn test_populates_the_instructions_for_array_items() {
            let mut context = CompilationContext::default();
            context.scopes.push_new();
            let local_id = context.scopes.push_new();
            let mut walker = CodegenWalker::new(context);

            let sym = Symbol {
                name: "marf".to_string(),
                type_: LpcType::Int(true),
                location: Some(RegisterVariant::Local(Register(666))),
                scope_id: Some(local_id),
                ..Default::default()
            };

            insert_symbol(&mut walker, sym);

            let mut node = AssignmentNode {
                lhs: Box::new(ExpressionNode::BinaryOp(BinaryOpNode {
                    l: Box::new(ExpressionNode::from(VarNode::new("marf"))),
                    r: Box::new(ExpressionNode::from(1)),
                    op: BinaryOperation::Index,
                    span: None,
                })),
                rhs: Box::new(ExpressionNode::from(-12)),
                span: None,
            };

            let _ = walker.visit_assignment(&mut node).await;
            assert_eq!(
                walker_init_instructions(&mut walker),
                [Store(
                    RegisterVariant::Constant(Register(0)),
                    RegisterVariant::Local(Register(666)),
                    RegisterVariant::Constant(Register(1))
                )]
            );
        }
    }

    mod test_binary_op {
        use lpc_rs_asm::instruction::Instruction::{IMul, Jnz, Load, MAdd, Range};

        use super::*;

        #[tokio::test]
        async fn populates_the_instructions_for_ints() {
            let mut walker = default_walker();

            let mut node = BinaryOpNode {
                l: Box::new(ExpressionNode::Int(IntNode::new(666))),
                r: Box::new(ExpressionNode::BinaryOp(BinaryOpNode {
                    l: Box::new(ExpressionNode::Int(IntNode::new(123))),
                    r: Box::new(ExpressionNode::Int(IntNode::new(456))),
                    op: BinaryOperation::Add,
                    span: None,
                })),
                op: BinaryOperation::Mul,
                span: None,
            };

            let _ = walker.visit_binary_op(&mut node).await;

            let expected = vec![
                IAdd(
                    RegisterVariant::Constant(Register(1)),
                    RegisterVariant::Constant(Register(2)),
                    RegisterVariant::Local(Register(1)),
                ),
                IMul(
                    RegisterVariant::Constant(Register(0)),
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Local(Register(2)),
                ),
            ];

            assert_eq!(walker_init_instructions(&mut walker), expected);
        }

        #[tokio::test]
        async fn populates_the_instructions_for_floats() {
            let mut context = CompilationContext::default();
            context.scopes.push_new();
            let mut sym = Symbol::new("foo", LpcType::Float(false));
            sym.location = Some(RegisterVariant::Local(Register(9)));
            context.scopes.current_mut().unwrap().insert(sym);

            let mut walker = CodegenWalker::new(context);

            let mut node = BinaryOpNode {
                l: Box::new(ExpressionNode::Float(FloatNode::new(123.45))),
                r: Box::new(ExpressionNode::BinaryOp(BinaryOpNode {
                    l: Box::new(ExpressionNode::Var(VarNode {
                        name: ustr("foo"),
                        span: None,
                        global: false,
                        function_name: false,
                    })),
                    r: Box::new(ExpressionNode::Int(IntNode::new(456))),
                    op: BinaryOperation::Mul,
                    span: None,
                })),
                op: BinaryOperation::Add,
                span: None,
            };

            let _ = walker.visit_binary_op(&mut node).await;

            let expected = vec![
                IMul(
                    RegisterVariant::Local(Register(9)),
                    RegisterVariant::Constant(Register(1)),
                    RegisterVariant::Local(Register(1)),
                ),
                IAdd(
                    RegisterVariant::Constant(Register(0)),
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Local(Register(2)),
                ),
            ];

            assert_eq!(walker_init_instructions(&mut walker), expected);
        }

        #[tokio::test]
        async fn populates_the_instructions_for_strings() {
            let mut walker = default_walker();

            let mut node = BinaryOpNode {
                l: Box::new(ExpressionNode::String(StringNode::new("foo"))),
                r: Box::new(ExpressionNode::BinaryOp(BinaryOpNode {
                    l: Box::new(ExpressionNode::String(StringNode::new("bar"))),
                    r: Box::new(ExpressionNode::String(StringNode::new("baz"))),
                    op: BinaryOperation::Add,
                    span: None,
                })),
                op: BinaryOperation::Add,
                span: None,
            };

            let _ = walker.visit_binary_op(&mut node).await;

            let expected = vec![
                MAdd(
                    RegisterVariant::Constant(Register(1)),
                    RegisterVariant::Constant(Register(2)),
                    RegisterVariant::Local(Register(1)),
                ),
                MAdd(
                    RegisterVariant::Constant(Register(0)),
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Local(Register(2)),
                ),
            ];

            assert_eq!(walker_init_instructions(&mut walker), expected);
        }

        #[tokio::test]
        async fn populates_the_instructions_for_arrays() {
            let mut walker = default_walker();

            let mut node = BinaryOpNode {
                l: Box::new(ExpressionNode::from(vec![ExpressionNode::from(123)])),
                r: Box::new(ExpressionNode::from(vec![ExpressionNode::from(456)])),
                op: BinaryOperation::Add,
                span: None,
            };

            let _ = walker.visit_binary_op(&mut node).await;

            let expected = vec![
                PushArrayItem(RegisterVariant::Constant(Register(0))),
                AConst(RegisterVariant::Local(Register(1))),
                PushArrayItem(RegisterVariant::Constant(Register(1))),
                AConst(RegisterVariant::Local(Register(2))),
                MAdd(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Local(Register(2)),
                    RegisterVariant::Local(Register(3)),
                ),
            ];

            assert_eq!(walker_init_instructions(&mut walker), expected);
        }

        #[tokio::test]
        async fn populates_the_instructions_for_indexes() {
            let context = CompilationContext::default();
            let mut walker = CodegenWalker::new(context);

            let mut node = BinaryOpNode {
                l: Box::new(ExpressionNode::from(vec![ExpressionNode::from(123)])),
                r: Box::new(ExpressionNode::from(0)),
                op: BinaryOperation::Index,
                span: None,
            };

            let _ = walker.visit_binary_op(&mut node).await;

            let expected = vec![
                PushArrayItem(RegisterVariant::Constant(Register(0))),
                AConst(RegisterVariant::Local(Register(1))),
                Load(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Constant(Register(1)),
                    RegisterVariant::Local(Register(2)),
                ),
            ];

            assert_eq!(walker_init_instructions(&mut walker), expected);
        }

        #[tokio::test]
        async fn populates_the_instructions_for_slices() {
            let mut walker = default_walker();

            let mut node = BinaryOpNode {
                l: Box::new(ExpressionNode::from(vec![ExpressionNode::from(123)])),
                r: Box::new(ExpressionNode::Range(RangeNode {
                    l: Box::new(Some(ExpressionNode::from(1))),
                    r: Box::new(None),
                    span: None,
                })),
                op: BinaryOperation::Index,
                span: None,
            };

            let _ = walker.visit_binary_op(&mut node).await;

            let expected = vec![
                PushArrayItem(RegisterVariant::Constant(Register(0))),
                AConst(RegisterVariant::Local(Register(1))),
                Range(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Constant(Register(1)),
                    RegisterVariant::Constant(Register(2)),
                    RegisterVariant::Local(Register(2)),
                ),
            ];

            assert_eq!(walker_init_instructions(&mut walker), expected);
        }

        #[tokio::test]
        async fn populates_the_instructions_for_andand_expressions() {
            let mut walker = default_walker();
            walker.backpatch_maps.push(HashMap::new());

            let mut node = BinaryOpNode {
                l: Box::new(ExpressionNode::from(123)),
                r: Box::new(ExpressionNode::from("marf!")),
                op: BinaryOperation::AndAnd,
                span: None,
            };

            let _ = walker.visit_binary_op(&mut node).await;

            // A literal-true left operand needs no jump.
            let expected = vec![
                Copy(
                    RegisterVariant::Constant(Register(0)),
                    RegisterVariant::Local(Register(1)),
                ),
                Copy(
                    RegisterVariant::Constant(Register(1)),
                    RegisterVariant::Local(Register(1)),
                ),
            ];

            assert_eq!(walker_init_instructions(&mut walker), expected);
        }

        #[tokio::test]
        async fn populates_the_instructions_for_oror_expressions() {
            let mut walker = default_walker();
            walker.backpatch_maps.push(HashMap::new());

            let mut node = BinaryOpNode {
                l: Box::new(ExpressionNode::from(123)),
                r: Box::new(ExpressionNode::from("sup?")),
                op: BinaryOperation::OrOr,
                span: None,
            };

            let _ = walker.visit_binary_op(&mut node).await;

            let expected = vec![
                Copy(
                    RegisterVariant::Constant(Register(0)),
                    RegisterVariant::Local(Register(1)),
                ),
                Jnz(RegisterVariant::Local(Register(1)), Address(0)),
                Copy(
                    RegisterVariant::Constant(Register(1)),
                    RegisterVariant::Local(Register(1)),
                ),
            ];

            assert_eq!(walker_init_instructions(&mut walker), expected);
        }

        #[tokio::test]
        async fn populates_the_instructions_for_function_composition() {
            let mut walker = default_walker();
            walker.backpatch_maps.push(HashMap::new());

            let mut node = BinaryOpNode {
                l: Box::new(ExpressionNode::FunctionPtr(FunctionPtrNode {
                    receiver: None,
                    arguments: None,
                    name: ustr("dump"),
                    span: None,
                })),
                r: Box::new(ExpressionNode::FunctionPtr(FunctionPtrNode {
                    receiver: None,
                    arguments: None,
                    name: ustr("this_object"),
                    span: None,
                })),
                op: BinaryOperation::Compose,
                span: None,
            };

            let _ = walker.visit_binary_op(&mut node).await;

            let expected = vec![
                FunctionPtrConst {
                    location: RegisterVariant::Local(Register(1)),
                    name: ustr("dump"),
                    receiver: FunctionReceiver::Efun,
                },
                FunctionPtrConst {
                    location: RegisterVariant::Local(Register(2)),
                    name: ustr("this_object"),
                    receiver: FunctionReceiver::Efun,
                },
                PushArg(RegisterVariant::Local(Register(1))),
                PushArg(RegisterVariant::Local(Register(2))),
                CallEfun(10),
                Copy(
                    RegisterVariant::Local(Register(0)),
                    RegisterVariant::Local(Register(3)),
                ),
            ];

            assert_eq!(walker_init_instructions(&mut walker), expected);
        }
    }

    mod test_break {
        use lpc_rs_core::register::Register;

        use super::*;

        #[tokio::test]
        async fn breaks_out_of_while_loops() {
            let code = r#"
                void create() {
                    int i;
                    while (i < 10) {
                        dump(i);
                        if (i > 5) {
                            dump("breaking");
                            break;
                        }
                        i += 1;
                    }
                }
            "#;

            let mut walker = walk_prog(code).await;
            let expected = vec![
                Lt(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Constant(Register(0)),
                    RegisterVariant::Local(Register(2)),
                ),
                Jz(RegisterVariant::Local(Register(2)), Address(11)),
                PushArg(RegisterVariant::Local(Register(1))),
                CallEfun(15),
                Gt(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Constant(Register(1)),
                    RegisterVariant::Local(Register(3)),
                ),
                Jz(RegisterVariant::Local(Register(3)), Address(9)),
                PushArg(RegisterVariant::Constant(Register(2))),
                CallEfun(15),
                Jmp(Address(11)),
                IAdd(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Constant(Register(3)),
                    RegisterVariant::Local(Register(1)),
                ),
                Jmp(Address(0)),
                Ret,
            ];

            assert_eq!(
                walker_function_instructions(&mut walker, "create"),
                expected
            );
        }

        #[tokio::test]
        async fn breaks_out_of_for_loops() {
            let code = r#"
                void create() {
                    for (int i = 0; i < 10; i += 1) {
                        dump(i);
                        if (i > 5) {
                            dump("breaking");
                            break;
                        }
                        i += 1;
                    }
                }
            "#;

            let mut walker = walk_prog(code).await;
            let expected = vec![
                Copy(
                    RegisterVariant::Constant(Register(0)),
                    RegisterVariant::Local(Register(1)),
                ),
                Lt(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Constant(Register(1)),
                    RegisterVariant::Local(Register(2)),
                ),
                Jz(RegisterVariant::Local(Register(2)), Address(13)),
                PushArg(RegisterVariant::Local(Register(1))),
                CallEfun(15),
                Gt(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Constant(Register(2)),
                    RegisterVariant::Local(Register(3)),
                ),
                Jz(RegisterVariant::Local(Register(3)), Address(10)),
                PushArg(RegisterVariant::Constant(Register(3))),
                CallEfun(15),
                Jmp(Address(13)),
                IAdd(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Constant(Register(4)),
                    RegisterVariant::Local(Register(1)),
                ),
                IAdd(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Constant(Register(4)),
                    RegisterVariant::Local(Register(1)),
                ),
                Jmp(Address(1)),
                Ret,
            ];

            assert_eq!(
                walker_function_instructions(&mut walker, "create"),
                expected
            );
        }

        #[tokio::test]
        async fn breaks_out_of_do_while_loops() {
            let code = r#"
                void create() {
                    int i;
                    do {
                        dump(i);
                        if (i > 5) {
                            dump("breaking");
                            break;
                        }
                        i += 1;
                    } while (i < 10);
                }
            "#;

            let mut walker = walk_prog(code).await;
            let expected = vec![
                PushArg(RegisterVariant::Local(Register(1))),
                CallEfun(15),
                Gt(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Constant(Register(0)),
                    RegisterVariant::Local(Register(2)),
                ),
                Jz(RegisterVariant::Local(Register(2)), Address(7)),
                PushArg(RegisterVariant::Constant(Register(1))),
                CallEfun(15),
                Jmp(Address(10)),
                IAdd(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Constant(Register(2)),
                    RegisterVariant::Local(Register(1)),
                ),
                Lt(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Constant(Register(3)),
                    RegisterVariant::Local(Register(4)),
                ),
                Jnz(RegisterVariant::Local(Register(4)), Address(0)),
                Ret,
            ];

            assert_eq!(
                walker_function_instructions(&mut walker, "create"),
                expected
            );
        }

        #[tokio::test]
        async fn breaks_out_of_switch_statements() {
            let code = r#"
                void create() {
                    int i = 666;
                    switch (i) {
                    case 666:
                        dump("YEAH BABY");
                        break;
                    default:
                        dump("very");
                    case 10..200:
                        dump("weak");
                        break;
                    }
                }
            "#;

            let mut walker = walk_prog(code).await;
            let expected = vec![
                Copy(
                    RegisterVariant::Constant(Register(0)),
                    RegisterVariant::Local(Register(1)),
                ),
                Jmp(Address(10)),
                PushArg(RegisterVariant::Constant(Register(1))),
                CallEfun(15),
                Jmp(Address(17)),
                PushArg(RegisterVariant::Constant(Register(2))),
                CallEfun(15),
                PushArg(RegisterVariant::Constant(Register(3))),
                CallEfun(15),
                Jmp(Address(17)),
                EqEq(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Constant(Register(0)),
                    RegisterVariant::Local(Register(2)),
                ),
                Jnz(RegisterVariant::Local(Register(2)), Address(2)),
                Gte(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Constant(Register(4)),
                    RegisterVariant::Local(Register(4)),
                ),
                Lte(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Constant(Register(5)),
                    RegisterVariant::Local(Register(5)),
                ),
                And(
                    RegisterVariant::Local(Register(4)),
                    RegisterVariant::Local(Register(5)),
                    RegisterVariant::Local(Register(3)),
                ),
                Jnz(RegisterVariant::Local(Register(3)), Address(7)),
                Jmp(Address(5)),
                Ret,
            ];

            assert_eq!(
                walker_function_instructions(&mut walker, "create"),
                expected
            );
        }
    }

    mod test_visit_call {
        use lpc_rs_asm::instruction::Instruction::{Call, CallOther, CatchEnd, CatchStart, IDiv};
        use lpc_rs_core::{function_arity::FunctionArity, function_flags::FunctionFlags};

        use super::*;
        use crate::test_support::compile_prog;

        fn get_call_node(code: &str, context: &mut CompilationContext) -> CallNode {
            let mut prog_node = lpc_parser::ProgramParser::new()
                .parse(context, LexWrapper::new(code, 0).triples())
                .unwrap();
            if_chain! {
                if let Some(AstNode::Decl(mut node)) = prog_node.body.pop();
                if let Some(VarInitNode { value, .. }) = node.initializations.pop();
                if let Some(ExpressionNode::Call(node)) = value;
                then {
                    node
                } else {
                    panic!("expected call node");
                }
            }
        }

        #[tokio::test]
        async fn populates_the_instructions_for_local_calls() {
            let mut walker = default_walker();
            let call = "mixed m = local_function(4 - 5);";
            let mut node = get_call_node(call, &mut walker.context);

            walker.visit_call(&mut node).await.unwrap();

            let expected = vec![
                PushArg(RegisterVariant::Constant(Register(0))),
                Call(ustr("local_function__v__/test/local.c__pb__")),
            ];

            assert_eq!(walker_init_instructions(&mut walker), expected);
        }

        #[tokio::test]
        async fn populates_the_instructions_for_efuns() {
            let mut walker = default_walker();
            let call = "mixed m = dump(4 - 5);";
            let mut node = get_call_node(call, &mut walker.context);

            let _ = walker.visit_call(&mut node).await;

            let expected = vec![
                PushArg(RegisterVariant::Constant(Register(0))),
                CallEfun(15),
            ];

            assert_eq!(walker_init_instructions(&mut walker), expected);
        }

        #[tokio::test]
        async fn populates_the_instructions_for_simul_efuns() {
            let mut walker = default_walker();
            let call = "mixed m = simul_efun(4 - 5);";
            let mut node = get_call_node(call, &mut walker.context);

            let _ = walker.visit_call(&mut node).await;

            let expected = vec![
                PushArg(RegisterVariant::Constant(Register(0))),
                CallSimulEfun(ustr("simul_efun")),
            ];

            assert_eq!(walker_init_instructions(&mut walker), expected);
        }

        #[tokio::test]
        async fn populates_the_instructions_for_call_other() {
            let check = |code: &'static str, expected: Vec<Instruction>| async move {
                let wrapped = format!("void create() {{ {}; }}", code);
                let (prog, _, _) = compile_prog(&wrapped).await;

                // `closure-1` is the outer closure that refers to $1.
                let instructions = &find_function(&prog.functions, "create")
                    .unwrap()
                    .instructions;

                assert_eq!(instructions, &expected);
            };

            let expected = vec![
                PushArg(RegisterVariant::Constant(Register(1))),
                CallOther(
                    RegisterVariant::Constant(Register(0)),
                    RegisterVariant::Constant(Register(2)),
                ),
                Ret,
            ];
            check(r#""foo"->print(4 - 5)"#, expected).await;

            let expected = vec![
                PushArg(RegisterVariant::Constant(Register(0))),
                PushArg(RegisterVariant::Constant(Register(1))),
                PushArg(RegisterVariant::Constant(Register(2))),
                CallOther(
                    RegisterVariant::Constant(Register(0)),
                    RegisterVariant::Constant(Register(1)),
                ),
                Ret,
            ];
            check(r#"call_other("foo", "print", 4 - 5)"#, expected).await;
        }

        #[tokio::test]
        async fn populates_the_instructions_for_sizeof() {
            let check = |code: &'static str, expected: Vec<Instruction>| async move {
                let wrapped = format!("void create() {{ {}; }}", code);
                let (prog, _, _) = compile_prog(&wrapped).await;

                let instructions = &find_function(&prog.functions, "create")
                    .unwrap()
                    .instructions;

                assert_eq!(instructions, &expected);
            };

            let expected = vec![
                PushArrayItem(RegisterVariant::Constant(Register(0))),
                PushArrayItem(RegisterVariant::Constant(Register(1))),
                PushArrayItem(RegisterVariant::Constant(Register(2))),
                AConst(RegisterVariant::Local(Register(1))),
                Sizeof(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Local(Register(2)),
                ),
                Ret,
            ];
            check(r#"sizeof(({ 1, 2, "c" }))"#, expected).await;
        }

        #[tokio::test]
        async fn populates_the_instructions_for_catch() {
            let call = "void create() { catch(12 / 0); }";
            let (prog, _, _) = compile_prog(call).await;
            let instructions = &find_function(&prog.functions, "create")
                .unwrap()
                .instructions;

            let expected = vec![
                CatchStart(RegisterVariant::Local(Register(1)), Address(2)),
                IDiv(
                    RegisterVariant::Constant(Register(0)),
                    RegisterVariant::Constant(Register(1)),
                    RegisterVariant::Local(Register(2)),
                ),
                CatchEnd,
                Ret,
            ];

            assert_eq!(instructions, &expected);
        }

        #[tokio::test]
        async fn populates_the_instructions_for_function_pointers() {
            let mut context = CompilationContext::default();
            let prototype = FunctionPrototypeBuilder::default()
                .name("marfin")
                .filename(Arc::new("marfin".into()))
                .return_type(LpcType::Int(false))
                .arity(FunctionArity::new(1))
                .build()
                .unwrap();

            context
                .function_prototypes
                .insert("marfin".into(), prototype);

            context.scopes.push_new(); // push a global scope
            context.scopes.push_new(); // push a local scope
            let mut sym = Symbol::new("my_func", LpcType::Function(false));
            sym.location = Some(RegisterVariant::Local(Register(1)));
            context.scopes.current_mut().unwrap().insert(sym);

            let call = "void create() { function my_func = (: $1 :); my_func(666); }";
            let (prog, _, _) = compile_prog(call).await;
            let instructions = &find_function(&prog.functions, "create")
                .unwrap()
                .instructions;

            let expected = vec![
                FunctionPtrConst {
                    location: RegisterVariant::Local(Register(1)),
                    receiver: FunctionReceiver::Local,
                    name: ustr("closure-0__x__/my_file.c__pv__x"),
                },
                PushArg(RegisterVariant::Constant(Register(0))),
                CallFp(RegisterVariant::Local(Register(1))),
                Ret,
            ];

            assert_eq!(instructions, &expected);
        }

        #[tokio::test]
        async fn populates_the_instructions_for_global_function_pointers() {
            let mut context = CompilationContext::default();
            let prototype = FunctionPrototypeBuilder::default()
                .name("marfin")
                .filename(Arc::new("marfin".into()))
                .return_type(LpcType::Int(false))
                .arity(FunctionArity::new(1))
                .build()
                .unwrap();

            context
                .function_prototypes
                .insert("marfin".into(), prototype);

            context.scopes.push_new(); // push a global scope
            let mut sym = Symbol::new("my_func", LpcType::Function(false));
            sym.location = Some(RegisterVariant::Global(Register(0)));
            context.scopes.current_mut().unwrap().insert(sym);

            let call = "function my_func = (: $1 :); void create() { my_func(666); }";
            let (prog, _, _) = compile_prog(call).await;
            let instructions = &find_function(&prog.functions, "create")
                .unwrap()
                .instructions;

            let expected = vec![
                PushArg(RegisterVariant::Constant(Register(0))),
                CallFp(RegisterVariant::Global(Register(0))),
                Ret,
            ];

            assert_eq!(instructions, &expected);
        }

        #[tokio::test]
        async fn copies_non_void_call_results() {
            let mut context = CompilationContext::default();
            let prototype = FunctionPrototypeBuilder::default()
                .name("marfin")
                .filename(Arc::new("marfin.c".into()))
                .return_type(LpcType::Int(false))
                .arity(FunctionArity::new(1))
                .span(Some(Span::new(0, 0..0)))
                .build()
                .unwrap();

            context
                .function_prototypes
                .insert("marfin".into(), prototype);
            let mut walker = CodegenWalker::new(context);
            let call = "mixed m = marfin(666);";
            let mut node = get_call_node(call, &mut walker.context);

            let _ = walker.visit_call(&mut node).await;

            let expected = vec![
                PushArg(RegisterVariant::Constant(Register(0))),
                Call(ustr("marfin__i__marfin.c__pb__")),
                Copy(
                    RegisterVariant::Local(Register(0)),
                    RegisterVariant::Local(Register(1)),
                ),
            ];

            assert_eq!(walker_init_instructions(&mut walker), expected);
        }

        #[tokio::test]
        async fn does_not_copy_void_call_results() {
            let mut context = CompilationContext::default();
            let prototype = FunctionPrototypeBuilder::default()
                .name("void_thing")
                .filename(Arc::new("void_thing.c".into()))
                .return_type(LpcType::Void)
                .arity(FunctionArity::new(1))
                .span(Some(Span::new(0, 0..0)))
                .build()
                .unwrap();

            context
                .function_prototypes
                .insert("void_thing".into(), prototype);
            let mut walker = CodegenWalker::new(context);
            let call = "mixed m = void_thing(666);";
            let mut node = get_call_node(call, &mut walker.context);

            let _ = walker.visit_call(&mut node).await;

            let expected = vec![
                PushArg(RegisterVariant::Constant(Register(0))),
                Call(ustr("void_thing__v__void_thing.c__pb__")),
            ];

            assert_eq!(walker_init_instructions(&mut walker), expected);
        }

        #[tokio::test]
        async fn copies_non_void_efun_results() {
            let mut walker = default_walker();
            let call = r#"mixed m = clone_object("/foo.c");"#;
            let mut node = get_call_node(call, &mut walker.context);

            let _ = walker.visit_call(&mut node).await;

            let expected = vec![
                PushArg(RegisterVariant::Constant(Register(0))),
                CallEfun(8),
                Copy(
                    RegisterVariant::Local(Register(0)),
                    RegisterVariant::Local(Register(1)),
                ),
            ];

            assert_eq!(walker_init_instructions(&mut walker), expected);
        }

        #[tokio::test]
        async fn does_not_copy_void_efun_results() {
            let mut walker = default_walker();
            let call = r#"mixed m = dump("lkajsdflkajsdf");"#;
            let mut node = get_call_node(call, &mut walker.context);

            let _ = walker.visit_call(&mut node).await;

            let expected = vec![
                PushArg(RegisterVariant::Constant(Register(0))),
                CallEfun(15),
            ];

            assert_eq!(walker_init_instructions(&mut walker), expected);
        }

        #[tokio::test]
        async fn handles_ellipsis_functions() {
            let mut context = CompilationContext::default();
            let prototype = FunctionPrototypeBuilder::default()
                .name("my_func")
                .filename(Arc::new("my_func.c".into()))
                .return_type(LpcType::Void)
                .arity(FunctionArity::new(1))
                .arg_types(vec![LpcType::String(false)])
                .flags(FunctionFlags::default().with_ellipsis(true))
                .span(Some(Span::new(0, 0..0)))
                .build()
                .unwrap();

            context
                .function_prototypes
                .insert("my_func".into(), prototype);
            let mut walker = CodegenWalker::new(context);
            let call = "mixed m = my_func(\"hello!\", 42, \"cool beans\");";
            let mut node = get_call_node(call, &mut walker.context);

            let _ = walker.visit_call(&mut node).await;

            let expected = vec![
                PushArg(RegisterVariant::Constant(Register(0))),
                PushArg(RegisterVariant::Constant(Register(1))),
                PushArg(RegisterVariant::Constant(Register(2))),
                Call(ustr("my_func__v__my_func.c__pb_e__s")),
            ];

            assert_eq!(walker_init_instructions(&mut walker), expected);
        }

        #[tokio::test]
        async fn handles_chained_calls() {
            let call = "mixed m = papplyv(dump, ({ \"foo\", 25 }))();";

            // do a stupid dance to get the efuns into the context
            let walker = default_walker();

            let mut ctx = walker.into_context();
            let mut node = get_call_node(call, &mut ctx);

            // This walk by Scope Walker will actually populate the efuns.
            let mut walker = ScopeWalker::new(ctx);
            walker.visit_call(&mut node).await.unwrap();

            let ctx = walker.into_context();
            let mut walker = CodegenWalker::new(ctx);
            walker.visit_call(&mut node).await.unwrap();

            let expected = vec![
                FunctionPtrConst {
                    location: RegisterVariant::Local(Register(1)),
                    receiver: FunctionReceiver::Efun,
                    name: ustr("dump"),
                },
                PushArrayItem(RegisterVariant::Constant(Register(0))),
                PushArrayItem(RegisterVariant::Constant(Register(1))),
                AConst(RegisterVariant::Local(Register(2))),
                PushArg(RegisterVariant::Local(Register(1))),
                PushArg(RegisterVariant::Local(Register(2))),
                CallEfun(33),
                Copy(
                    RegisterVariant::Local(Register(0)),
                    RegisterVariant::Local(Register(3)),
                ),
                CallFp(RegisterVariant::Local(Register(3))),
                Copy(
                    RegisterVariant::Local(Register(0)),
                    RegisterVariant::Local(Register(4)),
                ),
            ];

            assert_eq!(walker_init_instructions(&mut walker), expected);
        }
    }

    mod test_visit_block {
        use super::*;

        #[tokio::test]
        async fn test_visit_block_populates_instructions() {
            let block = "void marf() { { int a = '🏯'; dump(a); } }";
            let mut prog_node: ProgramNode = lpc_parser::ProgramParser::new()
                .parse(
                    &mut CompilationContext::default(),
                    LexWrapper::new(block, 0).triples(),
                )
                .unwrap();
            let node = if let AstNode::FunctionDef(n) = prog_node.body.first_mut().unwrap() {
                if let AstNode::Block(n) = n.body.first_mut().unwrap() {
                    n
                } else {
                    panic!("Expected a block node");
                }
            } else {
                panic!("Expected a function def node");
            };

            let mut scope_walker = ScopeWalker::default();
            scope_walker.enter_scope(&mut node.scope_id);
            let _ = scope_walker.visit_block(node).await;
            scope_walker.exit_scope();

            let context = scope_walker.into_context();
            let mut walker = CodegenWalker::new(context);
            walker.enter_scope(&mut node.scope_id);
            let _ = walker.visit_block(node).await;
            walker.exit_scope();

            let expected = vec![
                Copy(
                    RegisterVariant::Constant(Register(0)),
                    RegisterVariant::Local(Register(1)),
                ),
                PushArg(RegisterVariant::Local(Register(1))),
                CallEfun(15),
            ];

            assert_eq!(walker_init_instructions(&mut walker), expected);
        }
    }

    #[tokio::test]
    async fn test_visit_comma_expression_populates_the_instructions() {
        let mut walker = default_walker();

        let mut expr = CommaExpressionNode::new(vec![
            ExpressionNode::from(123),
            ExpressionNode::from("foo"),
            ExpressionNode::from(vec![ExpressionNode::from(666)]),
        ]);

        let _ = walker.visit_comma_expression(&mut expr).await;

        let expected = vec![
            PushArrayItem(RegisterVariant::Constant(Register(2))),
            AConst(RegisterVariant::Local(Register(1))),
        ];

        assert_eq!(walker_init_instructions(&mut walker), expected);
        assert_eq!(walker.current_result, RegisterVariant::Local(Register(1)));
    }

    mod test_visit_closure {
        use indoc::indoc;

        use super::*;

        fn get_closure_node(code: &str, context: &mut CompilationContext) -> ClosureNode {
            let mut prog_node = lpc_parser::ProgramParser::new()
                .parse(context, LexWrapper::new(code, 0).triples())
                .unwrap();
            if_chain! {
                if let Some(AstNode::Decl(mut node)) = prog_node.body.pop();
                if let Some(VarInitNode { value, .. }) = node.initializations.pop();
                if let Some(ExpressionNode::Closure(node)) = value;
                then {
                    node
                } else {
                    panic!("expected call node");
                }
            }
        }

        async fn compile(code: &str) -> CodegenWalker {
            let mut context = CompilationContext::default();

            let mut node = get_closure_node(code, &mut context);

            let mut prototype_walker = FunctionPrototypeWalker::new(context);
            let _ = prototype_walker.visit_closure(&mut node).await;
            let mut context = prototype_walker.into_context();

            context.scopes.push_new(); // global scope

            let mut scope_walker = ScopeWalker::new(context);
            scope_walker.enter_scope(&mut node.scope_id);
            let _ = scope_walker.visit_closure(&mut node).await;
            scope_walker.exit_scope();

            let mut context = scope_walker.into_context();
            context.scopes.goto_root();

            let mut walker = CodegenWalker::new(context);
            walker.enter_scope(&mut node.scope_id);
            let _ = walker.visit_closure(&mut node).await;
            walker.exit_scope();

            walker
        }

        #[tokio::test]
        async fn populates_the_instructions() {
            let mut walker = compile("function f = (: dump(4 + 5 + $1) :);").await;

            assert_eq!(
                walker_function_instructions(&mut walker, "closure-0"),
                vec![
                    MAdd(
                        RegisterVariant::Constant(Register(0)),
                        RegisterVariant::Local(Register(1)),
                        RegisterVariant::Local(Register(2))
                    ),
                    PushArg(RegisterVariant::Local(Register(2))),
                    CallEfun(15),
                    Ret,
                ]
            );
        }

        #[tokio::test]
        async fn handles_ellipses() {
            let mut walker = compile("function f = (: [int i, ...] argv :);").await;

            assert_eq!(
                walker_function_instructions(&mut walker, "closure-0"),
                vec![
                    PopulateArgv(RegisterVariant::Local(Register(2)), 1, 1),
                    Copy(
                        RegisterVariant::Local(Register(2)),
                        RegisterVariant::Local(Register(0)),
                    ),
                    Ret,
                ]
            );
        }

        #[tokio::test]
        async fn populates_the_default_arguments() {
            let mut walker =
                compile("function f = (: [int i, int j = 666, float d = 3.54] i * j :);").await;

            assert_eq!(
                walker_function_instructions(&mut walker, "closure-0"),
                vec![
                    PopulateDefaults,
                    Jmp(Address(5)),
                    Jmp(Address(6)),
                    IMul(
                        RegisterVariant::Local(Register(1)),
                        RegisterVariant::Local(Register(2)),
                        RegisterVariant::Local(Register(0)),
                    ),
                    Ret,
                    Copy(
                        RegisterVariant::Constant(Register(0)),
                        RegisterVariant::Local(Register(2))
                    ),
                    Copy(
                        RegisterVariant::Constant(Register(1)),
                        RegisterVariant::Local(Register(3))
                    ),
                    Jmp(Address(3)),
                ],
            );
        }

        #[tokio::test]
        async fn sets_the_correct_upvalue_information() {
            let code = indoc! {r##"
                int g = 42;

                void create() {
                    int i = 666;
                    function f = (:
                        int s = 123;
                        g + i + s
                    :);
                }
            "##};
            let walker = walk_prog(code).await;

            let closure = walker
                .functions
                .values()
                .find(|f| f.name() == "closure-0")
                .expect("where's the closure?");
            assert_eq!(closure.num_upvalues, 0);
            assert_eq!(closure.local_variables.len(), 1);
            assert_eq!(&closure.local_variables.first().unwrap().name, "s");
            assert_eq!(
                &closure.local_variables.first().unwrap().location.unwrap(),
                &RegisterVariant::Local(Register(1))
            );

            let func = walker
                .functions
                .values()
                .find(|f| f.name() == "create")
                .expect("where's create()?");
            assert_eq!(func.num_upvalues, 1);
            assert_eq!(func.local_variables.len(), 2);
            assert_eq!(func.local_variables.first().unwrap().name, "i");
            assert_eq!(func.local_variables.last().unwrap().name, "f");
        }

        #[tokio::test]
        async fn a_closure_pointer_names_the_mangled_function() {
            let code = indoc! {r##"
                void create() { function f = (: 1 :); }
            "##};
            let walker = walk_prog(code).await;
            let create = walker
                .functions
                .values()
                .find(|f| f.name() == "create")
                .unwrap();
            let name_index = create
                .instructions
                .iter()
                .find_map(|i| match i {
                    Instruction::FunctionPtrConst { name, .. } => Some(*name),
                    _ => None,
                })
                .unwrap();
            let name = name_index.to_string();

            let closure = &walker.functions[&name];
            assert!(closure.is_closure());
            assert_eq!(closure.name(), "closure-0");

            let program = walker.into_program().unwrap();
            assert!(program.functions.contains_key(&name));
            assert!(!program.unmangled_functions.contains_key("closure-0"));
        }

        #[tokio::test]
        async fn positional_args_are_parameters_of_their_own_closure() {
            let code = indoc! {r##"
                void create() { function a = (: [int x] $3 :); function b = (: 1 :); }
            "##};
            let walker = walk_prog(code).await;

            let a = &walker.context.function_prototypes["closure-0"];
            assert_eq!(a.arity.num_args, 3);
            assert_eq!(
                a.arg_types,
                vec![
                    LpcType::Int(false),
                    LpcType::Mixed(false),
                    LpcType::Mixed(false)
                ]
            );

            let b = &walker.context.function_prototypes["closure-1"];
            assert_eq!(b.arity.num_args, 0);
        }

        #[tokio::test]
        async fn a_positional_arg_beyond_a_defaulted_parameter_is_an_error() {
            let code = indoc! {r##"
                void create() { function a = (: [int i = 5] $2 :); }
            "##};
            let walker = walk_prog(code).await;
            let errors: Vec<String> = walker
                .context
                .diagnostics
                .errors()
                .iter()
                .map(|e| e.to_string())
                .collect();
            assert!(errors.iter().any(|e| e.contains("defaulted")), "{errors:?}");
        }

        #[tokio::test]
        async fn a_captured_declaration_mints_its_cell_before_the_store() {
            let code = indoc! {r##"
                void create() { int plain = 1; int j = 1; function f = (: j :); }
            "##};
            let walker = walk_prog(code).await;
            let (create, cell) = owner_of(&walker, "j");
            let (_, plain) = owner_of(&walker, "plain");
            assert_eq!(plain, RegisterVariant::Local(Register(1)));

            let instructions = &create.instructions;
            let mint = instructions
                .iter()
                .position(|i| *i == Instruction::NewUpvalue(cell))
                .expect("the captured declaration mints its cell");
            let store = instructions
                .iter()
                .position(|i| i.dest_register() == Some(cell))
                .unwrap();
            assert!(mint < store);
            assert!(
                !instructions
                    .iter()
                    .any(|i| matches!(i, Instruction::NewUpvalue(RegisterVariant::Local(_))))
            );
        }

        /// The function owning local `var`, with that local's location.
        fn owner_of<'a>(
            walker: &'a CodegenWalker,
            var: &str,
        ) -> (&'a ProgramFunction, RegisterVariant) {
            walker
                .functions
                .values()
                .find_map(|f| {
                    let sym = f.local_variables.iter().find(|s| s.name == var)?;
                    Some((&**f, sym.location.unwrap()))
                })
                .unwrap_or_else(|| panic!("no function declares {var}"))
        }

        #[tokio::test]
        async fn a_closure_numbers_its_own_captures_after_the_creators_total() {
            let code = indoc! {r##"
                void create() {
                    function a = (: int j = 1; function x = (: j :); return x(); :);
                    int later = 5;
                    function g = (: later :);
                    function b = (: int k = 2; function y = (: k :); return y(); :);
                }
            "##};
            let walker = walk_prog(code).await;

            let (create, later) = owner_of(&walker, "later");
            assert_eq!(create.num_upvalues, 1);
            assert_eq!(later, RegisterVariant::Upvalue(Register(0)));

            let (a, j) = owner_of(&walker, "j");
            assert_eq!(a.num_upvalues, 1);
            assert_eq!(j, RegisterVariant::Upvalue(Register(1)));

            let (b, k) = owner_of(&walker, "k");
            assert_eq!(b.num_upvalues, 1);
            assert_eq!(k, RegisterVariant::Upvalue(Register(1)));
        }
    }

    mod test_continue {
        use lpc_rs_core::register::Register;

        use super::*;

        #[tokio::test]
        async fn continues_while_loops() {
            let code = r#"
                void create() {
                    int i;
                    while (i < 10) {
                        dump(i);
                        if (i > 5) {
                            dump("goin' infinite!");
                            continue;
                        }
                        i += 1;
                    }
                }
            "#;

            let mut walker = walk_prog(code).await;
            let expected = vec![
                Lt(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Constant(Register(0)),
                    RegisterVariant::Local(Register(2)),
                ),
                Jz(RegisterVariant::Local(Register(2)), Address(11)),
                PushArg(RegisterVariant::Local(Register(1))),
                CallEfun(15),
                Gt(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Constant(Register(1)),
                    RegisterVariant::Local(Register(3)),
                ),
                Jz(RegisterVariant::Local(Register(3)), Address(9)),
                PushArg(RegisterVariant::Constant(Register(2))),
                CallEfun(15),
                Jmp(Address(0)),
                IAdd(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Constant(Register(3)),
                    RegisterVariant::Local(Register(1)),
                ),
                Jmp(Address(0)),
                Ret,
            ];

            assert_eq!(
                walker_function_instructions(&mut walker, "create"),
                expected
            );
        }

        #[tokio::test]
        async fn continues_for_loops() {
            let code = r#"
                void create() {
                    for (int i = 0; i < 10; i += 1) {
                        dump(i);
                        if (i > 5) {
                            dump("goin' infinite!");
                            continue;
                        }
                        i += 1;
                    }
                }
            "#;

            let mut walker = walk_prog(code).await;
            let expected = vec![
                Copy(
                    RegisterVariant::Constant(Register(0)),
                    RegisterVariant::Local(Register(1)),
                ),
                Lt(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Constant(Register(1)),
                    RegisterVariant::Local(Register(2)),
                ),
                Jz(RegisterVariant::Local(Register(2)), Address(13)),
                PushArg(RegisterVariant::Local(Register(1))),
                CallEfun(15),
                Gt(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Constant(Register(2)),
                    RegisterVariant::Local(Register(3)),
                ),
                Jz(RegisterVariant::Local(Register(3)), Address(10)),
                PushArg(RegisterVariant::Constant(Register(3))),
                CallEfun(15),
                Jmp(Address(11)),
                IAdd(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Constant(Register(4)),
                    RegisterVariant::Local(Register(1)),
                ),
                IAdd(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Constant(Register(4)),
                    RegisterVariant::Local(Register(1)),
                ),
                Jmp(Address(1)),
                Ret,
            ];

            assert_eq!(
                walker_function_instructions(&mut walker, CREATE_FUNCTION),
                expected
            );
        }

        #[tokio::test]
        async fn continues_do_while_loops() {
            let code = r#"
                void create() {
                    int i;
                    do {
                        dump(i);
                        if (i > 5) {
                            dump("goin' infinite!");
                            continue;
                        }
                        i += 1;
                    } while (i < 10);
                }
            "#;

            let mut walker = walk_prog(code).await;
            let expected = vec![
                PushArg(RegisterVariant::Local(Register(1))),
                CallEfun(15),
                Gt(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Constant(Register(0)),
                    RegisterVariant::Local(Register(2)),
                ),
                Jz(RegisterVariant::Local(Register(2)), Address(7)),
                PushArg(RegisterVariant::Constant(Register(1))),
                CallEfun(15),
                Jmp(Address(8)),
                IAdd(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Constant(Register(2)),
                    RegisterVariant::Local(Register(1)),
                ),
                Lt(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Constant(Register(3)),
                    RegisterVariant::Local(Register(4)),
                ),
                Jnz(RegisterVariant::Local(Register(4)), Address(0)),
                Ret,
            ];

            assert_eq!(
                walker_function_instructions(&mut walker, CREATE_FUNCTION),
                expected
            );
        }
    }

    #[tokio::test]
    async fn test_decl_sets_scope_and_instructions() {
        let call = "int foo = 1, *bar = ({ 56 });";
        let mut prog_node: ProgramNode = lpc_parser::ProgramParser::new()
            .parse(
                &mut CompilationContext::default(),
                LexWrapper::new(call, 0).triples(),
            )
            .unwrap();
        let node = if let AstNode::Decl(node) = prog_node.body.first_mut().unwrap() {
            node
        } else {
            panic!("Expected decl node");
        };

        let mut scope_walker = ScopeWalker::default();
        let _ = scope_walker.visit_decl(node).await;

        let context = scope_walker.into_context();
        let mut walker = CodegenWalker::new(context);
        let _ = walker.visit_decl(node).await;

        let expected = vec![
            Copy(
                RegisterVariant::Constant(Register(0)),
                RegisterVariant::Global(Register(0)),
            ),
            PushArrayItem(RegisterVariant::Constant(Register(1))),
            AConst(RegisterVariant::Local(Register(1))),
            Copy(
                RegisterVariant::Local(Register(1)),
                RegisterVariant::Global(Register(1)),
            ),
        ];

        assert_eq!(walker_init_instructions(&mut walker), expected);

        let scope = walker.context.scopes.current().unwrap();

        let foo = scope.lookup("foo").unwrap();
        assert_eq!(&foo.name, "foo");
        assert_eq!(foo.type_, LpcType::Int(false));
        assert_eq!(foo.location, Some(RegisterVariant::Global(Register(0))));
        assert_some!(foo.scope_id);
        assert_eq!(foo.span, Some(Span::new(0, 4..11)));

        let bar = scope.lookup("bar").unwrap();
        assert_eq!(&bar.name, "bar");
        assert_eq!(bar.type_, LpcType::Int(true));
        assert_eq!(bar.location, Some(RegisterVariant::Global(Register(1))));
        assert_some!(bar.scope_id);
        assert_eq!(bar.span, Some(Span::new(0, 13..25)));
    }

    mod test_visit_do_while {
        use lpc_rs_asm::instruction::Instruction::{EqEq, Jnz};

        use super::*;
        use crate::compiler::ast::do_while_node::DoWhileNode;

        #[tokio::test]
        async fn test_populates_the_instructions() {
            let mut walker = default_walker();
            walker.backpatch_maps.push(HashMap::new());

            let mut node = DoWhileNode {
                condition: ExpressionNode::BinaryOp(BinaryOpNode {
                    l: Box::new(ExpressionNode::from(666)),
                    r: Box::new(ExpressionNode::from(777)),
                    op: BinaryOperation::EqEq,
                    span: None,
                }),
                body: Box::new(AstNode::Call(create!(
                    CallNode,
                    chain: create!(CallChain, name: ustr("dump")),
                    arguments: vec![ExpressionNode::from("body")],
                ))),
                scope_id: None,
                span: None,
            };

            let _ = walker.visit_do_while(&mut node).await;

            let expected = vec![
                PushArg(RegisterVariant::Constant(Register(0))),
                CallEfun(15),
                EqEq(
                    RegisterVariant::Constant(Register(1)),
                    RegisterVariant::Constant(Register(2)),
                    RegisterVariant::Local(Register(1)),
                ),
                Jnz(RegisterVariant::Local(Register(1)), Address(0)),
            ];

            assert_eq!(walker_init_instructions(&mut walker), expected);
        }
    }

    mod test_visit_for {
        use super::*;
        use crate::compiler::ast::for_node::ForNode;

        #[tokio::test]
        async fn populates_the_instructions() {
            let var = VarNode {
                name: ustr("i"),
                span: None,
                global: false,
                function_name: false,
            };

            let mut node = ForNode {
                initializer: Box::new(Some(AstNode::VarInit(VarInitNode {
                    type_: LpcType::Int(false),
                    name: ustr("i"),
                    value: Some(ExpressionNode::from(10)),
                    array: false,
                    global: false,
                    span: None,
                    flags: None,
                    by_ref: false,
                }))),
                condition: Some(ExpressionNode::Var(var.clone())),
                incrementer: Some(ExpressionNode::Assignment(AssignmentNode {
                    lhs: Box::new(ExpressionNode::Var(var.clone())),
                    rhs: Box::new(ExpressionNode::BinaryOp(BinaryOpNode {
                        l: Box::new(ExpressionNode::Var(var.clone())),
                        r: Box::new(ExpressionNode::from(1)),
                        op: BinaryOperation::Sub,
                        span: None,
                    })),
                    span: None,
                })),
                body: Box::new(AstNode::Block(BlockNode {
                    body: vec![AstNode::Call(CallNode {
                        chain: create!(CallChain, name: ustr("dump")),
                        arguments: vec![ExpressionNode::Var(var)],
                        span: None,
                    })],
                    scope_id: None,
                })),
                scope_id: None,
                span: None,
            };

            let mut scope_walker = ScopeWalker::default();
            scope_walker.enter_scope(&mut node.scope_id);
            let _ = scope_walker.visit_for(&mut node).await;
            scope_walker.exit_scope();

            let context = scope_walker.into_context();
            let mut walker = CodegenWalker::new(context);
            walker.backpatch_maps.push(HashMap::new());

            walker.enter_scope(&mut node.scope_id);
            walker.visit_for(&mut node).await.unwrap();
            walker.exit_scope();

            let expected = vec![
                Copy(
                    RegisterVariant::Constant(Register(0)),
                    RegisterVariant::Local(Register(1)),
                ),
                Jz(RegisterVariant::Local(Register(1)), Address(0)),
                PushArg(RegisterVariant::Local(Register(1))),
                CallEfun(15),
                ISub(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Constant(Register(1)),
                    RegisterVariant::Local(Register(2)),
                ),
                Copy(
                    RegisterVariant::Local(Register(2)),
                    RegisterVariant::Local(Register(1)),
                ),
                Jmp(Address(0)),
            ];

            assert_eq!(walker_init_instructions(&mut walker), expected);
        }
    }

    mod test_visit_function_def {
        use super::*;

        async fn assert_compiles_to(code: &str, expected: Vec<Instruction>) {
            let mut prototype_walker = FunctionPrototypeWalker::default();

            let mut prog_node: ProgramNode = lpc_parser::ProgramParser::new()
                .parse(
                    &mut CompilationContext::default(),
                    LexWrapper::new(code, 0).triples(),
                )
                .unwrap();
            let ast_node = prog_node.body.first_mut().unwrap();
            let node = if let AstNode::FunctionDef(node) = ast_node {
                node
            } else {
                panic!("Didn't receive a function def?");
            };

            let _ = prototype_walker.visit_function_def(node).await;
            let mut context = prototype_walker.into_context();

            context.scopes.push_new(); // global scope

            let mut scope_walker = ScopeWalker::new(context);
            let _ = scope_walker.visit_function_def(node).await;

            let mut context = scope_walker.into_context();
            context.scopes.goto_root();

            let mut walker = CodegenWalker::new(context);
            let _ = walker.visit_function_def(node).await;

            assert_eq!(walker_function_instructions(&mut walker, "main"), expected);
        }

        #[tokio::test]
        async fn populates_the_data() {
            assert_compiles_to(
                "int main(int i) { return i + 4; }",
                vec![
                    IAdd(
                        RegisterVariant::Local(Register(1)),
                        RegisterVariant::Constant(Register(0)),
                        RegisterVariant::Local(Register(0)),
                    ),
                    Ret,
                ],
            )
            .await;
        }

        #[tokio::test]
        async fn handles_ellipses() {
            assert_compiles_to(
                "int main(int i, ...) { return argv; }",
                vec![
                    PopulateArgv(RegisterVariant::Local(Register(2)), 1, 1),
                    Copy(
                        RegisterVariant::Local(Register(2)),
                        RegisterVariant::Local(Register(0)),
                    ),
                    Ret,
                ],
            )
            .await;
        }

        #[tokio::test]
        async fn populates_the_default_arguments() {
            assert_compiles_to(
                "int main(int i, int j = 666, float d = 3.54) { return i * j; }",
                vec![
                    PopulateDefaults,
                    Jmp(Address(5)),
                    Jmp(Address(6)),
                    IMul(
                        RegisterVariant::Local(Register(1)),
                        RegisterVariant::Local(Register(2)),
                        RegisterVariant::Local(Register(0)),
                    ),
                    Ret,
                    Copy(
                        RegisterVariant::Constant(Register(0)),
                        RegisterVariant::Local(Register(2)),
                    ),
                    Copy(
                        RegisterVariant::Constant(Register(1)),
                        RegisterVariant::Local(Register(3)),
                    ),
                    Jmp(Address(3)),
                ],
            )
            .await;
        }
    }

    mod test_visit_function_ptr {
        use super::*;
        use crate::compiler::ast::function_ptr_node::FunctionPtrNode;

        #[tokio::test]
        async fn populates_the_instructions_for_efuns() {
            let mut node = FunctionPtrNode {
                receiver: None,
                name: ustr("dump"),
                arguments: None,
                span: None,
            };

            let mut walker = default_walker();
            walker.visit_function_ptr(&mut node).await.unwrap();

            let expected = vec![FunctionPtrConst {
                location: RegisterVariant::Local(Register(1)),
                receiver: FunctionReceiver::Efun,
                name: ustr("dump"),
            }];

            assert_eq!(walker_init_instructions(&mut walker), expected);
        }

        #[tokio::test]
        async fn populates_the_instructions_for_simul_efuns() {
            let mut node = FunctionPtrNode {
                receiver: None,
                name: ustr("simul_efun"),
                arguments: None,
                span: None,
            };

            let mut walker = default_walker();
            walker.visit_function_ptr(&mut node).await.unwrap();

            let expected = vec![FunctionPtrConst {
                location: RegisterVariant::Local(Register(1)),
                receiver: FunctionReceiver::SimulEfun,
                name: ustr("simul_efun"),
            }];

            assert_eq!(walker_init_instructions(&mut walker), expected);
        }
    }

    mod test_visit_if {
        use lpc_rs_asm::instruction::Instruction::{EqEq, Jmp, Jz};

        use super::*;

        #[tokio::test]
        async fn test_populates_the_instructions() {
            let mut walker = default_walker();
            walker.backpatch_maps.push(HashMap::new());

            let mut node = IfNode {
                condition: ExpressionNode::BinaryOp(BinaryOpNode {
                    l: Box::new(ExpressionNode::from(666)),
                    r: Box::new(ExpressionNode::from(777)),
                    op: BinaryOperation::EqEq,
                    span: None,
                }),
                body: Box::new(AstNode::Call(create!(
                    CallNode,
                    chain: create!(CallChain, name: ustr("dump")),
                    arguments: vec![ExpressionNode::from("true")]
                ))),
                else_clause: Box::new(Some(AstNode::Call(CallNode {
                    chain: create!(CallChain, name: ustr("dump")),
                    arguments: vec![ExpressionNode::from("false")],
                    span: None,
                }))),
                scope_id: None,
                span: None,
            };

            let _ = walker.visit_if(&mut node).await;

            let expected = vec![
                EqEq(
                    RegisterVariant::Constant(Register(0)),
                    RegisterVariant::Constant(Register(1)),
                    RegisterVariant::Local(Register(1)),
                ),
                Jz(RegisterVariant::Local(Register(1)), Address(0)),
                PushArg(RegisterVariant::Constant(Register(2))),
                CallEfun(15),
                Jmp(Address(0)),
                PushArg(RegisterVariant::Constant(Register(3))),
                CallEfun(15),
            ];

            assert_eq!(walker_init_instructions(&mut walker), expected);
        }
    }

    mod test_constant_pool {
        use lpc_rs_function_support::constant::LpcConstant;
        use lpc_rs_utils::lpc_string::LpcString;

        use super::*;

        async fn function(code: &str, name: &str) -> Arc<ProgramFunction> {
            let walker = walk_prog(code).await;
            walker
                .functions
                .values()
                .find(|f| f.name() == name)
                .unwrap_or_else(|| panic!("no function named `{name}`"))
                .clone()
        }

        fn k(i: RegisterSize) -> RegisterVariant {
            Register(i).as_constant()
        }

        fn l(i: RegisterSize) -> RegisterVariant {
            Register(i).as_local()
        }

        fn string(s: &str) -> LpcConstant {
            LpcConstant::String(Arc::new(LpcString::Static(ustr(s))))
        }

        #[tokio::test]
        async fn a_literal_costs_no_instruction() {
            let f = function("void create() { int x = 5; }", "create").await;
            assert_eq!(f.instructions, vec![Copy(k(0), l(1)), Ret]);
            assert_eq!(f.constants, vec![LpcConstant::Int(5)]);
        }

        #[tokio::test]
        async fn an_operand_reads_the_constant_directly() {
            let f = function("int f(int n) { return n - 1; }", "f").await;
            assert_eq!(f.instructions, vec![ISub(l(1), k(0), l(0)), Ret]);
            assert_eq!(f.constants, vec![LpcConstant::Int(1)]);
        }

        #[tokio::test]
        async fn a_repeated_literal_interns_once() {
            let f = function("int f(int n) { return n + 5 + 5; }", "f").await;
            assert_eq!(
                f.instructions,
                vec![IAdd(l(1), k(0), l(2)), IAdd(l(2), k(0), l(0)), Ret]
            );
            assert_eq!(f.constants, vec![LpcConstant::Int(5)]);
        }

        #[tokio::test]
        async fn distinct_literals_take_pool_order() {
            let f = function("int f(int n) { return n + 5 + 6 + 5; }", "f").await;
            assert_eq!(
                f.instructions,
                vec![
                    IAdd(l(1), k(0), l(2)),
                    IAdd(l(2), k(1), l(3)),
                    IAdd(l(3), k(0), l(0)),
                    Ret,
                ]
            );
            assert_eq!(f.constants, vec![LpcConstant::Int(5), LpcConstant::Int(6)]);
        }

        #[tokio::test]
        async fn a_string_literal_is_built_once() {
            let f = function("string f() { return \"a\"; }", "f").await;
            assert_eq!(f.instructions, vec![Copy(k(0), l(0)), Ret]);
            assert_eq!(f.constants, vec![string("a")]);
        }

        #[tokio::test]
        async fn zero_and_negative_zero_are_distinct_floats() {
            let f = function("float f(float x) { return x + 0.0 + -0.0; }", "f").await;
            assert_eq!(
                f.constants,
                vec![
                    LpcConstant::Float(0.0.into()),
                    LpcConstant::Float((-0.0).into())
                ]
            );
        }

        #[tokio::test]
        async fn a_closure_owns_its_pool() {
            let code = "void create() { int x = 7; function f = (: 7 + $1 :); int y = 8; }";
            let closure = function(code, "closure-0").await;
            let create = function(code, "create").await;
            assert_eq!(closure.constants, vec![LpcConstant::Int(7)]);
            assert_eq!(
                create.constants,
                vec![LpcConstant::Int(7), LpcConstant::Int(8)]
            );
        }

        #[tokio::test]
        async fn an_inherited_function_keeps_its_pool() {
            let code = "inherit \"/parent\";\nint i = 123;";
            let program = walk_prog(code).await.into_program().unwrap();
            let f = program
                .functions
                .values()
                .find(|f| f.name() == "parent_method")
                .unwrap();
            assert_eq!(f.constants, vec![string("parent method!")]);
            assert_eq!(f.instructions, vec![PushArg(k(0)), CallEfun(15), Ret]);
        }

        #[tokio::test]
        async fn a_negation_multiplies_by_a_pooled_minus_one() {
            let f = function("int f(int x) { return -x; }", "f").await;
            assert_eq!(f.instructions, vec![MMul(l(1), k(0), l(0)), Ret]);
            assert_eq!(f.constants, vec![LpcConstant::Int(-1)]);
        }

        #[tokio::test]
        async fn an_andand_value_zeroes_from_the_pool() {
            let f = function("int f(int a, int b) { return a && b; }", "f").await;
            assert_eq!(
                f.instructions,
                vec![
                    Copy(k(0), l(3)),
                    Jz(l(1), Address(3)),
                    Copy(l(2), l(3)),
                    Copy(l(3), l(0)),
                    Ret,
                ]
            );
            assert_eq!(f.constants, vec![LpcConstant::Int(0)]);
        }

        #[tokio::test]
        async fn an_open_switch_range_reads_its_one_from_the_pool() {
            let code = "int f(int x) { switch (x) { case ..5: return 1; } return 0; }";
            let f = function(code, "f").await;
            assert!(
                f.instructions
                    .iter()
                    .any(|i| matches!(i, And(RegisterVariant::Constant(_), _, _))),
                "{:?}",
                f.instructions
            );
            assert!(f.constants.contains(&LpcConstant::Int(1)));
        }

        #[tokio::test]
        async fn a_call_other_name_is_pooled() {
            let f = function("void create() { this_object()->foo(); }", "create").await;
            assert!(
                f.instructions
                    .iter()
                    .any(|i| matches!(i, CallOther(_, RegisterVariant::Constant(_)))),
                "{:?}",
                f.instructions
            );
            assert_eq!(f.constants, vec![string("foo")]);
        }

        #[tokio::test]
        async fn an_argument_literal_is_pushed_from_the_pool() {
            let f = function("void create() { dump(5); }", "create").await;
            assert_eq!(f.instructions, vec![PushArg(k(0)), CallEfun(15), Ret]);
        }

        #[tokio::test]
        async fn a_missing_range_end_reads_a_pooled_minus_one() {
            let f = function("int *f(int *a) { return a[1..]; }", "f").await;
            assert!(
                f.instructions
                    .iter()
                    .any(|i| matches!(i, Range(_, _, RegisterVariant::Constant(_), _))),
                "{:?}",
                f.instructions
            );
            assert_eq!(f.constants, vec![LpcConstant::Int(1), LpcConstant::Int(-1)]);
        }

        #[tokio::test]
        async fn the_listing_shows_the_pool_and_its_operands() {
            let program = walk_prog("int f() { return 5; }")
                .await
                .into_program()
                .unwrap();
            let listing = program.listing().join("\n");
            assert!(listing.contains("    k0 = 5"), "{listing}");
            assert!(listing.contains("copy k0, r0"), "{listing}");
        }

        #[tokio::test]
        async fn a_pool_holds_at_most_65536_constants() {
            let mut walker = default_walker();
            for i in 0..65536 {
                walker.constant(LpcConstant::Int(i), None).unwrap();
            }
            let err = walker.constant(LpcConstant::Int(65536), None).unwrap_err();
            assert!(err.to_string().contains("constants"), "{err}");
        }
    }

    mod test_condition_form {
        use super::*;

        async fn create_instructions(code: &str) -> Vec<Instruction> {
            let mut walker = walk_prog(code).await;
            walker_function_instructions(&mut walker, "create")
        }

        #[tokio::test]
        async fn a_negated_if_jumps_on_true() {
            let code = "void create() { int x; if (!x) { x = 1; } }";
            let expected = vec![
                Jnz(Register(1).as_local(), Address(2)),
                Copy(Register(0).as_constant(), Register(1).as_local()),
                Ret,
            ];
            assert_eq!(create_instructions(code).await, expected);
        }

        #[tokio::test]
        async fn a_negated_while_jumps_on_true() {
            let code = "void create() { int x; while (!x) { x = 1; } }";
            let expected = vec![
                Jnz(Register(1).as_local(), Address(3)),
                Copy(Register(0).as_constant(), Register(1).as_local()),
                Jmp(Address(0)),
                Ret,
            ];
            assert_eq!(create_instructions(code).await, expected);
        }

        #[tokio::test]
        async fn an_andand_if_jumps_past_on_either_false_operand() {
            let code = "void create() { int a; int b; int c; if (a && b) { c = 1; } }";
            let expected = vec![
                Jz(Register(1).as_local(), Address(3)),
                Jz(Register(2).as_local(), Address(3)),
                Copy(Register(0).as_constant(), Register(3).as_local()),
                Ret,
            ];
            assert_eq!(create_instructions(code).await, expected);
        }

        #[tokio::test]
        async fn an_oror_if_enters_on_the_first_true_operand() {
            let code = "void create() { int a; int b; int c; if (a || b) { c = 1; } }";
            let expected = vec![
                Jnz(Register(1).as_local(), Address(2)),
                Jz(Register(2).as_local(), Address(3)),
                Copy(Register(0).as_constant(), Register(3).as_local()),
                Ret,
            ];
            assert_eq!(create_instructions(code).await, expected);
        }

        #[tokio::test]
        async fn an_andand_do_while_loops_on_both_true_operands() {
            let code = "void create() { int a; int b; int c; do { c++; } while (a && b); }";
            let expected = vec![
                Inc(Register(3).as_local()),
                Jz(Register(1).as_local(), Address(3)),
                Jnz(Register(2).as_local(), Address(0)),
                Ret,
            ];
            assert_eq!(create_instructions(code).await, expected);
        }

        #[tokio::test]
        async fn a_negated_andand_flips_both_operands() {
            let code = "void create() { int a; int b; int c; if (!(a && b)) { c = 1; } }";
            let expected = vec![
                Jz(Register(1).as_local(), Address(2)),
                Jnz(Register(2).as_local(), Address(3)),
                Copy(Register(0).as_constant(), Register(3).as_local()),
                Ret,
            ];
            assert_eq!(create_instructions(code).await, expected);
        }

        #[tokio::test]
        async fn a_negated_oror_with_a_negated_operand_flips_twice() {
            let code = "void create() { int a; int b; int c; if (!(a || !b)) { c = 1; } }";
            let expected = vec![
                Jnz(Register(1).as_local(), Address(3)),
                Jz(Register(2).as_local(), Address(3)),
                Copy(Register(0).as_constant(), Register(3).as_local()),
                Ret,
            ];
            assert_eq!(create_instructions(code).await, expected);
        }

        #[tokio::test]
        async fn an_andand_ternary_condition_leaves_no_value() {
            let code = "void create() { int a; int b; int c; c = (a && b) ? 1 : 2; }";
            let expected = vec![
                Jz(Register(1).as_local(), Address(4)),
                Jz(Register(2).as_local(), Address(4)),
                Copy(Register(0).as_constant(), Register(4).as_local()),
                Jmp(Address(5)),
                Copy(Register(1).as_constant(), Register(4).as_local()),
                Copy(Register(4).as_local(), Register(3).as_local()),
                Ret,
            ];
            assert_eq!(create_instructions(code).await, expected);
        }

        #[tokio::test]
        async fn a_literal_true_while_has_no_test() {
            let code = "void create() { int c; while (1) { c++; if (c > 2) break; } }";
            let expected = vec![
                Inc(Register(1).as_local()),
                Gt(
                    Register(1).as_local(),
                    Register(0).as_constant(),
                    Register(2).as_local(),
                ),
                Jz(Register(2).as_local(), Address(4)),
                Jmp(Address(5)),
                Jmp(Address(0)),
                Ret,
            ];
            assert_eq!(create_instructions(code).await, expected);
        }

        #[tokio::test]
        async fn a_literal_false_do_while_runs_once() {
            let code = "void create() { int c; do { c++; } while (0); }";
            let expected = vec![Inc(Register(1).as_local()), Ret];
            assert_eq!(create_instructions(code).await, expected);
        }

        #[tokio::test]
        async fn a_literal_false_if_jumps_past_its_body() {
            let code = "void create() { int c; if (0) { c = 1; } }";
            let expected = vec![
                Jmp(Address(2)),
                Copy(Register(0).as_constant(), Register(1).as_local()),
                Ret,
            ];
            assert_eq!(create_instructions(code).await, expected);
        }

        #[tokio::test]
        async fn an_andand_value_sends_its_left_operand_through_the_condition_form() {
            let code = "void create() { int a; int b; int c; c = (a && b) && 1; }";
            let expected = vec![
                Copy(Register(0).as_constant(), Register(4).as_local()),
                Jz(Register(1).as_local(), Address(4)),
                Jz(Register(2).as_local(), Address(4)),
                Copy(Register(1).as_constant(), Register(4).as_local()),
                Copy(Register(4).as_local(), Register(3).as_local()),
                Ret,
            ];
            assert_eq!(create_instructions(code).await, expected);
        }
    }

    #[tokio::test]
    async fn test_visit_int_populates_the_instructions() {
        let mut walker = default_walker();

        let mut tree = IntNode::new(666);
        let mut tree0 = IntNode::new(0);
        let mut tree1 = IntNode::new(1);

        let _ = walker.visit_int(&mut tree).await;
        let _ = walker.visit_int(&mut tree0).await;
        let _ = walker.visit_int(&mut tree1).await;

        assert_eq!(walker.current_result, Register(2).as_constant());
        assert!(walker_init_instructions(&mut walker).is_empty());
        assert_eq!(
            walker.function_stack.last().unwrap().constants,
            vec![
                LpcConstant::Int(666),
                LpcConstant::Int(0),
                LpcConstant::Int(1)
            ]
        );
    }

    mod test_statement_bump {
        use super::*;

        #[tokio::test]
        async fn a_statement_increment_on_a_global_emits_a_bare_inc() {
            let mut walker = walk_prog("int g;\nvoid create() { g++; }").await;

            let expected = vec![Inc(Register(0).as_global()), Ret];
            assert_eq!(
                walker_function_instructions(&mut walker, "create"),
                expected
            );
        }

        #[tokio::test]
        async fn a_used_post_increment_keeps_its_pre_value_copy() {
            let mut walker = walk_prog("int g;\nvoid create() { int k = g++; }").await;

            let instructions = walker_function_instructions(&mut walker, "create");
            assert!(
                instructions
                    .iter()
                    .any(|i| matches!(i, Copy(RegisterVariant::Global(_), _))),
                "the observed pre-value needs its copy: {instructions:?}"
            );
        }

        #[tokio::test]
        async fn a_global_for_increment_emits_a_bare_inc() {
            let mut walker =
                walk_prog("int g;\nvoid create() { for (g = 0; g < 2; g++) {} }").await;

            let instructions = walker_function_instructions(&mut walker, "create");
            assert!(
                instructions
                    .iter()
                    .all(|i| !matches!(i, Copy(RegisterVariant::Local(_), _))),
                "a for-increment's value is unused: {instructions:?}"
            );
            assert!(instructions.contains(&Inc(Register(0).as_global())));
        }
    }

    mod test_coalesce {
        use super::*;

        #[tokio::test]
        async fn a_global_assignment_lands_in_the_global() {
            let mut walker = walk_prog("int c;\nvoid create() { c = c + 1; }").await;

            let expected = vec![
                IAdd(
                    RegisterVariant::Global(Register(0)),
                    RegisterVariant::Constant(Register(0)),
                    RegisterVariant::Global(Register(0)),
                ),
                Ret,
            ];
            assert_eq!(
                walker_function_instructions(&mut walker, "create"),
                expected
            );
        }

        #[tokio::test]
        async fn a_returned_expression_lands_in_r0() {
            let mut walker = walk_prog(
                "int f() { int a = 1; int b = 2; return a + b; }\nvoid create() { f(); }",
            )
            .await;

            let expected = vec![
                Copy(
                    RegisterVariant::Constant(Register(0)),
                    RegisterVariant::Local(Register(1)),
                ),
                Copy(
                    RegisterVariant::Constant(Register(1)),
                    RegisterVariant::Local(Register(2)),
                ),
                IAdd(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Local(Register(2)),
                    RegisterVariant::Local(Register(0)),
                ),
                Ret,
            ];
            assert_eq!(walker_function_instructions(&mut walker, "f"), expected);

            // The statement-position call result is dead, so its copy is too.
            let expected = vec![Call(ustr("f__i____pb__")), Ret];
            assert_eq!(
                walker_function_instructions(&mut walker, "create"),
                expected
            );
        }

        #[tokio::test]
        async fn a_statement_increment_keeps_no_pre_value() {
            let mut walker = walk_prog("void create() { int i; i++; }").await;

            let expected = vec![Inc(Register(1).as_local()), Ret];
            assert_eq!(
                walker_function_instructions(&mut walker, "create"),
                expected
            );
        }

        #[tokio::test]
        async fn a_second_call_result_reads_r0_where_the_first_cannot() {
            let mut walker =
                walk_prog("int f() { return 1; }\nint g() { return 2; }\nint a = f() + g();").await;

            let expected = vec![
                Call(ustr("f__i____pb__")),
                // g clobbers r0 before the add, so this copy must stay.
                Copy(Register(0).as_local(), Register(1).as_local()),
                Call(ustr("g__i____pb__")),
                IAdd(
                    Register(1).as_local(),
                    Register(0).as_local(),
                    Register(0).as_global(),
                ),
                Ret,
            ];
            assert_eq!(
                walker_function_instructions(&mut walker, INIT_GLOBALS),
                expected
            );
        }

        #[tokio::test]
        async fn a_staged_argument_reads_r0() {
            let mut walker = walk_prog("int f() { return 1; }\nvoid create() { dump(f()); }").await;

            let expected = vec![
                Call(ustr("f__i____pb__")),
                PushArg(Register(0).as_local()),
                CallEfun(15),
                Ret,
            ];
            assert_eq!(
                walker_function_instructions(&mut walker, "create"),
                expected
            );
        }

        #[tokio::test]
        async fn a_void_return_call_copies_nothing() {
            let mut walker = walk_prog("void f() { }\nvoid create() { return f(); }").await;

            let expected = vec![Call(ustr("f__v____pb__")), Ret];
            assert_eq!(
                walker_function_instructions(&mut walker, "create"),
                expected
            );
        }
    }

    mod test_visit_program {
        use super::*;

        #[tokio::test]
        async fn populates_the_instructions() {
            let prog = "
                void create() {
                    1 + 3 - 5;
                    dump(4 + 5);
                }
            ";

            let walker = walk_prog(prog).await;

            let expected = vec![
                Call(ustr("init-globals__v____pv__")),
                Call(ustr("create__v____pb__")),
                Ret,
            ];

            assert_eq!(walker.initializer.unwrap().instructions, expected);

            let expected = vec![
                PushArg(RegisterVariant::Constant(Register(1))),
                CallEfun(15),
                Ret,
            ];

            assert_eq!(
                walker
                    .functions
                    .values()
                    .find(|f| f.name() == CREATE_FUNCTION)
                    .unwrap()
                    .instructions,
                expected
            );
        }

        #[tokio::test]
        async fn initializes_the_globals() {
            let prog = r#"
                int j = 123;
                string q = "cool";
                void marf() {
                    dump(q + j);
                }
            "#;

            let instructions = generate_init_instructions(prog).await;

            let expected = [
                Copy(
                    RegisterVariant::Constant(Register(0)),
                    RegisterVariant::Global(Register(0)),
                ),
                Copy(
                    RegisterVariant::Constant(Register(1)),
                    RegisterVariant::Global(Register(1)),
                ),
                Ret,
            ];

            assert_eq!(instructions, expected);
        }

        #[tokio::test]
        async fn calls_create_if_create_is_defined() {
            let prog = r#"
                int q = 666;
                int marf() {
                    return 3;
                }
                void create() {
                    dump(marf() + " times a winner!");
                }
            "#;

            let walker = walk_prog(prog).await;

            let expected = [
                Call(ustr("init-globals__v____pv__")),
                Call(ustr("create__v____pb__")),
                Ret,
            ];

            assert_eq!(walker.initializer.unwrap().instructions, expected);
        }

        #[tokio::test]
        async fn tracks_global_registers_over_multiple_sections() {
            let prog = r#"
                int q = 666;
                int marf() {
                    return 3;
                }
                int r = 777;
            "#;

            let instructions = generate_init_instructions(prog).await;

            let expected = [
                Copy(
                    RegisterVariant::Constant(Register(0)),
                    RegisterVariant::Global(Register(0)),
                ),
                Copy(
                    RegisterVariant::Constant(Register(1)),
                    RegisterVariant::Global(Register(1)),
                ),
                Ret,
            ];

            assert_eq!(instructions, expected);
        }
    }

    #[tokio::test]
    async fn visit_return_populates_the_instructions() {
        let mut walker = default_walker();

        let mut node = ReturnNode::new(Some(ExpressionNode::from(IntNode::new(666))));
        let _ = walker.visit_return(&mut node).await;

        let expected = vec![
            Copy(
                RegisterVariant::Constant(Register(0)),
                RegisterVariant::Local(Register(0)),
            ),
            Ret,
        ];

        assert_eq!(walker_init_instructions(&mut walker), expected);

        /* === */

        let mut walker = default_walker();
        let mut node = ReturnNode::new(None);
        let _ = walker.visit_return(&mut node).await;

        let expected = vec![Ret];

        assert_eq!(walker_init_instructions(&mut walker), expected);
    }

    #[tokio::test]
    async fn test_visit_string_populates_the_instructions() {
        let mut walker = default_walker();
        let mut node = StringNode::new("marf");
        let mut node2 = StringNode::new("tacos");
        let mut node3 = StringNode::new("marf");

        let _ = walker.visit_string(&mut node).await;
        let _ = walker.visit_string(&mut node2).await;
        let _ = walker.visit_string(&mut node3).await;

        // The third literal is the first one again.
        assert_eq!(walker.current_result, Register(0).as_constant());
        assert!(walker_init_instructions(&mut walker).is_empty());
        let pool = &walker.function_stack.last().unwrap().constants;
        assert_eq!(pool.len(), 2);
        assert!(matches!(&pool[0], LpcConstant::String(s) if s.to_str() == "marf"));
        assert!(matches!(&pool[1], LpcConstant::String(s) if s.to_str() == "tacos"));
    }

    mod test_visit_switch {
        use super::*;

        #[tokio::test]
        async fn populates_the_instructions() {
            let code = r#"
                void create() {
                    switch(666) {
                        case 1:
                            dump("one");
                            break;
                        case 2:
                            dump("two");
                            break;
                        default:
                            dump("default");
                            break;
                    }
                }
            "#;

            let walker = walk_prog(code).await;
            let func = walker
                .functions
                .values()
                .find(|f| f.name() == "create")
                .unwrap();
            let instructions = func.instructions.clone();
            let expected = vec![
                Jmp(Address(10)),
                PushArg(RegisterVariant::Constant(Register(1))),
                CallEfun(15),
                Jmp(Address(15)),
                PushArg(RegisterVariant::Constant(Register(2))),
                CallEfun(15),
                Jmp(Address(15)),
                PushArg(RegisterVariant::Constant(Register(3))),
                CallEfun(15),
                Jmp(Address(15)),
                EqEq(
                    RegisterVariant::Constant(Register(0)),
                    RegisterVariant::Constant(Register(4)),
                    RegisterVariant::Local(Register(1)),
                ),
                Jnz(RegisterVariant::Local(Register(1)), Address(1)),
                EqEq(
                    RegisterVariant::Constant(Register(0)),
                    RegisterVariant::Constant(Register(5)),
                    RegisterVariant::Local(Register(2)),
                ),
                Jnz(RegisterVariant::Local(Register(2)), Address(4)),
                Jmp(Address(7)),
                Ret,
            ];

            assert_eq!(instructions, expected);
        }
    }

    mod test_visit_ternary {
        use lpc_rs_asm::instruction::Instruction::{Jmp, Jz, Lte};

        use super::*;
        use crate::compiler::ast::ternary_node::TernaryNode;

        #[tokio::test]
        async fn populates_the_instructions() {
            let mut node = TernaryNode {
                condition: Box::new(ExpressionNode::BinaryOp(BinaryOpNode {
                    l: Box::new(ExpressionNode::from(2)),
                    r: Box::new(ExpressionNode::from(3)),
                    op: BinaryOperation::Lte,
                    span: None,
                })),
                body: Box::new(ExpressionNode::from(666)),
                else_clause: Box::new(ExpressionNode::from(777)),
                span: None,
            };

            let mut walker = CodegenWalker::new(CompilationContext::default());
            walker.backpatch_maps.push(HashMap::new());

            walker.visit_ternary(&mut node).await.unwrap();

            let expected = vec![
                Lte(
                    RegisterVariant::Constant(Register(0)),
                    RegisterVariant::Constant(Register(1)),
                    RegisterVariant::Local(Register(2)),
                ),
                Jz(RegisterVariant::Local(Register(2)), Address(0)),
                Copy(
                    RegisterVariant::Constant(Register(2)),
                    RegisterVariant::Local(Register(1)),
                ),
                Jmp(Address(0)),
                Copy(
                    RegisterVariant::Constant(Register(3)),
                    RegisterVariant::Local(Register(1)),
                ),
            ];

            assert_eq!(walker_init_instructions(&mut walker), expected);
        }
    }

    mod test_unary_op {
        use super::*;

        async fn setup(op: UnaryOperation, is_post: bool) -> CodegenWalker {
            let mut walker = default_walker();
            let mut node = UnaryOpNode {
                op,
                expr: Box::new(ExpressionNode::from(666)),
                span: None,
                is_post,
            };

            let _ = walker.visit_unary_op(&mut node).await;
            walker
        }

        /// `op` applied to a variable living in local register 9.
        async fn setup_on_var(op: UnaryOperation, is_post: bool) -> CodegenWalker {
            let mut context = CompilationContext::default();
            context.scopes.push_new();
            let mut sym = Symbol::new("x", LpcType::Int(false));
            sym.location = Some(RegisterVariant::Local(Register(9)));
            context.scopes.current_mut().unwrap().insert(sym);
            let mut walker = CodegenWalker::new(context);

            let mut node = UnaryOpNode {
                op,
                expr: Box::new(ExpressionNode::Var(VarNode {
                    name: ustr("x"),
                    span: None,
                    global: false,
                    function_name: false,
                })),
                span: None,
                is_post,
            };

            let _ = walker.visit_unary_op(&mut node).await;
            walker
        }

        mod negate {
            use super::*;

            #[tokio::test]
            async fn populates_instructions() {
                let mut walker = setup(UnaryOperation::Negate, false).await;

                let expected = vec![MMul(
                    RegisterVariant::Constant(Register(0)),
                    RegisterVariant::Constant(Register(1)),
                    RegisterVariant::Local(Register(1)),
                )];

                assert_eq!(walker_init_instructions(&mut walker), expected);
            }
        }

        mod inc {
            use super::*;

            #[tokio::test]
            async fn populates_instructions_for_pre() {
                let mut walker = setup_on_var(UnaryOperation::Inc, false).await;

                let expected = vec![Inc(RegisterVariant::Local(Register(9)))];

                assert_eq!(walker_init_instructions(&mut walker), expected);
            }

            #[tokio::test]
            async fn populates_instructions_for_post() {
                let mut walker = setup_on_var(UnaryOperation::Inc, true).await;

                let expected = vec![
                    Copy(
                        RegisterVariant::Local(Register(9)),
                        RegisterVariant::Local(Register(1)),
                    ),
                    Inc(RegisterVariant::Local(Register(9))),
                ];

                assert_eq!(walker_init_instructions(&mut walker), expected);
            }
        }

        mod dec {
            use super::*;

            #[tokio::test]
            async fn populates_instructions_for_pre() {
                let mut walker = setup_on_var(UnaryOperation::Dec, false).await;

                let expected = vec![Dec(RegisterVariant::Local(Register(9)))];

                assert_eq!(walker_init_instructions(&mut walker), expected);
            }

            #[tokio::test]
            async fn populates_instructions_for_post() {
                let mut walker = setup_on_var(UnaryOperation::Dec, true).await;

                let expected = vec![
                    Copy(
                        RegisterVariant::Local(Register(9)),
                        RegisterVariant::Local(Register(1)),
                    ),
                    Dec(RegisterVariant::Local(Register(9))),
                ];

                assert_eq!(walker_init_instructions(&mut walker), expected);
            }
        }

        mod bang {
            use super::*;

            #[tokio::test]
            async fn populates_instructions() {
                let mut walker = setup(UnaryOperation::Bang, false).await;

                let expected = vec![Not(
                    RegisterVariant::Constant(Register(0)),
                    RegisterVariant::Local(Register(1)),
                )];

                assert_eq!(walker_init_instructions(&mut walker), expected);
            }
        }

        mod bitwise_not {
            use super::*;

            #[tokio::test]
            async fn populates_instructions() {
                let mut walker = setup(UnaryOperation::BitwiseNot, false).await;

                let expected = vec![BitwiseNot(
                    RegisterVariant::Constant(Register(0)),
                    RegisterVariant::Local(Register(1)),
                )];

                assert_eq!(walker_init_instructions(&mut walker), expected);
            }
        }
    }

    mod test_visit_var {
        use indoc::indoc;

        use super::*;
        use crate::test_support::compile_prog;

        #[tokio::test]
        async fn test_visit_var_loads_the_var_and_sets_the_result_for_globals() {
            let mut context = CompilationContext::default();
            context.scopes.push_new();

            let mut walker = CodegenWalker::new(context);

            insert_symbol(
                &mut walker,
                Symbol {
                    name: "marf".to_string(),
                    type_: LpcType::Int(false),
                    location: Some(RegisterVariant::Global(Register(666))),
                    ..Default::default()
                },
            );

            let mut node = VarNode {
                name: ustr("marf"),
                span: None,
                global: true,
                function_name: false,
            };

            let _ = walker.visit_var(&mut node).await;
            assert_eq!(
                walker.current_result,
                RegisterVariant::Global(Register(666))
            );

            let expected = vec![];
            assert_eq!(walker_init_instructions(&mut walker), expected);
        }

        #[tokio::test]
        async fn test_visit_var_sets_the_result_for_locals() {
            let mut context = CompilationContext::default();
            context.scopes.push_new();
            let mut walker = CodegenWalker::new(context);

            insert_symbol(
                &mut walker,
                // push a global marf to ensure we don't find it.
                Symbol {
                    name: "marf".to_string(),
                    type_: LpcType::Int(false),
                    location: Some(RegisterVariant::Local(Register(444))),
                    ..Default::default()
                },
            );
            let local_id = walker.context.scopes.push_new(); // push a local scope
            insert_symbol(
                &mut walker,
                Symbol {
                    name: "marf".to_string(),
                    type_: LpcType::Int(false),
                    location: Some(RegisterVariant::Local(Register(666))),
                    scope_id: Some(local_id),
                    ..Default::default()
                },
            );

            let mut node = VarNode::new("marf");

            let _ = walker.visit_var(&mut node).await;
            assert_eq!(walker.current_result, RegisterVariant::Local(Register(666)));

            let expected = vec![];
            assert_eq!(walker_init_instructions(&mut walker), expected);
        }

        #[tokio::test]
        async fn test_closure_positional_arguments() {
            let code = indoc! { r##"
                function maker() {
                    return (: [int i] dump("i", $1); (: i :) :);
                }

                void create() {
                    function f = maker();
                    mixed i = f(1);
                }
            "## };

            let (prog, _, _) = compile_prog(code).await;

            // `closure-1` is the outer closure that refers to $1.
            let instructions = &find_function(&prog.functions, "closure-1")
                .unwrap()
                .instructions;
            let expected = vec![
                PushArg(RegisterVariant::Constant(Register(0))),
                PushArg(RegisterVariant::Upvalue(Register(0))),
                /* This is what we're really testing for */
                CallEfun(15),
                // ...etc. We don't care about the rest.
            ];
            assert_eq!(&instructions[0..=2], expected);
        }
    }

    mod test_visit_var_init {
        use lpc_rs_asm::instruction::Instruction::MapConst;

        use super::*;

        fn setup() -> CodegenWalker {
            let mut context = CompilationContext::default();
            context.scopes.push_new();
            context.scopes.push_new();
            CodegenWalker::new(context)
        }

        async fn setup_var(type_: LpcType, walker: &mut CodegenWalker) {
            let scope_id = walker.context.scopes.current().unwrap().id;

            let sym = Symbol {
                location: Some(Register(1).as_local()),
                scope_id,
                ..Symbol::new("marf", type_)
            };
            walker.register_counter.next(); // force-increment to mimic the scope walker
            insert_symbol(walker, sym);

            let mut node = VarInitNode {
                type_,
                name: ustr("muffins"),
                value: Some(ExpressionNode::Var(VarNode::new("marf"))),
                array: false,
                global: false,
                span: None,
                flags: None,
                by_ref: false,
            };

            let mut new_sym = Symbol::from(&mut node.clone());
            new_sym.scope_id = scope_id;
            insert_symbol(walker, new_sym);

            let _ = walker.visit_var_init(&mut node).await;
        }

        async fn setup_literal(type_: LpcType, value: ExpressionNode, walker: &mut CodegenWalker) {
            let mut node = VarInitNode {
                type_,
                name: ustr("muffins"),
                value: Some(value),
                array: false,
                global: false,
                span: None,
                flags: None,
                by_ref: false,
            };

            insert_symbol(walker, Symbol::from(&mut node));

            let _ = walker.visit_var_init(&mut node).await;
        }

        #[tokio::test]
        async fn test_does_not_copy_mapping_literals() {
            let mut walker = setup();
            let pairs = vec![(ExpressionNode::from("foo"), ExpressionNode::from("bar"))];
            setup_literal(
                LpcType::Mapping(false),
                ExpressionNode::Mapping(MappingNode::new(pairs, None)),
                &mut walker,
            )
            .await;

            assert_eq!(
                walker_init_instructions(&mut walker),
                [
                    PushArrayItem(RegisterVariant::Constant(Register(0))),
                    PushArrayItem(RegisterVariant::Constant(Register(1))),
                    MapConst(RegisterVariant::Local(Register(1))),
                ]
            );
        }

        #[tokio::test]
        async fn test_copies_mapping_vars() {
            let mut walker = setup();
            setup_var(LpcType::Mapping(false), &mut walker).await;

            assert_eq!(
                walker_init_instructions(&mut walker),
                [Copy(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Local(Register(2))
                )]
            );
        }

        #[tokio::test]
        async fn copies_int_literals_from_the_pool() {
            let mut walker = setup();
            setup_literal(
                LpcType::Int(false),
                ExpressionNode::Int(IntNode::new(123)),
                &mut walker,
            )
            .await;

            assert_eq!(
                walker_init_instructions(&mut walker),
                [Copy(
                    RegisterVariant::Constant(Register(0)),
                    RegisterVariant::Local(Register(1))
                )]
            );
        }

        #[tokio::test]
        async fn test_copies_int_vars() {
            let mut walker = setup();
            setup_var(LpcType::Int(false), &mut walker).await;

            assert_eq!(
                walker_init_instructions(&mut walker),
                [Copy(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Local(Register(2))
                )]
            );
        }

        #[tokio::test]
        async fn copies_float_literals_from_the_pool() {
            let mut walker = setup();
            setup_literal(
                LpcType::Float(false),
                ExpressionNode::Float(FloatNode::new(123.0)),
                &mut walker,
            )
            .await;

            assert_eq!(
                walker_init_instructions(&mut walker),
                [Copy(
                    RegisterVariant::Constant(Register(0)),
                    RegisterVariant::Local(Register(1))
                )]
            );
        }

        #[tokio::test]
        async fn test_copies_float_vars() {
            let mut walker = setup();
            setup_var(LpcType::Float(false), &mut walker).await;

            assert_eq!(
                walker_init_instructions(&mut walker),
                [Copy(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Local(Register(2))
                )]
            );
        }

        #[tokio::test]
        async fn copies_string_literals_from_the_pool() {
            let mut walker = setup();
            setup_literal(
                LpcType::Int(true),
                ExpressionNode::String(StringNode::new("foo")),
                &mut walker,
            )
            .await;

            assert_eq!(
                walker_init_instructions(&mut walker),
                [Copy(
                    RegisterVariant::Constant(Register(0)),
                    RegisterVariant::Local(Register(1))
                )]
            );
        }

        #[tokio::test]
        async fn test_copies_string_vars() {
            let mut walker = setup();
            setup_var(LpcType::String(false), &mut walker).await;

            assert_eq!(
                walker_init_instructions(&mut walker),
                [Copy(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Local(Register(2))
                )]
            );
        }

        #[tokio::test]
        async fn test_does_not_copy_array_literals() {
            let mut walker = setup();
            setup_literal(
                LpcType::Int(true),
                ExpressionNode::Array(ArrayNode::new(vec![ExpressionNode::from(1234)])),
                &mut walker,
            )
            .await;

            assert_eq!(
                walker_init_instructions(&mut walker),
                [
                    PushArrayItem(RegisterVariant::Constant(Register(0))),
                    AConst(RegisterVariant::Local(Register(1))),
                ]
            );
        }

        #[tokio::test]
        async fn test_copies_array_vars() {
            let mut walker = setup();
            setup_var(LpcType::Int(true), &mut walker).await;

            assert_eq!(
                walker_init_instructions(&mut walker),
                [Copy(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Local(Register(2))
                )]
            );
        }

        #[tokio::test]
        async fn copies_calls() {
            let mut walker = setup();

            let mut node = VarInitNode {
                type_: LpcType::Object(false),
                name: ustr("muffins"),
                value: Some(ExpressionNode::Call(CallNode {
                    chain: create!(CallChain, name: ustr("clone_object")),
                    arguments: vec![ExpressionNode::from("/foo/bar.c")],
                    span: None,
                })),
                array: false,
                global: false,
                span: None,
                flags: None,
                by_ref: false,
            };

            insert_symbol(&mut walker, Symbol::from(&mut node.clone()));

            let _ = walker.visit_var_init(&mut node).await;

            assert_eq!(
                walker_init_instructions(&mut walker),
                [
                    PushArg(RegisterVariant::Constant(Register(0))),
                    CallEfun(8),
                    Copy(
                        RegisterVariant::Local(Register(0)),
                        RegisterVariant::Local(Register(1))
                    ),
                ]
            );
        }

        #[tokio::test]
        async fn sets_up_globals() {
            let mut context = CompilationContext::default();
            context.scopes.push_new();
            let mut walker = CodegenWalker::new(context);

            let mut node = VarInitNode {
                type_: LpcType::Mixed(true),
                name: ustr("arr"),
                value: Some(ExpressionNode::from(vec![
                    ExpressionNode::from(12),
                    ExpressionNode::from(4.3),
                    ExpressionNode::from("hello"),
                    ExpressionNode::from(vec![
                        ExpressionNode::from(1),
                        ExpressionNode::from(2),
                        ExpressionNode::from(3),
                    ]),
                ])),
                array: false,
                global: true,
                span: None,
                flags: None,
                by_ref: false,
            };

            insert_symbol(&mut walker, Symbol::from(&mut node.clone()));

            let mut node2 = VarInitNode {
                type_: LpcType::Mixed(true),
                name: ustr("str"),
                value: Some(ExpressionNode::from("sup")),
                array: false,
                global: true,
                span: None,
                flags: None,
                by_ref: false,
            };

            insert_symbol(&mut walker, Symbol::from(&mut node.clone()));
            insert_symbol(&mut walker, Symbol::from(&mut node2.clone()));

            let _ = walker.visit_var_init(&mut node).await;
            let _ = walker.visit_var_init(&mut node2).await;

            let expected = vec![
                PushArrayItem(RegisterVariant::Constant(Register(3))),
                PushArrayItem(RegisterVariant::Constant(Register(4))),
                PushArrayItem(RegisterVariant::Constant(Register(5))),
                AConst(RegisterVariant::Local(Register(1))),
                PushArrayItem(RegisterVariant::Constant(Register(0))),
                PushArrayItem(RegisterVariant::Constant(Register(1))),
                PushArrayItem(RegisterVariant::Constant(Register(2))),
                PushArrayItem(RegisterVariant::Local(Register(1))),
                AConst(RegisterVariant::Local(Register(2))),
                Copy(
                    RegisterVariant::Local(Register(2)),
                    RegisterVariant::Global(Register(0)),
                ),
                Copy(
                    RegisterVariant::Constant(Register(6)),
                    RegisterVariant::Global(Register(1)),
                ),
            ];

            assert_eq!(walker_init_instructions(&mut walker), expected);
            assert_eq!(walker.global_counter.number_emitted(), 2);
        }

        #[tokio::test]
        async fn copies_an_upvalued_var_into_its_cell() {
            let mut context = CompilationContext::default();
            context.scopes.push_new(); // push a global scope
            context.scopes.push_new(); // push a local scope
            let mut walker = CodegenWalker::new(context);

            let existing_name = "existing";

            let mut node = create!(
                VarInitNode,
                name: ustr("a"),
                value: Some(ExpressionNode::from(create!(VarNode, name: ustr(existing_name)))),
            );

            let mut sym = Symbol::from(&mut node.clone());
            sym.upvalue = true;
            sym.location = Some(RegisterVariant::Upvalue(Register(3)));

            let symbol_factory = SymbolFactory::new();
            let mut existing = symbol_factory.build(|sym| sym.name = existing_name.to_string());
            existing.location = Some(RegisterVariant::Local(Register(1)));

            insert_symbol(&mut walker, existing);
            insert_symbol(&mut walker, sym);

            walker.visit_var_init(&mut node).await.unwrap();

            assert_eq!(
                walker_init_instructions(&mut walker).last(),
                Some(&Instruction::Copy(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Upvalue(Register(3))
                ))
            );
            let sym = walker.context.lookup_var("a").unwrap();
            assert_eq!(sym.location.unwrap(), RegisterVariant::Upvalue(Register(3)));
        }

        #[tokio::test]
        async fn copies_an_upvalued_value_into_its_cell() {
            let mut context = CompilationContext::default();
            context.scopes.push_new(); // push a global scope
            context.scopes.push_new(); // push a local scope
            let mut walker = CodegenWalker::new(context);

            let mut node = create!(
                VarInitNode,
                name: ustr("a"),
                value: Some(ExpressionNode::from(666))
            );

            let mut sym = Symbol::from(&mut node.clone());
            sym.upvalue = true;
            sym.location = Some(RegisterVariant::Upvalue(Register(3)));

            insert_symbol(&mut walker, sym);

            walker.visit_var_init(&mut node).await.unwrap();

            assert!(matches!(
                walker_init_instructions(&mut walker).last(),
                Some(Instruction::Copy(_, RegisterVariant::Upvalue(Register(3))))
            ));
        }
    }

    mod test_visit_while {
        use lpc_rs_asm::instruction::Instruction::{EqEq, Jmp, Jz};

        use super::*;

        #[tokio::test]
        async fn test_populates_the_instructions() {
            let mut walker = default_walker();
            walker.backpatch_maps.push(HashMap::new());

            let mut node = WhileNode {
                condition: ExpressionNode::BinaryOp(BinaryOpNode {
                    l: Box::new(ExpressionNode::from(666)),
                    r: Box::new(ExpressionNode::from(777)),
                    op: BinaryOperation::EqEq,
                    span: None,
                }),
                body: Box::new(AstNode::Call(CallNode {
                    chain: create!(CallChain, name: ustr("dump")),
                    arguments: vec![ExpressionNode::from("body")],
                    span: None,
                })),
                scope_id: None,
                span: None,
            };

            let _ = walker.visit_while(&mut node).await;

            let expected = vec![
                EqEq(
                    RegisterVariant::Constant(Register(0)),
                    RegisterVariant::Constant(Register(1)),
                    RegisterVariant::Local(Register(1)),
                ),
                Jz(RegisterVariant::Local(Register(1)), Address(0)),
                PushArg(RegisterVariant::Constant(Register(2))),
                CallEfun(15),
                Jmp(Address(0)),
            ];

            assert_eq!(walker_init_instructions(&mut walker), expected);
        }
    }

    mod test_into_program {
        use super::*;

        #[tokio::test]
        async fn sets_num_globals() {
            let code = r##"
                int i = 123, j;
                mixed *arr = ({ "foo", "bar", "baz", ({ "quux", 0 }) });
                string asdf = "asdf";
                string b;
            "##;

            let program = walk_prog(code)
                .await
                .into_program()
                .expect("failed to compile");
            assert_eq!(program.num_globals, 5)
        }

        #[tokio::test]
        async fn sizes_the_global_initializer() {
            let code = r##"
                int i = 123, j;
                mixed *arr = ({ "foo", "bar", "baz", ({ "quux", 0 }) });
                string asdf = "asdf";
                string b;
            "##;

            let program = walk_prog(code)
                .await
                .into_program()
                .expect("failed to compile");
            let init_globals = program
                .functions
                .values()
                .find(|f| f.name() == INIT_GLOBALS)
                .unwrap();
            assert_eq!(init_globals.num_locals, 2)
        }

        #[tokio::test]
        async fn sets_strings_on_functions() {
            let code = r##"
                int create() {
                    dump("sup dawg");
                    int b = 123;
                    return b;
                }
            "##;

            let program = walk_prog(code)
                .await
                .into_program()
                .expect("failed to compile");
            let create = program
                .functions
                .values()
                .find(|f| f.name() == CREATE_FUNCTION)
                .unwrap();
            assert!(create.constants.iter().any(|c| matches!(
                c,
                lpc_rs_function_support::constant::LpcConstant::String(s) if s.to_str() == "sup dawg"
            )));
        }
    }

    #[tokio::test]
    async fn sibling_parents_are_imported_with_shifted_globals() {
        let code = r##"
            inherit "/sibling_a";
            inherit "/sibling_b";
            int own = 5;
        "##;
        let program = walk_prog(code).await.into_program().unwrap();

        // sibling_a declares 4 globals, so sibling_b's first global follows them.
        assert_eq!(
            program.global_variables["sb"].location,
            Some(RegisterVariant::Global(Register(4)))
        );
        assert_eq!(
            program.global_variables["own"].location,
            Some(RegisterVariant::Global(Register(8)))
        );

        let b_init = &program.functions["init-globals__v__/sibling_b.c__pv__"];
        let globals_written: Vec<_> = b_init
            .instructions
            .iter()
            .filter_map(|i| match i.dest_register() {
                Some(RegisterVariant::Global(r)) => Some(r.index()),
                _ => None,
            })
            .collect();
        assert_eq!(globals_written, vec![4, 5, 6]);

        let own_init = &program.functions["init-globals__v____pv__"];
        assert!(own_init.instructions.iter().all(|i| !matches!(i, Call(_))));

        assert_eq!(
            program.initializer.unwrap().instructions,
            vec![
                Call(ustr("init-globals__v__/sibling_a.c__pv__")),
                Call(ustr("init-globals__v__/sibling_b.c__pv__")),
                Call(ustr("init-globals__v____pv__")),
                Call(ustr("create__v__/sibling_b.c__pb__")),
                Ret,
            ]
        );
    }

    #[tokio::test]
    async fn a_program_reached_through_two_parents_has_one_block() {
        let code = r##"
            inherit "/diamond_left";
            inherit "/diamond_right";
            int own = 5;
        "##;
        let program = walk_prog(code).await.into_program().unwrap();

        // grandparent's 3 slots are shared, so `own` follows them directly.
        assert_eq!(program.num_globals, 4);
        assert_eq!(
            program.global_variables["own"].location,
            Some(RegisterVariant::Global(Register(3)))
        );
        let blocks: Vec<_> = program
            .layout
            .iter()
            .map(|r| (r.filename.to_string(), r.base, r.count))
            .collect();
        assert_eq!(
            blocks,
            [
                ("/grandparent.c".to_string(), 0, 3),
                ("/diamond_left.c".to_string(), 3, 0),
                ("/diamond_right.c".to_string(), 3, 0),
                (program.filename.to_string(), 3, 1),
            ]
        );

        let set_left_a = &program.functions["set_left_a__v__/diamond_left.c__pb__i"];
        assert!(
            set_left_a
                .instructions
                .iter()
                .any(|i| { i.dest_register() == Some(RegisterVariant::Global(Register(0))) })
        );

        assert_eq!(
            program.initializer.unwrap().instructions,
            vec![
                Call(ustr("init-globals__v__/grandparent.c__pv__")),
                Call(ustr("init-globals__v__/diamond_left.c__pv__")),
                Call(ustr("init-globals__v__/diamond_right.c__pv__")),
                Call(ustr("init-globals__v____pv__")),
                Call(ustr("create__v__/grandparent.c__pb__")),
                Ret,
            ]
        );
    }

    #[tokio::test]
    async fn tracks_inherited_globals_for_init() {
        let code = r##"
            inherit "/parent";
            int i = 123, j;
            string asdf = "asdf";
            string b;
        "##;

        let program = walk_prog(code)
            .await
            .into_program()
            .expect("failed to compile");
        let init_globals = program
            .functions
            .values()
            .find(|f| f.name() == INIT_GLOBALS)
            .unwrap();

        assert_eq!(program.num_globals, 9);
        // Only this program's own initializers; the parent's run in their own frame.
        assert_eq!(init_globals.num_locals, 0);
    }

    fn insert_symbol(walker: &mut CodegenWalker, symbol: Symbol) {
        walker
            .context
            .scopes
            .current_mut()
            .expect("No current scope to insert the symbol into.")
            .insert(symbol)
    }

    mod references {
        // `Instruction::*` is already in scope via `super::*` (outer `mod tests`).
        use super::*;

        #[tokio::test]
        async fn a_ref_argument_pushes_its_cell() {
            let mut walker =
                walk_prog("void inc(int ref x) { x++; } void f() { int y = 1; inc(ref y); }").await;
            let instructions = walker_function_instructions(&mut walker, "f");
            assert!(
                instructions.contains(&PushRef(RegisterVariant::Upvalue(Register(0)))),
                "{instructions:?}"
            );
            assert!(instructions.contains(&NewUpvalue(RegisterVariant::Upvalue(Register(0)))));
            assert!(!instructions.iter().any(|i| matches!(i, PushArg(_))));
        }

        #[tokio::test]
        async fn a_ref_of_a_global_pushes_the_global() {
            let mut walker =
                walk_prog("int g; void inc(int ref x) { x++; } void f() { inc(ref g); }").await;
            let instructions = walker_function_instructions(&mut walker, "f");
            assert!(
                instructions.contains(&PushRef(RegisterVariant::Global(Register(0)))),
                "{instructions:?}"
            );
        }

        #[tokio::test]
        async fn a_ref_parameter_gets_no_fresh_cell() {
            let mut walker = walk_prog("void inc(int ref x) { x++; }").await;
            let instructions = walker_function_instructions(&mut walker, "inc");
            assert!(
                !instructions.iter().any(|i| matches!(i, NewUpvalue(_))),
                "{instructions:?}"
            );
            let inc = walker
                .functions
                .values()
                .find(|f| f.name() == "inc")
                .unwrap();
            assert_eq!(
                inc.arg_locations,
                vec![RegisterVariant::Upvalue(Register(0))]
            );
            assert_eq!(inc.num_upvalues, 1);
        }

        #[tokio::test]
        async fn an_implicit_efun_lvalue_pushes_its_cell() {
            let mut walker = walk_prog(r#"void f() { int n; sscanf("1", "%d", n); }"#).await;
            let instructions = walker_function_instructions(&mut walker, "f");
            assert!(
                instructions
                    .iter()
                    .any(|i| matches!(i, PushRef(RegisterVariant::Upvalue(_)))),
                "{instructions:?}"
            );
            assert_eq!(
                instructions
                    .iter()
                    .filter(|i| matches!(i, PushArg(_)))
                    .count(),
                2,
                "{instructions:?}"
            );
        }
    }
}
