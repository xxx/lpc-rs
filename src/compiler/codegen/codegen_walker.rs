use std::{collections::HashMap, ops::Range, rc::Rc};

use bit_set::BitSet;
use if_chain::if_chain;
use indexmap::IndexMap;
use lpc_rs_asm::{
    address::{Address, Label},
    instruction::Instruction,
};
use lpc_rs_core::{
    call_namespace::CallNamespace,
    function::{FunctionName, FunctionReceiver},
    function_arity::FunctionArity,
    lpc_type::LpcType,
    mangle::Mangle,
    register::{Register, RegisterVariant},
    register_counter::RegisterCounter,
    ScopeId, CREATE_FUNCTION, INIT_PROGRAM,
};
use lpc_rs_errors::{span::Span, LpcError, Result};
use lpc_rs_function_support::{
    function_prototype::FunctionPrototypeBuilder,
    program_function::{ProgramFunction, ProgramFunctionBuilder},
    symbol::Symbol,
};
use lpc_rs_utils::string::closure_arg_number;
use qcell::QCellOwner;
use tracing::{instrument, trace};
use tree_walker::TreeWalker;

use crate::{
    compiler::{
        ast::{
            array_node::ArrayNode,
            assignment_node::AssignmentNode,
            ast_node::{AstNode, AstNodeTrait, SpannedNode},
            binary_op_node::{BinaryOpNode, BinaryOperation},
            block_node::BlockNode,
            break_node::BreakNode,
            call_node::CallNode,
            closure_node::ClosureNode,
            continue_node::ContinueNode,
            decl_node::DeclNode,
            do_while_node::DoWhileNode,
            expression_node::ExpressionNode,
            float_node::FloatNode,
            for_each_node::{ForEachInit, ForEachNode, FOREACH_INDEX, FOREACH_LENGTH},
            for_node::ForNode,
            function_def_node::{FunctionDefNode, ARGV},
            function_ptr_node::{FunctionPtrNode, FunctionPtrReceiver},
            if_node::IfNode,
            int_node::IntNode,
            label_node::LabelNode,
            mapping_node::MappingNode,
            program_node::ProgramNode,
            range_node::RangeNode,
            return_node::ReturnNode,
            string_node::StringNode,
            switch_node::SwitchNode,
            ternary_node::TernaryNode,
            unary_op_node::{UnaryOpNode, UnaryOperation},
            var_init_node::VarInitNode,
            var_node::VarNode,
            while_node::WhileNode,
        },
        codegen::{tree_walker, tree_walker::ContextHolder},
        compilation_context::CompilationContext,
    },
    interpreter::{
        efun::{CALL_OTHER, CATCH, EFUN_PROTOTYPES, SIZEOF},
        program::Program,
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

/// a pair to store where to jump to in the case of a `break` or `continue`
#[derive(Debug)]
struct JumpTarget {
    pub break_target: Label,
    pub continue_target: Label,
}

impl JumpTarget {
    fn new(break_target: Label, continue_target: Label) -> Self {
        Self {
            break_target,
            continue_target,
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

/// A tree walker that generates assembly language instructions based on an AST.
#[derive(Debug)]
pub struct CodegenWalker {
    /// Keep track of the current function being generated (including global
    /// initialization)
    function_stack: Vec<ProgramFunction>,

    /// Stack of HashMaps, with keys being Labels, and values being a set of indices of
    /// [`Instruction`]s that need to be patched once the labels have known [`Address`]es.
    backpatch_maps: Vec<HashMap<Label, BitSet>>,

    /// Track the currently-processing closure, so we know where to copy
    /// captured variable Symbols.
    closure_scope_stack: Vec<ScopeId>,

    /// Counter for labels, as they need to be unique.
    label_count: usize,

    /// Function table. The keys are the mangled names of the functions.
    pub functions: HashMap<String, Rc<ProgramFunction>>,

    /// The initialization function for the program, which sets up global variables.
    initializer: Option<Rc<ProgramFunction>>,

    /// Track where the result of a child branch is
    current_result: RegisterVariant,

    /// Internal counter to track which registers are used.
    register_counter: RegisterCounter,

    /// Counter for tracking globals
    global_counter: RegisterCounter,

    /// Counter for tracking upvalues.
    /// This counter is used to track the count of upvalues from within the
    /// entirety of a static function, including all nested closures.
    upvalue_counter: RegisterCounter,

    /// Counter for tracking upvalues.
    /// This counter is used to track the count of upvalues from within a single
    /// function or closure.
    function_upvalue_counter: RegisterCounter,

    /// Number of [`Register`]s needed for global initialization.
    /// This counter contains the total number for *all* inherited init-program
    /// functions.
    // TODO: this is likely a vestige of the past, and this same value should be
    //       calculated as normal in the `init-program` function.
    global_init_registers: usize,

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
        let num_init_registers = context.num_init_registers;

        let mut result = Self {
            context,
            ..Self::default()
        };

        result.global_counter.set(num_globals);

        result.global_init_registers = num_init_registers;
        result.register_counter.set(num_init_registers);

        result.setup_init();

        result
    }

    /// Create the combined initialization function, with code taken from all
    /// of our inherited-from parents.
    #[instrument(skip_all)]
    pub fn setup_init(&mut self) {
        let prototype = FunctionPrototypeBuilder::default()
            .name(INIT_PROGRAM)
            .filename(self.context.filename.clone())
            .return_type(LpcType::Void)
            .build()
            .expect("Failed to build init prototype");

        let mut new_init_instructions = vec![];
        let mut new_init_debug_spans = vec![];
        self.combine_inits(&mut new_init_instructions, &mut new_init_debug_spans);

        let func = ProgramFunctionBuilder::default()
            .prototype(prototype)
            .instructions(new_init_instructions)
            .debug_spans(new_init_debug_spans)
            .build()
            .expect("Failed to build init function");

        self.function_stack.push(func);
    }

    /// Consume this walker and convert it into a [`Program`]
    pub fn into_program(mut self) -> Result<Program> {
        // These are expected and assumed to be in 1:1 correspondence at runtime
        self.ensure_sync()?;

        self.context.scopes.goto_root();
        let global_variables =
            std::mem::take(&mut self.context.scopes.current_mut().unwrap().symbols);

        let strings = Rc::new(self.context.strings.into_iter().collect::<Vec<_>>());
        for func in self.functions.values() {
            func.strings.set(strings.clone()).unwrap();
        }
        self.initializer
            .as_ref()
            .unwrap()
            .strings
            .set(strings.clone())
            .unwrap();

        let functions: IndexMap<_, _> = self
            .context
            .inherited_functions
            .into_iter()
            .chain(self.functions.into_iter())
            .collect();

        // Note that due to name clashes, only the latest seen version of a function is included,
        // but that should be fine, as they are inserted in the order they are processed.
        let unmangled_functions = functions
            .values()
            .map(|f| (f.prototype.name.to_string(), f.clone()))
            .collect();

        Ok(Program {
            filename: self.context.filename.clone(),
            functions,
            initializer: self.initializer,
            unmangled_functions,
            global_variables,
            num_globals: self.global_counter.number_emitted(),
            // add +1 for r0, which is skipped
            num_init_registers: self.global_init_registers + 1,
            pragmas: self.context.pragmas,
            // inherits: self.context.inherits,
            // inherit_names: self.context.inherit_names,
            strings,
        })
    }

    fn ensure_sync(&self) -> Result<()> {
        for func in self.functions.values() {
            let a = func.instructions.len();
            let b = func.debug_spans.len();
            if a != b {
                return Err(LpcError::new_bug(format!(
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
    fn to_operation_type(&self, node: &ExpressionNode, cell_key: &QCellOwner) -> OperationType {
        match node {
            ExpressionNode::Int(_) | ExpressionNode::Float(_) => OperationType::Register,

            ExpressionNode::String(_)
            | ExpressionNode::Array(_)
            | ExpressionNode::Mapping(_)
            | ExpressionNode::Closure(_)
            | ExpressionNode::CommaExpression(_)
            | ExpressionNode::Range(_)
            | ExpressionNode::FunctionPtr(_) => OperationType::Memory,
            ExpressionNode::Assignment(node) => self.to_operation_type(&node.lhs, cell_key),
            ExpressionNode::Call(node) => {
                if let Some(func) =
                    self.context
                        .lookup_function_complete(&node.name, &node.namespace, cell_key)
                {
                    match func.prototype().return_type {
                        LpcType::Int(_) | LpcType::Float(_) => OperationType::Register,
                        _ => OperationType::Memory,
                    }
                } else {
                    OperationType::Memory
                }
            }
            ExpressionNode::BinaryOp(node) => {
                let left_type = self.to_operation_type(&node.l, cell_key);
                let right_type = self.to_operation_type(&node.r, cell_key);
                match (left_type, right_type) {
                    (OperationType::Register, OperationType::Register) => OperationType::Register,
                    _ => OperationType::Memory,
                }
            }
            ExpressionNode::Ternary(node) => {
                let body_type = self.to_operation_type(&node.body, cell_key);
                let else_type = self.to_operation_type(&node.else_clause, cell_key);
                match (body_type, else_type) {
                    (OperationType::Register, OperationType::Register) => OperationType::Register,
                    _ => OperationType::Memory,
                }
            }
            ExpressionNode::UnaryOp(node) => {
                let expr_type = self.to_operation_type(&node.expr, cell_key);

                if matches!(expr_type, OperationType::Register) {
                    OperationType::Register
                } else {
                    OperationType::Memory
                }
            }
            ExpressionNode::Var(v) => {
                match self.context.lookup_var(&v.name) {
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
        cell_key: &QCellOwner,
    ) -> Instruction {
        match node.op {
            BinaryOperation::Add => self.choose_num_or_mixed(
                node,
                || Instruction::IAdd(reg_left, reg_right, reg_result),
                || Instruction::MAdd(reg_left, reg_right, reg_result),
                cell_key,
            ),
            BinaryOperation::Sub => self.choose_num_or_mixed(
                node,
                || Instruction::ISub(reg_left, reg_right, reg_result),
                || Instruction::MSub(reg_left, reg_right, reg_result),
                cell_key,
            ),
            BinaryOperation::Mul => self.choose_num_or_mixed(
                node,
                || Instruction::IMul(reg_left, reg_right, reg_result),
                || Instruction::MMul(reg_left, reg_right, reg_result),
                cell_key,
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
            BinaryOperation::Lt => Instruction::Lt(reg_left, reg_right, reg_result),
            BinaryOperation::Lte => Instruction::Lte(reg_left, reg_right, reg_result),
            BinaryOperation::Gt => Instruction::Gt(reg_left, reg_right, reg_result),
            BinaryOperation::Gte => Instruction::Gte(reg_left, reg_right, reg_result),
            BinaryOperation::Shl => Instruction::Shl(reg_left, reg_right, reg_result),
            BinaryOperation::Shr => Instruction::Shr(reg_left, reg_right, reg_result),
        }
    }

    /// Allows for recursive determination of typed binary operator
    /// instructions, allowing choice between a numeric (i.e. held in
    /// registers) and mixed (i.e. tracked via references) Switching on the
    /// instructions lets us avoid some value lookups at runtime.
    fn choose_num_or_mixed<F, G>(
        &self,
        node: &BinaryOpNode,
        a: F,
        b: G,
        cell_key: &QCellOwner,
    ) -> Instruction
    where
        F: Fn() -> Instruction,
        G: Fn() -> Instruction,
    {
        let left_type = self.to_operation_type(&node.l, cell_key);
        let right_type = self.to_operation_type(&node.r, cell_key);

        match (left_type, right_type) {
            (OperationType::Register, OperationType::Register) => a(),
            _ => b(),
        }
    }

    /// A special case for function def parameters, where we don't want to
    /// generate code for default arguments - we just want to have it on
    /// hand to refer to when we generate code for calls.
    fn visit_parameter(&mut self, node: &VarInitNode) -> RegisterVariant {
        let loc = self.assign_sym_location(&node.name);

        if let Some(sym) = self.context.lookup_var(&node.name) {
            if matches!(loc, RegisterVariant::Upvalue(_)) {
                // increment the counter for parameters that are captured by closures
                self.function_upvalue_counter.next().unwrap();
            }
            let func = self.function_stack.last_mut().unwrap();
            func.local_variables.push(sym.clone())
        }

        loc
    }

    /// A helper to assign the next free [`Register`] to a [`Symbol`]
    /// of the given name, within the current scope.
    fn assign_sym_location(&mut self, name: &str) -> RegisterVariant {
        let Some(sym) = self.context.lookup_var_mut(name) else {
            return RegisterVariant::Local(Register(0));
        };

        let current_register = if sym.upvalue {
            self.upvalue_counter.next().unwrap().as_upvalue()
        } else if sym.is_global() {
            self.global_counter.next().unwrap().as_global()
        } else {
            self.register_counter.next().unwrap().as_local()
        };

        trace!("Assigning location {} to {}", current_register, sym);

        sym.location = Some(current_register);

        current_register
    }

    /// Emit the instruction(s) to take the range of an array or string
    /// # Arguments
    /// `reference` - The [`Register`] holding the reference to the ref we're
    /// taking a slice from. `node` - A reference to the [`RangeNode`] that
    /// holds the range of the slice we're taking.
    fn emit_range(
        &mut self,
        reference: RegisterVariant,
        node: &mut RangeNode,
        cell_key: &mut QCellOwner,
    ) -> Result<()> {
        let first_index = if let Some(expr) = &mut *node.l {
            expr.visit(self, cell_key)?;
            self.current_result
        } else {
            // Default to 0. No instruction needed as the value in registers defaults to int
            // 0.
            self.register_counter.next().unwrap().as_local()
        };

        let second_index = if let Some(expr) = &mut *node.r {
            expr.visit(self, cell_key)?;
            self.current_result
        } else {
            // A missing range end means just go to the end of the array.
            let register = self.register_counter.next().unwrap().as_local();
            let instruction = Instruction::IConst(register, -1);
            push_instruction!(self, instruction, node.span);
            register
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
        for (label, addresses) in backpatch_map {
            let Some(label_address) = function.labels.get(label) else {
                return Err(LpcError::new_bug(format!("Label `{}` not found in function `{}", label, function.name())));
            };

            for address in addresses {
                let Some(instruction) = function.instructions.get_mut(address) else {
                    return Err(LpcError::new_bug(format!("Instruction at address {} not found in function `{}", address, function.name())));
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
            return Err(LpcError::new_bug("No backpatch map found to append to".to_string()));
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
    fn emit_catch(&mut self, node: &mut CallNode, cell_key: &mut QCellOwner) -> Result<()> {
        let result_register = self.register_counter.next().unwrap().as_local();
        let label = self.new_label("catch_end");

        let start_address = self.current_address();
        push_instruction!(
            self,
            Instruction::CatchStart(result_register, Address(0)),
            node.span
        );

        for argument in &mut node.arguments {
            argument.visit(self, cell_key)?;
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
            .labels
            .insert(label.into(), address);
    }

    /// Create the combined initializer from all of my inherited-from parents.
    /// This method assumes that my immediate parents already have their own
    /// init functions correctly combined from *their* parents, etc.
    ///
    /// # Arguments
    /// * instructions: A mutable reference to a vector, where the combined
    ///   [`Instruction`]s will be stored. Done this way to avoid a lot of vector
    ///   creations in the recursion.
    /// * debug_spans: A mutable reference to a vector, where the debug [`Span`]s of
    ///   the combined instructions will be stored.
    fn combine_inits(
        &mut self,
        instructions: &mut Vec<Instruction>,
        debug_spans: &mut Vec<Option<Span>>,
    ) {
        let calls_create = |instructions: &[Instruction]| -> bool {
            let len = instructions.len();

            debug_assert!(CREATE_FUNCTION == "create"); // have to hardcode this below in the starts_with check
            if_chain! {
                if len > 1;
                if let Instruction::Call(name_idx) = &instructions[len - 2];
                if let Some(name) = self.context.strings.get_index(*name_idx);
                if name.starts_with("create__") && matches!(&instructions[len - 1], Instruction::Ret);
                then {
                    true
                } else {
                    false
                }
            }
        };

        let get_range = |instructions: &[Instruction]| -> Range<usize> {
            let len = instructions.len();
            // remove any calls to `create` from the inherited initializers -
            // we'll call it once at the end, if necessary.
            if calls_create(instructions) {
                0..len - 3 // drop ClearArgs, Call, and Ret
            } else if len > 0 && matches!(&instructions[len - 1], Instruction::Ret) {
                0..len - 1 // drop Ret
            } else {
                0..len
            }
        };

        let extend_instructions =
            |func: &Rc<ProgramFunction>,
             instructions: &mut Vec<Instruction>,
             debug_spans: &mut Vec<Option<Span>>| {
                let range = get_range(&func.instructions);
                if calls_create(instructions) {
                    instructions.truncate(instructions.len() - 3);
                    debug_spans.truncate(debug_spans.len() - 3);
                }
                instructions.extend(func.instructions[range.clone()].iter().cloned());
                debug_spans.extend(func.debug_spans[range].iter());
            };
        for inherit in &self.context.inherits {
            if let Some(func) = &inherit.initializer {
                extend_instructions(func, instructions, debug_spans);
            }
        }
    }

    // Get a reference to the current [`CompilationContext`]
    pub fn context(&self) -> &CompilationContext {
        &self.context
    }

    fn setup_populate_defaults(
        &mut self,
        span: Option<Span>,
        num_default_args: usize,
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
        passed_param_count: usize,
    ) -> Option<Address> {
        if ellipsis {
            let argv_location = self.assign_sym_location(ARGV);
            // yep, argv can be upvalued
            if matches!(argv_location, RegisterVariant::Upvalue(_)) {
                self.function_upvalue_counter.next().unwrap();
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

            result
        } else {
            None
        }
    }

    fn init_default_params(
        &mut self,
        parameters: &mut [VarInitNode],
        declared_arg_locations: &[RegisterVariant],
        span: Option<Span>,
        populate_defaults_index: Address,
        cell_key: &mut QCellOwner,
    ) -> Result<()> {
        let mut default_init_addresses = vec![];

        for (idx, parameter) in parameters.iter_mut().enumerate() {
            if let Some(value) = &mut parameter.value {
                default_init_addresses.push(self.current_address());

                // generate code for only the value, then copy by hand, because we
                // pre-generated locations of the parameters above.
                value.visit(self, cell_key)?;
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
            return Err(LpcError::new_bug("Invalid populate_defaults_index").with_span(span));
        }

        // jump back to the function now that defaults are populated.
        let instruction =
            Instruction::Jmp(populate_defaults_index + 1 + default_init_addresses.len());
        push_instruction!(self, instruction, span);

        Ok(())
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
            Err(LpcError::new("Invalid populate_argv_index").with_span(span))
        }
    }

    fn visit_parameters(&mut self, nodes: &[VarInitNode]) -> Vec<RegisterVariant> {
        nodes
            .iter()
            .map(|parameter| self.visit_parameter(parameter))
            .collect::<Vec<_>>()
    }
}

impl ContextHolder for CodegenWalker {
    fn into_context(self) -> CompilationContext {
        self.context
    }
}

impl TreeWalker for CodegenWalker {
    #[instrument(skip_all)]
    fn visit_array(&mut self, node: &mut ArrayNode, cell_key: &mut QCellOwner) -> Result<()> {
        let mut items = Vec::with_capacity(node.value.len());
        for member in &mut node.value {
            let _ = member.visit(self, cell_key);
            items.push(self.current_result);
        }

        let register = self.register_counter.next().unwrap().as_local();
        self.current_result = register;
        push_instruction!(self, Instruction::ClearArrayItems, node.span);
        for item in items.iter() {
            let instruction = Instruction::PushArrayItem(*item);
            push_instruction!(self, instruction, node.span);
        }
        push_instruction!(self, Instruction::AConst(register), node.span);

        Ok(())
    }

    #[instrument(skip_all)]
    fn visit_assignment(
        &mut self,
        node: &mut AssignmentNode,
        cell_key: &mut QCellOwner,
    ) -> Result<()> {
        node.rhs.visit(self, cell_key)?;
        let rhs_result = self.current_result;
        let lhs = &mut *node.lhs;

        match lhs {
            ExpressionNode::Var(_) => {
                lhs.visit(self, cell_key)?;
                let lhs_result = self.current_result;
                trace!("assignment: lhs: {}, rhs: {}", lhs_result, rhs_result);

                let assign = Instruction::Copy(rhs_result, lhs_result);

                push_instruction!(self, assign, node.span);

                self.current_result = lhs_result;
            }
            ExpressionNode::BinaryOp(BinaryOpNode {
                op: BinaryOperation::Index,
                ref mut l,
                ref mut r,
                ..
            }) => {
                l.visit(self, cell_key)?;
                let var_result = self.current_result;
                r.visit(self, cell_key)?;
                let index_result = self.current_result;

                let store = Instruction::Store(rhs_result, var_result, index_result);

                push_instruction!(self, store, node.span);

                self.current_result = rhs_result;
            }
            x => {
                return Err(
                    LpcError::new(format!("Attempt to assign to an invalid lvalue: `{x}`"))
                        .with_span(node.span),
                )
            }
        }

        Ok(())
    }

    #[instrument(skip_all)]
    fn visit_binary_op(
        &mut self,
        node: &mut BinaryOpNode,
        cell_key: &mut QCellOwner,
    ) -> Result<()> {
        node.l.visit(self, cell_key)?;
        let reg_left = self.current_result;

        match node.op {
            BinaryOperation::Index => {
                // Ranges need special handling that complicates this function otherwise, due to
                // the visit to node.r needing to handle multiple results.
                if let ExpressionNode::Range(range_node) = &mut *node.r {
                    self.emit_range(reg_left, range_node, cell_key)?;
                    return Ok(());
                }
            }
            BinaryOperation::AndAnd => {
                // Handle short-circuit behavior
                let end_label = self.new_label("andand-end");
                self.schedule_backpatch(&end_label, self.current_address())?;
                let instruction = Instruction::Jz(reg_left, Address(0));
                push_instruction!(self, instruction, node.span);

                node.r.visit(self, cell_key)?;
                let reg_right = self.current_result;
                self.schedule_backpatch(&end_label, self.current_address())?;
                let instruction = Instruction::Jz(reg_right, Address(0));
                push_instruction!(self, instruction, node.span);

                let reg_result = self.register_counter.next().unwrap().as_local();
                self.current_result = reg_result;

                let instruction = Instruction::Copy(reg_right, reg_result);
                push_instruction!(self, instruction, node.span);

                self.insert_label(end_label, self.current_address());

                return Ok(());
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

                node.r.visit(self, cell_key)?;
                let reg_right = self.current_result;
                let instruction = Instruction::Copy(reg_right, reg_result);
                push_instruction!(self, instruction, node.span);

                self.insert_label(end_label, self.current_address());
                self.current_result = reg_result;

                return Ok(());
            }
            _ => {}
        }

        node.r.visit(self, cell_key)?;
        let reg_right = self.current_result;

        let reg_result = self.register_counter.next().unwrap().as_local();
        self.current_result = reg_result;

        let instruction =
            self.choose_op_instruction(node, reg_left, reg_right, reg_result, cell_key);
        push_instruction!(self, instruction, node.span);

        Ok(())
    }

    #[instrument(skip_all)]
    fn visit_block(&mut self, node: &mut BlockNode, cell_key: &mut QCellOwner) -> Result<()> {
        self.context.scopes.goto(node.scope_id);

        for stmt in &mut node.body {
            stmt.visit(self, cell_key)?;
        }

        self.context.scopes.pop();
        Ok(())
    }

    #[instrument(skip_all)]
    fn visit_break(&mut self, node: &mut BreakNode, _cell_key: &mut QCellOwner) -> Result<()> {
        if let Some(JumpTarget { break_target, .. }) = self.jump_targets.last() {
            self.schedule_backpatch(&break_target.clone(), self.current_address())?;
            let instruction = Instruction::Jmp(0.into());
            push_instruction!(self, instruction, node.span);
            return Ok(());
        }

        Err(LpcError::new_bug("`break` statement without a jump target?").with_span(node.span))
    }

    #[instrument(skip_all)]
    fn visit_call(&mut self, node: &mut CallNode, cell_key: &mut QCellOwner) -> Result<()> {
        if node.name == CATCH {
            return self.emit_catch(node, cell_key);
        }

        let argument_len = node.arguments.len();
        let mut arg_results = Vec::with_capacity(argument_len);

        for argument in &mut node.arguments {
            argument.visit(self, cell_key)?;
            arg_results.push(self.current_result);
        }

        if node.name == SIZEOF {
            let result = self.register_counter.next().unwrap().as_local();
            let instruction = Instruction::Sizeof(*arg_results.first().unwrap(), result);
            push_instruction!(self, instruction, node.span);

            return Ok(());
        }

        let instruction = {
            push_instruction!(self, Instruction::ClearArgs, node.span);

            // populate the args vector
            for result in &arg_results {
                push_instruction!(self, Instruction::PushArg(*result), node.span);
            }

            if let Some(rcvr) = &mut node.receiver {
                rcvr.visit(self, cell_key)?;
                let receiver_result = self.current_result;

                let name_register = self.register_counter.next().unwrap().as_local();
                let (index, _) = self.context.strings.insert_full(node.name.clone());

                push_instruction!(self, Instruction::SConst(name_register, index), node.span);

                Instruction::CallOther(receiver_result, name_register)
            } else if node.name == CALL_OTHER {
                debug_assert!(
                    arg_results.len() >= 2,
                    "CallOther requires at least 2 arguments, for the receiver and function name"
                );
                let receiver = arg_results[0];
                let name_index = arg_results[1];

                Instruction::CallOther(receiver, name_index)
            } else {
                if_chain! {
                    if let Some(x) = self.context.lookup_var(&node.name);
                    if x.type_.matches_type(LpcType::Function(false));
                    then {
                        Instruction::CallFp(x.location.unwrap())
                    } else {
                        let Some(func) =
                            self.context.lookup_function_complete(&node.name, &node.namespace, cell_key) else {
                            return Err(LpcError::new_bug(
                                format!("Cannot find function during code gen: {}", node.name)
                            ).with_span(node.span));
                        };

                        if func.is_efun() {
                            let (idx, _) = self.context.strings.insert_full(node.name.clone());
                            Instruction::CallEfun(idx)
                        } else {
                            let (idx, _) = self.context.strings.insert_full(func.mangle());
                            Instruction::Call(idx)
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
                node.span()
            );

            walker.current_result = next_register;
        };

        // Take care of the result after the call returns.
        if let Some(func) =
            self.context
                .lookup_function_complete(&node.name, &node.namespace, cell_key)
        {
            if func.as_ref().return_type == LpcType::Void {
                self.current_result = Register(0).as_local();
            } else {
                push_copy(self);
            }
        } else if let Some(Symbol {
            type_: LpcType::Function(false) | LpcType::Mixed(false),
            ..
        }) = self.context.lookup_var(&node.name)
        {
            push_copy(self);
        } else if node.receiver.is_some()
            || matches!(
                self.context.scopes.lookup(&node.name),
                Some(Symbol {
                    type_: LpcType::Function(false),
                    ..
                })
            )
        {
            push_copy(self);
        } else {
            return Err(LpcError::new_bug(format!(
                "Unable to find the return type for `{}`. This is a weird issue that indicates \
                something very broken in the semantic checks, or that I'm not looking hard enough.",
                node.name
            ))
            .with_span(node.span));
        }

        Ok(())
    }

    #[instrument(skip_all)]
    fn visit_closure(&mut self, node: &mut ClosureNode, cell_key: &mut QCellOwner) -> Result<()> {
        let Some(prototype) = self.context.function_prototypes.get(&node.name) else {
            return Err(LpcError::new(format!(
                "closure prototype for {} not found",
                node.name
            ))
            .with_span(node.span));
        };

        let arity = prototype.arity;
        let num_args = arity.num_args;
        let num_default_args = arity.num_default_args;

        let func = ProgramFunction::new(prototype.clone(), 0);

        self.function_stack.push(func);
        self.backpatch_maps.push(HashMap::new());

        if let Some(scope_id) = node.scope_id {
            self.closure_scope_stack.push(scope_id);
        } else {
            return Err(
                LpcError::new(format!("closure scope for {} not found", node.name))
                    .with_span(node.span),
            );
        }

        let len = self.current_address();

        let parent_scope_id = self.context.scopes.current_id;

        self.register_counter.push();
        // Note that `upvalue_counter` is *not* pushed here.
        // We want to keep a consistent count of upvalues across all closures
        // that are declared somewhere within the static function
        self.function_upvalue_counter.push();

        self.context.scopes.goto(node.scope_id); // XXX difference between closure and function def
        let declared_arg_locations = node // XXX
            .parameters
            .as_ref()
            .map(|nodes| self.visit_parameters(nodes))
            .unwrap_or_default();
        let declared_arg_count = declared_arg_locations.len();

        self.closure_arg_locations.push(declared_arg_locations);

        let populate_defaults_index = self.setup_populate_defaults(node.span, num_default_args);

        // bump the register counter if they have used `$\d` vars that go beyond
        // declared parameters, so that the positional params point to the correct slot.
        let current_count = self.register_counter.number_emitted();
        if num_args > current_count {
            self.register_counter.set(num_args + 1);
            self.current_result = Register(num_args + 1).as_local();
        }

        let populate_argv_index =
            self.setup_populate_argv(node.flags.ellipsis(), node.span, declared_arg_count);

        let start_label = self.new_label("closure-body-start");
        self.insert_label(&start_label, self.current_address());

        for expression in &mut node.body {
            expression.visit(self, cell_key)?;
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

        if num_default_args > 0 {
            if let Some(parameters) = &mut node.parameters {
                debug_assert!(populate_defaults_index.is_some());

                self.init_default_params(
                    parameters,
                    &declared_arg_locations,
                    node.span,
                    populate_defaults_index.unwrap(),
                    cell_key,
                )?;
            }
        }

        self.context.scopes.pop();
        self.closure_scope_stack.pop();
        let mut func = self.function_stack.pop().unwrap();

        let (name, _) = self.context.strings.insert_full(func.prototype.name.to_string());

        func.num_locals = self.register_counter.number_emitted() - num_args;
        func.num_upvalues = self.function_upvalue_counter.number_emitted();
        func.arg_locations = declared_arg_locations;

        if let Some(idx) = populate_argv_index {
            Self::backpatch_populate_argv(&mut func, idx, node.span)?;
        }

        let backpatch_map = self.backpatch_maps.pop().unwrap();
        Self::backpatch(&backpatch_map, &mut func)?;

        let mangled = func.mangle();
        self.functions.insert(mangled.clone(), func.into());

        self.function_upvalue_counter.pop();
        self.register_counter.pop();

        self.context.scopes.goto(parent_scope_id);

        // At this point, the closure has been generated and stored.
        // We just need to store a reference to it in the current result.
        let location = self.register_counter.next().unwrap().as_local();
        self.current_result = location;

        // closures are just pointers to functions
        let instruction = Instruction::ClearPartialArgs;
        push_instruction!(self, instruction, node.span);
        let instruction = Instruction::FunctionPtrConst {
            location,
            receiver: FunctionReceiver::Local,
            name_index: name,
        };

        push_instruction!(self, instruction, node.span);

        Ok(())
    }

    #[instrument(skip_all)]
    fn visit_continue(
        &mut self,
        node: &mut ContinueNode,
        _cell_key: &mut QCellOwner,
    ) -> Result<()> {
        if let Some(JumpTarget {
            continue_target, ..
        }) = self.jump_targets.last()
        {
            self.schedule_backpatch(&continue_target.clone(), self.current_address())?;
            let instruction = Instruction::Jmp(Address(0));
            push_instruction!(self, instruction, node.span);
            return Ok(());
        }

        Err(LpcError::new("`continue` statement without a jump target?").with_span(node.span))
    }

    #[instrument(skip_all)]
    fn visit_decl(&mut self, node: &mut DeclNode, cell_key: &mut QCellOwner) -> Result<()> {
        for init in &mut node.initializations {
            self.visit_var_init(init, cell_key)?;
        }

        Ok(())
    }

    #[instrument(skip_all)]
    fn visit_do_while(&mut self, node: &mut DoWhileNode, cell_key: &mut QCellOwner) -> Result<()> {
        self.context.scopes.goto(node.scope_id);

        let start_label = self.new_label("do-while-start");
        let end_label = self.new_label("do-while-end");
        let continue_label = self.new_label("do-while-continue");
        let jump_target = JumpTarget::new(end_label.clone(), continue_label.clone());
        self.jump_targets.push(jump_target);

        let start_addr = self.current_address();
        self.insert_label(start_label.clone(), start_addr);

        node.body.visit(self, cell_key)?;

        let continue_addr = self.current_address();
        self.insert_label(continue_label, continue_addr);

        node.condition.visit(self, cell_key)?;

        // Go back to the start of the loop if the result isn't zero
        self.schedule_backpatch(&start_label, self.current_address())?;
        let instruction = Instruction::Jnz(self.current_result, Address(0));
        push_instruction!(self, instruction, node.span);
        let end_addr = self.current_address();
        self.insert_label(end_label, end_addr);

        self.context.scopes.pop();
        self.jump_targets.pop();
        Ok(())
    }

    #[instrument(skip_all)]
    fn visit_float(&mut self, node: &mut FloatNode, _cell_key: &mut QCellOwner) -> Result<()> {
        let register = self.register_counter.next().unwrap().as_local();
        self.current_result = register;
        let instruction = Instruction::FConst(self.current_result, node.value);
        push_instruction!(self, instruction, node.span);

        Ok(())
    }

    #[instrument(skip_all)]
    fn visit_for(&mut self, node: &mut ForNode, cell_key: &mut QCellOwner) -> Result<()> {
        self.context.scopes.goto(node.scope_id);

        if let Some(i) = &mut *node.initializer {
            i.visit(self, cell_key)?;
        }

        let start_label = self.new_label("for-start");
        let end_label = self.new_label("for-end");
        let continue_label = self.new_label("for-continue");
        let jump_target = JumpTarget::new(end_label.clone(), continue_label.clone());
        self.jump_targets.push(jump_target);
        let start_addr = self.current_address();
        self.insert_label(start_label.clone(), start_addr);

        if let Some(cond) = &mut node.condition {
            cond.visit(self, cell_key)?;

            self.schedule_backpatch(&end_label, self.current_address())?;
            let instruction = Instruction::Jz(self.current_result, Address(0));
            push_instruction!(self, instruction, cond.span());
        };

        node.body.visit(self, cell_key)?;

        let continue_addr = self.current_address();
        self.insert_label(continue_label, continue_addr);

        if let Some(i) = &mut node.incrementer {
            i.visit(self, cell_key)?;
        }

        // go back to the start of the loop
        self.schedule_backpatch(&start_label, self.current_address())?;
        let instruction = Instruction::Jmp(Address(0));
        push_instruction!(self, instruction, node.span);

        let addr = self.current_address();
        self.insert_label(end_label, addr);

        self.context.scopes.pop();
        self.jump_targets.pop();
        Ok(())
    }

    #[instrument(skip_all)]
    fn visit_foreach(&mut self, node: &mut ForEachNode, cell_key: &mut QCellOwner) -> Result<()> {
        self.context.scopes.goto(node.scope_id);

        node.collection.visit(self, cell_key)?;
        let collection_location = self.current_result;

        let index_location = self.assign_sym_location(FOREACH_INDEX);
        let length_location = self.assign_sym_location(FOREACH_LENGTH);

        let instruction = Instruction::Sizeof(collection_location, length_location);
        push_instruction!(self, instruction, node.span);

        let locations = match &mut node.initializer {
            ForEachInit::Array(ref mut node) | ForEachInit::String(ref mut node) => {
                node.visit(self, cell_key)?;

                vec![self.current_result]
            }
            ForEachInit::Mapping {
                ref mut key,
                ref mut value,
            } => {
                key.visit(self, cell_key)?;
                let key_result = self.current_result;
                value.visit(self, cell_key)?;
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

        node.body.visit(self, cell_key)?;

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

        self.context.scopes.pop();
        self.jump_targets.pop();
        Ok(())
    }

    #[instrument(skip_all)]
    fn visit_function_def(
        &mut self,
        node: &mut FunctionDefNode,
        cell_key: &mut QCellOwner,
    ) -> Result<()> {
        // Note we don't look to inherited files at all for this -
        // We're generating code for a function defined _in this object_
        let prototype = match self.context.function_prototypes.get(&node.name) {
            Some(p) => p,
            None => {
                return Err(LpcError::new(format!(
                    "function prototype for {} not found",
                    node.name
                ))
                .with_span(node.span));
            }
        };

        let arity = prototype.arity;
        let num_args = arity.num_args;
        let num_default_args = arity.num_default_args;

        let sym = ProgramFunction::new(prototype.clone(), 0);

        self.function_stack.push(sym);
        self.backpatch_maps.push(HashMap::new());

        let len = self.current_address();
        self.register_counter.push();
        self.upvalue_counter.push();
        self.function_upvalue_counter.push();

        self.context.scopes.goto_function(&node.name)?;
        let declared_arg_count = node.parameters.len();
        let declared_arg_locations = self.visit_parameters(&node.parameters);

        let populate_defaults_index = self.setup_populate_defaults(node.span, num_default_args);

        let populate_argv_index =
            self.setup_populate_argv(node.flags.ellipsis(), node.span, declared_arg_count);

        let start_label = self.new_label("function-body-start");
        self.insert_label(&start_label, self.current_address());

        for expression in &mut node.body {
            expression.visit(self, cell_key)?;
        }

        // insert a final return if one isn't already there.
        {
            let sym = self.function_stack.last_mut().unwrap();
            if sym.instructions.len() == len.0
                || (!sym.instructions.is_empty()
                    && *sym.instructions.last().unwrap() != Instruction::Ret)
            {
                if sym.return_type() != LpcType::Void {
                    self.context.errors.push(
                        LpcError::new_warning(
                            "non-void function does not return a value. defaulting to 0."
                                .to_string(),
                        )
                        .with_span(node.span),
                    );
                }
                sym.push_instruction(Instruction::Ret, node.span);
            }
        }

        debug_assert_eq!(declared_arg_count, declared_arg_locations.len());

        if num_default_args > 0 {
            // always set when num_default_args > 0
            debug_assert!(populate_defaults_index.is_some());

            self.init_default_params(
                &mut node.parameters,
                &declared_arg_locations,
                node.span,
                populate_defaults_index.unwrap(),
                cell_key,
            )?;
        }

        self.context.scopes.pop();
        let mut func = self.function_stack.pop().unwrap();
        func.num_locals = self
            .register_counter
            .number_emitted()
            .saturating_sub(num_args);
        func.num_upvalues = self.function_upvalue_counter.number_emitted();

        func.arg_locations = declared_arg_locations;

        if let Some(idx) = populate_argv_index {
            Self::backpatch_populate_argv(&mut func, idx, node.span)?;
        }

        let backpatch_map = self.backpatch_maps.pop().unwrap();
        Self::backpatch(&backpatch_map, &mut func)?;
        self.functions.insert(func.mangle(), func.into());

        self.function_upvalue_counter.pop();
        self.upvalue_counter.pop();
        self.register_counter.pop();

        Ok(())
    }

    #[instrument(skip_all)]
    fn visit_function_ptr(
        &mut self,
        node: &mut FunctionPtrNode,
        cell_key: &mut QCellOwner,
    ) -> Result<()> {
        let mut applied_arguments = vec![];
        if let Some(args) = &mut node.arguments {
            for argument in args {
                if let Some(n) = argument {
                    n.visit(self, cell_key)?;
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
                    rcvr_node.visit(self, cell_key)?;
                    FunctionReceiver::Var(self.current_result)
                }

                // `&` used as the receiver
                FunctionPtrReceiver::Dynamic => FunctionReceiver::Dynamic,
            }
        } else if self.context.contains_function(node.name.as_str(), &CallNamespace::Local) {
            // A local / inherited function
            FunctionReceiver::Local
        } else {
            if_chain! {
                if let Some(se) = &self.context.simul_efuns;
                let simul_efuns = se.ro(cell_key);
                if simul_efuns.as_ref().contains_function(node.name.as_str());
                then {
                    FunctionReceiver::SimulEfun
                } else {
                    if EFUN_PROTOTYPES.contains_key(node.name.as_str()) {
                        FunctionReceiver::Efun
                    } else {
                        return Err(LpcError::new(format!(
                            "unknown call in function pointer: `{}`",
                            node.name
                        ))
                            .with_span(node.span));
                    }
                }
            }
        };

        let (name, _) = self.context.strings.insert_full(node.name.clone());

        // prepare the partially-applied arguments
        let instruction = Instruction::ClearPartialArgs;
        push_instruction!(self, instruction, node.span);
        for a in &applied_arguments {
            let instruction = Instruction::PushPartialArg(*a);
            push_instruction!(self, instruction, node.span);
        }

        let location = self.register_counter.next().unwrap().as_local();
        self.current_result = location;

        let instruction = Instruction::FunctionPtrConst {
            location,
            name_index: name,
            receiver,
        };

        push_instruction!(self, instruction, node.span);

        Ok(())
    }

    #[instrument(skip_all)]
    fn visit_if(&mut self, node: &mut IfNode, cell_key: &mut QCellOwner) -> Result<()> {
        self.context.scopes.goto(node.scope_id);
        let else_label = self.new_label("if-else");
        let end_label = self.new_label("if-end");

        // Visit the condition
        node.condition.visit(self, cell_key)?;

        // If the condition is false (i.e. equal to 0 or 0.0), jump to the end of the
        // "then" body. Insert a placeholder address, which we correct below
        // after the body's code is generated
        self.schedule_backpatch(&else_label, self.current_address())?;
        let instruction = Instruction::Jz(self.current_result, Address(0));
        push_instruction!(self, instruction, node.span);

        // Generate the main body of the statement
        node.body.visit(self, cell_key)?;

        if node.else_clause.is_some() {
            self.schedule_backpatch(&end_label, self.current_address())?;
            let instruction = Instruction::Jmp(Address(0));
            push_instruction!(self, instruction, node.span);
        }

        let addr = self.current_address();
        self.insert_label(else_label, addr);

        // Generate the else clause code if necessary
        if let Some(n) = &mut *node.else_clause {
            n.visit(self, cell_key)?;

            let addr = self.current_address();
            self.insert_label(end_label, addr);
        }

        self.context.scopes.pop();
        Ok(())
    }

    #[instrument(skip_all)]
    fn visit_int(&mut self, node: &mut IntNode, _cell_key: &mut QCellOwner) -> Result<()> {
        let register = self.register_counter.next().unwrap().as_local();
        self.current_result = register;
        let instruction = match node.value {
            0 => Instruction::IConst0(register),
            1 => Instruction::IConst1(register),
            v => Instruction::IConst(register, v),
        };
        push_instruction!(self, instruction, node.span);

        Ok(())
    }

    #[instrument(skip_all)]
    fn visit_label(&mut self, node: &mut LabelNode, _cell_key: &mut QCellOwner) -> Result<()> {
        let address = self.current_address();
        match self.case_addresses.last_mut() {
            Some(x) => {
                // track address of where this label will point
                let case = SwitchCase(node.case.clone());
                x.push((case, address));
                Ok(())
            }
            None => Err(LpcError::new(
                "Found a label in the code generator, but nowhere to store the address?",
            )
            .with_span(node.span)),
        }
    }

    #[instrument(skip_all)]
    fn visit_mapping(&mut self, node: &mut MappingNode, cell_key: &mut QCellOwner) -> Result<()> {
        let mut items = Vec::with_capacity(node.value.len() * 2);

        for (key, value) in &mut node.value {
            key.visit(self, cell_key)?;
            items.push(self.current_result);

            value.visit(self, cell_key)?;
            items.push(self.current_result);
        }

        push_instruction!(self, Instruction::ClearArrayItems, node.span);

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
    fn visit_program(
        &mut self,
        program: &mut ProgramNode,
        cell_key: &mut QCellOwner,
    ) -> Result<()> {
        self.context.scopes.goto_root();
        self.setup_init();
        self.backpatch_maps.push(HashMap::new());

        // Partition global variable initializations vs everything else
        let (global_init, functions): (Vec<&mut AstNode>, Vec<&mut AstNode>) = program
            .body
            .iter_mut()
            .partition(|x| matches!(**x, AstNode::Decl(_)));

        // Hoist all global variables, and initialize them at the very start
        // of the program (i.e. at the time it's cloned)
        for node in global_init {
            node.visit(self, cell_key)?;
        }

        // Insert a call to `create`, if it's been defined.
        if self
            .context
            .lookup_function(CREATE_FUNCTION, &CallNamespace::Local)
            .is_some()
        {
            let mut call = CallNode {
                receiver: None,
                arguments: vec![],
                name: CREATE_FUNCTION.to_string(),
                span: None,
                namespace: CallNamespace::Local,
            };
            call.visit(self, cell_key)?;
        }

        let mut ret = ReturnNode {
            value: None,
            span: None,
        };
        ret.visit(self, cell_key)?;

        for node in functions {
            node.visit(self, cell_key)?;
        }

        let backpatch_map = self.backpatch_maps.pop().unwrap();
        // populate the initializer
        let mut func = self.function_stack.pop().unwrap();
        debug_assert!(func.name() == INIT_PROGRAM);
        func.num_locals = self.register_counter.number_emitted();

        Self::backpatch(&backpatch_map, &mut func)?;

        self.initializer = Some(Rc::new(func));
        // self.functions.insert(func.mangle(), func.into());

        self.context.scopes.pop();

        Ok(())
    }

    #[instrument(skip_all)]
    fn visit_range(&mut self, node: &mut RangeNode, cell_key: &mut QCellOwner) -> Result<()> {
        let mut result_left: Option<RegisterVariant> = None;
        let mut result_right: Option<RegisterVariant> = None;
        if let Some(expr) = &mut *node.l {
            expr.visit(self, cell_key)?;
            result_left = Some(self.current_result);
        }

        if let Some(expr) = &mut *node.r {
            expr.visit(self, cell_key)?;
            result_right = Some(self.current_result);
        }

        self.visit_range_results = Some((result_left, result_right));

        Ok(())
    }

    #[instrument(skip_all)]
    fn visit_return(&mut self, node: &mut ReturnNode, cell_key: &mut QCellOwner) -> Result<()> {
        if let Some(expression) = &mut node.value {
            expression.visit(self, cell_key)?;
            let copy = Instruction::Copy(self.current_result, Register(0).as_local());
            push_instruction!(self, copy, expression.span());
        }

        push_instruction!(self, Instruction::Ret, node.span);

        Ok(())
    }

    #[instrument(skip_all)]
    fn visit_string(&mut self, node: &mut StringNode, _cell_key: &mut QCellOwner) -> Result<()> {
        let register = self.register_counter.next().unwrap().as_local();
        self.current_result = register;

        let (index, _) = self.context.strings.insert_full(node.value.clone());

        push_instruction!(self, Instruction::SConst(register, index), node.span);

        Ok(())
    }

    #[instrument(skip_all)]
    fn visit_switch(&mut self, node: &mut SwitchNode, cell_key: &mut QCellOwner) -> Result<()> {
        node.expression.visit(self, cell_key)?;
        let expr_result = self.current_result;

        let test_label = self.new_label("switch-test");
        self.schedule_backpatch(&test_label, self.current_address())?;
        let instruction = Instruction::Jmp(Address(0));
        push_instruction!(self, instruction, node.span);

        let end_label = self.new_label("switch-end");
        self.jump_targets
            .push(JumpTarget::new(end_label.clone(), "".into()));
        let addresses = vec![];
        self.case_addresses.push(addresses);

        node.body.visit(self, cell_key)?;

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
            // TODO: this may be sketchy, since the jump address we're testing against has not been set
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
            match case_address.0 .0 {
                Some(mut case_expr) => {
                    case_expr.visit(self, cell_key)?;
                    let case_result = self.current_result;
                    let test_result = self.register_counter.next().unwrap().as_local();

                    if let ExpressionNode::Range(range_node) = case_expr {
                        let (range_left, range_right) = self.visit_range_results.unwrap();

                        // check if >= start of range
                        let gte_result = self.register_counter.next().unwrap().as_local();
                        let instruction = if let Some(left_reg) = range_left {
                            Instruction::Gte(case_result, left_reg, gte_result)
                        } else {
                            Instruction::IConst1(gte_result)
                        };
                        push_instruction!(self, instruction, range_node.span);

                        // check if <= end of range
                        let lte_result = self.register_counter.next().unwrap().as_local();
                        let instruction = if let Some(right_reg) = range_right {
                            Instruction::Lte(case_result, right_reg, lte_result)
                        } else {
                            Instruction::IConst1(lte_result)
                        };
                        push_instruction!(self, instruction, range_node.span);

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
    fn visit_ternary(&mut self, node: &mut TernaryNode, cell_key: &mut QCellOwner) -> Result<()> {
        let result_reg = self.register_counter.next().unwrap().as_local();
        let else_label = self.new_label("ternary-else");
        let end_label = self.new_label("ternary-end");

        node.condition.visit(self, cell_key)?;

        self.schedule_backpatch(&else_label, self.current_address())?;
        let instruction = Instruction::Jz(self.current_result, Address(0));
        push_instruction!(self, instruction, node.span);

        node.body.visit(self, cell_key)?;
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

        node.else_clause.visit(self, cell_key)?;
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
    fn visit_unary_op(&mut self, node: &mut UnaryOpNode, cell_key: &mut QCellOwner) -> Result<()> {
        node.expr.visit(self, cell_key)?;
        let location = self.current_result;

        self.current_result = match node.op {
            UnaryOperation::Negate => {
                // multiply by -1
                let reg = self.register_counter.next().unwrap().as_local();
                let instruction = Instruction::IConst(reg, -1);
                push_instruction!(self, instruction, node.span);

                let reg_result = self.register_counter.next().unwrap().as_local();

                let instruction = Instruction::MMul(location, reg, reg_result);
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
    fn visit_var(&mut self, node: &mut VarNode, cell_key: &mut QCellOwner) -> Result<()> {
        if node.is_closure_arg_var() {
            let idx = closure_arg_number(&node.name)?;
            let loc = self
                .closure_arg_locations
                .last()
                .and_then(|locs| locs.get(idx - 1))
                .copied()
                .unwrap_or_else(|| Register(idx).as_local());
            self.current_result = loc;

            return Ok(());
        }

        if node.function_name {
            let mut fptr_node = FunctionPtrNode {
                receiver: None,
                arguments: None,
                name: node.name.clone(),
                span: node.span,
            };

            return self.visit_function_ptr(&mut fptr_node, cell_key);
        }

        let Some(sym) = self.context.lookup_var(&node.name) else {
            return Err(
                LpcError::new(format!("Unable to find symbol `{}`", node.name))
                    .with_span(node.span),
            );
        };

        let Some(sym_loc) = sym.location else {
            return Err(
                LpcError::new(format!("Symbol `{}` has no location set.", sym.name))
                    .with_span(node.span),
            );
        };

        self.current_result = sym_loc;

        Ok(())
    }

    #[instrument(skip_all)]
    fn visit_var_init(&mut self, node: &mut VarInitNode, cell_key: &mut QCellOwner) -> Result<()> {
        let symbol = self.context.lookup_var(&node.name);

        let Some(sym) = symbol else {
            return Err(LpcError::new(format!(
                "Missing symbol, that somehow passed semantic checks?: {}",
                node.name
            ))
            .with_span(node.span))
        };

        let global = sym.is_global();
        let upvalue = sym.upvalue;

        let current_register = if let Some(expression) = &mut node.value {
            expression.visit(self, cell_key)?;

            // TODO: This whole thing sucks. We'd rather have the `expression.visit()` call
            //       above put the result into the correct location directly.
            if global {
                let next_register = self.global_counter.next().unwrap().as_global();

                trace!("Copying global to {:?}", next_register);
                push_instruction!(
                    self,
                    Instruction::Copy(self.current_result, next_register),
                    node.span()
                );

                self.global_init_registers = next_register.index();

                next_register
            } else if upvalue {
                let next_register = self.upvalue_counter.next().unwrap().as_upvalue();
                // increment the counter of upvalues declared in *the current function*
                self.function_upvalue_counter.next().unwrap();
                trace!("Copying upvalue to {:?}", next_register);
                push_instruction!(
                    self,
                    Instruction::Copy(self.current_result, next_register),
                    node.span()
                );
                next_register
            } else if matches!(expression, ExpressionNode::Var(_)) {
                // Copy to a new register so the new var isn't literally
                // sharing a register with the old one.
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
            self.assign_sym_location(&node.name)
        };

        self.current_result = current_register;

        if let Some(sym) = self.context.lookup_var_mut(&node.name) {
            sym.location = Some(current_register);

            if let Some(func) = self.function_stack.last_mut() {
                func.local_variables.push(sym.clone())
            }
        }

        Ok(())
    }

    #[instrument(skip_all)]
    fn visit_while(&mut self, node: &mut WhileNode, cell_key: &mut QCellOwner) -> Result<()> {
        self.context.scopes.goto(node.scope_id);

        let start_label = self.new_label("while-start");
        let end_label = self.new_label("while-end");
        self.jump_targets
            .push(JumpTarget::new(end_label.clone(), start_label.clone()));
        let start_addr = self.current_address();
        self.insert_label(start_label.clone(), start_addr);

        node.condition.visit(self, cell_key)?;

        let cond_result = self.current_result;

        self.schedule_backpatch(&end_label, self.current_address())?;
        let instruction = Instruction::Jz(cond_result, Address(0));
        push_instruction!(self, instruction, node.span);

        node.body.visit(self, cell_key)?;

        // go back to the start of the loop
        self.schedule_backpatch(&start_label, self.current_address())?;
        let instruction = Instruction::Jmp(Address(0));
        push_instruction!(self, instruction, node.span);

        let addr = self.current_address();
        self.insert_label(end_label, addr);

        self.context.scopes.pop();
        self.jump_targets.pop();
        Ok(())
    }
}

impl Default for CodegenWalker {
    fn default() -> Self {
        // The local counter starts at 1, as r0 is reserved for return values.
        let register_counter = RegisterCounter::new(1);
        let global_counter = RegisterCounter::new(0);
        let upvalue_counter = RegisterCounter::new(0);
        let function_upvalue_counter = RegisterCounter::new(0);

        Self {
            function_stack: vec![],
            backpatch_maps: vec![],
            label_count: 0,
            functions: Default::default(),
            initializer: None,
            current_result: RegisterVariant::Local(Register(0)),
            register_counter,
            global_counter,
            upvalue_counter,
            function_upvalue_counter,
            global_init_registers: 0,
            context: Default::default(),
            jump_targets: vec![],
            case_addresses: vec![],
            visit_range_results: None,
            closure_scope_stack: vec![],
            closure_arg_locations: vec![],
        }
    }
}

#[cfg(test)]
mod tests {
    use std::{collections::VecDeque, sync::Arc};

    use claim::assert_some;
    use factori::create;
    use lpc_rs_asm::instruction::Instruction::*;
    use lpc_rs_core::{lpc_path::LpcPath, lpc_type::LpcType, LpcFloat};
    use lpc_rs_errors::{span::Span, LpcErrorSeverity, Result};
    use lpc_rs_utils::config::ConfigBuilder;

    use super::*;
    use crate::{
        apply_walker,
        compiler::{
            ast::{
                ast_node::AstNode, comma_expression_node::CommaExpressionNode,
                expression_node::ExpressionNode,
            },
            codegen::{
                codegen_walker::CodegenWalker, default_params_walker::DefaultParamsWalker,
                function_prototype_walker::FunctionPrototypeWalker,
                inheritance_walker::InheritanceWalker, scope_walker::ScopeWalker,
                semantic_check_walker::SemanticCheckWalker,
            },
            lexer::LexWrapper,
            CompilerBuilder,
        },
        interpreter::{process::Process, program::Program},
        lpc_parser,
        test_support::factories::*,
    };

    const LIB_DIR: &str = "./tests/fixtures/code";

    fn default_walker(cell_key: &mut QCellOwner) -> CodegenWalker {
        let mut walker = CodegenWalker::default();
        walker.setup_init();

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
        let process = cell_key.cell(Process::new(prog));
        walker.context.simul_efuns = Some(process.into());

        walker
    }

    fn walk_prog(prog: &str, cell_key: &mut QCellOwner) -> CodegenWalker {
        walk_code(prog, cell_key).expect("failed to walk.")
    }

    fn walk_code(code: &str, cell_key: &mut QCellOwner) -> Result<CodegenWalker> {
        let config = ConfigBuilder::default()
            .lib_dir(LIB_DIR)
            .simul_efun_file("/secure/simul_efuns")
            .build()
            .unwrap();

        let compiler = CompilerBuilder::default().config(config).build()?;
        let (mut program, context) = compiler
            .parse_string(&LpcPath::new_in_game("/my_test.c", "/", LIB_DIR), code)
            .expect("failed to parse");

        let context = apply_walker!(InheritanceWalker, program, context, cell_key, false);
        let context = apply_walker!(FunctionPrototypeWalker, program, context, cell_key, false);
        let context = apply_walker!(ScopeWalker, program, context, cell_key, false);
        let context = apply_walker!(DefaultParamsWalker, program, context, cell_key, false);
        let context = apply_walker!(SemanticCheckWalker, program, context, cell_key, false);

        let mut walker = CodegenWalker::new(context);
        let _ = program.visit(&mut walker, cell_key);

        Ok(walker)
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

    fn generate_init_instructions(prog: &str, cell_key: &mut QCellOwner) -> Vec<Instruction> {
        // walker_init_instructions(&mut walk_prog(prog))
        walk_prog(prog, cell_key)
            .initializer
            .unwrap()
            .instructions
            .clone()
    }

    fn find_function<'a, K>(
        map: &'a IndexMap<K, Rc<ProgramFunction>>,
        name: &str,
    ) -> Option<&'a Rc<ProgramFunction>> {
        map.values().find(|f| f.name() == name)
    }

    #[test]
    fn test_visit_array_populates_the_instructions() {
        let mut cell_key = QCellOwner::new();
        let mut walker = default_walker(&mut cell_key);

        let mut arr = ArrayNode::new(vec![
            ExpressionNode::from(123),
            ExpressionNode::from("foo"),
            ExpressionNode::from(vec![ExpressionNode::from(666)]),
        ]);

        let _ = walker.visit_array(&mut arr, &mut cell_key);

        let expected = vec![
            IConst(RegisterVariant::Local(Register(1)), 123),
            SConst(RegisterVariant::Local(Register(2)), 0),
            IConst(RegisterVariant::Local(Register(3)), 666),
            ClearArrayItems,
            PushArrayItem(RegisterVariant::Local(Register(3))),
            AConst(RegisterVariant::Local(Register(4))),
            ClearArrayItems,
            PushArrayItem(RegisterVariant::Local(Register(1))),
            PushArrayItem(RegisterVariant::Local(Register(2))),
            PushArrayItem(RegisterVariant::Local(Register(4))),
            AConst(RegisterVariant::Local(Register(5))),
        ];

        assert_eq!(walker_init_instructions(&mut walker), expected);
    }

    mod test_visit_assignment {
        use super::*;

        #[test]
        fn test_populates_the_instructions_for_globals() {
            let mut cell_key = QCellOwner::new();
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
                    name: "marf".to_string(),
                    span: None,
                    global: true,
                    function_name: false,
                    external_capture: false,
                })),
                rhs: Box::new(ExpressionNode::Int(IntNode::new(-12))),
                span: None,
            };

            let _ = walker.visit_assignment(&mut node, &mut cell_key);
            assert_eq!(
                walker_init_instructions(&mut walker),
                [
                    IConst(RegisterVariant::Local(Register(1)), -12),
                    Copy(
                        RegisterVariant::Local(Register(1)),
                        RegisterVariant::Global(Register(666))
                    ),
                ]
            );
        }

        #[test]
        fn test_populates_the_instructions_for_locals() {
            let mut cell_key = QCellOwner::new();
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

            let _ = walker.visit_assignment(&mut node, &mut cell_key);
            assert_eq!(
                walker_init_instructions(&mut walker),
                [
                    IConst(RegisterVariant::Local(Register(1)), -12),
                    Copy(
                        RegisterVariant::Local(Register(1)),
                        RegisterVariant::Local(Register(666))
                    )
                ]
            );
        }

        #[test]
        fn test_populates_the_instructions_for_array_items() {
            let mut cell_key = QCellOwner::new();
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

            let _ = walker.visit_assignment(&mut node, &mut cell_key);
            assert_eq!(
                walker_init_instructions(&mut walker),
                [
                    IConst(RegisterVariant::Local(Register(1)), -12),
                    IConst1(RegisterVariant::Local(Register(2))),
                    Store(
                        RegisterVariant::Local(Register(1)),
                        RegisterVariant::Local(Register(666)),
                        RegisterVariant::Local(Register(2))
                    )
                ]
            );
        }
    }

    mod test_binary_op {
        use lpc_rs_asm::instruction::Instruction::{
            FConst, IConst0, IMul, Jnz, Jz, Load, MAdd, Range,
        };

        use super::*;

        #[test]
        fn populates_the_instructions_for_ints() {
            let mut cell_key = QCellOwner::new();
            let mut walker = default_walker(&mut cell_key);

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

            let _ = walker.visit_binary_op(&mut node, &mut cell_key);

            let expected = vec![
                IConst(RegisterVariant::Local(Register(1)), 666),
                IConst(RegisterVariant::Local(Register(2)), 123),
                IConst(RegisterVariant::Local(Register(3)), 456),
                IAdd(
                    RegisterVariant::Local(Register(2)),
                    RegisterVariant::Local(Register(3)),
                    RegisterVariant::Local(Register(4)),
                ),
                IMul(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Local(Register(4)),
                    RegisterVariant::Local(Register(5)),
                ),
            ];

            assert_eq!(walker_init_instructions(&mut walker), expected);
        }

        #[test]
        fn populates_the_instructions_for_floats() {
            let mut cell_key = QCellOwner::new();
            let mut context = CompilationContext::default();
            context.scopes.push_new();
            let mut sym = Symbol::new("foo", LpcType::Float(false));
            sym.location = Some(RegisterVariant::Local(Register(1)));
            context.scopes.current_mut().unwrap().insert(sym);

            let mut walker = CodegenWalker::new(context);

            let mut node = BinaryOpNode {
                l: Box::new(ExpressionNode::Float(FloatNode::new(123.45))),
                r: Box::new(ExpressionNode::BinaryOp(BinaryOpNode {
                    l: Box::new(ExpressionNode::Var(VarNode {
                        name: "foo".to_string(),
                        span: None,
                        global: false,
                        function_name: false,
                        external_capture: false,
                    })),
                    r: Box::new(ExpressionNode::Int(IntNode::new(456))),
                    op: BinaryOperation::Mul,
                    span: None,
                })),
                op: BinaryOperation::Add,
                span: None,
            };

            let _ = walker.visit_binary_op(&mut node, &mut cell_key);

            let expected = vec![
                FConst(RegisterVariant::Local(Register(1)), LpcFloat::from(123.45)),
                IConst(RegisterVariant::Local(Register(2)), 456),
                IMul(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Local(Register(2)),
                    RegisterVariant::Local(Register(3)),
                ),
                IAdd(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Local(Register(3)),
                    RegisterVariant::Local(Register(4)),
                ),
            ];

            assert_eq!(walker_init_instructions(&mut walker), expected);
        }

        #[test]
        fn populates_the_instructions_for_strings() {
            let mut cell_key = QCellOwner::new();
            let mut walker = default_walker(&mut cell_key);

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

            let _ = walker.visit_binary_op(&mut node, &mut cell_key);

            let expected = vec![
                SConst(RegisterVariant::Local(Register(1)), 0),
                SConst(RegisterVariant::Local(Register(2)), 1),
                SConst(RegisterVariant::Local(Register(3)), 2),
                MAdd(
                    RegisterVariant::Local(Register(2)),
                    RegisterVariant::Local(Register(3)),
                    RegisterVariant::Local(Register(4)),
                ),
                MAdd(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Local(Register(4)),
                    RegisterVariant::Local(Register(5)),
                ),
            ];

            assert_eq!(walker_init_instructions(&mut walker), expected);
        }

        #[test]
        fn populates_the_instructions_for_arrays() {
            let mut cell_key = QCellOwner::new();
            let mut walker = default_walker(&mut cell_key);

            let mut node = BinaryOpNode {
                l: Box::new(ExpressionNode::from(vec![ExpressionNode::from(123)])),
                r: Box::new(ExpressionNode::from(vec![ExpressionNode::from(456)])),
                op: BinaryOperation::Add,
                span: None,
            };

            let _ = walker.visit_binary_op(&mut node, &mut cell_key);

            let expected = vec![
                IConst(RegisterVariant::Local(Register(1)), 123),
                ClearArrayItems,
                PushArrayItem(RegisterVariant::Local(Register(1))),
                AConst(RegisterVariant::Local(Register(2))),
                IConst(RegisterVariant::Local(Register(3)), 456),
                ClearArrayItems,
                PushArrayItem(RegisterVariant::Local(Register(3))),
                AConst(RegisterVariant::Local(Register(4))),
                MAdd(
                    RegisterVariant::Local(Register(2)),
                    RegisterVariant::Local(Register(4)),
                    RegisterVariant::Local(Register(5)),
                ),
            ];

            assert_eq!(walker_init_instructions(&mut walker), expected);
        }

        #[test]
        fn populates_the_instructions_for_indexes() {
            let mut cell_key = QCellOwner::new();
            let context = CompilationContext::default();
            let mut walker = CodegenWalker::new(context);

            let mut node = BinaryOpNode {
                l: Box::new(ExpressionNode::from(vec![ExpressionNode::from(123)])),
                r: Box::new(ExpressionNode::from(0)),
                op: BinaryOperation::Index,
                span: None,
            };

            let _ = walker.visit_binary_op(&mut node, &mut cell_key);

            let expected = vec![
                IConst(RegisterVariant::Local(Register(1)), 123),
                ClearArrayItems,
                PushArrayItem(RegisterVariant::Local(Register(1))),
                AConst(RegisterVariant::Local(Register(2))),
                IConst0(RegisterVariant::Local(Register(3))),
                Load(
                    RegisterVariant::Local(Register(2)),
                    RegisterVariant::Local(Register(3)),
                    RegisterVariant::Local(Register(4)),
                ),
            ];

            assert_eq!(walker_init_instructions(&mut walker), expected);
        }

        #[test]
        fn populates_the_instructions_for_slices() {
            let mut cell_key = QCellOwner::new();
            let mut walker = default_walker(&mut cell_key);

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

            let _ = walker.visit_binary_op(&mut node, &mut cell_key);

            let expected = vec![
                IConst(RegisterVariant::Local(Register(1)), 123),
                ClearArrayItems,
                PushArrayItem(RegisterVariant::Local(Register(1))),
                AConst(RegisterVariant::Local(Register(2))),
                IConst1(RegisterVariant::Local(Register(3))),
                IConst(RegisterVariant::Local(Register(4)), -1),
                Range(
                    RegisterVariant::Local(Register(2)),
                    RegisterVariant::Local(Register(3)),
                    RegisterVariant::Local(Register(4)),
                    RegisterVariant::Local(Register(5)),
                ),
            ];

            assert_eq!(walker_init_instructions(&mut walker), expected);
        }

        #[test]
        fn populates_the_instructions_for_andand_expressions() {
            let mut cell_key = QCellOwner::new();
            let mut walker = default_walker(&mut cell_key);
            walker.backpatch_maps.push(HashMap::new());

            let mut node = BinaryOpNode {
                l: Box::new(ExpressionNode::from(123)),
                r: Box::new(ExpressionNode::from("marf!")),
                op: BinaryOperation::AndAnd,
                span: None,
            };

            let _ = walker.visit_binary_op(&mut node, &mut cell_key);

            let expected = vec![
                IConst(RegisterVariant::Local(Register(1)), 123),
                Jz(RegisterVariant::Local(Register(1)), Address(0)),
                // and also
                SConst(RegisterVariant::Local(Register(2)), 0),
                Jz(RegisterVariant::Local(Register(2)), Address(0)),
                Copy(
                    RegisterVariant::Local(Register(2)),
                    RegisterVariant::Local(Register(3)),
                ),
                // end is here
            ];

            assert_eq!(walker_init_instructions(&mut walker), expected);
        }

        #[test]
        fn populates_the_instructions_for_oror_expressions() {
            let mut cell_key = QCellOwner::new();
            let mut walker = default_walker(&mut cell_key);
            walker.backpatch_maps.push(HashMap::new());

            let mut node = BinaryOpNode {
                l: Box::new(ExpressionNode::from(123)),
                r: Box::new(ExpressionNode::from("sup?")),
                op: BinaryOperation::OrOr,
                span: None,
            };

            let _ = walker.visit_binary_op(&mut node, &mut cell_key);

            let expected = vec![
                IConst(RegisterVariant::Local(Register(1)), 123),
                Copy(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Local(Register(2)),
                ),
                Jnz(RegisterVariant::Local(Register(2)), Address(0)),
                // else
                SConst(RegisterVariant::Local(Register(3)), 0),
                Copy(
                    RegisterVariant::Local(Register(3)),
                    RegisterVariant::Local(Register(2)),
                ),
                // end is here
            ];

            assert_eq!(walker_init_instructions(&mut walker), expected);
        }
    }

    mod test_break {
        use lpc_rs_core::register::Register;

        use super::*;

        #[test]
        fn breaks_out_of_while_loops() {
            let mut cell_key = QCellOwner::new();
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

            let mut walker = walk_prog(code, &mut cell_key);
            let expected = vec![
                IConst(RegisterVariant::Local(Register(2)), 10),
                Lt(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Local(Register(2)),
                    RegisterVariant::Local(Register(3)),
                ),
                Jz(RegisterVariant::Local(Register(3)), Address(18)),
                ClearArgs,
                PushArg(RegisterVariant::Local(Register(1))),
                CallEfun(1),
                IConst(RegisterVariant::Local(Register(4)), 5),
                Gt(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Local(Register(4)),
                    RegisterVariant::Local(Register(5)),
                ),
                Jz(RegisterVariant::Local(Register(5)), Address(14)),
                SConst(RegisterVariant::Local(Register(6)), 2),
                ClearArgs,
                PushArg(RegisterVariant::Local(Register(6))),
                CallEfun(1),
                Jmp(Address(18)),
                IConst1(RegisterVariant::Local(Register(7))),
                IAdd(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Local(Register(7)),
                    RegisterVariant::Local(Register(8)),
                ),
                Copy(
                    RegisterVariant::Local(Register(8)),
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

        #[test]
        fn breaks_out_of_for_loops() {
            let mut cell_key = QCellOwner::new();
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

            let mut walker = walk_prog(code, &mut cell_key);
            let expected = vec![
                IConst0(RegisterVariant::Local(Register(1))),
                IConst(RegisterVariant::Local(Register(2)), 10),
                Lt(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Local(Register(2)),
                    RegisterVariant::Local(Register(3)),
                ),
                Jz(RegisterVariant::Local(Register(3)), Address(22)),
                ClearArgs,
                PushArg(RegisterVariant::Local(Register(1))),
                CallEfun(1),
                IConst(RegisterVariant::Local(Register(4)), 5),
                Gt(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Local(Register(4)),
                    RegisterVariant::Local(Register(5)),
                ),
                Jz(RegisterVariant::Local(Register(5)), Address(15)),
                SConst(RegisterVariant::Local(Register(6)), 2),
                ClearArgs,
                PushArg(RegisterVariant::Local(Register(6))),
                CallEfun(1),
                Jmp(Address(22)),
                IConst1(RegisterVariant::Local(Register(7))),
                IAdd(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Local(Register(7)),
                    RegisterVariant::Local(Register(8)),
                ),
                Copy(
                    RegisterVariant::Local(Register(8)),
                    RegisterVariant::Local(Register(1)),
                ),
                IConst1(RegisterVariant::Local(Register(9))),
                IAdd(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Local(Register(9)),
                    RegisterVariant::Local(Register(10)),
                ),
                Copy(
                    RegisterVariant::Local(Register(10)),
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

        #[test]
        fn breaks_out_of_do_while_loops() {
            let mut cell_key = QCellOwner::new();

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

            let mut walker = walk_prog(code, &mut cell_key);
            let expected = vec![
                ClearArgs,
                PushArg(RegisterVariant::Local(Register(1))),
                CallEfun(1),
                IConst(RegisterVariant::Local(Register(2)), 5),
                Gt(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Local(Register(2)),
                    RegisterVariant::Local(Register(3)),
                ),
                Jz(RegisterVariant::Local(Register(3)), Address(11)),
                SConst(RegisterVariant::Local(Register(4)), 2),
                ClearArgs,
                PushArg(RegisterVariant::Local(Register(4))),
                CallEfun(1),
                Jmp(Address(17)),
                IConst1(RegisterVariant::Local(Register(5))),
                IAdd(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Local(Register(5)),
                    RegisterVariant::Local(Register(6)),
                ),
                Copy(
                    RegisterVariant::Local(Register(6)),
                    RegisterVariant::Local(Register(1)),
                ),
                IConst(RegisterVariant::Local(Register(7)), 10),
                Lt(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Local(Register(7)),
                    RegisterVariant::Local(Register(8)),
                ),
                Jnz(RegisterVariant::Local(Register(8)), Address(0)),
                Ret,
            ];

            assert_eq!(
                walker_function_instructions(&mut walker, "create"),
                expected
            );
        }

        #[test]
        fn breaks_out_of_switch_statements() {
            let mut cell_key = QCellOwner::new();
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

            let mut walker = walk_prog(code, &mut cell_key);
            let expected = vec![
                IConst(RegisterVariant::Local(Register(1)), 666),
                Jmp(Address(16)),
                SConst(RegisterVariant::Local(Register(2)), 1),
                ClearArgs,
                PushArg(RegisterVariant::Local(Register(2))),
                CallEfun(2),
                Jmp(Address(26)),
                SConst(RegisterVariant::Local(Register(3)), 3),
                ClearArgs,
                PushArg(RegisterVariant::Local(Register(3))),
                CallEfun(2),
                SConst(RegisterVariant::Local(Register(4)), 4),
                ClearArgs,
                PushArg(RegisterVariant::Local(Register(4))),
                CallEfun(2),
                Jmp(Address(26)),
                IConst(RegisterVariant::Local(Register(5)), 666),
                EqEq(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Local(Register(5)),
                    RegisterVariant::Local(Register(6)),
                ),
                Jnz(RegisterVariant::Local(Register(6)), Address(2)),
                IConst(RegisterVariant::Local(Register(7)), 10),
                IConst(RegisterVariant::Local(Register(8)), 200),
                Gte(
                    RegisterVariant::Local(Register(8)),
                    RegisterVariant::Local(Register(7)),
                    RegisterVariant::Local(Register(10)),
                ),
                Lte(
                    RegisterVariant::Local(Register(8)),
                    RegisterVariant::Local(Register(8)),
                    RegisterVariant::Local(Register(11)),
                ),
                And(
                    RegisterVariant::Local(Register(10)),
                    RegisterVariant::Local(Register(11)),
                    RegisterVariant::Local(Register(9)),
                ),
                Jnz(RegisterVariant::Local(Register(9)), Address(11)),
                Jmp(Address(7)),
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

        fn get_call_node(code: &str, context: &mut CompilationContext) -> CallNode {
            let mut prog_node = lpc_parser::ProgramParser::new()
                .parse(context, LexWrapper::new(code))
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

        #[test]
        fn populates_the_instructions() {
            let mut cell_key = QCellOwner::new();
            let mut walker = default_walker(&mut cell_key);
            let call = "mixed m = dump(4 - 5);";
            let mut node = get_call_node(call, &mut walker.context);

            let _ = walker.visit_call(&mut node, &mut cell_key);

            let expected = vec![
                IConst(RegisterVariant::Local(Register(1)), -1),
                ClearArgs,
                PushArg(RegisterVariant::Local(Register(1))),
                CallEfun(0),
            ];

            assert_eq!(walker_init_instructions(&mut walker), expected);
        }

        #[test]
        fn populates_the_instructions_for_call_other() {
            let mut cell_key = QCellOwner::new();
            let mut check = |code: &str, expected: &[Instruction]| {
                let mut walker = default_walker(&mut cell_key);
                let mut tree = lpc_parser::ExpressionParser::new()
                    .parse(&mut CompilationContext::default(), LexWrapper::new(code))
                    .unwrap();

                let _ = tree.visit(&mut walker, &mut cell_key);

                assert_eq!(walker_init_instructions(&mut walker), expected);
            };

            let expected = vec![
                IConst(RegisterVariant::Local(Register(1)), -1),
                ClearArgs,
                PushArg(RegisterVariant::Local(Register(1))),
                SConst(RegisterVariant::Local(Register(2)), 0),
                SConst(RegisterVariant::Local(Register(3)), 1),
                CallOther(
                    RegisterVariant::Local(Register(2)),
                    RegisterVariant::Local(Register(3)),
                ),
                Copy(
                    RegisterVariant::Local(Register(0)),
                    RegisterVariant::Local(Register(4)),
                ),
            ];
            check(r#""foo"->print(4 - 5)"#, &expected);

            let expected = vec![
                SConst(RegisterVariant::Local(Register(1)), 0),
                SConst(RegisterVariant::Local(Register(2)), 1),
                IConst(RegisterVariant::Local(Register(3)), -1),
                ClearArgs,
                PushArg(RegisterVariant::Local(Register(1))),
                PushArg(RegisterVariant::Local(Register(2))),
                PushArg(RegisterVariant::Local(Register(3))),
                CallOther(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Local(Register(2)),
                ),
                Copy(
                    RegisterVariant::Local(Register(0)),
                    RegisterVariant::Local(Register(4)),
                ),
            ];
            check(r#"call_other("foo", "print", 4 - 5)"#, &expected);
        }

        #[test]
        fn populates_the_instructions_for_sizeof() {
            let mut cell_key = QCellOwner::new();
            let mut check = |code: &str, expected: &[Instruction]| {
                let mut walker = default_walker(&mut cell_key);
                let mut tree = lpc_parser::ExpressionParser::new()
                    .parse(&mut CompilationContext::default(), LexWrapper::new(code))
                    .unwrap();

                let _ = tree.visit(&mut walker, &mut cell_key);

                assert_eq!(walker_init_instructions(&mut walker), expected);
            };

            let expected = vec![
                IConst1(RegisterVariant::Local(Register(1))),
                IConst(RegisterVariant::Local(Register(2)), 2),
                SConst(RegisterVariant::Local(Register(3)), 0),
                ClearArrayItems,
                PushArrayItem(RegisterVariant::Local(Register(1))),
                PushArrayItem(RegisterVariant::Local(Register(2))),
                PushArrayItem(RegisterVariant::Local(Register(3))),
                AConst(RegisterVariant::Local(Register(4))),
                Sizeof(
                    RegisterVariant::Local(Register(4)),
                    RegisterVariant::Local(Register(5)),
                ),
            ];
            check(r#"sizeof(({ 1, 2, "c" }))"#, &expected);
        }

        #[test]
        fn populates_the_instructions_for_catch() {
            let mut cell_key = QCellOwner::new();
            let mut walker = default_walker(&mut cell_key);
            let call = "catch(12 / 0)";
            let mut tree = lpc_parser::ExpressionParser::new()
                .parse(&mut CompilationContext::default(), LexWrapper::new(call))
                .unwrap();

            let _ = tree.visit(&mut walker, &mut cell_key);

            let expected = vec![
                CatchStart(RegisterVariant::Local(Register(1)), Address(4)),
                IConst(RegisterVariant::Local(Register(2)), 12),
                IConst0(RegisterVariant::Local(Register(3))),
                IDiv(
                    RegisterVariant::Local(Register(2)),
                    RegisterVariant::Local(Register(3)),
                    RegisterVariant::Local(Register(4)),
                ),
                CatchEnd,
            ];

            assert_eq!(walker_init_instructions(&mut walker), expected);
        }

        #[test]
        fn populates_the_instructions_for_function_pointers() {
            let mut cell_key = QCellOwner::new();
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

            let call = "my_func(666)";
            let mut tree = lpc_parser::ExpressionParser::new()
                .parse(&mut context, LexWrapper::new(call))
                .unwrap();

            let mut walker = CodegenWalker::new(context);
            let _ = tree.visit(&mut walker, &mut cell_key);

            let expected = vec![
                IConst(RegisterVariant::Local(Register(1)), 666),
                ClearArgs,
                PushArg(RegisterVariant::Local(Register(1))),
                CallFp(RegisterVariant::Local(Register(1))),
                Copy(
                    RegisterVariant::Local(Register(0)),
                    RegisterVariant::Local(Register(2)),
                ),
            ];

            assert_eq!(walker_init_instructions(&mut walker), expected);
        }

        #[test]
        fn populates_the_instructions_for_global_function_pointers() {
            let mut cell_key = QCellOwner::new();
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

            let call = "my_func(666)";
            let mut tree = lpc_parser::ExpressionParser::new()
                .parse(&mut context, LexWrapper::new(call))
                .unwrap();

            let mut walker = CodegenWalker::new(context);
            let _ = tree.visit(&mut walker, &mut cell_key);

            let expected = vec![
                IConst(RegisterVariant::Local(Register(1)), 666),
                ClearArgs,
                PushArg(RegisterVariant::Local(Register(1))),
                CallFp(RegisterVariant::Global(Register(0))),
                Copy(
                    RegisterVariant::Local(Register(0)),
                    RegisterVariant::Local(Register(2)),
                ),
            ];

            assert_eq!(walker_init_instructions(&mut walker), expected);
        }

        #[test]
        fn copies_non_void_call_results() {
            let mut cell_key = QCellOwner::new();
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

            let _ = walker.visit_call(&mut node, &mut cell_key);

            let expected = vec![
                IConst(RegisterVariant::Local(Register(1)), 666),
                ClearArgs,
                PushArg(RegisterVariant::Local(Register(1))),
                Call(0),
                Copy(
                    RegisterVariant::Local(Register(0)),
                    RegisterVariant::Local(Register(2)),
                ),
            ];

            assert_eq!(walker_init_instructions(&mut walker), expected);
        }

        #[test]
        fn does_not_copy_void_call_results() {
            let mut cell_key = QCellOwner::new();
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

            let _ = walker.visit_call(&mut node, &mut cell_key);

            let expected = vec![
                IConst(RegisterVariant::Local(Register(1)), 666),
                ClearArgs,
                PushArg(RegisterVariant::Local(Register(1))),
                Call(0),
            ];

            assert_eq!(walker_init_instructions(&mut walker), expected);
        }

        #[test]
        fn copies_non_void_efun_results() {
            let mut cell_key = QCellOwner::new();
            let mut walker = default_walker(&mut cell_key);
            let call = r#"mixed m = clone_object("/foo.c");"#;
            let mut node = get_call_node(call, &mut walker.context);

            let _ = walker.visit_call(&mut node, &mut cell_key);

            let expected = vec![
                SConst(RegisterVariant::Local(Register(1)), 0),
                ClearArgs,
                PushArg(RegisterVariant::Local(Register(1))),
                CallEfun(1),
                Copy(
                    RegisterVariant::Local(Register(0)),
                    RegisterVariant::Local(Register(2)),
                ),
            ];

            assert_eq!(walker_init_instructions(&mut walker), expected);
        }

        #[test]
        fn does_not_copy_void_efun_results() {
            let mut cell_key = QCellOwner::new();
            let mut walker = default_walker(&mut cell_key);
            let call = r#"mixed m = dump("lkajsdflkajsdf");"#;
            let mut node = get_call_node(call, &mut walker.context);

            let _ = walker.visit_call(&mut node, &mut cell_key);

            let expected = vec![
                SConst(RegisterVariant::Local(Register(1)), 0),
                ClearArgs,
                PushArg(RegisterVariant::Local(Register(1))),
                CallEfun(1),
            ];

            assert_eq!(walker_init_instructions(&mut walker), expected);
        }

        #[test]
        fn handles_ellipsis_functions() {
            let mut cell_key = QCellOwner::new();
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

            let _ = walker.visit_call(&mut node, &mut cell_key);

            let expected = vec![
                SConst(RegisterVariant::Local(Register(1)), 0),
                IConst(RegisterVariant::Local(Register(2)), 42),
                SConst(RegisterVariant::Local(Register(3)), 1),
                ClearArgs,
                PushArg(RegisterVariant::Local(Register(1))),
                PushArg(RegisterVariant::Local(Register(2))),
                PushArg(RegisterVariant::Local(Register(3))),
                Call(2),
            ];

            assert_eq!(walker_init_instructions(&mut walker), expected);
        }
    }

    mod test_visit_block {
        use super::*;

        #[test]
        fn test_visit_block_populates_instructions() {
            let mut cell_key = QCellOwner::new();
            let block = "void marf() { { int a = '🏯'; dump(a); } }";
            let mut prog_node = lpc_parser::ProgramParser::new()
                .parse(&mut CompilationContext::default(), LexWrapper::new(block))
                .unwrap();
            let node = if let AstNode::FunctionDef(ref mut n) = prog_node.body.first_mut().unwrap()
            {
                if let AstNode::Block(n) = n.body.first_mut().unwrap() {
                    n
                } else {
                    panic!("Expected a block node");
                }
            } else {
                panic!("Expected a function def node");
            };

            let mut scope_walker = ScopeWalker::default();
            let _ = scope_walker.visit_block(node, &mut cell_key);

            let context = scope_walker.into_context();
            let mut walker = CodegenWalker::new(context);
            let _ = walker.visit_block(node, &mut cell_key);

            let expected = vec![
                IConst(RegisterVariant::Local(Register(1)), 127983),
                ClearArgs,
                PushArg(RegisterVariant::Local(Register(1))),
                CallEfun(0),
            ];

            assert_eq!(walker_init_instructions(&mut walker), expected);
        }
    }

    #[test]
    fn test_visit_comma_expression_populates_the_instructions() {
        let mut cell_key = QCellOwner::new();
        let mut walker = default_walker(&mut cell_key);

        let mut expr = CommaExpressionNode::new(vec![
            ExpressionNode::from(123),
            ExpressionNode::from("foo"),
            ExpressionNode::from(vec![ExpressionNode::from(666)]),
        ]);

        let _ = walker.visit_comma_expression(&mut expr, &mut cell_key);

        let expected = vec![
            IConst(RegisterVariant::Local(Register(1)), 123),
            SConst(RegisterVariant::Local(Register(2)), 0),
            IConst(RegisterVariant::Local(Register(3)), 666),
            ClearArrayItems,
            PushArrayItem(RegisterVariant::Local(Register(3))),
            AConst(RegisterVariant::Local(Register(4))),
        ];

        assert_eq!(walker_init_instructions(&mut walker), expected);
        assert_eq!(walker.current_result, RegisterVariant::Local(Register(4)));
    }

    mod test_visit_closure {
        use indoc::indoc;

        use super::*;

        fn get_closure_node(code: &str, context: &mut CompilationContext) -> ClosureNode {
            let mut prog_node = lpc_parser::ProgramParser::new()
                .parse(context, LexWrapper::new(code))
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

        fn compile(code: &str, cell_key: &mut QCellOwner) -> CodegenWalker {
            let mut context = CompilationContext::default();

            let mut node = get_closure_node(code, &mut context);

            let mut prototype_walker = FunctionPrototypeWalker::new(context);
            let _ = prototype_walker.visit_closure(&mut node, cell_key);
            let mut context = prototype_walker.into_context();

            context.scopes.push_new(); // global scope

            let mut scope_walker = ScopeWalker::new(context);
            let _ = scope_walker.visit_closure(&mut node, cell_key);

            let mut context = scope_walker.into_context();
            context.scopes.goto_root();

            let mut walker = CodegenWalker::new(context);
            let _ = walker.visit_closure(&mut node, cell_key);

            walker
        }

        #[test]
        fn populates_the_instructions() {
            let mut cell_key = QCellOwner::new();
            let mut walker = compile("function f = (: dump(4 + 5 + $1) :);", &mut cell_key);

            assert_eq!(
                walker_function_instructions(&mut walker, "closure-0"),
                vec![
                    IConst(RegisterVariant::Local(Register(2)), 9),
                    MAdd(
                        RegisterVariant::Local(Register(2)),
                        RegisterVariant::Local(Register(1)),
                        RegisterVariant::Local(Register(3))
                    ),
                    ClearArgs,
                    PushArg(RegisterVariant::Local(Register(3))),
                    CallEfun(0),
                    Ret
                ]
            );
        }

        #[test]
        fn handles_ellipses() {
            let mut cell_key = QCellOwner::new();
            let mut walker = compile("function f = (: [int i, ...] argv :);", &mut cell_key);

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

        #[test]
        fn populates_the_default_arguments() {
            let mut cell_key = QCellOwner::new();
            let mut walker = compile(
                "function f = (: [int i, int j = 666, float d = 3.54] i * j :);",
                &mut cell_key,
            );

            assert_eq!(
                walker_function_instructions(&mut walker, "closure-0"),
                vec![
                    PopulateDefaults,
                    Jmp(Address(6)),
                    Jmp(Address(8)),
                    IMul(
                        RegisterVariant::Local(Register(1)),
                        RegisterVariant::Local(Register(2)),
                        RegisterVariant::Local(Register(4)),
                    ),
                    Copy(
                        RegisterVariant::Local(Register(4)),
                        RegisterVariant::Local(Register(0)),
                    ),
                    Ret,
                    IConst(RegisterVariant::Local(Register(5)), 666),
                    Copy(
                        RegisterVariant::Local(Register(5)),
                        RegisterVariant::Local(Register(2)),
                    ),
                    FConst(RegisterVariant::Local(Register(6)), 3.54.into()),
                    Copy(
                        RegisterVariant::Local(Register(6)),
                        RegisterVariant::Local(Register(3)),
                    ),
                    Jmp(Address(3)),
                ],
            );
        }

        #[test]
        fn sets_the_correct_upvalue_information() {
            let mut cell_key = QCellOwner::new();
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
            let walker = walk_prog(code, &mut cell_key);

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
    }

    mod test_continue {
        use lpc_rs_core::register::Register;

        use super::*;

        #[test]
        fn continues_while_loops() {
            let mut cell_key = QCellOwner::new();
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

            let mut walker = walk_prog(code, &mut cell_key);
            let expected = vec![
                IConst(RegisterVariant::Local(Register(2)), 10),
                Lt(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Local(Register(2)),
                    RegisterVariant::Local(Register(3)),
                ),
                Jz(RegisterVariant::Local(Register(3)), Address(18)),
                ClearArgs,
                PushArg(RegisterVariant::Local(Register(1))),
                CallEfun(1),
                IConst(RegisterVariant::Local(Register(4)), 5),
                Gt(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Local(Register(4)),
                    RegisterVariant::Local(Register(5)),
                ),
                Jz(RegisterVariant::Local(Register(5)), Address(14)),
                SConst(RegisterVariant::Local(Register(6)), 2),
                ClearArgs,
                PushArg(RegisterVariant::Local(Register(6))),
                CallEfun(1),
                Jmp(Address(0)),
                IConst1(RegisterVariant::Local(Register(7))),
                IAdd(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Local(Register(7)),
                    RegisterVariant::Local(Register(8)),
                ),
                Copy(
                    RegisterVariant::Local(Register(8)),
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

        #[test]
        fn continues_for_loops() {
            let mut cell_key = QCellOwner::new();
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

            let mut walker = walk_prog(code, &mut cell_key);
            let expected = vec![
                IConst0(RegisterVariant::Local(Register(1))),
                IConst(RegisterVariant::Local(Register(2)), 10),
                Lt(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Local(Register(2)),
                    RegisterVariant::Local(Register(3)),
                ),
                Jz(RegisterVariant::Local(Register(3)), Address(22)),
                ClearArgs,
                PushArg(RegisterVariant::Local(Register(1))),
                CallEfun(1),
                IConst(RegisterVariant::Local(Register(4)), 5),
                Gt(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Local(Register(4)),
                    RegisterVariant::Local(Register(5)),
                ),
                Jz(RegisterVariant::Local(Register(5)), Address(15)),
                SConst(RegisterVariant::Local(Register(6)), 2),
                ClearArgs,
                PushArg(RegisterVariant::Local(Register(6))),
                CallEfun(1),
                Jmp(Address(18)),
                IConst1(RegisterVariant::Local(Register(7))),
                IAdd(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Local(Register(7)),
                    RegisterVariant::Local(Register(8)),
                ),
                Copy(
                    RegisterVariant::Local(Register(8)),
                    RegisterVariant::Local(Register(1)),
                ),
                IConst1(RegisterVariant::Local(Register(9))),
                IAdd(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Local(Register(9)),
                    RegisterVariant::Local(Register(10)),
                ),
                Copy(
                    RegisterVariant::Local(Register(10)),
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

        #[test]
        fn continues_do_while_loops() {
            let mut cell_key = QCellOwner::new();
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

            let mut walker = walk_prog(code, &mut cell_key);
            let expected = vec![
                ClearArgs,
                PushArg(RegisterVariant::Local(Register(1))),
                CallEfun(1),
                IConst(RegisterVariant::Local(Register(2)), 5),
                Gt(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Local(Register(2)),
                    RegisterVariant::Local(Register(3)),
                ),
                Jz(RegisterVariant::Local(Register(3)), Address(11)),
                SConst(RegisterVariant::Local(Register(4)), 2),
                ClearArgs,
                PushArg(RegisterVariant::Local(Register(4))),
                CallEfun(1),
                Jmp(Address(14)),
                IConst1(RegisterVariant::Local(Register(5))),
                IAdd(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Local(Register(5)),
                    RegisterVariant::Local(Register(6)),
                ),
                Copy(
                    RegisterVariant::Local(Register(6)),
                    RegisterVariant::Local(Register(1)),
                ),
                IConst(RegisterVariant::Local(Register(7)), 10),
                Lt(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Local(Register(7)),
                    RegisterVariant::Local(Register(8)),
                ),
                Jnz(RegisterVariant::Local(Register(8)), Address(0)),
                Ret,
            ];

            assert_eq!(
                walker_function_instructions(&mut walker, CREATE_FUNCTION),
                expected
            );
        }
    }

    #[test]
    fn test_decl_sets_scope_and_instructions() {
        let mut cell_key = QCellOwner::new();
        let call = "int foo = 1, *bar = ({ 56 });";
        let mut prog_node: ProgramNode = lpc_parser::ProgramParser::new()
            .parse(&mut CompilationContext::default(), LexWrapper::new(call))
            .unwrap();
        let node = if let AstNode::Decl(node) = prog_node.body.first_mut().unwrap() {
            node
        } else {
            panic!("Expected decl node");
        };

        let mut scope_walker = ScopeWalker::default();
        let _ = scope_walker.visit_decl(node, &mut cell_key);

        let context = scope_walker.into_context();
        let mut walker = CodegenWalker::new(context);
        let _ = walker.visit_decl(node, &mut cell_key);

        let expected = vec![
            IConst1(RegisterVariant::Local(Register(1))),
            Copy(
                RegisterVariant::Local(Register(1)),
                RegisterVariant::Global(Register(0)),
            ),
            IConst(RegisterVariant::Local(Register(2)), 56),
            ClearArrayItems,
            PushArrayItem(RegisterVariant::Local(Register(2))),
            AConst(RegisterVariant::Local(Register(3))),
            Copy(
                RegisterVariant::Local(Register(3)),
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
        assert_eq!(
            foo.span,
            Some(Span {
                file_id: 0,
                l: 4,
                r: 11
            })
        );

        let bar = scope.lookup("bar").unwrap();
        assert_eq!(&bar.name, "bar");
        assert_eq!(bar.type_, LpcType::Int(true));
        assert_eq!(bar.location, Some(RegisterVariant::Global(Register(1))));
        assert_some!(bar.scope_id);
        assert_eq!(
            bar.span,
            Some(Span {
                file_id: 0,
                l: 13,
                r: 25
            })
        );
    }

    mod test_visit_do_while {
        use lpc_rs_asm::instruction::Instruction::{EqEq, Jnz};

        use super::*;
        use crate::compiler::ast::do_while_node::DoWhileNode;

        #[test]
        fn test_populates_the_instructions() {
            let mut cell_key = QCellOwner::new();
            let mut walker = default_walker(&mut cell_key);
            walker.backpatch_maps.push(HashMap::new());

            let mut node = DoWhileNode {
                condition: ExpressionNode::BinaryOp(BinaryOpNode {
                    l: Box::new(ExpressionNode::from(666)),
                    r: Box::new(ExpressionNode::from(777)),
                    op: BinaryOperation::EqEq,
                    span: None,
                }),
                body: Box::new(AstNode::Call(CallNode {
                    receiver: None,
                    arguments: vec![ExpressionNode::from("body")],
                    name: "dump".to_string(),
                    span: None,
                    namespace: CallNamespace::default(),
                })),
                scope_id: None,
                span: None,
            };

            let _ = walker.visit_do_while(&mut node, &mut cell_key);

            let expected = vec![
                SConst(RegisterVariant::Local(Register(1)), 0),
                ClearArgs,
                PushArg(RegisterVariant::Local(Register(1))),
                CallEfun(1),
                IConst(RegisterVariant::Local(Register(2)), 666),
                IConst(RegisterVariant::Local(Register(3)), 777),
                EqEq(
                    RegisterVariant::Local(Register(2)),
                    RegisterVariant::Local(Register(3)),
                    RegisterVariant::Local(Register(4)),
                ),
                Jnz(RegisterVariant::Local(Register(4)), Address(0)),
            ];

            assert_eq!(walker_init_instructions(&mut walker), expected);
        }
    }

    mod test_visit_for {
        use super::*;
        use crate::compiler::ast::for_node::ForNode;

        #[test]
        fn populates_the_instructions() {
            let mut cell_key = QCellOwner::new();

            let var = VarNode {
                name: "i".to_string(),
                span: None,
                global: false,
                function_name: false,
                external_capture: false,
            };

            let mut node = ForNode {
                initializer: Box::new(Some(AstNode::VarInit(VarInitNode {
                    type_: LpcType::Int(false),
                    name: "i".to_string(),
                    value: Some(ExpressionNode::from(10)),
                    array: false,
                    global: false,
                    span: None,
                    flags: None,
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
                        receiver: None,
                        arguments: vec![ExpressionNode::Var(var)],
                        name: "dump".to_string(),
                        span: None,
                        namespace: CallNamespace::default(),
                    })],
                    scope_id: None,
                })),
                scope_id: None,
                span: None,
            };

            let mut scope_walker = ScopeWalker::default();
            let _ = scope_walker.visit_for(&mut node, &mut cell_key);

            let context = scope_walker.into_context();
            let mut walker = CodegenWalker::new(context);
            walker.backpatch_maps.push(HashMap::new());

            walker.visit_for(&mut node, &mut cell_key).unwrap();

            let expected = vec![
                IConst(RegisterVariant::Local(Register(1)), 10),
                Jz(RegisterVariant::Local(Register(1)), Address(0)),
                ClearArgs,
                PushArg(RegisterVariant::Local(Register(1))),
                CallEfun(0),
                IConst1(RegisterVariant::Local(Register(2))),
                ISub(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Local(Register(2)),
                    RegisterVariant::Local(Register(3)),
                ),
                Copy(
                    RegisterVariant::Local(Register(3)),
                    RegisterVariant::Local(Register(1)),
                ),
                Jmp(Address(0)),
            ];

            assert_eq!(walker_init_instructions(&mut walker), expected);
        }
    }

    mod test_visit_function_def {
        use super::*;

        fn assert_compiles_to(code: &str, expected: Vec<Instruction>) {
            let mut cell_key = QCellOwner::new();
            let mut prototype_walker = FunctionPrototypeWalker::default();

            let mut prog_node: ProgramNode = lpc_parser::ProgramParser::new()
                .parse(&mut CompilationContext::default(), LexWrapper::new(code))
                .unwrap();
            let ast_node = prog_node.body.first_mut().unwrap();
            let node = if let AstNode::FunctionDef(node) = ast_node {
                node
            } else {
                panic!("Didn't receive a function def?");
            };

            let _ = prototype_walker.visit_function_def(node, &mut cell_key);
            let mut context = prototype_walker.into_context();

            context.scopes.push_new(); // global scope

            let mut scope_walker = ScopeWalker::new(context);
            let _ = scope_walker.visit_function_def(node, &mut cell_key);

            let mut context = scope_walker.into_context();
            context.scopes.goto_root();

            let mut walker = CodegenWalker::new(context);
            let _ = walker.visit_function_def(node, &mut cell_key);

            assert_eq!(walker_function_instructions(&mut walker, "main"), expected);
        }

        #[test]
        fn populates_the_data() {
            assert_compiles_to(
                "int main(int i) { return i + 4; }",
                vec![
                    IConst(RegisterVariant::Local(Register(2)), 4),
                    IAdd(
                        RegisterVariant::Local(Register(1)),
                        RegisterVariant::Local(Register(2)),
                        RegisterVariant::Local(Register(3)),
                    ),
                    Copy(
                        RegisterVariant::Local(Register(3)),
                        RegisterVariant::Local(Register(0)),
                    ),
                    Ret,
                ],
            );
        }

        #[test]
        fn handles_ellipses() {
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
            );
        }

        #[test]
        fn populates_the_default_arguments() {
            assert_compiles_to(
                "int main(int i, int j = 666, float d = 3.54) { return i * j; }",
                vec![
                    PopulateDefaults,
                    Jmp(Address(6)),
                    Jmp(Address(8)),
                    IMul(
                        RegisterVariant::Local(Register(1)),
                        RegisterVariant::Local(Register(2)),
                        RegisterVariant::Local(Register(4)),
                    ),
                    Copy(
                        RegisterVariant::Local(Register(4)),
                        RegisterVariant::Local(Register(0)),
                    ),
                    Ret,
                    IConst(RegisterVariant::Local(Register(5)), 666),
                    Copy(
                        RegisterVariant::Local(Register(5)),
                        RegisterVariant::Local(Register(2)),
                    ),
                    FConst(RegisterVariant::Local(Register(6)), 3.54.into()),
                    Copy(
                        RegisterVariant::Local(Register(6)),
                        RegisterVariant::Local(Register(3)),
                    ),
                    Jmp(Address(3)),
                ],
            );
        }
    }

    mod test_visit_function_ptr {
        use super::*;
        use crate::compiler::ast::function_ptr_node::FunctionPtrNode;

        #[test]
        fn populates_the_instructions_for_efuns() {
            let mut cell_key = QCellOwner::new();

            let mut node = FunctionPtrNode {
                receiver: None,
                name: "dump".to_string(),
                arguments: None,
                span: None,
            };

            let mut walker = default_walker(&mut cell_key);
            walker.visit_function_ptr(&mut node, &mut cell_key).unwrap();

            let expected = vec![
                ClearPartialArgs,
                FunctionPtrConst {
                    location: RegisterVariant::Local(Register(1)),
                    receiver: FunctionReceiver::Efun,
                    name_index: 0,
                }
            ];

            assert_eq!(walker_init_instructions(&mut walker), expected);
        }

        #[test]
        fn populates_the_instructions_for_simul_efuns() {
            let mut cell_key = QCellOwner::new();

            let mut node = FunctionPtrNode {
                receiver: None,
                name: "simul_efun".to_string(),
                arguments: None,
                span: None,
            };

            let mut walker = default_walker(&mut cell_key);
            walker.visit_function_ptr(&mut node, &mut cell_key).unwrap();

            let expected = vec![
                ClearPartialArgs,
                FunctionPtrConst {
                    location: RegisterVariant::Local(Register(1)),
                    receiver: FunctionReceiver::SimulEfun,
                    name_index: 0
                }
            ];

            assert_eq!(walker_init_instructions(&mut walker), expected);
        }
    }

    mod test_visit_if {
        use lpc_rs_asm::instruction::Instruction::{EqEq, Jmp, Jz};

        use super::*;

        #[test]
        fn test_populates_the_instructions() {
            let mut cell_key = QCellOwner::new();

            let mut walker = default_walker(&mut cell_key);
            walker.backpatch_maps.push(HashMap::new());

            let mut node = IfNode {
                condition: ExpressionNode::BinaryOp(BinaryOpNode {
                    l: Box::new(ExpressionNode::from(666)),
                    r: Box::new(ExpressionNode::from(777)),
                    op: BinaryOperation::EqEq,
                    span: None,
                }),
                body: Box::new(AstNode::Call(CallNode {
                    receiver: None,
                    arguments: vec![ExpressionNode::from("true")],
                    name: "dump".to_string(),
                    span: None,
                    namespace: CallNamespace::default(),
                })),
                else_clause: Box::new(Some(AstNode::Call(CallNode {
                    receiver: None,
                    arguments: vec![ExpressionNode::from("false")],
                    name: "dump".to_string(),
                    span: None,
                    namespace: CallNamespace::default(),
                }))),
                scope_id: None,
                span: None,
            };

            let _ = walker.visit_if(&mut node, &mut cell_key);

            let expected = vec![
                IConst(RegisterVariant::Local(Register(1)), 666),
                IConst(RegisterVariant::Local(Register(2)), 777),
                EqEq(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Local(Register(2)),
                    RegisterVariant::Local(Register(3)),
                ),
                Jz(RegisterVariant::Local(Register(3)), Address(0)),
                SConst(RegisterVariant::Local(Register(4)), 0),
                ClearArgs,
                PushArg(RegisterVariant::Local(Register(4))),
                CallEfun(1),
                Jmp(Address(0)),
                SConst(RegisterVariant::Local(Register(5)), 2),
                ClearArgs,
                PushArg(RegisterVariant::Local(Register(5))),
                CallEfun(1),
            ];

            assert_eq!(walker_init_instructions(&mut walker), expected);
        }
    }

    #[test]
    fn test_visit_int_populates_the_instructions() {
        let mut cell_key = QCellOwner::new();
        let mut walker = default_walker(&mut cell_key);

        let mut tree = IntNode::new(666);
        let mut tree0 = IntNode::new(0);
        let mut tree1 = IntNode::new(1);

        let _ = walker.visit_int(&mut tree, &mut cell_key);
        let _ = walker.visit_int(&mut tree0, &mut cell_key);
        let _ = walker.visit_int(&mut tree1, &mut cell_key);

        let expected = vec![
            IConst(RegisterVariant::Local(Register(1)), 666),
            IConst0(RegisterVariant::Local(Register(2))),
            IConst1(RegisterVariant::Local(Register(3))),
        ];

        assert_eq!(walker_init_instructions(&mut walker), expected);
    }

    mod test_visit_program {
        use super::*;

        #[test]
        fn populates_the_instructions() {
            let mut cell_key = QCellOwner::new();
            let prog = "
                void create() {
                    1 + 3 - 5;
                    dump(4 + 5);
                }
            ";

            let walker = walk_prog(prog, &mut cell_key);

            let expected = vec![ClearArgs, Call(0), Ret];

            assert_eq!(walker.initializer.unwrap().instructions, expected);

            let expected = vec![
                IConst(RegisterVariant::Local(Register(1)), -1),
                IConst(RegisterVariant::Local(Register(2)), 9),
                ClearArgs,
                PushArg(RegisterVariant::Local(Register(2))),
                CallEfun(1),
                Ret, // Automatically added due to no explicit return
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

        #[test]
        fn initializes_the_globals() {
            let mut cell_key = QCellOwner::new();
            let prog = r#"
                int j = 123;
                string q = "cool";
                void marf() {
                    dump(q + j);
                }
            "#;

            let instructions = generate_init_instructions(prog, &mut cell_key);

            let expected = [
                IConst(RegisterVariant::Local(Register(1)), 123),
                Copy(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Global(Register(0)),
                ),
                SConst(RegisterVariant::Local(Register(2)), 0),
                Copy(
                    RegisterVariant::Local(Register(2)),
                    RegisterVariant::Global(Register(1)),
                ),
                Ret,
            ];

            assert_eq!(instructions, expected);
        }

        #[test]
        fn calls_create_if_create_is_defined() {
            let mut cell_key = QCellOwner::new();
            let prog = r#"
                int q = 666;
                int marf() {
                    return 3;
                }
                void create() {
                    dump(marf() + " times a winner!");
                }
            "#;

            let instructions = generate_init_instructions(prog, &mut cell_key);

            let expected = [
                IConst(RegisterVariant::Local(Register(1)), 666),
                Copy(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Global(Register(0)),
                ),
                ClearArgs,
                Call(0),
                Ret, // end of initialization
            ];

            assert_eq!(instructions, expected);
        }

        #[test]
        fn tracks_global_registers_over_multiple_sections() {
            let mut cell_key = QCellOwner::new();
            let prog = r#"
                int q = 666;
                int marf() {
                    return 3;
                }
                int r = 777;
            "#;

            let instructions = generate_init_instructions(prog, &mut cell_key);

            let expected = [
                IConst(RegisterVariant::Local(Register(1)), 666),
                Copy(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Global(Register(0)),
                ),
                IConst(RegisterVariant::Local(Register(2)), 777),
                Copy(
                    RegisterVariant::Local(Register(2)),
                    RegisterVariant::Global(Register(1)),
                ),
                Ret,
            ];

            assert_eq!(instructions, expected);
        }
    }

    #[test]
    fn visit_return_populates_the_instructions() {
        let mut cell_key = QCellOwner::new();
        let mut walker = default_walker(&mut cell_key);

        let mut node = ReturnNode::new(Some(ExpressionNode::from(IntNode::new(666))));
        let _ = walker.visit_return(&mut node, &mut cell_key);

        let expected = vec![
            IConst(RegisterVariant::Local(Register(1)), 666),
            Copy(
                RegisterVariant::Local(Register(1)),
                RegisterVariant::Local(Register(0)),
            ),
            Ret,
        ];

        assert_eq!(walker_init_instructions(&mut walker), expected);

        /* === */

        let mut walker = default_walker(&mut cell_key);
        let mut node = ReturnNode::new(None);
        let _ = walker.visit_return(&mut node, &mut cell_key);

        let expected = vec![Ret];

        assert_eq!(walker_init_instructions(&mut walker), expected);
    }

    #[test]
    fn test_visit_string_populates_the_instructions() {
        let mut cell_key = QCellOwner::new();
        let mut walker = default_walker(&mut cell_key);
        let mut node = StringNode::new("marf");
        let mut node2 = StringNode::new("tacos");
        let mut node3 = StringNode::new("marf");

        let _ = walker.visit_string(&mut node, &mut cell_key);
        let _ = walker.visit_string(&mut node2, &mut cell_key);
        let _ = walker.visit_string(&mut node3, &mut cell_key);

        let expected = vec![
            SConst(RegisterVariant::Local(Register(1)), 0),
            SConst(RegisterVariant::Local(Register(2)), 1),
            SConst(RegisterVariant::Local(Register(3)), 0),
        ];

        assert_eq!(walker_init_instructions(&mut walker), expected);
    }

    mod test_visit_switch {
        use super::*;

        #[test]
        fn populates_the_instructions() {
            let mut cell_key = QCellOwner::new();

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

            let walker = walk_prog(code, &mut cell_key);
            let func = walker
                .functions
                .values()
                .find(|f| f.name() == "create")
                .unwrap();
            let instructions = func.instructions.clone();
            let expected = vec![
                IConst(RegisterVariant::Local(Register(1)), 666),
                Jmp(Address(17)),
                SConst(RegisterVariant::Local(Register(2)), 1),
                ClearArgs,
                PushArg(RegisterVariant::Local(Register(2))),
                CallEfun(2),
                Jmp(Address(24)),
                SConst(RegisterVariant::Local(Register(3)), 3),
                ClearArgs,
                PushArg(RegisterVariant::Local(Register(3))),
                CallEfun(2),
                Jmp(Address(24)),
                SConst(RegisterVariant::Local(Register(4)), 4),
                ClearArgs,
                PushArg(RegisterVariant::Local(Register(4))),
                CallEfun(2),
                Jmp(Address(24)),
                IConst1(RegisterVariant::Local(Register(5))),
                EqEq(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Local(Register(5)),
                    RegisterVariant::Local(Register(6)),
                ),
                Jnz(RegisterVariant::Local(Register(6)), Address(2)),
                IConst(RegisterVariant::Local(Register(7)), 2),
                EqEq(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Local(Register(7)),
                    RegisterVariant::Local(Register(8)),
                ),
                Jnz(RegisterVariant::Local(Register(8)), Address(7)),
                Jmp(Address(12)),
                Ret,
            ];

            assert_eq!(instructions, expected);
        }
    }

    mod test_visit_ternary {
        use lpc_rs_asm::instruction::Instruction::{Jmp, Jz, Lte};

        use super::*;
        use crate::compiler::ast::ternary_node::TernaryNode;

        #[test]
        fn populates_the_instructions() {
            let mut cell_key = QCellOwner::new();

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

            walker.visit_ternary(&mut node, &mut cell_key).unwrap();

            let expected = vec![
                IConst(RegisterVariant::Local(Register(2)), 2),
                IConst(RegisterVariant::Local(Register(3)), 3),
                Lte(
                    RegisterVariant::Local(Register(2)),
                    RegisterVariant::Local(Register(3)),
                    RegisterVariant::Local(Register(4)),
                ),
                Jz(RegisterVariant::Local(Register(4)), Address(0)), // jump to else
                IConst(RegisterVariant::Local(Register(5)), 666),
                Copy(
                    RegisterVariant::Local(Register(5)),
                    RegisterVariant::Local(Register(1)),
                ),
                Jmp(Address(0)), // jump to end
                IConst(RegisterVariant::Local(Register(6)), 777),
                Copy(
                    RegisterVariant::Local(Register(6)),
                    RegisterVariant::Local(Register(1)),
                ),
            ];

            assert_eq!(walker_init_instructions(&mut walker), expected);
        }
    }

    mod test_unary_op {
        use super::*;

        fn setup(op: UnaryOperation, is_post: bool, cell_key: &mut QCellOwner) -> CodegenWalker {
            let mut walker = default_walker(cell_key);
            let mut node = UnaryOpNode {
                op,
                expr: Box::new(ExpressionNode::from(666)),
                span: None,
                is_post,
            };

            let _ = walker.visit_unary_op(&mut node, cell_key);
            walker
        }

        mod negate {
            use super::*;

            #[test]
            fn populates_instructions() {
                let mut cell_key = QCellOwner::new();
                let mut walker = setup(UnaryOperation::Negate, false, &mut cell_key);

                let expected = vec![
                    IConst(RegisterVariant::Local(Register(1)), 666),
                    IConst(RegisterVariant::Local(Register(2)), -1),
                    MMul(
                        RegisterVariant::Local(Register(1)),
                        RegisterVariant::Local(Register(2)),
                        RegisterVariant::Local(Register(3)),
                    ),
                ];

                assert_eq!(walker_init_instructions(&mut walker), expected);
            }
        }

        mod inc {
            use super::*;

            #[test]
            fn populates_instructions_for_pre() {
                let mut cell_key = QCellOwner::new();
                let mut walker = setup(UnaryOperation::Inc, false, &mut cell_key);

                let expected = vec![
                    IConst(RegisterVariant::Local(Register(1)), 666),
                    Inc(RegisterVariant::Local(Register(1))),
                ];

                assert_eq!(walker_init_instructions(&mut walker), expected);
            }

            #[test]
            fn populates_instructions_for_post() {
                let mut cell_key = QCellOwner::new();
                let mut walker = setup(UnaryOperation::Inc, true, &mut cell_key);

                let expected = vec![
                    IConst(RegisterVariant::Local(Register(1)), 666),
                    Copy(
                        RegisterVariant::Local(Register(1)),
                        RegisterVariant::Local(Register(2)),
                    ),
                    Inc(RegisterVariant::Local(Register(1))),
                ];

                assert_eq!(walker_init_instructions(&mut walker), expected);
            }
        }

        mod dec {
            use super::*;

            #[test]
            fn populates_instructions_for_pre() {
                let mut cell_key = QCellOwner::new();
                let mut walker = setup(UnaryOperation::Dec, false, &mut cell_key);

                let expected = vec![
                    IConst(RegisterVariant::Local(Register(1)), 666),
                    Dec(RegisterVariant::Local(Register(1))),
                ];

                assert_eq!(walker_init_instructions(&mut walker), expected);
            }

            #[test]
            fn populates_instructions_for_post() {
                let mut cell_key = QCellOwner::new();
                let mut walker = setup(UnaryOperation::Dec, true, &mut cell_key);

                let expected = vec![
                    IConst(RegisterVariant::Local(Register(1)), 666),
                    Copy(
                        RegisterVariant::Local(Register(1)),
                        RegisterVariant::Local(Register(2)),
                    ),
                    Dec(RegisterVariant::Local(Register(1))),
                ];

                assert_eq!(walker_init_instructions(&mut walker), expected);
            }
        }

        mod bang {
            use super::*;

            #[test]
            fn populates_instructions() {
                let mut cell_key = QCellOwner::new();
                let mut walker = setup(UnaryOperation::Bang, false, &mut cell_key);

                let expected = vec![
                    IConst(RegisterVariant::Local(Register(1)), 666),
                    Not(
                        RegisterVariant::Local(Register(1)),
                        RegisterVariant::Local(Register(2)),
                    ),
                ];

                assert_eq!(walker_init_instructions(&mut walker), expected);
            }
        }

        mod bitwise_not {
            use super::*;

            #[test]
            fn populates_instructions() {
                let mut cell_key = QCellOwner::new();
                let mut walker = setup(UnaryOperation::BitwiseNot, false, &mut cell_key);

                let expected = vec![
                    IConst(RegisterVariant::Local(Register(1)), 666),
                    BitwiseNot(
                        RegisterVariant::Local(Register(1)),
                        RegisterVariant::Local(Register(2)),
                    ),
                ];

                assert_eq!(walker_init_instructions(&mut walker), expected);
            }
        }
    }

    mod test_visit_var {
        use indoc::indoc;

        use super::*;
        use crate::test_support::compile_prog;

        #[test]
        fn test_visit_var_loads_the_var_and_sets_the_result_for_globals() {
            let mut cell_key = QCellOwner::new();

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
                name: "marf".to_string(),
                span: None,
                global: true,
                function_name: false,
                external_capture: false,
            };

            let _ = walker.visit_var(&mut node, &mut cell_key);
            assert_eq!(
                walker.current_result,
                RegisterVariant::Global(Register(666))
            );

            let expected = vec![];
            assert_eq!(walker_init_instructions(&mut walker), expected);
        }

        #[test]
        fn test_visit_var_sets_the_result_for_locals() {
            let mut cell_key = QCellOwner::new();
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

            let _ = walker.visit_var(&mut node, &mut cell_key);
            assert_eq!(walker.current_result, RegisterVariant::Local(Register(666)));

            let expected = vec![];
            assert_eq!(walker_init_instructions(&mut walker), expected);
        }

        #[test]
        fn test_closure_positional_arguments() {
            let mut cell_key = QCellOwner::new();
            let code = indoc! { r##"
                function maker() {
                    return (: [int i] dump("i", $1); (: i :) :);
                }

                void create() {
                    function f = maker();
                    mixed i = f(1);
                }
            "## };

            let (prog, _, _) = compile_prog(code, &mut cell_key);

            // `closure-1` is the outer closure that refers to $1.
            let instructions = &find_function(&prog.functions, "closure-1")
                .unwrap()
                .instructions;
            let expected = vec![
                SConst(RegisterVariant::Local(Register(2)), 1),
                ClearArgs,
                PushArg(RegisterVariant::Local(Register(2))),
                PushArg(RegisterVariant::Upvalue(Register(0))), /* This is what we're really testing for */
                CallEfun(2),
                // ...etc. We don't care about the rest.
            ];
            assert_eq!(&instructions[0..=4], expected);
        }
    }

    mod test_visit_var_init {
        use decorum::Total;
        use lpc_rs_asm::instruction::Instruction::{FConst, MapConst};

        use super::*;

        fn setup() -> CodegenWalker {
            let mut context = CompilationContext::default();
            context.scopes.push_new();
            context.scopes.push_new();
            CodegenWalker::new(context)
        }

        fn setup_var(type_: LpcType, walker: &mut CodegenWalker, cell_key: &mut QCellOwner) {
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
                name: "muffins".to_string(),
                value: Some(ExpressionNode::Var(VarNode::new("marf"))),
                array: false,
                global: false,
                span: None,
                flags: None,
            };

            let mut new_sym = Symbol::from(&mut node.clone());
            new_sym.scope_id = scope_id;
            insert_symbol(walker, new_sym);

            let _ = walker.visit_var_init(&mut node, cell_key);
        }

        fn setup_literal(
            type_: LpcType,
            value: ExpressionNode,
            walker: &mut CodegenWalker,
            cell_key: &mut QCellOwner,
        ) {
            let mut node = VarInitNode {
                type_,
                name: "muffins".to_string(),
                value: Some(value),
                array: false,
                global: false,
                span: None,
                flags: None,
            };

            insert_symbol(walker, Symbol::from(&mut node));

            let _ = walker.visit_var_init(&mut node, cell_key);
        }

        #[test]
        fn test_does_not_copy_mapping_literals() {
            let mut cell_key = QCellOwner::new();
            let mut walker = setup();
            let pairs = vec![(ExpressionNode::from("foo"), ExpressionNode::from("bar"))];
            setup_literal(
                LpcType::Mapping(false),
                ExpressionNode::Mapping(MappingNode::new(pairs, None)),
                &mut walker,
                &mut cell_key,
            );

            assert_eq!(
                walker_init_instructions(&mut walker),
                [
                    SConst(RegisterVariant::Local(Register(1)), 0),
                    SConst(RegisterVariant::Local(Register(2)), 1),
                    ClearArrayItems,
                    PushArrayItem(RegisterVariant::Local(Register(1))),
                    PushArrayItem(RegisterVariant::Local(Register(2))),
                    MapConst(RegisterVariant::Local(Register(3)))
                ]
            );
        }

        #[test]
        fn test_copies_mapping_vars() {
            let mut cell_key = QCellOwner::new();
            let mut walker = setup();
            setup_var(LpcType::Mapping(false), &mut walker, &mut cell_key);

            assert_eq!(
                walker_init_instructions(&mut walker),
                [Copy(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Local(Register(2))
                )]
            );
        }

        #[test]
        fn test_does_not_copy_int_literals() {
            let mut cell_key = QCellOwner::new();
            let mut walker = setup();
            setup_literal(
                LpcType::Int(false),
                ExpressionNode::Int(IntNode::new(123)),
                &mut walker,
                &mut cell_key,
            );

            assert_eq!(
                walker_init_instructions(&mut walker),
                [IConst(RegisterVariant::Local(Register(1)), 123)]
            );
        }

        #[test]
        fn test_copies_int_vars() {
            let mut cell_key = QCellOwner::new();
            let mut walker = setup();
            setup_var(LpcType::Int(false), &mut walker, &mut cell_key);

            assert_eq!(
                walker_init_instructions(&mut walker),
                [Copy(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Local(Register(2))
                )]
            );
        }

        #[test]
        fn test_does_not_copy_float_literals() {
            let mut cell_key = QCellOwner::new();
            let mut walker = setup();
            setup_literal(
                LpcType::Float(false),
                ExpressionNode::Float(FloatNode::new(123.0)),
                &mut walker,
                &mut cell_key,
            );

            assert_eq!(
                walker_init_instructions(&mut walker),
                [FConst(
                    RegisterVariant::Local(Register(1)),
                    Total::from(123.0)
                )]
            );
        }

        #[test]
        fn test_copies_float_vars() {
            let mut cell_key = QCellOwner::new();
            let mut walker = setup();
            setup_var(LpcType::Float(false), &mut walker, &mut cell_key);

            assert_eq!(
                walker_init_instructions(&mut walker),
                [Copy(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Local(Register(2))
                )]
            );
        }

        #[test]
        fn test_does_not_copy_string_literals() {
            let mut cell_key = QCellOwner::new();
            let mut walker = setup();
            setup_literal(
                LpcType::Int(true),
                ExpressionNode::String(StringNode::new("foo")),
                &mut walker,
                &mut cell_key,
            );

            assert_eq!(
                walker_init_instructions(&mut walker),
                [SConst(RegisterVariant::Local(Register(1)), 0)]
            );
        }

        #[test]
        fn test_copies_string_vars() {
            let mut cell_key = QCellOwner::new();
            let mut walker = setup();
            setup_var(LpcType::String(false), &mut walker, &mut cell_key);

            assert_eq!(
                walker_init_instructions(&mut walker),
                [Copy(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Local(Register(2))
                )]
            );
        }

        #[test]
        fn test_does_not_copy_array_literals() {
            let mut cell_key = QCellOwner::new();
            let mut walker = setup();
            setup_literal(
                LpcType::Int(true),
                ExpressionNode::Array(ArrayNode::new(vec![ExpressionNode::from(1234)])),
                &mut walker,
                &mut cell_key,
            );

            assert_eq!(
                walker_init_instructions(&mut walker),
                [
                    IConst(RegisterVariant::Local(Register(1)), 1234),
                    ClearArrayItems,
                    PushArrayItem(RegisterVariant::Local(Register(1))),
                    AConst(RegisterVariant::Local(Register(2)),)
                ]
            );
        }

        #[test]
        fn test_copies_array_vars() {
            let mut cell_key = QCellOwner::new();
            let mut walker = setup();
            setup_var(LpcType::Int(true), &mut walker, &mut cell_key);

            assert_eq!(
                walker_init_instructions(&mut walker),
                [Copy(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Local(Register(2))
                )]
            );
        }

        #[test]
        fn copies_calls() {
            let mut cell_key = QCellOwner::new();
            let mut walker = setup();

            let mut node = VarInitNode {
                type_: LpcType::Object(false),
                name: "muffins".to_string(),
                value: Some(ExpressionNode::Call(CallNode {
                    receiver: None,
                    arguments: vec![ExpressionNode::from("/foo/bar.c")],
                    name: "clone_object".to_string(),
                    span: None,
                    namespace: CallNamespace::default(),
                })),
                array: false,
                global: false,
                span: None,
                flags: None,
            };

            insert_symbol(&mut walker, Symbol::from(&mut node.clone()));

            let _ = walker.visit_var_init(&mut node, &mut cell_key);

            assert_eq!(
                walker_init_instructions(&mut walker),
                [
                    SConst(RegisterVariant::Local(Register(1)), 0),
                    ClearArgs,
                    PushArg(RegisterVariant::Local(Register(1))),
                    CallEfun(1),
                    Copy(
                        RegisterVariant::Local(Register(0)),
                        RegisterVariant::Local(Register(2))
                    )
                ]
            );
        }

        #[test]
        fn sets_up_globals() {
            let mut cell_key = QCellOwner::new();
            let mut context = CompilationContext::default();
            context.scopes.push_new();
            let mut walker = CodegenWalker::new(context);

            let mut node = VarInitNode {
                type_: LpcType::Mixed(true),
                name: "arr".to_string(),
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
            };

            insert_symbol(&mut walker, Symbol::from(&mut node.clone()));

            let mut node2 = VarInitNode {
                type_: LpcType::Mixed(true),
                name: "str".to_string(),
                value: Some(ExpressionNode::from("sup")),
                array: false,
                global: true,
                span: None,
                flags: None,
            };

            insert_symbol(&mut walker, Symbol::from(&mut node.clone()));
            insert_symbol(&mut walker, Symbol::from(&mut node2.clone()));

            let _ = walker.visit_var_init(&mut node, &mut cell_key);
            let _ = walker.visit_var_init(&mut node2, &mut cell_key);

            let expected = vec![
                IConst(RegisterVariant::Local(Register(1)), 12),
                FConst(RegisterVariant::Local(Register(2)), 4.3.into()),
                SConst(RegisterVariant::Local(Register(3)), 0),
                IConst1(RegisterVariant::Local(Register(4))),
                IConst(RegisterVariant::Local(Register(5)), 2),
                IConst(RegisterVariant::Local(Register(6)), 3),
                ClearArrayItems,
                PushArrayItem(RegisterVariant::Local(Register(4))),
                PushArrayItem(RegisterVariant::Local(Register(5))),
                PushArrayItem(RegisterVariant::Local(Register(6))),
                AConst(RegisterVariant::Local(Register(7))),
                ClearArrayItems,
                PushArrayItem(RegisterVariant::Local(Register(1))),
                PushArrayItem(RegisterVariant::Local(Register(2))),
                PushArrayItem(RegisterVariant::Local(Register(3))),
                PushArrayItem(RegisterVariant::Local(Register(7))),
                AConst(RegisterVariant::Local(Register(8))),
                Copy(
                    RegisterVariant::Local(Register(8)),
                    RegisterVariant::Global(Register(0)),
                ),
                SConst(RegisterVariant::Local(Register(9)), 1),
                Copy(
                    RegisterVariant::Local(Register(9)),
                    RegisterVariant::Global(Register(1)),
                ),
            ];

            assert_eq!(walker_init_instructions(&mut walker), expected);
            assert_eq!(walker.global_counter.number_emitted(), 2);
            assert_eq!(walker.global_init_registers, 1);
        }

        #[test]
        fn sets_up_upvalues_when_initialized_to_upvalued_var() {
            let mut cell_key = QCellOwner::new();
            let mut context = CompilationContext::default();
            context.scopes.push_new(); // push a global scope
            context.scopes.push_new(); // push a local scope
            let mut walker = CodegenWalker::new(context);

            let existing_name = "existing";

            let mut node = create!(
                VarInitNode,
                name: "a".to_string(),
                value: Some(ExpressionNode::from(create!(VarNode, name: existing_name.to_string()))),
            );

            let mut sym = Symbol::from(&mut node.clone());
            sym.upvalue = true;

            let symbol_factory = SymbolFactory::new();
            let mut existing = symbol_factory.build(|sym| sym.name = existing_name.to_string());
            // let mut existing = create!(Symbol, name: existing_name.to_string());
            existing.location = Some(RegisterVariant::Local(Register(1)));

            insert_symbol(&mut walker, existing);
            insert_symbol(&mut walker, sym);

            let _ = walker.visit_var_init(&mut node, &mut cell_key);

            let sym = walker.context.lookup_var("a").unwrap();
            assert_eq!(sym.location.unwrap(), RegisterVariant::Upvalue(Register(0)));
        }

        #[test]
        fn sets_up_upvalues_when_initialized_to_upvalued_value() {
            let mut cell_key = QCellOwner::new();
            let mut context = CompilationContext::default();
            context.scopes.push_new(); // push a global scope
            context.scopes.push_new(); // push a local scope
            let mut walker = CodegenWalker::new(context);

            let mut node = create!(
                VarInitNode,
                name: "a".to_string(),
                value: Some(ExpressionNode::from(666))
            );

            let mut sym = Symbol::from(&mut node.clone());
            sym.upvalue = true;

            insert_symbol(&mut walker, sym);

            let _ = walker.visit_var_init(&mut node, &mut cell_key);

            let sym = walker.context.lookup_var("a").unwrap();
            assert_eq!(sym.location.unwrap(), RegisterVariant::Upvalue(Register(0)));
        }
    }

    mod test_visit_while {
        use lpc_rs_asm::instruction::Instruction::{EqEq, Jmp, Jz};

        use super::*;

        #[test]
        fn test_populates_the_instructions() {
            let mut cell_key = QCellOwner::new();
            let mut walker = default_walker(&mut cell_key);
            walker.backpatch_maps.push(HashMap::new());

            let mut node = WhileNode {
                condition: ExpressionNode::BinaryOp(BinaryOpNode {
                    l: Box::new(ExpressionNode::from(666)),
                    r: Box::new(ExpressionNode::from(777)),
                    op: BinaryOperation::EqEq,
                    span: None,
                }),
                body: Box::new(AstNode::Call(CallNode {
                    receiver: None,
                    arguments: vec![ExpressionNode::from("body")],
                    name: "dump".to_string(),
                    span: None,
                    namespace: CallNamespace::default(),
                })),
                scope_id: None,
                span: None,
            };

            let _ = walker.visit_while(&mut node, &mut cell_key);

            let expected = vec![
                IConst(RegisterVariant::Local(Register(1)), 666),
                IConst(RegisterVariant::Local(Register(2)), 777),
                EqEq(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Local(Register(2)),
                    RegisterVariant::Local(Register(3)),
                ),
                Jz(RegisterVariant::Local(Register(3)), Address(0)),
                SConst(RegisterVariant::Local(Register(4)), 0),
                ClearArgs,
                PushArg(RegisterVariant::Local(Register(4))),
                CallEfun(1),
                Jmp(Address(0)),
            ];

            assert_eq!(walker_init_instructions(&mut walker), expected);
        }
    }

    mod test_into_program {
        use super::*;

        #[test]
        fn sets_num_globals() {
            let mut cell_key = QCellOwner::new();

            let code = r##"
                int i = 123, j;
                mixed *arr = ({ "foo", "bar", "baz", ({ "quux", 0 }) });
                string asdf = "asdf";
                string b;
            "##;

            let program = walk_prog(code, &mut cell_key)
                .into_program()
                .expect("failed to compile");
            assert_eq!(program.num_globals, 5)
        }

        #[test]
        fn sets_num_init_registers() {
            let mut cell_key = QCellOwner::new();
            let code = r##"
                int i = 123, j;
                mixed *arr = ({ "foo", "bar", "baz", ({ "quux", 0 }) });
                string asdf = "asdf";
                string b;
            "##;

            let program = walk_prog(code, &mut cell_key)
                .into_program()
                .expect("failed to compile");
            assert_eq!(program.num_init_registers, 4)
        }

        #[test]
        fn reserves_enough_global_registers_when_create_returns_non_void() {
            let mut cell_key = QCellOwner::new();
            let code = r##"
                int create() {
                    dump("sup dawg");
                    int b = 123;
                    return b;
                }
            "##;

            let program = walk_prog(code, &mut cell_key)
                .into_program()
                .expect("failed to compile");
            assert_eq!(program.num_init_registers, 1)
        }

        #[test]
        fn sets_strings_on_functions() {
            let mut cell_key = QCellOwner::new();
            let code = r##"
                int create() {
                    dump("sup dawg");
                    int b = 123;
                    return b;
                }
            "##;

            let program = walk_prog(code, &mut cell_key)
                .into_program()
                .expect("failed to compile");
            assert_eq!(program.functions.len(), 1);
            assert_eq!(
                &**program
                    .functions
                    .values()
                    .next()
                    .unwrap()
                    .strings
                    .get()
                    .unwrap(),
                &vec![
                    "create__i____pb__".to_string(),
                    "sup dawg".to_string(),
                    "dump".to_string()
                ]
            );
        }
    }

    #[test]
    fn tracks_inherited_globals_for_init() {
        let mut cell_key = QCellOwner::new();
        let code = r##"
            inherit "/parent";
            int i = 123, j;
            string asdf = "asdf";
            string b;
        "##;

        let program = walk_prog(code, &mut cell_key)
            .into_program()
            .expect("failed to compile");
        let init = program.initializer.unwrap();

        assert_eq!(program.num_globals, 9);
        assert_eq!(init.num_locals, 6);
    }

    #[test]
    fn test_combine_inits() {
        let mut cell_key = QCellOwner::new();
        let init_prototype = FunctionPrototypeBuilder::default()
            .name(INIT_PROGRAM)
            .filename(LpcPath::InGame("/grandparent.c".into()))
            .return_type(LpcType::Void)
            .build()
            .unwrap();
        let create_prototype = FunctionPrototypeBuilder::default()
            .name(CREATE_FUNCTION)
            .filename(LpcPath::InGame("/grandparent.c".into()))
            .return_type(LpcType::Void)
            .build()
            .unwrap();

        let mut grandparent_init = ProgramFunction::new(init_prototype, 0);
        let grandparent_init_instructions = vec![
            IConst1(RegisterVariant::Local(Register(0))),
            IConst(RegisterVariant::Local(Register(0)), 666),
            ClearArgs,
            Call(1),
            Ret,
        ];
        let grandparent_spans = vec![
            Some(Span {
                l: 0,
                r: 1,
                file_id: 1
            });
            grandparent_init_instructions.len()
        ];
        let _ = std::mem::replace(
            &mut grandparent_init.instructions,
            grandparent_init_instructions,
        );
        let _ = std::mem::replace(&mut grandparent_init.debug_spans, grandparent_spans);

        let grandparent_create_mangle = create_prototype.mangle();

        let grandparent_create = ProgramFunction::new(create_prototype, 0);

        let parent_init_prototype = FunctionPrototypeBuilder::default()
            .name(INIT_PROGRAM)
            .filename(LpcPath::InGame("/parent.c".into()))
            .return_type(LpcType::Void)
            .build()
            .unwrap();

        let mut parent_init = ProgramFunction::new(parent_init_prototype, 0);

        let strings = Rc::new(vec![
            grandparent_init.mangle(),
            grandparent_create_mangle,
            parent_init.mangle(),
        ]);

        let mut grandparent = Program {
            initializer: Some(grandparent_init.into()),
            ..Default::default()
        };
        grandparent
            .functions
            .insert(CREATE_FUNCTION.to_string(), grandparent_create.into());

        let parent_init_instructions = vec![
            IConst1(RegisterVariant::Local(Register(0))),
            IConst(RegisterVariant::Local(Register(0)), 666),
            SConst(RegisterVariant::Local(Register(1)), 0),
            IConst(RegisterVariant::Local(Register(5)), 4321),
            ClearArgs,
            Call(1),
            Ret,
        ];
        let parent_spans = vec![
            Some(Span {
                l: 0,
                r: 1,
                file_id: 1
            });
            parent_init_instructions.len()
        ];
        let _ = std::mem::replace(&mut parent_init.instructions, parent_init_instructions);
        let _ = std::mem::replace(&mut parent_init.debug_spans, parent_spans);

        let parent = Program {
            functions: grandparent.functions.clone(),
            initializer: Some(parent_init.into()),
            ..Default::default()
        };

        let mut walker = default_walker(&mut cell_key);
        walker.context.inherits.push(parent);
        walker.context.strings = strings.iter().cloned().collect();

        let expected = vec![
            IConst1(RegisterVariant::Local(Register(0))),
            IConst(RegisterVariant::Local(Register(0)), 666),
            SConst(RegisterVariant::Local(Register(1)), 0),
            IConst(RegisterVariant::Local(Register(5)), 4321),
            // Note call to create is added later in the process, at the end of visit_program()
        ];

        let mut new_instructions = vec![];
        let mut new_spans = vec![];
        walker.combine_inits(&mut new_instructions, &mut new_spans);

        assert_eq!(new_instructions, expected);
    }

    fn insert_symbol(walker: &mut CodegenWalker, symbol: Symbol) {
        walker
            .context
            .scopes
            .current_mut()
            .expect("No current scope to insert the symbol into.")
            .insert(symbol)
    }
}
