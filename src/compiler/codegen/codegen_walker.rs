use std::{collections::HashMap, ops::Range, rc::Rc};

use if_chain::if_chain;
use indexmap::IndexMap;
use lpc_rs_asm::instruction::{Address, Instruction, Instruction::RegCopy, Label};
use lpc_rs_core::{
    call_namespace::CallNamespace,
    function::{FunctionName, FunctionReceiver, FunctionTarget},
    function_arity::FunctionArity,
    function_flags::FunctionFlags,
    lpc_type::LpcType,
    register::{Register, RegisterVariant},
    register_counter::RegisterCounter,
    ScopeId, CREATE_FUNCTION, INIT_PROGRAM,
};
use lpc_rs_errors::{span::Span, LpcError, Result};
use lpc_rs_function_support::{
    function_prototype::FunctionPrototype, program_function::ProgramFunction, symbol::Symbol,
};
use lpc_rs_utils::string::closure_arg_number;
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

    /// Track the currently-processing closure, so we know where to copy
    /// captured variables Symbols.
    closure_scope_stack: Vec<ScopeId>,

    /// Counter for labels, as they need to be unique.
    label_count: usize,

    /// Map of function Symbols, to their respective addresses
    pub functions: HashMap<String, Rc<ProgramFunction>>,

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

    /// Number of [`Register`]s needed for global initialization
    global_init_registers: usize,

    /// Compilation context
    context: CompilationContext,

    /// Labels where jumps at any particular time need to go to.
    jump_targets: Vec<JumpTarget>,

    /// Mapping of `switch` cases to the address of the first instruction for a
    /// match
    case_addresses: Vec<Vec<(SwitchCase, Address)>>,

    /// Because Ranges have two results, we store both locations when we
    /// `visit_range`.
    // TODO: test if this works with multi-dimensional arrays, or do we need to use a stack to
    // track these?
    visit_range_results: Option<(Option<RegisterVariant>, Option<RegisterVariant>)>,
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

        // subtract 1 because globals are stored in global r0 as well.
        result.global_counter.set(num_globals.saturating_sub(1));

        result.global_init_registers = num_init_registers;
        // subtract 1, so the next() call gives us the correct next register.
        result
            .register_counter
            .set(num_init_registers.saturating_sub(1));

        result.setup_init();

        result
    }

    /// Create the combined initialization function, with code taken from all
    /// of our inherited-from parents.
    #[instrument(skip_all)]
    pub fn setup_init(&mut self) {
        let prototype = FunctionPrototype::new(
            INIT_PROGRAM,
            LpcType::Void,
            FunctionArity::default(),
            FunctionFlags::default(),
            None,
            vec![],
            vec![],
        );

        let mut func = ProgramFunction::new(prototype, 0);

        let mut new_init_instructions = vec![];
        let mut new_init_debug_spans = vec![];
        self.combine_inits(&mut new_init_instructions, &mut new_init_debug_spans);

        func.instructions = new_init_instructions;
        func.debug_spans = new_init_debug_spans;

        self.function_stack.push(func);
    }

    /// Consume this walker and convert it into a [`Program`]
    pub fn into_program(mut self) -> Result<Program> {
        // These are expected and assumed to be in 1:1 correspondence at runtime
        self.ensure_sync()?;

        self.context.scopes.goto_root();
        let global_variables =
            std::mem::take(&mut self.context.scopes.current_mut().unwrap().symbols);

        Ok(Program {
            filename: self.context.filename,
            functions: self.functions,
            global_variables,
            // add +1 because return values for calls used to initialize
            // globals are stored in r0
            num_globals: self.global_counter.as_usize() + 1,
            num_upvalues: self.upvalue_counter.as_usize() + 1,
            // add +1 for r0, which is skipped
            num_init_registers: self.global_init_registers + 1,
            pragmas: self.context.pragmas,
            inherits: self.context.inherits,
            inherit_names: self.context.inherit_names,
        })
    }

    fn ensure_sync(&self) -> Result<()> {
        for func in self.functions.values() {
            let a = func.instructions.len();
            let b = func.debug_spans.len();
            if a != b {
                return Err(LpcError::new(format!(
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

    // /// Check for a symbol in the global scope
    // fn lookup_global(&self, name: &str) -> Option<&Symbol> {
    //     self.context.scopes.lookup_global(name)
    // }

    /// helper to choose operation instructions
    fn to_operation_type(&self, node: &ExpressionNode) -> OperationType {
        match node {
            ExpressionNode::Int(_)
            | ExpressionNode::Float(_) => OperationType::Register,

            ExpressionNode::String(_)
            | ExpressionNode::Array(_)
            | ExpressionNode::Mapping(_)
            // TODO: Calls can be optimized if we can get the return types available here
            | ExpressionNode::Call(_)
            | ExpressionNode::Closure(_)
            | ExpressionNode::CommaExpression(_)
            | ExpressionNode::Range(_)
            | ExpressionNode::FunctionPtr(_) => OperationType::Memory,
            ExpressionNode::Assignment(node) => self.to_operation_type(&node.lhs),
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
            ExpressionNode::Var(v) => {
                match self.context.lookup_var(&v.name) {
                    Some(Symbol { type_: ty, .. }) => {
                        match ty {
                            LpcType::Int(false) => OperationType::Register,
                            LpcType::Float(false) => OperationType::Register,
                            _ => OperationType::Memory,
                        }
                    }
                    None => OperationType::Memory // arbitrary - doing this instead of panicking
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
    fn visit_parameter(&mut self, node: &VarInitNode) -> RegisterVariant {
        let loc = self.assign_sym_location(&node.name);

        self.context.lookup_var(&node.name).map(|sym| {
            if matches!(loc, RegisterVariant::Upvalue(_)) {
                // increment the counter for parameters that are captured by closures
                self.function_upvalue_counter.next().unwrap();
            }
            let func = self.function_stack.last_mut().unwrap();
            func.local_variables.push(sym.clone())
        });

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

        sym.location = Some(current_register);

        current_register
    }

    /// Emit the instruction(s) to take the range of an array or string
    /// # Arguments
    /// `reference` - The [`Register`] holding the reference to the ref we're
    /// taking a slice from. `node` - A reference to the [`RangeNode`] that
    /// holds the range of the slice we're taking.
    fn emit_range(&mut self, reference: RegisterVariant, node: &mut RangeNode) -> Result<()> {
        let first_index = if let Some(expr) = &mut *node.l {
            expr.visit(self)?;
            self.current_result
        } else {
            // Default to 0. No instruction needed as the value in registers defaults to int
            // 0.
            self.register_counter.next().unwrap().as_local()
        };

        let second_index = if let Some(expr) = &mut *node.r {
            expr.visit(self)?;
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

    // special case for `catch()`
    fn emit_catch(&mut self, node: &mut CallNode) -> Result<()> {
        let result_register = self.register_counter.next().unwrap().as_local();
        let label = self.new_label("catch_end");

        push_instruction!(
            self,
            Instruction::CatchStart(result_register, label.clone()),
            node.span
        );

        for argument in &mut node.arguments {
            argument.visit(self)?;
        }

        // get the address of the `catchend` pseudo-instruction, so we can jump to a
        // location that is both guaranteed to have an instruction, as well as
        // clean up the handled catch point
        let label_address = self.current_address();
        self.insert_label(label, label_address);

        push_instruction!(self, Instruction::CatchEnd, node.span);

        self.current_result = result_register;

        Ok(())
    }

    #[inline]
    fn current_address(&self) -> Address {
        match self.function_stack.last() {
            Some(x) => x.instructions.len(),
            None => 0,
        }
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
    ///   instructions will be stored. Done this way to avoid a lot of vector
    ///   creations in the recursion.
    /// * debug_spans: A mutable reference to a vector, where the debug spans of
    ///   the combined instructions will be stored.
    fn combine_inits(
        &mut self,
        instructions: &mut Vec<Instruction>,
        debug_spans: &mut Vec<Option<Span>>,
    ) {
        let calls_create = |instructions: &[Instruction]| -> bool {
            let len = instructions.len();

            if_chain! {
                if len > 1;
                if let Instruction::Call { name, .. } = &instructions[len - 2];
                if name == CREATE_FUNCTION && matches!(&instructions[len - 1], Instruction::Ret);
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
                0..len - 2
            } else if len > 0 && matches!(&instructions[len - 1], Instruction::Ret) {
                0..len - 1
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
                    instructions.truncate(instructions.len() - 2);
                    debug_spans.truncate(debug_spans.len() - 2);
                }
                instructions.extend(func.instructions[range.clone()].iter().cloned());
                debug_spans.extend(func.debug_spans[range].iter());
            };
        for inherit in &self.context.inherits {
            if let Some(func) = inherit.functions.get(INIT_PROGRAM) {
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
    ) -> Option<usize> {
        if num_default_args > 0 {
            let address = Some(self.current_address());

            // the addresses are backpatched below, once we have them.
            let instruction = Instruction::PopulateDefaults(vec![]);
            push_instruction!(self, instruction, span);

            address
        } else {
            None
        }
    }

    fn setup_populate_argv(
        &mut self,
        ellipsis: bool,
        span: Option<Span>,
        passed_param_count: usize,
    ) -> Option<usize> {
        if ellipsis {
            let argv_location = self.assign_sym_location(ARGV);

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
        populate_defaults_index: Option<usize>,
        start_label: Label,
    ) -> Result<()> {
        let mut default_init_addresses = vec![];

        for (idx, parameter) in parameters.iter_mut().enumerate() {
            if let Some(value) = &mut parameter.value {
                default_init_addresses.push(self.current_address());

                // generate code for only the value, then assign by hand, because we
                // pre-generated locations of the parameters above.
                value.visit(self)?;
                let instruction = RegCopy(self.current_result, declared_arg_locations[idx]);
                push_instruction!(self, instruction, span);
            }
        }

        // backpatch the the correct init addresses for the PopulateDefaults call.
        if let Some(idx) = populate_defaults_index {
            let sym = self.function_stack.last_mut().unwrap();
            let instruction = &sym.instructions[idx];
            if let Instruction::PopulateDefaults(_) = instruction {
                let new_instruction = Instruction::PopulateDefaults(default_init_addresses);
                sym.instructions[idx] = new_instruction;
            } else {
                return Err(LpcError::new("Invalid populate_defaults_index").with_span(span));
            }
        }

        // jump back to the function now that defaults are populated.
        let instruction = Instruction::Jmp(start_label);
        push_instruction!(self, instruction, span);

        Ok(())
    }

    fn backpatch_populate_argv(
        func: &mut ProgramFunction,
        populate_argv_index: usize,
        span: Option<Span>,
    ) -> Result<()> {
        let instruction = &func.instructions[populate_argv_index];

        if let Instruction::PopulateArgv(loc, num_args, _) = instruction {
            let new_instruction = Instruction::PopulateArgv(*loc, *num_args, func.num_locals);
            func.instructions[populate_argv_index] = new_instruction;

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
    fn visit_array(&mut self, node: &mut ArrayNode) -> Result<()> {
        let mut items = Vec::with_capacity(node.value.len());
        for member in &mut node.value {
            let _ = member.visit(self);
            items.push(self.current_result);
        }

        let register = self.register_counter.next().unwrap().as_local();
        self.current_result = register;
        push_instruction!(self, Instruction::AConst(register, items), node.span);

        Ok(())
    }

    #[instrument(skip_all)]
    fn visit_assignment(&mut self, node: &mut AssignmentNode) -> Result<()> {
        node.rhs.visit(self)?;
        let rhs_result = self.current_result;
        let lhs = &mut *node.lhs;

        match lhs {
            ExpressionNode::Var(_) => {
                lhs.visit(self)?;
                let lhs_result = self.current_result;

                let assign = Instruction::RegCopy(rhs_result, lhs_result);

                push_instruction!(self, assign, node.span);

                self.current_result = lhs_result;
            }
            ExpressionNode::BinaryOp(BinaryOpNode {
                op: BinaryOperation::Index,
                ref mut l,
                ref mut r,
                ..
            }) => {
                l.visit(self)?;
                let var_result = self.current_result;
                r.visit(self)?;
                let index_result = self.current_result;

                let store = Instruction::Store(rhs_result, var_result, index_result);

                push_instruction!(self, store, node.span);

                self.current_result = rhs_result;
            }
            x => {
                return Err(LpcError::new(format!(
                    "Attempt to assign to an invalid lvalue: `{}`",
                    x
                ))
                .with_span(node.span))
            }
        }

        Ok(())
    }

    #[instrument(skip_all)]
    fn visit_binary_op(&mut self, node: &mut BinaryOpNode) -> Result<()> {
        node.l.visit(self)?;
        let reg_left = self.current_result;

        match node.op {
            BinaryOperation::Index => {
                // Ranges need special handling that complicates this function otherwise, due to
                // the visit to node.r needing to handle multiple results.
                if let ExpressionNode::Range(range_node) = &mut *node.r {
                    self.emit_range(reg_left, range_node)?;
                    return Ok(());
                }
            }
            BinaryOperation::AndAnd => {
                // Handle short-circuit behavior
                let end_label = self.new_label("andand-end");
                let instruction = Instruction::Jz(reg_left, end_label.clone());
                push_instruction!(self, instruction, node.span);

                node.r.visit(self)?;
                let reg_right = self.current_result;
                let instruction = Instruction::Jz(reg_right, end_label.clone());
                push_instruction!(self, instruction, node.span);

                let reg_result = self.register_counter.next().unwrap().as_local();
                self.current_result = reg_result;

                let instruction = Instruction::RegCopy(reg_right, reg_result);
                push_instruction!(self, instruction, node.span);

                self.insert_label(end_label, self.current_address());

                return Ok(());
            }
            BinaryOperation::OrOr => {
                // Handle short-circuit behavior
                let end_label = self.new_label("oror-end");

                let reg_result = self.register_counter.next().unwrap().as_local();
                let instruction = Instruction::RegCopy(reg_left, reg_result);
                push_instruction!(self, instruction, node.span);

                let instruction = Instruction::Jnz(reg_result, end_label.clone());
                push_instruction!(self, instruction, node.span);

                node.r.visit(self)?;
                let reg_right = self.current_result;
                let instruction = Instruction::RegCopy(reg_right, reg_result);
                push_instruction!(self, instruction, node.span);

                self.insert_label(end_label, self.current_address());
                self.current_result = reg_result;

                return Ok(());
            }
            _ => {}
        }

        node.r.visit(self)?;
        let reg_right = self.current_result;

        let reg_result = self.register_counter.next().unwrap().as_local();
        self.current_result = reg_result;

        let instruction = self.choose_op_instruction(node, reg_left, reg_right, reg_result);
        push_instruction!(self, instruction, node.span);

        Ok(())
    }

    #[instrument(skip_all)]
    fn visit_block(&mut self, node: &mut BlockNode) -> Result<()> {
        self.context.scopes.goto(node.scope_id);

        for stmt in &mut node.body {
            stmt.visit(self)?;
        }

        self.context.scopes.pop();
        Ok(())
    }

    #[instrument(skip_all)]
    fn visit_break(&mut self, node: &mut BreakNode) -> Result<()> {
        if let Some(JumpTarget { break_target, .. }) = self.jump_targets.last() {
            let instruction = Instruction::Jmp(break_target.into());
            push_instruction!(self, instruction, node.span);
            return Ok(());
        }

        Err(LpcError::new("`break` statement without a jump target?").with_span(node.span))
    }

    #[instrument(skip_all)]
    fn visit_call(&mut self, node: &mut CallNode) -> Result<()> {
        if node.name == CATCH {
            return self.emit_catch(node);
        }

        let argument_len = node.arguments.len();
        let mut arg_results = Vec::with_capacity(argument_len);

        for argument in &mut node.arguments {
            argument.visit(self)?;
            arg_results.push(self.current_result);
        }

        let instruction = if arg_results.len() == 1 {
            // no need to serialize args for the `Call` instruction if there's only one.
            if let Some(rcvr) = &mut node.receiver {
                rcvr.visit(self)?;
                let receiver_result = self.current_result;
                let name_register = self.register_counter.next().unwrap().as_local();
                push_instruction!(
                    self,
                    Instruction::SConst(name_register, node.name.clone()),
                    node.span
                );

                Instruction::CallOther {
                    receiver: receiver_result,
                    name: name_register,
                    num_args: arg_results.len(),
                    initial_arg: arg_results[0],
                }
            } else if node.name == SIZEOF {
                let result = self.register_counter.next().unwrap().as_local();

                let instruction = Instruction::Sizeof(*arg_results.first().unwrap(), result);
                push_instruction!(self, instruction, node.span);

                return Ok(());
            } else {
                if_chain! {
                    if let Some(x) = self.context.lookup_var(&node.name);
                    if x.type_.matches_type(LpcType::Function(false));
                    then {
                        // if there's a function pointer with this name in scope, call that.
                        Instruction::CallFp {
                            location: x.location.unwrap(),
                            num_args: arg_results.len(),
                            initial_arg: arg_results[0],
                        }
                    } else {
                        Instruction::Call {
                            name: node.name.clone(),
                            namespace: node.namespace.clone(),
                            num_args: arg_results.len(),
                            initial_arg: arg_results[0],
                        }
                    }
                }
            }
        } else {
            let start_register = self.register_counter.next().unwrap().as_local();
            let mut register = start_register;

            // copy each result to the start of the arg register
            for result in &arg_results {
                push_instruction!(self, Instruction::RegCopy(*result, register), node.span);
                register = self.register_counter.next().unwrap().as_local();
            }

            // Undo the final call to .next() in the above for-loop,
            // to avoid wasting a register
            self.register_counter.go_back();

            if let Some(rcvr) = &mut node.receiver {
                rcvr.visit(self)?;
                let receiver_result = self.current_result;

                let name_register = self.register_counter.next().unwrap().as_local();
                push_instruction!(
                    self,
                    Instruction::SConst(name_register, node.name.clone()),
                    node.span
                );

                Instruction::CallOther {
                    receiver: receiver_result,
                    name: name_register,
                    num_args: arg_results.len(),
                    initial_arg: start_register,
                }
            } else if node.name == CALL_OTHER {
                let receiver = arg_results[0];
                let name = arg_results[1];

                Instruction::CallOther {
                    receiver,
                    name,
                    num_args: arg_results.len() - 2,
                    initial_arg: if arg_results.len() > 2 {
                        arg_results[2]
                    } else {
                        arg_results[0] // i.e. no args used in the function
                    },
                }
            } else {
                if_chain! {
                    if let Some(x) = self.context.lookup_var(&node.name);
                    if x.type_.matches_type(LpcType::Function(false));
                    then {
                        Instruction::CallFp {
                            location: x.location.unwrap(),
                            num_args: arg_results.len(),
                            initial_arg: start_register,
                        }
                    } else {
                        Instruction::Call {
                            name: node.name.clone(),
                            namespace: node.namespace.clone(),
                            num_args: arg_results.len(),
                            initial_arg: start_register,
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
                Instruction::RegCopy(Register(0).as_local(), next_register),
                node.span()
            );

            walker.current_result = next_register;
        };

        // Take care of the result after the call returns.
        if let Some(func) = self
            .context
            .lookup_function_complete(&node.name, &node.namespace)
        {
            if func.as_ref().return_type == LpcType::Void {
                self.current_result = Register(0).as_local();
            } else {
                push_copy(self);
            }
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
            return Err(LpcError::new(format!(
                "Unable to find return type for `{}`. This is a weird issue that indicates \
                something very broken in the semantic checks.",
                node.name
            ))
            .with_span(node.span));
        }

        Ok(())
    }

    #[instrument(skip_all)]
    fn visit_closure(&mut self, node: &mut ClosureNode) -> Result<()> {
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

        let sym = ProgramFunction::new(prototype.clone(), 0);

        self.function_stack.push(sym);

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

        self.register_counter.push(0);
        // Note that `upvalue_counter` is *not* pushed here.
        // We want to keep a consistent count of upvalues across all closures
        // that are declared somewhere within this function
        self.function_upvalue_counter.push(0);

        self.context.scopes.goto(node.scope_id); // XXX difference between closure and function def
        let declared_arg_count = node // XXX difference btwn closures & functions
            .parameters
            .as_ref()
            .map(|nodes| nodes.len())
            .unwrap_or_default();
        // TODO: $ variables need to be able to refer to upvalues, and not assume local
        let declared_arg_locations = node // XXX
            .parameters
            .as_ref()
            .map(|nodes| self.visit_parameters(nodes))
            .unwrap_or_default();

        let populate_defaults_index = self.setup_populate_defaults(node.span, num_default_args);

        // bump the register counter if they have used `$\d` vars that go beyond
        // declared parameters, so that the positional params point to the correct slot.
        // TODO: fix this for upvalues / default parameters
        let current_index = self.register_counter.as_usize();
        if num_args > current_index {
            self.register_counter.set(num_args);
            self.current_result = Register(num_args).as_local();
        }

        let populate_argv_index =
            self.setup_populate_argv(node.flags.ellipsis(), node.span, declared_arg_count);

        let start_label = self.new_label("closure-body-start");
        self.insert_label(&start_label, self.current_address());

        for expression in &mut node.body {
            expression.visit(self)?;
        }

        // return the current result if there is no explicit return.
        {
            let sym = self.function_stack.last_mut().unwrap();
            if sym.instructions.len() == len
                || (!sym.instructions.is_empty()
                    && *sym.instructions.last().unwrap() != Instruction::Ret)
            {
                let target = RegisterVariant::Local(Register(0));

                if self.current_result != target {
                    sym.push_instruction(RegCopy(self.current_result, target), node.span);
                }

                sym.push_instruction(Instruction::Ret, node.span);
            }
        }

        debug_assert_eq!(declared_arg_count, declared_arg_locations.len());

        if num_default_args > 0 {
            if let Some(parameters) = &mut node.parameters {
                self.init_default_params(
                    parameters,
                    &declared_arg_locations,
                    node.span,
                    populate_defaults_index,
                    start_label,
                )?;
            }
        }

        self.context.scopes.pop();
        self.closure_scope_stack.pop();
        let mut func = self.function_stack.pop().unwrap();
        func.num_locals = self.register_counter.as_usize() - num_args;

        func.num_upvalues = self.function_upvalue_counter.number_emitted();

        func.arg_locations = declared_arg_locations;

        if let Some(idx) = populate_argv_index {
            Self::backpatch_populate_argv(&mut func, idx, node.span)?;
        }

        self.functions.insert(node.name.clone(), func.into());

        self.function_upvalue_counter.pop();
        self.register_counter.pop();

        // At this point, the closure has been generated and stored.
        // We just need to store a reference to it in the current result.
        self.context.scopes.goto(parent_scope_id);

        let location = self.register_counter.next().unwrap().as_local();
        self.current_result = location;

        // closures are just pointers to functions
        // TODO: Ensure this still works when calling a
        //       returned closure in another object
        let target = FunctionTarget::Local(
            FunctionName::Literal(node.name.clone()),
            FunctionReceiver::Local,
        );

        let instruction = Instruction::FunctionPtrConst {
            location,
            target,
            arity,
            applied_arguments: vec![],
        };

        push_instruction!(self, instruction, node.span);

        Ok(())
    }

    #[instrument(skip_all)]
    fn visit_continue(&mut self, node: &mut ContinueNode) -> Result<()> {
        if let Some(JumpTarget {
            continue_target, ..
        }) = self.jump_targets.last()
        {
            let instruction = Instruction::Jmp(continue_target.into());
            push_instruction!(self, instruction, node.span);
            return Ok(());
        }

        Err(LpcError::new("`continue` statement without a jump target?").with_span(node.span))
    }

    #[instrument(skip_all)]
    fn visit_decl(&mut self, node: &mut DeclNode) -> Result<()> {
        for init in &mut node.initializations {
            self.visit_var_init(init)?;
        }

        Ok(())
    }

    #[instrument(skip_all)]
    fn visit_do_while(&mut self, node: &mut DoWhileNode) -> Result<()> {
        self.context.scopes.goto(node.scope_id);

        let start_label = self.new_label("do-while-start");
        let end_label = self.new_label("do-while-end");
        let continue_label = self.new_label("do-while-continue");
        let jump_target = JumpTarget::new(end_label.clone(), continue_label.clone());
        self.jump_targets.push(jump_target);

        let start_addr = self.current_address();
        self.insert_label(start_label.clone(), start_addr);

        node.body.visit(self)?;

        let continue_addr = self.current_address();
        self.insert_label(continue_label, continue_addr);

        node.condition.visit(self)?;

        // Go back to the start of the loop if the result isn't zero
        let instruction = Instruction::Jnz(self.current_result, start_label);
        push_instruction!(self, instruction, node.span);
        let end_addr = self.current_address();
        self.insert_label(end_label, end_addr);

        self.context.scopes.pop();
        self.jump_targets.pop();
        Ok(())
    }

    #[instrument(skip_all)]
    fn visit_float(&mut self, node: &mut FloatNode) -> Result<()> {
        let register = self.register_counter.next().unwrap().as_local();
        self.current_result = register;
        let instruction = Instruction::FConst(self.current_result, node.value);
        push_instruction!(self, instruction, node.span);

        Ok(())
    }

    #[instrument(skip_all)]
    fn visit_for(&mut self, node: &mut ForNode) -> Result<()> {
        self.context.scopes.goto(node.scope_id);

        if let Some(i) = &mut *node.initializer {
            i.visit(self)?;
        }

        let start_label = self.new_label("for-start");
        let end_label = self.new_label("for-end");
        let continue_label = self.new_label("for-continue");
        let jump_target = JumpTarget::new(end_label.clone(), continue_label.clone());
        self.jump_targets.push(jump_target);
        let start_addr = self.current_address();
        self.insert_label(start_label.clone(), start_addr);

        if let Some(cond) = &mut node.condition {
            cond.visit(self)?;

            let instruction = Instruction::Jz(self.current_result, end_label.clone());
            push_instruction!(self, instruction, cond.span());
        };

        node.body.visit(self)?;

        let continue_addr = self.current_address();
        self.insert_label(continue_label, continue_addr);

        if let Some(i) = &mut node.incrementer {
            i.visit(self)?;
        }

        // go back to the start of the loop
        let instruction = Instruction::Jmp(start_label);
        push_instruction!(self, instruction, node.span);

        let addr = self.current_address();
        self.insert_label(end_label, addr);

        self.context.scopes.pop();
        self.jump_targets.pop();
        Ok(())
    }

    #[instrument(skip_all)]
    fn visit_foreach(&mut self, node: &mut ForEachNode) -> Result<()> {
        self.context.scopes.goto(node.scope_id);

        node.collection.visit(self)?;
        let collection_location = self.current_result;

        let index_location = self.assign_sym_location(FOREACH_INDEX);
        let length_location = self.assign_sym_location(FOREACH_LENGTH);

        let instruction = Instruction::Sizeof(collection_location, length_location);
        push_instruction!(self, instruction, node.span);

        let locations = match &mut node.initializer {
            ForEachInit::Array(ref mut node) | ForEachInit::String(ref mut node) => {
                node.visit(self)?;

                vec![self.current_result]
            }
            ForEachInit::Mapping {
                ref mut key,
                ref mut value,
            } => {
                key.visit(self)?;
                let key_result = self.current_result;
                value.visit(self)?;
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

        let instruction = Instruction::Jnz(eqeq_result, end_label.clone());
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

        node.body.visit(self)?;

        let continue_addr = self.current_address();
        self.insert_label(continue_label, continue_addr);

        let instruction = Instruction::Inc(index_location);
        push_instruction!(self, instruction, node.span);

        // go back to the start of the loop
        let instruction = Instruction::Jmp(start_label);
        push_instruction!(self, instruction, node.span);

        let addr = self.current_address();
        self.insert_label(end_label, addr);

        self.context.scopes.pop();
        self.jump_targets.pop();
        Ok(())
    }

    #[instrument(skip_all)]
    fn visit_function_def(&mut self, node: &mut FunctionDefNode) -> Result<()> {
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

        let len = self.current_address();
        self.register_counter.push(0);
        self.upvalue_counter.push(0);
        self.function_upvalue_counter.push(0);

        self.context.scopes.goto_function(&node.name)?;
        let declared_arg_count = node.parameters.len();
        let declared_arg_locations = self.visit_parameters(&node.parameters);

        let populate_defaults_index = self.setup_populate_defaults(node.span, num_default_args);

        let populate_argv_index =
            self.setup_populate_argv(node.flags.ellipsis(), node.span, declared_arg_count);

        let start_label = self.new_label("function-body-start");
        self.insert_label(&start_label, self.current_address());

        for expression in &mut node.body {
            expression.visit(self)?;
        }

        // insert a final return if one isn't already there.
        {
            let sym = self.function_stack.last_mut().unwrap();
            if sym.instructions.len() == len
                || (!sym.instructions.is_empty()
                    && *sym.instructions.last().unwrap() != Instruction::Ret)
            {
                // TODO: This should emit a warning unless the return type is void
                sym.push_instruction(Instruction::Ret, node.span);
            }
        }

        debug_assert_eq!(declared_arg_count, declared_arg_locations.len());

        if num_default_args > 0 {
            self.init_default_params(
                &mut node.parameters,
                &declared_arg_locations,
                node.span,
                populate_defaults_index,
                start_label,
            )?;
        }

        self.context.scopes.pop();
        let mut func = self.function_stack.pop().unwrap();
        func.num_locals = self.register_counter.as_usize() - num_args;
        func.num_upvalues = self.function_upvalue_counter.number_emitted();

        func.arg_locations = declared_arg_locations;

        if let Some(idx) = populate_argv_index {
            Self::backpatch_populate_argv(&mut func, idx, node.span)?;
        }

        self.functions.insert(node.name.clone(), func.into());

        self.function_upvalue_counter.pop();
        self.upvalue_counter.pop();
        self.register_counter.pop();

        Ok(())
    }

    #[instrument(skip_all)]
    fn visit_function_ptr(&mut self, node: &mut FunctionPtrNode) -> Result<()> {
        let mut applied_arguments = vec![];
        if let Some(args) = &mut node.arguments {
            for argument in args {
                if let Some(n) = argument {
                    n.visit(self)?;
                    applied_arguments.push(Some(self.current_result));
                } else {
                    applied_arguments.push(None);
                }
            }
        }

        let target;
        let arity;

        if let Some(rcvr) = &mut node.receiver {
            // remote receiver, i.e. `call_other`
            let receiver = match rcvr {
                FunctionPtrReceiver::Static(rcvr_node) => {
                    rcvr_node.visit(self)?;
                    FunctionReceiver::Var(self.current_result)
                }

                // `&` used as the receiver
                FunctionPtrReceiver::Dynamic => FunctionReceiver::Argument,
            };

            // `call_other` always assumes a literal name
            let name = FunctionName::Literal(node.name.clone());

            target = FunctionTarget::Local(name, receiver);

            // just use a placeholder arity that allows anything
            arity = FunctionArity {
                num_args: 0,
                num_default_args: 0,
                varargs: true,
                ellipsis: true,
            };
        } else if let Some(prototype) = self
            .context
            .lookup_function(node.name.as_str(), &CallNamespace::Local)
        {
            // A local / inherited function

            // Determine if the name is a var, or a literal function name.
            // Vars take precedence.
            let name = match self.context.lookup_var(&node.name) {
                Some(s) => {
                    if !s.type_.matches_type(LpcType::Function(false)) {
                        // if there are no function-type vars of this name, assume the name is
                        // literal
                        FunctionName::Literal(node.name.clone())
                    } else {
                        let sym_loc = match s.location {
                            Some(l) => l,
                            None => {
                                return Err(LpcError::new(format!(
                                    "Symbol `{}` has no location set.",
                                    s.name
                                ))
                                .with_span(node.span));
                            }
                        };

                        FunctionName::Var(sym_loc)
                    }
                }
                None => FunctionName::Literal(node.name.clone()),
            };

            target = FunctionTarget::Local(name, FunctionReceiver::Local);
            arity = prototype.arity;
        } else {
            if_chain! {
                if let Some(se) = &self.context.simul_efuns;
                let simul_efuns = se.borrow();
                if let Some(prototype) = simul_efuns.lookup_function(node.name.as_str(), &CallNamespace::Local);
                then {
                    let name = FunctionName::Literal(node.name.clone());
                    target = FunctionTarget::Local(name, FunctionReceiver::Local);
                    arity = prototype.arity();
                } else {
                    if let Some(prototype) = EFUN_PROTOTYPES.get(node.name.as_str()) {
                        target = FunctionTarget::Efun(node.name.clone());
                        arity = prototype.arity;
                    } else {
                        return Err(LpcError::new(format!(
                            "unknown call in function pointer: `{}`",
                            node.name
                        ))
                            .with_span(node.span));
                    }
                }
            }
        }

        let location = self.register_counter.next().unwrap().as_local();
        self.current_result = location;

        let instruction = Instruction::FunctionPtrConst {
            location,
            target,
            arity,
            applied_arguments,
        };

        push_instruction!(self, instruction, node.span);

        Ok(())
    }

    #[instrument(skip_all)]
    fn visit_if(&mut self, node: &mut IfNode) -> Result<()> {
        self.context.scopes.goto(node.scope_id);
        let else_label = self.new_label("if-else");
        let end_label = self.new_label("if-end");

        // Visit the condition
        node.condition.visit(self)?;

        // If the condition is false (i.e. equal to 0 or 0.0), jump to the end of the
        // "then" body. Insert a placeholder address, which we correct below
        // after the body's code is generated
        let instruction = Instruction::Jz(self.current_result, else_label.clone());
        push_instruction!(self, instruction, node.span);

        // Generate the main body of the statement
        node.body.visit(self)?;

        if node.else_clause.is_some() {
            let instruction = Instruction::Jmp(end_label.clone());
            push_instruction!(self, instruction, node.span);
        }

        let addr = self.current_address();
        self.insert_label(else_label, addr);

        // Generate the else clause code if necessary
        if let Some(n) = &mut *node.else_clause {
            n.visit(self)?;

            let addr = self.current_address();
            self.insert_label(end_label, addr);
        }

        self.context.scopes.pop();
        Ok(())
    }

    #[instrument(skip_all)]
    fn visit_int(&mut self, node: &mut IntNode) -> Result<()> {
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
    fn visit_label(&mut self, node: &mut LabelNode) -> Result<()> {
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
    fn visit_mapping(&mut self, node: &mut MappingNode) -> Result<()> {
        let mut map = IndexMap::new();
        for (key, value) in &mut node.value {
            key.visit(self)?;
            let key_result = self.current_result;
            value.visit(self)?;

            map.insert(key_result, self.current_result);
        }

        let register = self.register_counter.next().unwrap().as_local();
        self.current_result = register;
        push_instruction!(self, Instruction::MapConst(register, map), node.span);

        Ok(())
    }

    #[instrument(skip_all)]
    fn visit_program(&mut self, program: &mut ProgramNode) -> Result<()> {
        self.context.scopes.goto_root();
        self.setup_init();

        // Partition global variable initializations vs everything else
        let (global_init, functions): (Vec<&mut AstNode>, Vec<&mut AstNode>) = program
            .body
            .iter_mut()
            .partition(|x| matches!(**x, AstNode::Decl(_)));

        // Hoist all global variables, and initialize them at the very start
        // of the program (i.e. at the time it's cloned)
        for node in global_init {
            node.visit(self)?;
        }

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
                namespace: CallNamespace::default(),
            };
            call.visit(self)?;
        }

        let mut ret = ReturnNode {
            value: None,
            span: None,
        };
        ret.visit(self)?;

        for node in functions {
            node.visit(self)?;
        }

        let mut sym = self.function_stack.pop().unwrap();
        sym.num_locals = self.register_counter.as_usize();

        self.functions.insert(sym.name().to_string(), sym.into());

        self.context.scopes.pop();

        Ok(())
    }

    #[instrument(skip_all)]
    fn visit_range(&mut self, node: &mut RangeNode) -> Result<()> {
        let mut result_left: Option<RegisterVariant> = None;
        let mut result_right: Option<RegisterVariant> = None;
        if let Some(expr) = &mut *node.l {
            expr.visit(self)?;
            result_left = Some(self.current_result);
        }

        if let Some(expr) = &mut *node.r {
            expr.visit(self)?;
            result_right = Some(self.current_result);
        }

        self.visit_range_results = Some((result_left, result_right));

        Ok(())
    }

    #[instrument(skip_all)]
    fn visit_return(&mut self, node: &mut ReturnNode) -> Result<()> {
        if let Some(expression) = &mut node.value {
            expression.visit(self)?;
            let copy = Instruction::RegCopy(self.current_result, Register(0).as_local());
            push_instruction!(self, copy, expression.span());
        }

        push_instruction!(self, Instruction::Ret, node.span);

        Ok(())
    }

    #[instrument(skip_all)]
    fn visit_string(&mut self, node: &mut StringNode) -> Result<()> {
        let register = self.register_counter.next().unwrap().as_local();
        self.current_result = register;

        push_instruction!(
            self,
            Instruction::SConst(register, node.value.clone()),
            node.span
        );

        Ok(())
    }

    #[instrument(skip_all)]
    fn visit_switch(&mut self, node: &mut SwitchNode) -> Result<()> {
        node.expression.visit(self)?;
        let expr_result = self.current_result;

        let test_label = self.new_label("switch-test");
        let instruction = Instruction::Jmp(test_label.clone());
        push_instruction!(self, instruction, node.span);

        let end_label = self.new_label("switch-end");
        self.jump_targets
            .push(JumpTarget::new(end_label.clone(), "".into()));
        let addresses = vec![];
        self.case_addresses.push(addresses);

        node.body.visit(self)?;

        // skip over the tests that we're about to generate.
        let instruction = Instruction::Jmp(end_label.clone());
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
                    case_expr.visit(self)?;
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
                    let instruction = Instruction::Jnz(test_result, case_label.clone());
                    push_instruction!(self, instruction, node.span);
                    self.insert_label(case_label, case_address.1);
                }
                None => {
                    let default_label = self.new_label("switch-default");
                    let instruction = Instruction::Jmp(default_label.clone());
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
    fn visit_ternary(&mut self, node: &mut TernaryNode) -> Result<()> {
        let result_reg = self.register_counter.next().unwrap().as_local();
        let else_label = self.new_label("ternary-else");
        let end_label = self.new_label("ternary-end");

        node.condition.visit(self)?;

        let instruction = Instruction::Jz(self.current_result, else_label.clone());
        push_instruction!(self, instruction, node.span);

        node.body.visit(self)?;
        push_instruction!(
            self,
            Instruction::RegCopy(self.current_result, result_reg),
            node.span
        );

        let instruction = Instruction::Jmp(end_label.clone());
        push_instruction!(self, instruction, node.span);

        let else_addr = self.current_address();
        self.insert_label(else_label, else_addr);

        node.else_clause.visit(self)?;
        push_instruction!(
            self,
            Instruction::RegCopy(self.current_result, result_reg),
            node.span
        );

        let end_addr = self.current_address();
        self.insert_label(end_label, end_addr);

        self.current_result = result_reg;
        Ok(())
    }

    #[instrument(skip_all)]
    fn visit_unary_op(&mut self, node: &mut UnaryOpNode) -> Result<()> {
        node.expr.visit(self)?;
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
                    let copy = Instruction::RegCopy(location, temp);
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
    fn visit_var(&mut self, node: &mut VarNode) -> Result<()> {
        if node.is_closure_arg_var() {
            let idx = closure_arg_number(&node.name)?;
            self.current_result = Register(idx).as_local();

            return Ok(());
        }

        if node.function_name {
            let mut fptr_node = FunctionPtrNode {
                receiver: None,
                arguments: None,
                name: node.name.clone(),
                span: node.span,
            };

            return self.visit_function_ptr(&mut fptr_node);
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
    fn visit_var_init(&mut self, node: &mut VarInitNode) -> Result<()> {
        let symbol = self.context.lookup_var(&node.name);

        let sym = match symbol {
            Some(s) => s,
            None => {
                return Err(LpcError::new(format!(
                    "Missing symbol, that somehow passed semantic checks?: {}",
                    node.name
                ))
                .with_span(node.span))
            }
        };

        let global = sym.is_global();
        let upvalue = sym.upvalue;

        let current_register = if let Some(expression) = &mut node.value {
            expression.visit(self)?;

            // TODO: This whole thing sucks. We'd rather have the `expression.visit()` call
            //       above put the result into the correct location directly.
            if global {
                let next_register = self.global_counter.next().unwrap().as_global();

                trace!("Copying global to {:?}", next_register);
                push_instruction!(
                    self,
                    Instruction::RegCopy(self.current_result, next_register),
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
                    Instruction::RegCopy(self.current_result, next_register),
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
                    Instruction::RegCopy(self.current_result, next_register),
                    node.span()
                );
                next_register
            } else {
                trace!("Not copying the result");
                self.current_result
            }
        } else {
            trace!("No value, defaulting to NULL");
            // Default value to 0 when uninitialized.
            self.register_counter.next().unwrap().as_local()
        };

        self.current_result = current_register;

        self.context.lookup_var_mut(&node.name).map(|sym| {
            sym.location = Some(current_register);

            self.function_stack
                .last_mut()
                .map(|func| func.local_variables.push(sym.clone()));
        });

        Ok(())
    }

    #[instrument(skip_all)]
    fn visit_while(&mut self, node: &mut WhileNode) -> Result<()> {
        self.context.scopes.goto(node.scope_id);

        let start_label = self.new_label("while-start");
        let end_label = self.new_label("while-end");
        self.jump_targets
            .push(JumpTarget::new(end_label.clone(), start_label.clone()));
        let start_addr = self.current_address();
        self.insert_label(start_label.clone(), start_addr);

        node.condition.visit(self)?;

        let cond_result = self.current_result;

        let instruction = Instruction::Jz(cond_result, end_label.clone());
        push_instruction!(self, instruction, node.span);

        node.body.visit(self)?;

        // go back to the start of the loop
        let instruction = Instruction::Jmp(start_label);
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
        let mut global_counter = RegisterCounter::new();
        let mut upvalue_counter = RegisterCounter::new();
        let mut function_upvalue_counter = RegisterCounter::new();
        // do not skip r0 for these
        global_counter.start_at_zero(true);
        upvalue_counter.start_at_zero(true);
        function_upvalue_counter.start_at_zero(true);

        Self {
            function_stack: vec![],
            label_count: 0,
            functions: Default::default(),
            current_result: RegisterVariant::Local(Register(0)),
            register_counter: Default::default(),
            global_counter,
            upvalue_counter,
            function_upvalue_counter,
            global_init_registers: 0,
            context: Default::default(),
            jump_targets: vec![],
            case_addresses: vec![],
            visit_range_results: None,
            closure_scope_stack: vec![],
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::VecDeque;

    use claim::assert_some;
    use factori::create;
    use lpc_rs_asm::instruction::Instruction::*;
    use lpc_rs_core::{lpc_path::LpcPath, lpc_type::LpcType, LpcFloat};
    use lpc_rs_errors::{span::Span, LpcErrorSeverity, Result};
    use lpc_rs_utils::config::Config;

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
            Compiler,
        },
        interpreter::{process::Process, program::Program},
        lpc_parser,
        test_support::factories::*,
    };

    const LIB_DIR: &str = "./tests/fixtures/code";

    fn default_walker() -> CodegenWalker {
        let mut walker = CodegenWalker::default();
        walker.setup_init();

        let path = LpcPath::new_in_game("/secure/simul_efuns", "/", LIB_DIR);
        let mut prog = Program::new(path);
        prog.functions.insert(
            "simul_efun".into(),
            ProgramFunction::new(
                FunctionPrototype::new(
                    "simul_efun",
                    LpcType::Void,
                    Default::default(),
                    Default::default(),
                    None,
                    vec![],
                    vec![],
                ),
                0,
            )
            .into(),
        );
        let process = Process::new(prog);
        walker.context.simul_efuns = Some(process.into());

        walker
    }

    fn walk_prog(prog: &str) -> CodegenWalker {
        walk_code(prog).expect("failed to walk.")
    }

    fn walk_code(code: &str) -> Result<CodegenWalker> {
        let config = Config::default()
            .with_lib_dir(LIB_DIR)
            .with_simul_efun_file(Some("/secure/simul_efuns"));
        let compiler = Compiler::new(config.into());
        let (mut program, context) = compiler
            .parse_string(&LpcPath::new_in_game("/my_test.c", "/", LIB_DIR), code)
            .expect("failed to parse");

        let context = apply_walker!(InheritanceWalker, program, context, false);
        let context = apply_walker!(FunctionPrototypeWalker, program, context, false);
        let context = apply_walker!(ScopeWalker, program, context, false);
        let context = apply_walker!(DefaultParamsWalker, program, context, false);
        let context = apply_walker!(SemanticCheckWalker, program, context, false);

        let mut walker = CodegenWalker::new(context);
        let _ = program.visit(&mut walker);

        Ok(walker)
    }

    fn walker_function_instructions<T>(walker: &mut CodegenWalker, name: T) -> Vec<Instruction>
    where
        T: AsRef<str>,
    {
        let function = walker.functions.get_mut(name.as_ref()).unwrap();
        function.instructions.clone()
    }

    fn walker_init_instructions(walker: &mut CodegenWalker) -> Vec<Instruction> {
        walker.function_stack.last().unwrap().instructions.clone()
    }

    fn generate_init_instructions(prog: &str) -> Vec<Instruction> {
        // walker_init_instructions(&mut walk_prog(prog))
        walk_prog(prog)
            .functions
            .get_mut(INIT_PROGRAM)
            .unwrap()
            .instructions
            .clone()
    }

    #[test]
    fn test_visit_array_populates_the_instructions() {
        let mut walker = default_walker();

        let mut arr = ArrayNode::new(vec![
            ExpressionNode::from(123),
            ExpressionNode::from("foo"),
            ExpressionNode::from(vec![ExpressionNode::from(666)]),
        ]);

        let _ = walker.visit_array(&mut arr);

        let expected = vec![
            IConst(RegisterVariant::Local(Register(1)), 123),
            SConst(RegisterVariant::Local(Register(2)), String::from("foo")),
            IConst(RegisterVariant::Local(Register(3)), 666),
            AConst(
                RegisterVariant::Local(Register(4)),
                vec![RegisterVariant::Local(Register(3))],
            ),
            AConst(
                RegisterVariant::Local(Register(5)),
                vec![
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Local(Register(2)),
                    RegisterVariant::Local(Register(4)),
                ],
            ),
        ];

        assert_eq!(walker_init_instructions(&mut walker), expected);
    }

    mod test_visit_assignment {
        use super::*;

        #[test]
        fn test_populates_the_instructions_for_globals() {
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

            let _ = walker.visit_assignment(&mut node);
            assert_eq!(
                walker_init_instructions(&mut walker),
                [
                    IConst(RegisterVariant::Local(Register(1)), -12),
                    RegCopy(
                        RegisterVariant::Local(Register(1)),
                        RegisterVariant::Global(Register(666))
                    ),
                ]
            );
        }

        #[test]
        fn test_populates_the_instructions_for_locals() {
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

            let _ = walker.visit_assignment(&mut node);
            assert_eq!(
                walker_init_instructions(&mut walker),
                [
                    IConst(RegisterVariant::Local(Register(1)), -12),
                    RegCopy(
                        RegisterVariant::Local(Register(1)),
                        RegisterVariant::Local(Register(666))
                    )
                ]
            );
        }

        #[test]
        fn test_populates_the_instructions_for_array_items() {
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

            let _ = walker.visit_assignment(&mut node);
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

            let _ = walker.visit_binary_op(&mut node);

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

            let _ = walker.visit_binary_op(&mut node);

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

            let _ = walker.visit_binary_op(&mut node);

            let expected = vec![
                SConst(RegisterVariant::Local(Register(1)), String::from("foo")),
                SConst(RegisterVariant::Local(Register(2)), String::from("bar")),
                SConst(RegisterVariant::Local(Register(3)), String::from("baz")),
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
            let mut walker = default_walker();

            let mut node = BinaryOpNode {
                l: Box::new(ExpressionNode::from(vec![ExpressionNode::from(123)])),
                r: Box::new(ExpressionNode::from(vec![ExpressionNode::from(456)])),
                op: BinaryOperation::Add,
                span: None,
            };

            let _ = walker.visit_binary_op(&mut node);

            let expected = vec![
                IConst(RegisterVariant::Local(Register(1)), 123),
                AConst(
                    RegisterVariant::Local(Register(2)),
                    vec![RegisterVariant::Local(Register(1))],
                ),
                IConst(RegisterVariant::Local(Register(3)), 456),
                AConst(
                    RegisterVariant::Local(Register(4)),
                    vec![RegisterVariant::Local(Register(3))],
                ),
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
            let context = CompilationContext::default();
            let mut walker = CodegenWalker::new(context);

            let mut node = BinaryOpNode {
                l: Box::new(ExpressionNode::from(vec![ExpressionNode::from(123)])),
                r: Box::new(ExpressionNode::from(0)),
                op: BinaryOperation::Index,
                span: None,
            };

            let _ = walker.visit_binary_op(&mut node);

            let expected = vec![
                IConst(RegisterVariant::Local(Register(1)), 123),
                AConst(
                    RegisterVariant::Local(Register(2)),
                    vec![RegisterVariant::Local(Register(1))],
                ),
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

            let _ = walker.visit_binary_op(&mut node);

            let expected = vec![
                IConst(RegisterVariant::Local(Register(1)), 123),
                AConst(
                    RegisterVariant::Local(Register(2)),
                    vec![RegisterVariant::Local(Register(1))],
                ),
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
            let mut walker = default_walker();

            let mut node = BinaryOpNode {
                l: Box::new(ExpressionNode::from(123)),
                r: Box::new(ExpressionNode::from("marf!")),
                op: BinaryOperation::AndAnd,
                span: None,
            };

            let _ = walker.visit_binary_op(&mut node);

            let expected = vec![
                IConst(RegisterVariant::Local(Register(1)), 123),
                Jz(RegisterVariant::Local(Register(1)), "andand-end_0".into()),
                // and also
                SConst(RegisterVariant::Local(Register(2)), "marf!".into()),
                Jz(RegisterVariant::Local(Register(2)), "andand-end_0".into()),
                RegCopy(
                    RegisterVariant::Local(Register(2)),
                    RegisterVariant::Local(Register(3)),
                ),
                // end is here
            ];

            assert_eq!(walker_init_instructions(&mut walker), expected);
        }

        #[test]
        fn populates_the_instructions_for_oror_expressions() {
            let mut walker = default_walker();

            let mut node = BinaryOpNode {
                l: Box::new(ExpressionNode::from(123)),
                r: Box::new(ExpressionNode::from("sup?")),
                op: BinaryOperation::OrOr,
                span: None,
            };

            let _ = walker.visit_binary_op(&mut node);

            let expected = vec![
                IConst(RegisterVariant::Local(Register(1)), 123),
                RegCopy(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Local(Register(2)),
                ),
                Jnz(RegisterVariant::Local(Register(2)), "oror-end_0".into()),
                // else
                SConst(RegisterVariant::Local(Register(3)), "sup?".into()),
                RegCopy(
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

            let mut walker = walk_prog(code);
            let expected = vec![
                IConst(RegisterVariant::Local(Register(2)), 10),
                Lt(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Local(Register(2)),
                    RegisterVariant::Local(Register(3)),
                ),
                Jz(RegisterVariant::Local(Register(3)), "while-end_2".into()),
                Call {
                    name: "dump".into(),
                    namespace: CallNamespace::Local,
                    num_args: 1,
                    initial_arg: RegisterVariant::Local(Register(1)),
                },
                IConst(RegisterVariant::Local(Register(4)), 5),
                Gt(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Local(Register(4)),
                    RegisterVariant::Local(Register(5)),
                ),
                Jz(RegisterVariant::Local(Register(5)), "if-else_3".into()),
                SConst(RegisterVariant::Local(Register(6)), "breaking".into()),
                Call {
                    name: "dump".into(),
                    namespace: CallNamespace::Local,
                    num_args: 1,
                    initial_arg: RegisterVariant::Local(Register(6)),
                },
                Jmp("while-end_2".into()),
                IConst1(RegisterVariant::Local(Register(7))),
                IAdd(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Local(Register(7)),
                    RegisterVariant::Local(Register(8)),
                ),
                RegCopy(
                    RegisterVariant::Local(Register(8)),
                    RegisterVariant::Local(Register(1)),
                ),
                Jmp("while-start_1".into()),
                Ret,
            ];

            assert_eq!(
                walker_function_instructions(&mut walker, "create"),
                expected
            );
        }

        #[test]
        fn breaks_out_of_for_loops() {
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

            let mut walker = walk_prog(code);
            let expected = vec![
                IConst0(RegisterVariant::Local(Register(1))),
                IConst(RegisterVariant::Local(Register(2)), 10),
                Lt(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Local(Register(2)),
                    RegisterVariant::Local(Register(3)),
                ),
                Jz(RegisterVariant::Local(Register(3)), "for-end_2".into()),
                Call {
                    name: "dump".into(),
                    namespace: CallNamespace::Local,
                    num_args: 1,
                    initial_arg: RegisterVariant::Local(Register(1)),
                },
                IConst(RegisterVariant::Local(Register(4)), 5),
                Gt(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Local(Register(4)),
                    RegisterVariant::Local(Register(5)),
                ),
                Jz(RegisterVariant::Local(Register(5)), "if-else_4".into()),
                SConst(RegisterVariant::Local(Register(6)), "breaking".into()),
                Call {
                    name: "dump".into(),
                    namespace: CallNamespace::Local,
                    num_args: 1,
                    initial_arg: RegisterVariant::Local(Register(6)),
                },
                Jmp("for-end_2".into()),
                IConst1(RegisterVariant::Local(Register(7))),
                IAdd(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Local(Register(7)),
                    RegisterVariant::Local(Register(8)),
                ),
                RegCopy(
                    RegisterVariant::Local(Register(8)),
                    RegisterVariant::Local(Register(1)),
                ),
                IConst1(RegisterVariant::Local(Register(9))),
                IAdd(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Local(Register(9)),
                    RegisterVariant::Local(Register(10)),
                ),
                RegCopy(
                    RegisterVariant::Local(Register(10)),
                    RegisterVariant::Local(Register(1)),
                ),
                Jmp("for-start_1".into()),
                Ret,
            ];

            assert_eq!(
                walker_function_instructions(&mut walker, "create"),
                expected
            );
        }

        #[test]
        fn breaks_out_of_do_while_loops() {
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

            let mut walker = walk_prog(code);
            let expected = vec![
                Call {
                    name: "dump".into(),
                    namespace: CallNamespace::Local,
                    num_args: 1,
                    initial_arg: RegisterVariant::Local(Register(1)),
                },
                IConst(RegisterVariant::Local(Register(2)), 5),
                Gt(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Local(Register(2)),
                    RegisterVariant::Local(Register(3)),
                ),
                Jz(RegisterVariant::Local(Register(3)), "if-else_4".into()),
                SConst(RegisterVariant::Local(Register(4)), "breaking".into()),
                Call {
                    name: "dump".into(),
                    namespace: CallNamespace::Local,
                    num_args: 1,
                    initial_arg: RegisterVariant::Local(Register(4)),
                },
                Jmp("do-while-end_2".into()),
                IConst1(RegisterVariant::Local(Register(5))),
                IAdd(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Local(Register(5)),
                    RegisterVariant::Local(Register(6)),
                ),
                RegCopy(
                    RegisterVariant::Local(Register(6)),
                    RegisterVariant::Local(Register(1)),
                ),
                IConst(RegisterVariant::Local(Register(7)), 10),
                Lt(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Local(Register(7)),
                    RegisterVariant::Local(Register(8)),
                ),
                Jnz(
                    RegisterVariant::Local(Register(8)),
                    "do-while-start_1".into(),
                ),
                Ret,
            ];

            assert_eq!(
                walker_function_instructions(&mut walker, "create"),
                expected
            );
        }

        #[test]
        fn breaks_out_of_switch_statements() {
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

            let mut walker = walk_prog(code);
            let expected = vec![
                IConst(RegisterVariant::Local(Register(1)), 666),
                Jmp("switch-test_1".into()),
                SConst(RegisterVariant::Local(Register(2)), "YEAH BABY".into()),
                Call {
                    name: "dump".into(),
                    namespace: CallNamespace::Local,
                    num_args: 1,
                    initial_arg: RegisterVariant::Local(Register(2)),
                },
                Jmp("switch-end_2".into()),
                SConst(RegisterVariant::Local(Register(3)), "very".into()),
                Call {
                    name: "dump".into(),
                    namespace: CallNamespace::Local,
                    num_args: 1,
                    initial_arg: RegisterVariant::Local(Register(3)),
                },
                SConst(RegisterVariant::Local(Register(4)), "weak".into()),
                Call {
                    name: "dump".into(),
                    namespace: CallNamespace::Local,
                    num_args: 1,
                    initial_arg: RegisterVariant::Local(Register(4)),
                },
                Jmp("switch-end_2".into()),
                IConst(RegisterVariant::Local(Register(5)), 666),
                EqEq(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Local(Register(5)),
                    RegisterVariant::Local(Register(6)),
                ),
                Jnz(RegisterVariant::Local(Register(6)), "switch-case_3".into()),
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
                Jnz(RegisterVariant::Local(Register(9)), "switch-case_4".into()),
                Jmp("switch-default_5".into()),
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
        use lpc_rs_function_support::function_prototype::FunctionPrototype;

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
            let mut walker = default_walker();
            let call = "mixed m = dump(4 - 5);";
            let mut node = get_call_node(call, &mut walker.context);

            let _ = walker.visit_call(&mut node);

            let expected = vec![
                IConst(RegisterVariant::Local(Register(1)), -1),
                Call {
                    name: String::from("dump"),
                    namespace: CallNamespace::Local,
                    num_args: 1,
                    initial_arg: RegisterVariant::Local(Register(1)),
                },
            ];

            assert_eq!(walker_init_instructions(&mut walker), expected);
        }

        #[test]
        fn populates_the_instructions_for_call_other() {
            let check = |code: &str, expected: &[Instruction]| {
                let mut walker = default_walker();
                let mut tree = lpc_parser::ExpressionParser::new()
                    .parse(&mut CompilationContext::default(), LexWrapper::new(code))
                    .unwrap();

                let _ = tree.visit(&mut walker);

                assert_eq!(walker_init_instructions(&mut walker), expected);
            };

            let expected = vec![
                IConst(RegisterVariant::Local(Register(1)), -1),
                SConst(RegisterVariant::Local(Register(2)), String::from("foo")),
                SConst(RegisterVariant::Local(Register(3)), String::from("print")),
                CallOther {
                    receiver: RegisterVariant::Local(Register(2)),
                    name: RegisterVariant::Local(Register(3)),
                    num_args: 1,
                    initial_arg: RegisterVariant::Local(Register(1)),
                },
                RegCopy(
                    RegisterVariant::Local(Register(0)),
                    RegisterVariant::Local(Register(4)),
                ),
            ];
            check(r#""foo"->print(4 - 5)"#, &expected);

            let expected = vec![
                SConst(RegisterVariant::Local(Register(1)), String::from("foo")),
                SConst(RegisterVariant::Local(Register(2)), String::from("print")),
                IConst(RegisterVariant::Local(Register(3)), -1),
                RegCopy(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Local(Register(4)),
                ),
                RegCopy(
                    RegisterVariant::Local(Register(2)),
                    RegisterVariant::Local(Register(5)),
                ),
                RegCopy(
                    RegisterVariant::Local(Register(3)),
                    RegisterVariant::Local(Register(6)),
                ),
                CallOther {
                    receiver: RegisterVariant::Local(Register(1)),
                    name: RegisterVariant::Local(Register(2)),
                    num_args: 1,
                    initial_arg: RegisterVariant::Local(Register(3)),
                },
                RegCopy(
                    RegisterVariant::Local(Register(0)),
                    RegisterVariant::Local(Register(7)),
                ),
            ];
            check(r#"call_other("foo", "print", 4 - 5)"#, &expected);
        }

        #[test]
        fn populates_the_instructions_for_sizeof() {
            let check = |code: &str, expected: &[Instruction]| {
                let mut walker = default_walker();
                let mut tree = lpc_parser::ExpressionParser::new()
                    .parse(&mut CompilationContext::default(), LexWrapper::new(code))
                    .unwrap();

                let _ = tree.visit(&mut walker);

                assert_eq!(walker_init_instructions(&mut walker), expected);
            };

            let expected = vec![
                IConst1(RegisterVariant::Local(Register(1))),
                IConst(RegisterVariant::Local(Register(2)), 2),
                SConst(RegisterVariant::Local(Register(3)), String::from("c")),
                AConst(
                    RegisterVariant::Local(Register(4)),
                    vec![
                        RegisterVariant::Local(Register(1)),
                        RegisterVariant::Local(Register(2)),
                        RegisterVariant::Local(Register(3)),
                    ],
                ),
                Sizeof(
                    RegisterVariant::Local(Register(4)),
                    RegisterVariant::Local(Register(5)),
                ),
            ];
            check(r#"sizeof(({ 1, 2, "c" }))"#, &expected);
        }

        #[test]
        fn populates_the_instructions_for_catch() {
            let mut walker = default_walker();
            let call = "catch(12 / 0)";
            let mut tree = lpc_parser::ExpressionParser::new()
                .parse(&mut CompilationContext::default(), LexWrapper::new(call))
                .unwrap();

            let _ = tree.visit(&mut walker);

            let expected = vec![
                CatchStart(RegisterVariant::Local(Register(1)), "catch_end_0".into()),
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
            let mut context = CompilationContext::default();
            let prototype = FunctionPrototype {
                name: "marfin".into(),
                return_type: LpcType::Int(false),
                arity: FunctionArity::new(1),
                arg_types: vec![],
                span: None,
                arg_spans: vec![],
                flags: FunctionFlags::default(),
            };

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
            let _ = tree.visit(&mut walker);

            let expected = vec![
                IConst(RegisterVariant::Local(Register(1)), 666),
                CallFp {
                    location: RegisterVariant::Local(Register(1)),
                    num_args: 1,
                    initial_arg: RegisterVariant::Local(Register(1)),
                },
                RegCopy(
                    RegisterVariant::Local(Register(0)),
                    RegisterVariant::Local(Register(2)),
                ),
            ];

            assert_eq!(walker_init_instructions(&mut walker), expected);
        }

        #[test]
        fn populates_the_instructions_for_global_function_pointers() {
            let mut context = CompilationContext::default();
            let prototype = FunctionPrototype {
                name: "marfin".into(),
                return_type: LpcType::Int(false),
                arity: FunctionArity::new(1),
                arg_types: vec![],
                span: None,
                arg_spans: vec![],
                flags: FunctionFlags::default(),
            };

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
            let _ = tree.visit(&mut walker);

            let expected = vec![
                IConst(RegisterVariant::Local(Register(1)), 666),
                CallFp {
                    location: RegisterVariant::Global(Register(0)),
                    num_args: 1,
                    initial_arg: RegisterVariant::Local(Register(1)),
                },
                RegCopy(
                    RegisterVariant::Local(Register(0)),
                    RegisterVariant::Local(Register(2)),
                ),
            ];

            assert_eq!(walker_init_instructions(&mut walker), expected);
        }

        #[test]
        fn copies_non_void_call_results() {
            let mut context = CompilationContext::default();
            let prototype = FunctionPrototype {
                name: "marfin".into(),
                return_type: LpcType::Int(false),
                arity: FunctionArity::new(1),
                arg_types: vec![],
                span: None,
                arg_spans: vec![],
                flags: FunctionFlags::default(),
            };

            context
                .function_prototypes
                .insert("marfin".into(), prototype);
            let mut walker = CodegenWalker::new(context);
            let call = "mixed m = marfin(666);";
            let mut node = get_call_node(call, &mut walker.context);

            let _ = walker.visit_call(&mut node);

            let expected = vec![
                IConst(RegisterVariant::Local(Register(1)), 666),
                Call {
                    name: String::from("marfin"),
                    namespace: CallNamespace::Local,
                    num_args: 1,
                    initial_arg: RegisterVariant::Local(Register(1)),
                },
                RegCopy(
                    RegisterVariant::Local(Register(0)),
                    RegisterVariant::Local(Register(2)),
                ),
            ];

            assert_eq!(walker_init_instructions(&mut walker), expected);
        }

        #[test]
        fn does_not_copy_void_call_results() {
            let mut context = CompilationContext::default();
            let prototype = FunctionPrototype {
                name: "void_thing".into(),
                return_type: LpcType::Void,
                arity: FunctionArity::new(1),
                arg_types: vec![],
                span: None,
                arg_spans: vec![],
                flags: FunctionFlags::default(),
            };

            context
                .function_prototypes
                .insert("void_thing".into(), prototype);
            let mut walker = CodegenWalker::new(context);
            let call = "mixed m = void_thing(666);";
            let mut node = get_call_node(call, &mut walker.context);

            let _ = walker.visit_call(&mut node);

            let expected = vec![
                IConst(RegisterVariant::Local(Register(1)), 666),
                Call {
                    name: String::from("void_thing"),
                    namespace: CallNamespace::Local,
                    num_args: 1,
                    initial_arg: RegisterVariant::Local(Register(1)),
                },
            ];

            assert_eq!(walker_init_instructions(&mut walker), expected);
        }

        #[test]
        fn copies_non_void_efun_results() {
            let mut walker = default_walker();
            let call = r#"mixed m = clone_object("/foo.c");"#;
            let mut node = get_call_node(call, &mut walker.context);

            let _ = walker.visit_call(&mut node);

            let expected = vec![
                SConst(RegisterVariant::Local(Register(1)), String::from("/foo.c")),
                Call {
                    name: String::from("clone_object"),
                    namespace: CallNamespace::Local,
                    num_args: 1,
                    initial_arg: RegisterVariant::Local(Register(1)),
                },
                RegCopy(
                    RegisterVariant::Local(Register(0)),
                    RegisterVariant::Local(Register(2)),
                ),
            ];

            assert_eq!(walker_init_instructions(&mut walker), expected);
        }

        #[test]
        fn does_not_copy_void_efun_results() {
            let mut walker = default_walker();
            let call = r#"mixed m = dump("lkajsdflkajsdf");"#;
            let mut node = get_call_node(call, &mut walker.context);

            let _ = walker.visit_call(&mut node);

            let expected = vec![
                SConst(
                    RegisterVariant::Local(Register(1)),
                    String::from("lkajsdflkajsdf"),
                ),
                Call {
                    name: String::from("dump"),
                    namespace: CallNamespace::Local,
                    num_args: 1,
                    initial_arg: RegisterVariant::Local(Register(1)),
                },
            ];

            assert_eq!(walker_init_instructions(&mut walker), expected);
        }

        #[test]
        fn handles_ellipsis_functions() {
            let mut context = CompilationContext::default();
            let prototype = FunctionPrototype {
                name: "my_func".into(),
                return_type: LpcType::Void,
                arity: FunctionArity::new(1),
                arg_types: vec![LpcType::String(false)],
                span: None,
                arg_spans: vec![],
                flags: FunctionFlags::default().with_ellipsis(true),
            };

            context
                .function_prototypes
                .insert("my_func".into(), prototype);
            let mut walker = CodegenWalker::new(context);
            let call = "mixed m = my_func(\"hello!\", 42, \"cool beans\");";
            let mut node = get_call_node(call, &mut walker.context);

            let _ = walker.visit_call(&mut node);

            let expected = vec![
                SConst(RegisterVariant::Local(Register(1)), "hello!".into()),
                IConst(RegisterVariant::Local(Register(2)), 42),
                SConst(RegisterVariant::Local(Register(3)), "cool beans".into()),
                RegCopy(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Local(Register(4)),
                ),
                RegCopy(
                    RegisterVariant::Local(Register(2)),
                    RegisterVariant::Local(Register(5)),
                ),
                RegCopy(
                    RegisterVariant::Local(Register(3)),
                    RegisterVariant::Local(Register(6)),
                ),
                Call {
                    name: "my_func".into(),
                    namespace: CallNamespace::Local,
                    num_args: 3,
                    initial_arg: RegisterVariant::Local(Register(4)),
                },
            ];

            assert_eq!(walker_init_instructions(&mut walker), expected);
        }
    }

    #[test]
    fn test_visit_block_populates_instructions() {
        let block = "void marf() { { int a = '🏯'; dump(a); } }";
        let mut prog_node = lpc_parser::ProgramParser::new()
            .parse(&mut CompilationContext::default(), LexWrapper::new(block))
            .unwrap();
        let mut node = if let AstNode::FunctionDef(ref mut n) = prog_node.body.first_mut().unwrap()
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
        let _ = scope_walker.visit_block(&mut node);

        let context = scope_walker.into_context();
        let mut walker = CodegenWalker::new(context);
        let _ = walker.visit_block(&mut node);

        let expected = vec![
            IConst(RegisterVariant::Local(Register(1)), 127983),
            Call {
                name: String::from("dump"),
                namespace: CallNamespace::Local,
                num_args: 1,
                initial_arg: RegisterVariant::Local(Register(1)),
            },
        ];

        assert_eq!(walker_init_instructions(&mut walker), expected);
    }

    #[test]
    fn test_visit_comma_expression_populates_the_instructions() {
        let mut walker = default_walker();

        let mut expr = CommaExpressionNode::new(vec![
            ExpressionNode::from(123),
            ExpressionNode::from("foo"),
            ExpressionNode::from(vec![ExpressionNode::from(666)]),
        ]);

        let _ = walker.visit_comma_expression(&mut expr);

        let expected = vec![
            IConst(RegisterVariant::Local(Register(1)), 123),
            SConst(RegisterVariant::Local(Register(2)), String::from("foo")),
            IConst(RegisterVariant::Local(Register(3)), 666),
            AConst(
                RegisterVariant::Local(Register(4)),
                vec![RegisterVariant::Local(Register(3))],
            ),
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

        fn compile(code: &str) -> CodegenWalker {
            let mut context = CompilationContext::default();

            let mut node = get_closure_node(code, &mut context);

            let mut prototype_walker = FunctionPrototypeWalker::new(context);
            let _ = prototype_walker.visit_closure(&mut node);
            let mut context = prototype_walker.into_context();

            context.scopes.push_new(); // global scope

            let mut scope_walker = ScopeWalker::new(context);
            let _ = scope_walker.visit_closure(&mut node);

            let mut context = scope_walker.into_context();
            context.scopes.goto_root();

            let mut walker = CodegenWalker::new(context);
            let _ = walker.visit_closure(&mut node);

            walker
        }

        #[test]
        fn populates_the_instructions() {
            let mut walker = compile("function f = (: dump(4 + 5 + $1) :);");

            assert_eq!(
                walker_function_instructions(&mut walker, "closure-0"),
                vec![
                    IConst(RegisterVariant::Local(Register(2)), 9),
                    MAdd(
                        RegisterVariant::Local(Register(2)),
                        RegisterVariant::Local(Register(1)),
                        RegisterVariant::Local(Register(3))
                    ),
                    Call {
                        name: "dump".into(),
                        namespace: CallNamespace::Local,
                        num_args: 1,
                        initial_arg: RegisterVariant::Local(Register(3))
                    },
                    Ret
                ]
            );
        }

        #[test]
        fn handles_ellipses() {
            let mut walker = compile("function f = (: [int i, ...] argv :);");

            assert_eq!(
                walker_function_instructions(&mut walker, "closure-0"),
                vec![
                    PopulateArgv(RegisterVariant::Local(Register(2)), 1, 1),
                    RegCopy(
                        RegisterVariant::Local(Register(2)),
                        RegisterVariant::Local(Register(0)),
                    ),
                    Ret,
                ]
            );
        }

        #[test]
        fn populates_the_default_arguments() {
            let mut walker =
                compile("function f = (: [int i, int j = 666, float d = 3.14] i * j :);");

            assert_eq!(
                walker_function_instructions(&mut walker, "closure-0"),
                vec![
                    PopulateDefaults(vec![4, 6]),
                    IMul(
                        RegisterVariant::Local(Register(1)),
                        RegisterVariant::Local(Register(2)),
                        RegisterVariant::Local(Register(4)),
                    ),
                    RegCopy(
                        RegisterVariant::Local(Register(4)),
                        RegisterVariant::Local(Register(0)),
                    ),
                    Ret,
                    IConst(RegisterVariant::Local(Register(5)), 666),
                    RegCopy(
                        RegisterVariant::Local(Register(5)),
                        RegisterVariant::Local(Register(2)),
                    ),
                    FConst(RegisterVariant::Local(Register(6)), 3.14.into()),
                    RegCopy(
                        RegisterVariant::Local(Register(6)),
                        RegisterVariant::Local(Register(3)),
                    ),
                    Jmp("closure-body-start_0".into()),
                ],
            );
        }

        #[test]
        fn sets_the_correct_upvalue_information() {
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
            let walker = walk_prog(code);

            let closure = walker
                .functions
                .get("closure-0")
                .expect("where's the closure?");
            assert_eq!(closure.num_upvalues, 0);
            assert_eq!(closure.local_variables.len(), 1);
            assert_eq!(&closure.local_variables.first().unwrap().name, "s");
            assert_eq!(
                &closure.local_variables.first().unwrap().location.unwrap(),
                &RegisterVariant::Local(Register(1))
            );

            let func = walker.functions.get("create").expect("where's create()?");
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

            let mut walker = walk_prog(code);
            let expected = vec![
                IConst(RegisterVariant::Local(Register(2)), 10),
                Lt(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Local(Register(2)),
                    RegisterVariant::Local(Register(3)),
                ),
                Jz(RegisterVariant::Local(Register(3)), "while-end_2".into()),
                Call {
                    name: "dump".into(),
                    namespace: CallNamespace::Local,
                    num_args: 1,
                    initial_arg: RegisterVariant::Local(Register(1)),
                },
                IConst(RegisterVariant::Local(Register(4)), 5),
                Gt(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Local(Register(4)),
                    RegisterVariant::Local(Register(5)),
                ),
                Jz(RegisterVariant::Local(Register(5)), "if-else_3".into()),
                SConst(
                    RegisterVariant::Local(Register(6)),
                    "goin' infinite!".into(),
                ),
                Call {
                    name: "dump".into(),
                    namespace: CallNamespace::Local,
                    num_args: 1,
                    initial_arg: RegisterVariant::Local(Register(6)),
                },
                Jmp("while-start_1".into()),
                IConst1(RegisterVariant::Local(Register(7))),
                IAdd(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Local(Register(7)),
                    RegisterVariant::Local(Register(8)),
                ),
                RegCopy(
                    RegisterVariant::Local(Register(8)),
                    RegisterVariant::Local(Register(1)),
                ),
                Jmp("while-start_1".into()),
                Ret,
            ];

            assert_eq!(
                walker_function_instructions(&mut walker, "create"),
                expected
            );
        }

        #[test]
        fn continues_for_loops() {
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

            let mut walker = walk_prog(code);
            let expected = vec![
                IConst0(RegisterVariant::Local(Register(1))),
                IConst(RegisterVariant::Local(Register(2)), 10),
                Lt(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Local(Register(2)),
                    RegisterVariant::Local(Register(3)),
                ),
                Jz(RegisterVariant::Local(Register(3)), "for-end_2".into()),
                Call {
                    name: "dump".into(),
                    namespace: CallNamespace::Local,
                    num_args: 1,
                    initial_arg: RegisterVariant::Local(Register(1)),
                },
                IConst(RegisterVariant::Local(Register(4)), 5),
                Gt(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Local(Register(4)),
                    RegisterVariant::Local(Register(5)),
                ),
                Jz(RegisterVariant::Local(Register(5)), "if-else_4".into()),
                SConst(
                    RegisterVariant::Local(Register(6)),
                    "goin' infinite!".into(),
                ),
                Call {
                    name: "dump".into(),
                    namespace: CallNamespace::Local,
                    num_args: 1,
                    initial_arg: RegisterVariant::Local(Register(6)),
                },
                Jmp("for-continue_3".into()),
                IConst1(RegisterVariant::Local(Register(7))),
                IAdd(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Local(Register(7)),
                    RegisterVariant::Local(Register(8)),
                ),
                RegCopy(
                    RegisterVariant::Local(Register(8)),
                    RegisterVariant::Local(Register(1)),
                ),
                IConst1(RegisterVariant::Local(Register(9))),
                IAdd(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Local(Register(9)),
                    RegisterVariant::Local(Register(10)),
                ),
                RegCopy(
                    RegisterVariant::Local(Register(10)),
                    RegisterVariant::Local(Register(1)),
                ),
                Jmp("for-start_1".into()),
                Ret,
            ];

            assert_eq!(
                walker_function_instructions(&mut walker, CREATE_FUNCTION),
                expected
            );
        }

        #[test]
        fn continues_do_while_loops() {
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

            let mut walker = walk_prog(code);
            let expected = vec![
                Call {
                    name: "dump".into(),
                    namespace: CallNamespace::Local,
                    num_args: 1,
                    initial_arg: RegisterVariant::Local(Register(1)),
                },
                IConst(RegisterVariant::Local(Register(2)), 5),
                Gt(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Local(Register(2)),
                    RegisterVariant::Local(Register(3)),
                ),
                Jz(RegisterVariant::Local(Register(3)), "if-else_4".into()),
                SConst(
                    RegisterVariant::Local(Register(4)),
                    "goin' infinite!".into(),
                ),
                Call {
                    name: "dump".into(),
                    namespace: CallNamespace::Local,
                    num_args: 1,
                    initial_arg: RegisterVariant::Local(Register(4)),
                },
                Jmp("do-while-continue_3".into()),
                IConst1(RegisterVariant::Local(Register(5))),
                IAdd(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Local(Register(5)),
                    RegisterVariant::Local(Register(6)),
                ),
                RegCopy(
                    RegisterVariant::Local(Register(6)),
                    RegisterVariant::Local(Register(1)),
                ),
                IConst(RegisterVariant::Local(Register(7)), 10),
                Lt(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Local(Register(7)),
                    RegisterVariant::Local(Register(8)),
                ),
                Jnz(
                    RegisterVariant::Local(Register(8)),
                    "do-while-start_1".into(),
                ),
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
        let call = "int foo = 1, *bar = ({ 56 });";
        let mut prog_node = lpc_parser::ProgramParser::new()
            .parse(&mut CompilationContext::default(), LexWrapper::new(call))
            .unwrap();
        let mut node = if let AstNode::Decl(node) = prog_node.body.first_mut().unwrap() {
            node
        } else {
            panic!("Expected decl node");
        };

        let mut scope_walker = ScopeWalker::default();
        let _ = scope_walker.visit_decl(&mut node);

        let context = scope_walker.into_context();
        let mut walker = CodegenWalker::new(context);
        let _ = walker.visit_decl(&mut node);

        let expected = vec![
            IConst1(RegisterVariant::Local(Register(1))),
            RegCopy(
                RegisterVariant::Local(Register(1)),
                RegisterVariant::Global(Register(0)),
            ),
            IConst(RegisterVariant::Local(Register(2)), 56),
            AConst(
                RegisterVariant::Local(Register(3)),
                vec![RegisterVariant::Local(Register(2))],
            ),
            RegCopy(
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
            let mut walker = default_walker();

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

            let _ = walker.visit_do_while(&mut node);

            let expected = vec![
                SConst(RegisterVariant::Local(Register(1)), String::from("body")),
                Call {
                    name: String::from("dump"),
                    namespace: CallNamespace::Local,
                    num_args: 1,
                    initial_arg: RegisterVariant::Local(Register(1)),
                },
                IConst(RegisterVariant::Local(Register(2)), 666),
                IConst(RegisterVariant::Local(Register(3)), 777),
                EqEq(
                    RegisterVariant::Local(Register(2)),
                    RegisterVariant::Local(Register(3)),
                    RegisterVariant::Local(Register(4)),
                ),
                Jnz(
                    RegisterVariant::Local(Register(4)),
                    "do-while-start_0".into(),
                ),
            ];

            assert_eq!(walker_init_instructions(&mut walker), expected);
        }
    }

    mod test_visit_for {
        use lpc_rs_asm::instruction::Instruction::{ISub, Jmp, Jz};

        use super::*;
        use crate::compiler::ast::for_node::ForNode;

        #[test]
        fn populates_the_instructions() {
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
            let _ = scope_walker.visit_for(&mut node);

            let context = scope_walker.into_context();
            let mut walker = CodegenWalker::new(context);

            let _ = walker.visit_for(&mut node).unwrap();

            let expected = vec![
                IConst(RegisterVariant::Local(Register(1)), 10),
                Jz(RegisterVariant::Local(Register(1)), "for-end_1".into()),
                Call {
                    name: String::from("dump"),
                    namespace: CallNamespace::Local,
                    num_args: 1,
                    initial_arg: RegisterVariant::Local(Register(1)),
                },
                IConst1(RegisterVariant::Local(Register(2))),
                ISub(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Local(Register(2)),
                    RegisterVariant::Local(Register(3)),
                ),
                RegCopy(
                    RegisterVariant::Local(Register(3)),
                    RegisterVariant::Local(Register(1)),
                ),
                Jmp("for-start_0".into()),
            ];

            assert_eq!(walker_init_instructions(&mut walker), expected);
        }
    }

    mod test_visit_function_def {
        use super::*;

        fn assert_compiles_to(code: &str, expected: Vec<Instruction>) {
            let mut prototype_walker = FunctionPrototypeWalker::default();

            let mut prog_node = lpc_parser::ProgramParser::new()
                .parse(&mut CompilationContext::default(), LexWrapper::new(code))
                .unwrap();
            let ast_node = prog_node.body.first_mut().unwrap();
            let mut node = if let AstNode::FunctionDef(node) = ast_node {
                node
            } else {
                panic!("Didn't receive a function def?");
            };

            let _ = prototype_walker.visit_function_def(&mut node);
            let mut context = prototype_walker.into_context();

            context.scopes.push_new(); // global scope

            let mut scope_walker = ScopeWalker::new(context);
            let _ = scope_walker.visit_function_def(&mut node);

            let mut context = scope_walker.into_context();
            context.scopes.goto_root();

            let mut walker = CodegenWalker::new(context);
            let _ = walker.visit_function_def(&mut node);

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
                    RegCopy(
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
                    RegCopy(
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
                "int main(int i, int j = 666, float d = 3.14) { return i * j; }",
                vec![
                    PopulateDefaults(vec![4, 6]),
                    IMul(
                        RegisterVariant::Local(Register(1)),
                        RegisterVariant::Local(Register(2)),
                        RegisterVariant::Local(Register(4)),
                    ),
                    RegCopy(
                        RegisterVariant::Local(Register(4)),
                        RegisterVariant::Local(Register(0)),
                    ),
                    Ret,
                    IConst(RegisterVariant::Local(Register(5)), 666),
                    RegCopy(
                        RegisterVariant::Local(Register(5)),
                        RegisterVariant::Local(Register(2)),
                    ),
                    FConst(RegisterVariant::Local(Register(6)), 3.14.into()),
                    RegCopy(
                        RegisterVariant::Local(Register(6)),
                        RegisterVariant::Local(Register(3)),
                    ),
                    Jmp("function-body-start_0".into()),
                ],
            );
        }
    }

    mod test_visit_function_ptr {
        use super::*;
        use crate::compiler::ast::function_ptr_node::FunctionPtrNode;

        #[test]
        fn populates_the_instructions_for_efuns() {
            let mut node = FunctionPtrNode {
                receiver: None,
                name: "dump".to_string(),
                arguments: None,
                span: None,
            };

            let mut walker = default_walker();
            walker.visit_function_ptr(&mut node).unwrap();

            let expected = vec![FunctionPtrConst {
                location: RegisterVariant::Local(Register(1)),
                target: FunctionTarget::Efun(String::from("dump")),
                arity: FunctionArity {
                    num_args: 1,
                    num_default_args: 0,
                    ellipsis: true,
                    varargs: false,
                },
                applied_arguments: vec![],
            }];

            assert_eq!(walker_init_instructions(&mut walker), expected);
        }

        #[test]
        fn populates_the_instructions_for_simul_efuns() {
            let mut node = FunctionPtrNode {
                receiver: None,
                name: "simul_efun".to_string(),
                arguments: None,
                span: None,
            };

            let mut walker = default_walker();
            walker.visit_function_ptr(&mut node).unwrap();

            let expected = vec![FunctionPtrConst {
                location: RegisterVariant::Local(Register(1)),
                target: FunctionTarget::Local(
                    FunctionName::Literal("simul_efun".into()),
                    FunctionReceiver::Local,
                ),
                arity: FunctionArity {
                    num_args: 0,
                    num_default_args: 0,
                    ellipsis: false,
                    varargs: false,
                },
                applied_arguments: vec![],
            }];

            assert_eq!(walker_init_instructions(&mut walker), expected);
        }
    }

    mod test_visit_if {
        use lpc_rs_asm::instruction::Instruction::{EqEq, Jmp, Jz};

        use super::*;

        #[test]
        fn test_populates_the_instructions() {
            let mut walker = default_walker();

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

            let _ = walker.visit_if(&mut node);

            let expected = vec![
                IConst(RegisterVariant::Local(Register(1)), 666),
                IConst(RegisterVariant::Local(Register(2)), 777),
                EqEq(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Local(Register(2)),
                    RegisterVariant::Local(Register(3)),
                ),
                Jz(RegisterVariant::Local(Register(3)), "if-else_0".into()),
                SConst(RegisterVariant::Local(Register(4)), String::from("true")),
                Call {
                    name: String::from("dump"),
                    namespace: CallNamespace::Local,
                    num_args: 1,
                    initial_arg: RegisterVariant::Local(Register(4)),
                },
                Jmp("if-end_1".into()),
                SConst(RegisterVariant::Local(Register(5)), String::from("false")),
                Call {
                    name: String::from("dump"),
                    namespace: CallNamespace::Local,
                    num_args: 1,
                    initial_arg: RegisterVariant::Local(Register(5)),
                },
            ];

            assert_eq!(walker_init_instructions(&mut walker), expected);
        }
    }

    #[test]
    fn test_visit_int_populates_the_instructions() {
        let mut walker = default_walker();

        let mut tree = IntNode::new(666);
        let mut tree0 = IntNode::new(0);
        let mut tree1 = IntNode::new(1);

        let _ = walker.visit_int(&mut tree);
        let _ = walker.visit_int(&mut tree0);
        let _ = walker.visit_int(&mut tree1);

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
            let prog = "
                void create() {
                    1 + 3 - 5;
                    dump(4 + 5);
                }
            ";

            let walker = walk_prog(prog);

            let expected = vec![
                Call {
                    name: String::from("create"),
                    namespace: CallNamespace::Local,
                    num_args: 0,
                    initial_arg: RegisterVariant::Local(Register(1)),
                },
                Ret,
            ];

            assert_eq!(
                walker.functions.get(INIT_PROGRAM).unwrap().instructions,
                expected
            );

            let expected = vec![
                IConst(RegisterVariant::Local(Register(1)), -1),
                IConst(RegisterVariant::Local(Register(2)), 9),
                Call {
                    name: String::from("dump"),
                    namespace: CallNamespace::Local,
                    num_args: 1,
                    initial_arg: RegisterVariant::Local(Register(2)),
                },
                Ret, // Automatically added due to no explicit return
            ];

            assert_eq!(
                walker.functions.get(CREATE_FUNCTION).unwrap().instructions,
                expected
            );
        }

        #[test]
        fn initializes_the_globals() {
            let prog = r#"
                int j = 123;
                string q = "cool";
                void marf() {
                    dump(q + j);
                }
            "#;

            let instructions = generate_init_instructions(prog);

            let expected = [
                IConst(RegisterVariant::Local(Register(1)), 123),
                RegCopy(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Global(Register(0)),
                ),
                SConst(RegisterVariant::Local(Register(2)), String::from("cool")),
                RegCopy(
                    RegisterVariant::Local(Register(2)),
                    RegisterVariant::Global(Register(1)),
                ),
                Ret,
            ];

            assert_eq!(instructions, expected);
        }

        #[test]
        fn calls_create_if_create_is_defined() {
            let prog = r#"
                int q = 666;
                int marf() {
                    return 3;
                }
                void create() {
                    dump(marf() + " times a winner!");
                }
            "#;

            let instructions = generate_init_instructions(prog);

            let expected = [
                IConst(RegisterVariant::Local(Register(1)), 666),
                RegCopy(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Global(Register(0)),
                ),
                Call {
                    name: String::from("create"),
                    namespace: CallNamespace::Local,
                    num_args: 0,
                    initial_arg: RegisterVariant::Local(Register(2)),
                },
                Ret, // end of initialization
            ];

            assert_eq!(instructions, expected);
        }

        #[test]
        fn tracks_global_registers_over_multiple_sections() {
            let prog = r#"
                int q = 666;
                int marf() {
                    return 3;
                }
                int r = 777;
            "#;

            let instructions = generate_init_instructions(prog);

            let expected = [
                IConst(RegisterVariant::Local(Register(1)), 666),
                RegCopy(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Global(Register(0)),
                ),
                IConst(RegisterVariant::Local(Register(2)), 777),
                RegCopy(
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
        let mut walker = default_walker();

        let mut node = ReturnNode::new(Some(ExpressionNode::from(IntNode::new(666))));
        let _ = walker.visit_return(&mut node);

        let expected = vec![
            IConst(RegisterVariant::Local(Register(1)), 666),
            RegCopy(
                RegisterVariant::Local(Register(1)),
                RegisterVariant::Local(Register(0)),
            ),
            Ret,
        ];

        assert_eq!(walker_init_instructions(&mut walker), expected);

        /* === */

        let mut walker = default_walker();
        let mut node = ReturnNode::new(None);
        let _ = walker.visit_return(&mut node);

        let expected = vec![Ret];

        assert_eq!(walker_init_instructions(&mut walker), expected);
    }

    #[test]
    fn test_visit_string_populates_the_instructions() {
        let mut walker = default_walker();
        let mut node = StringNode::new("marf");
        let mut node2 = StringNode::new("tacos");
        let mut node3 = StringNode::new("marf");

        let _ = walker.visit_string(&mut node);
        let _ = walker.visit_string(&mut node2);
        let _ = walker.visit_string(&mut node3);

        let expected = vec![
            SConst(RegisterVariant::Local(Register(1)), String::from("marf")),
            SConst(RegisterVariant::Local(Register(2)), String::from("tacos")),
            SConst(RegisterVariant::Local(Register(3)), String::from("marf")),
        ];

        assert_eq!(walker_init_instructions(&mut walker), expected);
    }

    mod test_visit_ternary {
        use lpc_rs_asm::instruction::Instruction::{Jmp, Jz, Lte};

        use super::*;
        use crate::compiler::ast::ternary_node::TernaryNode;

        #[test]
        fn populates_the_instructions() {
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

            let _ = walker.visit_ternary(&mut node).unwrap();

            let expected = vec![
                IConst(RegisterVariant::Local(Register(2)), 2),
                IConst(RegisterVariant::Local(Register(3)), 3),
                Lte(
                    RegisterVariant::Local(Register(2)),
                    RegisterVariant::Local(Register(3)),
                    RegisterVariant::Local(Register(4)),
                ),
                Jz(RegisterVariant::Local(Register(4)), "ternary-else_0".into()), // jump to else
                IConst(RegisterVariant::Local(Register(5)), 666),
                RegCopy(
                    RegisterVariant::Local(Register(5)),
                    RegisterVariant::Local(Register(1)),
                ),
                Jmp("ternary-end_1".into()), // jump to end
                IConst(RegisterVariant::Local(Register(6)), 777),
                RegCopy(
                    RegisterVariant::Local(Register(6)),
                    RegisterVariant::Local(Register(1)),
                ),
            ];

            assert_eq!(walker_init_instructions(&mut walker), expected);
        }
    }

    mod test_unary_op {
        use super::*;

        fn setup(op: UnaryOperation, is_post: bool) -> CodegenWalker {
            let mut walker = default_walker();
            let mut node = UnaryOpNode {
                op,
                expr: Box::new(ExpressionNode::from(666)),
                span: None,
                is_post,
            };

            let _ = walker.visit_unary_op(&mut node);
            walker
        }

        mod negate {
            use super::*;

            #[test]
            fn populates_instructions() {
                let mut walker = setup(UnaryOperation::Negate, false);

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
                let mut walker = setup(UnaryOperation::Inc, false);

                let expected = vec![
                    IConst(RegisterVariant::Local(Register(1)), 666),
                    Inc(RegisterVariant::Local(Register(1))),
                ];

                assert_eq!(walker_init_instructions(&mut walker), expected);
            }

            #[test]
            fn populates_instructions_for_post() {
                let mut walker = setup(UnaryOperation::Inc, true);

                let expected = vec![
                    IConst(RegisterVariant::Local(Register(1)), 666),
                    RegCopy(
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
                let mut walker = setup(UnaryOperation::Dec, false);

                let expected = vec![
                    IConst(RegisterVariant::Local(Register(1)), 666),
                    Dec(RegisterVariant::Local(Register(1))),
                ];

                assert_eq!(walker_init_instructions(&mut walker), expected);
            }

            #[test]
            fn populates_instructions_for_post() {
                let mut walker = setup(UnaryOperation::Dec, true);

                let expected = vec![
                    IConst(RegisterVariant::Local(Register(1)), 666),
                    RegCopy(
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
                let mut walker = setup(UnaryOperation::Bang, false);

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
                let mut walker = setup(UnaryOperation::BitwiseNot, false);

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
        use super::*;

        #[test]
        fn test_visit_var_loads_the_var_and_sets_the_result_for_globals() {
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

            let _ = walker.visit_var(&mut node);
            assert_eq!(
                walker.current_result,
                RegisterVariant::Global(Register(666))
            );

            let expected = vec![];
            assert_eq!(walker_init_instructions(&mut walker), expected);
        }

        #[test]
        fn test_visit_var_sets_the_result_for_locals() {
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

            let _ = walker.visit_var(&mut node);
            assert_eq!(walker.current_result, RegisterVariant::Local(Register(666)));

            let expected = vec![];
            assert_eq!(walker_init_instructions(&mut walker), expected);
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

        fn setup_var(type_: LpcType, walker: &mut CodegenWalker) {
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

            let _ = walker.visit_var_init(&mut node);
        }

        fn setup_literal(type_: LpcType, value: ExpressionNode, walker: &mut CodegenWalker) {
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

            let _ = walker.visit_var_init(&mut node);
        }

        #[test]
        fn test_does_not_copy_mapping_literals() {
            let mut walker = setup();
            let pairs = vec![(ExpressionNode::from("foo"), ExpressionNode::from("bar"))];
            setup_literal(
                LpcType::Mapping(false),
                ExpressionNode::Mapping(MappingNode::new(pairs, None)),
                &mut walker,
            );

            let mut map = IndexMap::new();
            map.insert(
                RegisterVariant::Local(Register(1)),
                RegisterVariant::Local(Register(2)),
            );
            assert_eq!(
                walker_init_instructions(&mut walker),
                [
                    SConst(RegisterVariant::Local(Register(1)), String::from("foo")),
                    SConst(RegisterVariant::Local(Register(2)), String::from("bar")),
                    MapConst(RegisterVariant::Local(Register(3)), map)
                ]
            );
        }

        #[test]
        fn test_copies_mapping_vars() {
            let mut walker = setup();
            setup_var(LpcType::Mapping(false), &mut walker);

            assert_eq!(
                walker_init_instructions(&mut walker),
                [RegCopy(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Local(Register(2))
                )]
            );
        }

        #[test]
        fn test_does_not_copy_int_literals() {
            let mut walker = setup();
            setup_literal(
                LpcType::Int(false),
                ExpressionNode::Int(IntNode::new(123)),
                &mut walker,
            );

            assert_eq!(
                walker_init_instructions(&mut walker),
                [IConst(RegisterVariant::Local(Register(1)), 123)]
            );
        }

        #[test]
        fn test_copies_int_vars() {
            let mut walker = setup();
            setup_var(LpcType::Int(false), &mut walker);

            assert_eq!(
                walker_init_instructions(&mut walker),
                [RegCopy(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Local(Register(2))
                )]
            );
        }

        #[test]
        fn test_does_not_copy_float_literals() {
            let mut walker = setup();
            setup_literal(
                LpcType::Float(false),
                ExpressionNode::Float(FloatNode::new(123.0)),
                &mut walker,
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
            let mut walker = setup();
            setup_var(LpcType::Float(false), &mut walker);

            assert_eq!(
                walker_init_instructions(&mut walker),
                [RegCopy(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Local(Register(2))
                )]
            );
        }

        #[test]
        fn test_does_not_copy_string_literals() {
            let mut walker = setup();
            setup_literal(
                LpcType::Int(true),
                ExpressionNode::String(StringNode::new("foo")),
                &mut walker,
            );

            assert_eq!(
                walker_init_instructions(&mut walker),
                [SConst(
                    RegisterVariant::Local(Register(1)),
                    String::from("foo")
                )]
            );
        }

        #[test]
        fn test_copies_string_vars() {
            let mut walker = setup();
            setup_var(LpcType::String(false), &mut walker);

            assert_eq!(
                walker_init_instructions(&mut walker),
                [RegCopy(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Local(Register(2))
                )]
            );
        }

        #[test]
        fn test_does_not_copy_array_literals() {
            let mut walker = setup();
            setup_literal(
                LpcType::Int(true),
                ExpressionNode::Array(ArrayNode::new(vec![ExpressionNode::from(1234)])),
                &mut walker,
            );

            assert_eq!(
                walker_init_instructions(&mut walker),
                [
                    IConst(RegisterVariant::Local(Register(1)), 1234),
                    AConst(
                        RegisterVariant::Local(Register(2)),
                        vec![RegisterVariant::Local(Register(1))]
                    )
                ]
            );
        }

        #[test]
        fn test_copies_array_vars() {
            let mut walker = setup();
            setup_var(LpcType::Int(true), &mut walker);

            assert_eq!(
                walker_init_instructions(&mut walker),
                [RegCopy(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Local(Register(2))
                )]
            );
        }

        #[test]
        fn copies_calls() {
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

            let _ = walker.visit_var_init(&mut node);

            assert_eq!(
                walker_init_instructions(&mut walker),
                [
                    SConst(
                        RegisterVariant::Local(Register(1)),
                        String::from("/foo/bar.c")
                    ),
                    Call {
                        name: String::from("clone_object"),
                        namespace: CallNamespace::Local,
                        num_args: 1,
                        initial_arg: RegisterVariant::Local(Register(1))
                    },
                    RegCopy(
                        RegisterVariant::Local(Register(0)),
                        RegisterVariant::Local(Register(2))
                    )
                ]
            );
        }

        #[test]
        fn sets_up_globals() {
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

            let _ = walker.visit_var_init(&mut node);
            let _ = walker.visit_var_init(&mut node2);

            let expected = vec![
                IConst(RegisterVariant::Local(Register(1)), 12),
                FConst(RegisterVariant::Local(Register(2)), 4.3.into()),
                SConst(RegisterVariant::Local(Register(3)), "hello".into()),
                IConst1(RegisterVariant::Local(Register(4))),
                IConst(RegisterVariant::Local(Register(5)), 2),
                IConst(RegisterVariant::Local(Register(6)), 3),
                AConst(
                    RegisterVariant::Local(Register(7)),
                    vec![
                        RegisterVariant::Local(Register(4)),
                        RegisterVariant::Local(Register(5)),
                        RegisterVariant::Local(Register(6)),
                    ],
                ),
                AConst(
                    RegisterVariant::Local(Register(8)),
                    vec![
                        RegisterVariant::Local(Register(1)),
                        RegisterVariant::Local(Register(2)),
                        RegisterVariant::Local(Register(3)),
                        RegisterVariant::Local(Register(7)),
                    ],
                ),
                RegCopy(
                    RegisterVariant::Local(Register(8)),
                    RegisterVariant::Global(Register(0)),
                ),
                SConst(RegisterVariant::Local(Register(9)), "sup".into()),
                RegCopy(
                    RegisterVariant::Local(Register(9)),
                    RegisterVariant::Global(Register(1)),
                ),
            ];

            assert_eq!(walker_init_instructions(&mut walker), expected);
            assert_eq!(walker.global_counter.as_usize(), 1);
            assert_eq!(walker.global_init_registers, 1);
        }

        #[test]
        fn sets_up_upvalues_when_initialized_to_upvalued_var() {
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

            let _ = walker.visit_var_init(&mut node);

            let sym = walker.context.lookup_var("a").unwrap();
            assert_eq!(sym.location.unwrap(), RegisterVariant::Upvalue(Register(0)));
        }

        #[test]
        fn sets_up_upvalues_when_initialized_to_upvalued_value() {
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

            let _ = walker.visit_var_init(&mut node);

            let sym = walker.context.lookup_var("a").unwrap();
            assert_eq!(sym.location.unwrap(), RegisterVariant::Upvalue(Register(0)));
        }
    }

    mod test_visit_while {
        use lpc_rs_asm::instruction::Instruction::{EqEq, Jmp, Jz};

        use super::*;

        #[test]
        fn test_populates_the_instructions() {
            let mut walker = default_walker();

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

            let _ = walker.visit_while(&mut node);

            let expected = vec![
                IConst(RegisterVariant::Local(Register(1)), 666),
                IConst(RegisterVariant::Local(Register(2)), 777),
                EqEq(
                    RegisterVariant::Local(Register(1)),
                    RegisterVariant::Local(Register(2)),
                    RegisterVariant::Local(Register(3)),
                ),
                Jz(RegisterVariant::Local(Register(3)), "while-end_1".into()),
                SConst(RegisterVariant::Local(Register(4)), String::from("body")),
                Call {
                    name: String::from("dump"),
                    namespace: CallNamespace::Local,
                    num_args: 1,
                    initial_arg: RegisterVariant::Local(Register(4)),
                },
                Jmp("while-start_0".into()),
            ];

            assert_eq!(walker_init_instructions(&mut walker), expected);
        }
    }

    mod test_into_program {
        use super::*;

        #[test]
        fn sets_num_globals() {
            let code = r##"
            int i = 123, j;
            mixed *arr = ({ "foo", "bar", "baz", ({ "quux", 0 }) });
            string asdf = "asdf";
            string b;
        "##;

            let program = walk_prog(code).into_program().expect("failed to compile");
            assert_eq!(program.num_globals, 3)
        }

        #[test]
        fn sets_num_init_registers() {
            let code = r##"
            int i = 123, j;
            mixed *arr = ({ "foo", "bar", "baz", ({ "quux", 0 }) });
            string asdf = "asdf";
            string b;
        "##;

            let program = walk_prog(code).into_program().expect("failed to compile");
            assert_eq!(program.num_init_registers, 3) // TODO: this used to be
                                                      // 12.
        }

        #[test]
        fn reserves_enough_global_registers_when_create_returns_non_void() {
            let code = r##"
                int create() {
                    dump("sup dawg");
                    int b = 123;
                    return b;
                }
            "##;

            let program = walk_prog(code).into_program().expect("failed to compile");
            assert_eq!(program.num_init_registers, 1)
        }
    }

    #[test]
    fn tracks_inherited_globals_for_init() {
        let code = r##"
            inherit "/parent";
            int i = 123, j;
            string asdf = "asdf";
            string b;
        "##;

        let program = walk_prog(code).into_program().expect("failed to compile");
        let init = program.functions.get(INIT_PROGRAM).unwrap();

        assert_eq!(program.num_globals, 7);
        assert_eq!(init.num_locals, 8);
    }

    #[test]
    fn test_combine_inits() {
        let prototype = FunctionPrototype::new(
            INIT_PROGRAM,
            LpcType::Void,
            Default::default(),
            Default::default(),
            None,
            vec![],
            vec![],
        );
        let create_prototype = FunctionPrototype::new(
            CREATE_FUNCTION,
            LpcType::Void,
            Default::default(),
            Default::default(),
            None,
            vec![],
            vec![],
        );

        let mut grandparent_init = ProgramFunction::new(prototype.clone(), 0);
        let grandparent_init_instructions = vec![
            IConst1(RegisterVariant::Local(Register(0))),
            IConst(RegisterVariant::Local(Register(0)), 666),
            Call {
                name: CREATE_FUNCTION.to_string(),
                namespace: CallNamespace::Local,
                num_args: 0,
                initial_arg: RegisterVariant::Local(Register(0)),
            },
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

        let grandparent_create = ProgramFunction::new(create_prototype, 0);

        let mut grandparent = Program::default();
        grandparent
            .functions
            .insert(INIT_PROGRAM.to_string(), grandparent_init.into());
        grandparent
            .functions
            .insert(CREATE_FUNCTION.to_string(), grandparent_create.into());

        let mut parent_init = ProgramFunction::new(prototype.clone(), 0);
        let parent_init_instructions = vec![
            Instruction::IConst1(RegisterVariant::Local(Register(0))),
            Instruction::IConst(RegisterVariant::Local(Register(0)), 666),
            Instruction::SConst(RegisterVariant::Local(Register(1)), "moop".to_string()),
            Instruction::IConst(RegisterVariant::Local(Register(5)), 4321),
            Call {
                name: CREATE_FUNCTION.to_string(),
                namespace: CallNamespace::Local,
                num_args: 0,
                initial_arg: RegisterVariant::Local(Register(0)),
            },
            Instruction::Ret,
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

        let mut parent = Program::default();
        parent
            .functions
            .insert(INIT_PROGRAM.to_string(), parent_init.into());
        parent.inherits.push(grandparent);

        let mut walker = default_walker();
        walker.context.inherits.push(parent);

        let expected = vec![
            IConst1(RegisterVariant::Local(Register(0))),
            IConst(RegisterVariant::Local(Register(0)), 666),
            SConst(RegisterVariant::Local(Register(1)), "moop".to_string()),
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
