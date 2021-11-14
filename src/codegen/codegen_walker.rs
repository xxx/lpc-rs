use crate::{
    asm::{
        instruction::{Address, Instruction, Label},
        register::Register,
        register_counter::RegisterCounter,
    },
    ast::{
        array_node::ArrayNode,
        assignment_node::AssignmentNode,
        ast_node::{AstNode, AstNodeTrait, SpannedNode},
        binary_op_node::{BinaryOpNode, BinaryOperation},
        block_node::BlockNode,
        break_node::BreakNode,
        call_node::CallNode,
        continue_node::ContinueNode,
        decl_node::DeclNode,
        do_while_node::DoWhileNode,
        expression_node::ExpressionNode,
        float_node::FloatNode,
        for_node::ForNode,
        function_def_node::{FunctionDefNode, ARGV},
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
    errors::LpcError,
    interpreter::{
        efun::{CALL_OTHER, CATCH},
        program::Program,
    },
    semantic::{lpc_type::LpcType, program_function::ProgramFunction, symbol::Symbol},
    Result,
};

use crate::{
    ast::function_ptr_node::FunctionPtrNode,
    interpreter::function_type::{FunctionName, FunctionReceiver, FunctionTarget},
};
use itertools::Itertools;
use std::{cmp::Ordering, collections::HashMap, rc::Rc};
use tree_walker::TreeWalker;
use if_chain::if_chain;

macro_rules! push_instruction {
    ($slf:expr, $inst:expr, $span:expr) => {
        $slf.function_stack
            .last_mut()
            .unwrap()
            .push_instruction($inst, $span);
    };
}

/// Name of the user-overridable initializer function for objects
const CREATE_FUNCTION: &str = "create";

/// Name of the function for initialization of a program's global variables.
// Note, it needs to be unparsable (i.e. cannot be overridden by the user)
pub const INIT_PROGRAM: &str = "init-program";

/// Partition on whether the value is stored in registers or memory, to help select instructions.
/// tl;dr - Value types use `Register`, while reference types use `Memory`.
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
#[derive(Debug, Default)]
pub struct CodegenWalker {
    /// Keep track of the current function being generated (including global initialization)
    function_stack: Vec<ProgramFunction>,

    /// Counter for labels, as they need to be unique.
    label_count: usize,

    /// Map of function Symbols, to their respective addresses
    pub functions: HashMap<String, Rc<ProgramFunction>>,

    /// Track where the result of a child branch is
    current_result: Register,

    /// Internal counter to track which registers are used.
    register_counter: RegisterCounter,

    /// Counter for tracking globals
    global_counter: RegisterCounter,

    /// Number of [`Register`]s needed for global initialization
    global_init_registers: usize,

    /// Compilation context
    context: CompilationContext,

    /// Labels where jumps at any particular time need to go to.
    jump_targets: Vec<JumpTarget>,

    /// Mapping of `switch` cases to the address of the first instruction for a match
    case_addresses: Vec<Vec<(SwitchCase, Address)>>,

    /// Because Ranges have two results, we store the registers when we `visit_range`.
    visit_range_results: Option<(Option<Register>, Option<Register>)>,
}

impl CodegenWalker {
    /// Create a new [`CodegenWalker`] that consumes the passed scopes
    ///
    /// # Arguments
    /// `context` - The [`Context`] state that this tree walker will use for its internal workings.
    pub fn new(context: CompilationContext) -> Self {
        let mut result = Self {
            context,
            ..Self::default()
        };

        result.setup_init();

        result
    }

    pub fn setup_init(&mut self) {
        self.function_stack
            .push(ProgramFunction::new(INIT_PROGRAM, 0, 0));
    }

    /// Get a listing of a translated AST, suitable for printing
    ///
    /// # Examples
    /// ```
    /// use lpc_rs::ast::binary_op_node::{BinaryOpNode, BinaryOperation};
    /// use lpc_rs::ast::int_node::IntNode;
    /// use lpc_rs::ast::expression_node::ExpressionNode;
    /// use lpc_rs::codegen::codegen_walker::CodegenWalker;
    /// use lpc_rs::codegen::tree_walker::TreeWalker;
    /// use lpc_rs::compilation_context::CompilationContext;
    ///
    /// let mut node = BinaryOpNode {
    ///     l: Box::new(ExpressionNode::Int(IntNode::new(123))),
    ///     r: Box::new(ExpressionNode::Int(IntNode::new(456))),
    ///     op: BinaryOperation::Sub,
    ///     span: None
    /// };
    /// let mut walker = CodegenWalker::new(CompilationContext::default());
    ///
    /// walker.visit_binary_op(&mut node);
    ///
    /// for instruction in walker.listing() {
    ///     println!("{}", instruction);
    /// }
    /// ```
    pub fn listing(&self) -> Vec<String> {
        let functions = self.functions.values().sorted_unstable_by(|a, b| {
            if a.name == INIT_PROGRAM {
                return Ordering::Less;
            }
            if b.name == INIT_PROGRAM {
                return Ordering::Greater;
            }

            Ord::cmp(&a.name, &b.name)
        });

        functions.flat_map(|func| func.listing()).collect()
    }

    /// Convert this walker's data into a [`Program`]
    pub fn to_program(&self) -> Result<Program> {
        // These are expected and assumed to be in 1:1 correspondence at runtime
        self.ensure_sync()?;

        Ok(Program {
            filename: self.context.filename.clone(),
            functions: self.functions.clone(),
            // add +1 to num_globals for r0, for call return values
            num_globals: self.global_counter.as_usize() + 1,
            num_init_registers: self.register_counter.as_usize(),
            pragmas: self.context.pragmas.clone(),
        })
    }

    fn ensure_sync(&self) -> Result<()> {
        for func in self.functions.values() {
            let a = func.instructions.len();
            let b = func.debug_spans.len();
            if a != b {
                return Err(LpcError::new(format!(
                        "Instructions (length {}) and `debug_spans` (length {}) for function `{}` are out of sync. This would be catastrophic at runtime, and indicates a major bug in the code generator.",
                        a, b, &func.name
                    )));
            }
        }

        Ok(())
    }

    /// Get a reference to a symbol in the current scope
    fn lookup_symbol(&self, name: &str) -> Option<&Symbol> {
        self.context.scopes.lookup(name)
    }

    /// Get a mutable reference to a symbol in the current scope
    fn lookup_symbol_mut(&mut self, name: &str) -> Option<&mut Symbol> {
        self.context.scopes.lookup_mut(name)
    }

    /// Check for a symbol in the global scope
    fn lookup_global(&self, name: &str) -> Option<&Symbol> {
        self.context.scopes.lookup_global(name)
    }

    /// encapsulate vars that can find themselves if they're global
    fn lookup_var_symbol(&self, node: &VarNode) -> Option<&Symbol> {
        if node.global {
            self.lookup_global(&node.name)
        } else {
            self.lookup_symbol(&node.name)
        }
    }

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
                match self.lookup_var_symbol(v) {
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

    /// The main switch to determine which instruction we select for a binary operation
    fn choose_op_instruction(
        &self,
        node: &BinaryOpNode,
        reg_left: Register,
        reg_right: Register,
        reg_result: Register,
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

    /// Allows for recursive determination of typed binary operator instructions, allowing
    /// choice between a numeric (i.e. held in registers) and mixed (i.e. tracked via references)
    /// Switching on the instructions lets us avoid some value lookups at runtime.
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

    /// A special case for function def parameters, where we don't want to generate code
    /// for default arguments - we just want to have it on hand to refer to
    /// when we generate code for calls.
    fn visit_parameter(&mut self, node: &VarInitNode) {
        self.assign_sym_location(&node.name)
    }

    /// A helper to assign the next free [`Register`] to a [`Symbol`]
    /// of the given name, within the current scope.
    fn assign_sym_location(&mut self, name: &str) {
        let current_register = self.register_counter.next().unwrap();

        let symbol = self.lookup_symbol_mut(name);
        if let Some(sym) = symbol {
            sym.location = Some(current_register);
        }
    }

    /// Emit the instruction(s) to take the range of an array or string
    /// # Arguments
    /// `reference` - The [`Register`] holding the reference to the ref we're taking a slice from.
    /// `node` - A reference to the [`RangeNode`] that holds the range of the slice we're taking.
    fn emit_range(&mut self, reference: Register, node: &mut RangeNode) -> Result<()> {
        let first_index = if let Some(expr) = &mut *node.l {
            expr.visit(self)?;
            self.current_result
        } else {
            // Default to 0. No instruction needed as the value in registers defaults to int 0.
            self.register_counter.next().unwrap()
        };

        let second_index = if let Some(expr) = &mut *node.r {
            expr.visit(self)?;
            self.current_result
        } else {
            // A missing range end means just go to the end of the array.
            let register = self.register_counter.next().unwrap();
            let instruction = Instruction::IConst(register, -1);
            push_instruction!(self, instruction, node.span);
            register
        };

        let result = self.register_counter.next().unwrap();
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
        let result_register = self.register_counter.next().unwrap();
        let label = self.new_label("catch_end");

        push_instruction!(
            self,
            Instruction::CatchStart(result_register, label.clone()),
            node.span
        );

        for argument in &mut node.arguments {
            argument.visit(self)?;
        }

        // get the address of the `catchend` pseudo-instruction, so we can jump to a location
        // that is both guaranteed to have an instruction, as well as clean up the handled
        // catch point
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
}

impl ContextHolder for CodegenWalker {
    fn into_context(self) -> CompilationContext {
        self.context
    }
}

impl TreeWalker for CodegenWalker {
    fn visit_array(&mut self, node: &mut ArrayNode) -> Result<()> {
        let mut items = Vec::with_capacity(node.value.len());
        for member in &mut node.value {
            let _ = member.visit(self);
            items.push(self.current_result);
        }

        let register = self.register_counter.next().unwrap();
        self.current_result = register;
        push_instruction!(self, Instruction::AConst(register, items), node.span);

        Ok(())
    }

    fn visit_assignment(&mut self, node: &mut AssignmentNode) -> Result<()> {
        node.rhs.visit(self)?;
        let rhs_result = self.current_result;
        let lhs = &mut *node.lhs;

        if matches!(lhs, ExpressionNode::Var(_)) {
            lhs.visit(self)?;
        }

        match lhs {
            ExpressionNode::Var(VarNode { name, global, .. }) => {
                let lhs_result = self.current_result;

                let assign = Instruction::RegCopy(rhs_result, lhs_result);

                push_instruction!(self, assign, node.span);

                // Copy over globals if necessary
                if *global {
                    if let Some(Symbol {
                        scope_id: 0,
                        location: Some(register),
                        ..
                    }) = self.lookup_global(name)
                    {
                        let store = Instruction::GStore(lhs_result, *register);
                        push_instruction!(self, store, node.span);
                    }
                }

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

                let reg_result = self.register_counter.next().unwrap();
                self.current_result = reg_result;

                let instruction = Instruction::RegCopy(reg_right, reg_result);
                push_instruction!(self, instruction, node.span);

                self.insert_label(end_label, self.current_address());

                return Ok(());
            }
            BinaryOperation::OrOr => {
                // Handle short-circuit behavior
                let end_label = self.new_label("oror-end");

                let reg_result = self.register_counter.next().unwrap();
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

        let reg_result = self.register_counter.next().unwrap();
        self.current_result = reg_result;

        let instruction = self.choose_op_instruction(node, reg_left, reg_right, reg_result);
        push_instruction!(self, instruction, node.span);

        Ok(())
    }

    fn visit_block(&mut self, node: &mut BlockNode) -> Result<()> {
        self.context.scopes.goto(node.scope_id);

        for stmt in &mut node.body {
            stmt.visit(self)?;
        }

        self.context.scopes.pop();
        Ok(())
    }

    fn visit_break(&mut self, node: &mut BreakNode) -> Result<()> {
        if let Some(JumpTarget { break_target, .. }) = self.jump_targets.last() {
            let instruction = Instruction::Jmp(break_target.into());
            push_instruction!(self, instruction, node.span);
            return Ok(());
        }

        Err(LpcError::new("`break` statement without a jump target?").with_span(node.span))
    }

    fn visit_call(&mut self, node: &mut CallNode) -> Result<()> {
        if node.name == CATCH {
            return self.emit_catch(node);
        }

        let mut arg_results = Vec::new();
        let mut ellipsis_results = Vec::new();
        let default_params = self.context.default_function_params.get(&node.name);
        let mut ellipsis = false;

        // All "normal" functions should have a set of default params to match the declared params,
        // even if the defaults are all `None`.
        if let Some(function_args) = default_params {
            // TODO: get rid of this clone or make it cheaper
            let mut function_args = function_args.clone();

            // generate code for the explicitly-specified arguments
            for (idx, function_arg) in function_args.iter_mut().enumerate() {
                // use passed parameters, or default parameters if applicable.
                if let Some(arg) = node.arguments.get_mut(idx) {
                    arg.visit(self)?;
                    arg_results.push(self.current_result);
                } else if let Some(arg) = function_arg {
                    arg.visit(self)?;
                    arg_results.push(self.current_result);
                }
            }

            // generate code for ellipsis arguments
            if let Some(prototype) = self.context.function_prototypes.get(&node.name) {
                if prototype.flags.ellipsis() {
                    ellipsis = true;
                    let ellipsis_arg_count =
                        node.arguments.len().saturating_sub(function_args.len());

                    if ellipsis_arg_count > 0 {
                        let ellipsis_args = &mut node.arguments[function_args.len()..];

                        ellipsis_results.reserve_exact(ellipsis_arg_count);

                        for arg in ellipsis_args {
                            arg.visit(self)?;
                            ellipsis_results.push(self.current_result);
                        }
                    }
                }
            }

            if ellipsis {
                let mut argv = match self.context.scopes.function_scope_mut(&node.name) {
                    Some(scope) => match scope.lookup_mut(ARGV) {
                        Some(sym) => sym,
                        None => {
                            return Err(LpcError::new(
                                    "Ellipsis args were passed, but cannot find `argv`'s location. This should not happen."
                                ).with_span(node.span));
                        }
                    },
                    None => {
                        return Err(LpcError::new(
                            "Ellipsis args were passed, but cannot find `argv`'s location. This should not happen."
                        ).with_span(node.span));
                    }
                };

                let current_register = self.register_counter.next().unwrap();
                argv.location = Some(current_register);

                self.current_result = current_register;
                push_instruction!(
                    self,
                    Instruction::AConst(current_register, ellipsis_results),
                    node.span
                );
                arg_results.push(current_register);
            }
        } else {
            // TODO: This is where efuns are handled
            for argument in &mut node.arguments {
                argument.visit(self)?;
                arg_results.push(self.current_result);
            }
        }

        let instruction = if arg_results.len() == 1 {
            // no need to serialize args for the `Call` instruction if there's only one.
            if let Some(rcvr) = &mut node.receiver {
                rcvr.visit(self)?;
                let receiver_result = self.current_result;
                let name_register = self.register_counter.next().unwrap();
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
            } else {
                if_chain! {
                    if let Some(x) = self.lookup_symbol(&node.name);
                    if x.type_ == LpcType::Function(false);
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
                            num_args: arg_results.len(),
                            initial_arg: arg_results[0],
                        }
                    }
                }
            }
        } else {
            let start_register = self.register_counter.next().unwrap();
            let mut register = start_register;

            // copy each result to the start of the arg register
            for result in &arg_results {
                push_instruction!(self, Instruction::RegCopy(*result, register), node.span);
                register = self.register_counter.next().unwrap();
            }

            // Undo the final call to .next() in the above for-loop to avoid wasting a register
            self.register_counter.go_back();

            if let Some(rcvr) = &mut node.receiver {
                rcvr.visit(self)?;
                let receiver_result = self.current_result;

                let name_register = self.register_counter.next().unwrap();
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
                    if let Some(x) = self.lookup_symbol(&node.name);
                    if x.type_ == LpcType::Function(false);
                    then {
                        // if there's a function pointer with this name in scope, call that.
                        Instruction::CallFp {
                            location: x.location.unwrap(),
                            num_args: arg_results.len(),
                            initial_arg: start_register,
                        }
                    } else {
                        Instruction::Call {
                            name: node.name.clone(),
                            num_args: arg_results.len(),
                            initial_arg: start_register,
                        }
                    }
                }
            }
        };

        push_instruction!(self, instruction, node.span);
        // self.instructions.push(instruction);
        // self.debug_spans.push(node.span);

        let push_copy = |walker: &mut CodegenWalker| {
            let next_register = walker.register_counter.next().unwrap();

            push_instruction!(
                walker,
                Instruction::RegCopy(Register(0), next_register),
                node.span()
            );

            walker.current_result = next_register;
        };

        // Take care of the result after the call returns.
        if let Some(func) = self.context.lookup_function_complete(&node.name) {
            if func.return_type == LpcType::Void {
                self.current_result = Register(0);
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

    fn visit_decl(&mut self, node: &mut DeclNode) -> Result<()> {
        for init in &mut node.initializations {
            self.visit_var_init(init)?;
        }

        Ok(())
    }

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

    fn visit_float(&mut self, node: &mut FloatNode) -> Result<()> {
        let register = self.register_counter.next().unwrap();
        self.current_result = register;
        let instruction = Instruction::FConst(self.current_result, node.value);
        push_instruction!(self, instruction, node.span);

        Ok(())
    }

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

    fn visit_function_def(&mut self, node: &mut FunctionDefNode) -> Result<()> {
        let num_args = node.parameters.len() + (node.flags.ellipsis() as usize);

        let sym = ProgramFunction::new(&node.name, num_args, 0);
        self.function_stack.push(sym);

        let len = self.current_address();
        self.context.scopes.goto_function(&node.name)?;
        self.register_counter.push(0);
        // self.instruction_vecs.push(Vec::new());
        // self.debug_span_vecs.push(Vec::new());

        for parameter in &node.parameters {
            self.visit_parameter(parameter);
        }

        if node.flags.ellipsis() {
            self.assign_sym_location(ARGV);
        }

        for expression in &mut node.body {
            expression.visit(self)?;
        }

        // let mut instructions = self.instruction_vecs.pop().unwrap();
        // let mut debug_spans = self.debug_span_vecs.pop().unwrap();

        self.context.scopes.pop();
        let mut sym = self.function_stack.pop().unwrap();
        sym.num_locals = self.register_counter.as_usize() - num_args;

        // insert a final return if one isn't already there.
        if sym.instructions.len() == len
            || (!sym.instructions.is_empty()
                && *sym.instructions.last().unwrap() != Instruction::Ret)
        {
            // TODO: This should emit a warning unless the return type is void
            sym.push_instruction(Instruction::Ret, node.span);
            // push_instruction!(self, Instruction::Ret, node.span);
        }

        self.functions.insert(node.name.clone(), sym.into());

        self.register_counter.pop();

        Ok(())
    }

    /// Visit a function pointer node
    fn visit_function_ptr(&mut self, node: &mut FunctionPtrNode) -> Result<()>
    where
        Self: Sized,
    {
        let mut applied_arguments = Vec::new();
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

        let target = if let Some(rcvr) = &mut node.receiver {
            // remote receiver, i.e. `call_other`
            rcvr.visit(self)?;
            let receiver = FunctionReceiver::Var(self.current_result);

            // `call_other` always assumes a literal name
            let name = FunctionName::Literal(node.name.clone());

            FunctionTarget::Local(name, receiver)
        } else if self.context.lookup_function(node.name.as_str()).is_some() {
            // A local / inherited function

            // Determine if the name is a var, or a literal function name.
            // Vars take precedence.
            let name = match self.lookup_symbol(&node.name) {
                Some(s) => {
                    if !matches!(s.type_, LpcType::Function(false)) {
                        // if there are no function-type vars of this name, assume the name is literal
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

            FunctionTarget::Local(name, FunctionReceiver::Local)
        } else {
            // Default to assume Efun
            FunctionTarget::Efun(node.name.clone())
        };

        let location = self.register_counter.next().unwrap();
        self.current_result = location;

        let instruction = Instruction::FunctionPtrConst {
            location,
            target,
            applied_arguments,
        };

        push_instruction!(self, instruction, node.span);

        Ok(())
    }

    fn visit_if(&mut self, node: &mut IfNode) -> Result<()> {
        self.context.scopes.goto(node.scope_id);
        let else_label = self.new_label("if-else");
        let end_label = self.new_label("if-end");

        // Visit the condition
        node.condition.visit(self)?;

        // If the condition is false (i.e. equal to 0 or 0.0), jump to the end of the "then" body.
        // Insert a placeholder address, which we correct below after the body's code is generated
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

    fn visit_int(&mut self, node: &mut IntNode) -> Result<()> {
        let register = self.register_counter.next().unwrap();
        self.current_result = register;
        let instruction = match node.value {
            0 => Instruction::IConst0(register),
            1 => Instruction::IConst1(register),
            v => Instruction::IConst(register, v),
        };
        push_instruction!(self, instruction, node.span);

        Ok(())
    }

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

    fn visit_mapping(&mut self, node: &mut MappingNode) -> Result<()> {
        let mut map = HashMap::new();
        for (key, value) in &mut node.value {
            key.visit(self)?;
            let key_result = self.current_result;
            value.visit(self)?;

            map.insert(key_result, self.current_result);
        }

        let register = self.register_counter.next().unwrap();
        self.current_result = register;
        push_instruction!(self, Instruction::MapConst(register, map), node.span);

        Ok(())
    }

    fn visit_program(&mut self, program: &mut ProgramNode) -> Result<()> {
        self.context.scopes.goto_root();
        self.setup_init();

        // Partition global variable initializations vs everything else
        let (global_init, functions): (Vec<&mut AstNode>, Vec<&mut AstNode>) = program
            .body
            .iter_mut()
            .partition(|x| matches!(**x, AstNode::Decl(_)));

        // Hoist all global variables, and initialize them at the very start of the program
        for node in global_init {
            node.visit(self).unwrap();
        }

        if self
            .context
            .function_prototypes
            .contains_key(CREATE_FUNCTION)
        {
            let mut call = CallNode {
                receiver: None,
                arguments: vec![],
                name: "create".to_string(),
                span: None,
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
        sym.num_locals = self.register_counter.as_usize(); // TODO: is this correct?
                                                           //     num_locals: self.process.num_init_registers,

        self.functions.insert(sym.name.clone(), sym.into());

        self.context.scopes.pop();

        Ok(())
    }

    /// Visit a range literal
    fn visit_range(&mut self, node: &mut RangeNode) -> Result<()> {
        let mut result_left: Option<Register> = None;
        let mut result_right: Option<Register> = None;
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

    fn visit_return(&mut self, node: &mut ReturnNode) -> Result<()> {
        if let Some(expression) = &mut node.value {
            expression.visit(self)?;
            let copy = Instruction::RegCopy(self.current_result, Register(0));
            push_instruction!(self, copy, expression.span());
        }

        push_instruction!(self, Instruction::Ret, node.span);

        Ok(())
    }

    fn visit_string(&mut self, node: &mut StringNode) -> Result<()> {
        let register = self.register_counter.next().unwrap();
        self.current_result = register;

        push_instruction!(
            self,
            Instruction::SConst(register, node.value.clone()),
            node.span
        );

        Ok(())
    }

    fn visit_switch(&mut self, node: &mut SwitchNode) -> Result<()> {
        node.expression.visit(self)?;
        let expr_result = self.current_result;

        let test_label = self.new_label("switch-test");
        let instruction = Instruction::Jmp(test_label.clone());
        push_instruction!(self, instruction, node.span);

        let end_label = self.new_label("switch-end");
        self.jump_targets
            .push(JumpTarget::new(end_label.clone(), "".into()));
        let addresses = Vec::new();
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
                    let test_result = self.register_counter.next().unwrap();

                    if let ExpressionNode::Range(range_node) = case_expr {
                        let (range_left, range_right) = self.visit_range_results.unwrap();

                        // check if >= start of range
                        let gte_result = self.register_counter.next().unwrap();
                        let instruction = if let Some(left_reg) = range_left {
                            Instruction::Gte(case_result, left_reg, gte_result)
                        } else {
                            Instruction::IConst1(gte_result)
                        };
                        push_instruction!(self, instruction, range_node.span);

                        // check if <= end of range
                        let lte_result = self.register_counter.next().unwrap();
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

    fn visit_ternary(&mut self, node: &mut TernaryNode) -> Result<()> {
        let result_reg = self.register_counter.next().unwrap();
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

    fn visit_unary_op(&mut self, node: &mut UnaryOpNode) -> Result<()> {
        node.expr.visit(self)?;
        let reg_expr = self.current_result;

        self.current_result = match node.op {
            UnaryOperation::Negate => {
                // multiply by -1
                let reg = self.register_counter.next().unwrap();
                let instruction = Instruction::IConst(reg, -1);
                push_instruction!(self, instruction, node.span);

                let reg_result = self.register_counter.next().unwrap();

                let instruction = Instruction::MMul(reg_expr, reg, reg_result);
                push_instruction!(self, instruction, node.span);

                reg_result
            }
            UnaryOperation::Inc => todo!(),
            UnaryOperation::Dec => todo!(),
            UnaryOperation::Bang => {
                let reg_result = self.register_counter.next().unwrap();

                let instruction = Instruction::Not(reg_expr, reg_result);
                push_instruction!(self, instruction, node.span);

                reg_result
            }
            UnaryOperation::Tilde => todo!(),
        };

        Ok(())
    }

    fn visit_var(&mut self, node: &mut VarNode) -> Result<()> {
        if node.function_name {
            let mut fptr_node = FunctionPtrNode {
                receiver: None,
                arguments: None,
                name: node.name.clone(),
                span: node.span,
            };

            return self.visit_function_ptr(&mut fptr_node);
        }

        let sym = match self.lookup_var_symbol(node) {
            Some(s) => s,
            None => {
                return Err(
                    LpcError::new(format!("Unable to find symbol `{}`", node.name))
                        .with_span(node.span),
                );
            }
        };

        let sym_loc = match sym.location {
            Some(l) => l,
            None => {
                return Err(
                    LpcError::new(format!("Symbol `{}` has no location set.", sym.name))
                        .with_span(node.span),
                );
            }
        };

        if sym.is_global() {
            let result_register = self.register_counter.next().unwrap();
            let instruction = Instruction::GLoad(sym_loc, result_register);
            push_instruction!(self, instruction, node.span);

            self.current_result = result_register;
        } else {
            self.current_result = sym_loc;
        }

        Ok(())
    }

    fn visit_var_init(&mut self, node: &mut VarInitNode) -> Result<()> {
        let current_register;
        let symbol = self.lookup_symbol(&node.name);

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

        current_register = if let Some(expression) = &mut node.value {
            expression.visit(self)?;

            if matches!(expression, ExpressionNode::Var(_)) {
                // Copy to a new register so the new var isn't literally
                // sharing a register with the old one.
                let next_register = self.register_counter.next().unwrap();
                push_instruction!(
                    self,
                    Instruction::RegCopy(self.current_result, next_register),
                    node.span()
                );
                next_register
            } else {
                self.current_result
            }
        } else {
            // Default value to 0 when uninitialized.
            self.register_counter.next().unwrap()
        };

        if global {
            // Store the reference in the globals register.
            // Using next() means global r0 is never used,
            // but makes it possible to skip a bunch of conditionals.
            let dest_register = self.global_counter.next().unwrap();
            let instruction = Instruction::GStore(current_register, dest_register);
            self.global_init_registers = current_register.index();
            push_instruction!(self, instruction, node.span);
        }

        let current_global_register = self.global_counter.current();
        let symbol = self.lookup_symbol_mut(&node.name);

        if let Some(sym) = symbol {
            if global {
                sym.location = Some(current_global_register);
            } else {
                sym.location = Some(current_register);
            }
        }

        Ok(())
    }

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

#[cfg(test)]
mod tests {
    use crate::{
        asm::instruction::Instruction::*,
        ast::{
            ast_node::AstNode, comma_expression_node::CommaExpressionNode,
            expression_node::ExpressionNode,
        },
        codegen::scope_walker::ScopeWalker,
        lpc_parser,
        parser::{lexer::LexWrapper, span::Span},
        semantic::lpc_type::LpcType,
        LpcFloat,
    };

    use super::*;
    use crate::{
        apply_walker,
        codegen::{
            default_params_walker::DefaultParamsWalker, semantic_check_walker::SemanticCheckWalker,
        },
        compiler::{compiler_error::CompilerError, Compiler},
        errors,
        util::path_maker::LpcPath,
    };

    fn default_walker() -> CodegenWalker {
        let mut walker = CodegenWalker::default();
        walker.setup_init();
        walker
    }

    fn walk_prog(prog: &str) -> CodegenWalker {
        walk_code(prog).expect("failed to walk.")
    }

    fn walk_code(code: &str) -> std::result::Result<CodegenWalker, CompilerError> {
        let compiler = Compiler::default();
        let (mut program, context) = compiler
            .parse_string(
                &LpcPath::new_in_game("/my_test.c", "/", "./tests/fixtures/code"),
                code,
            )
            .expect("failed to parse");

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
        println!("walker {:?}", walker);
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
            IConst(Register(1), 123),
            SConst(Register(2), String::from("foo")),
            IConst(Register(3), 666),
            AConst(Register(4), vec![Register(3)]),
            AConst(Register(5), vec![Register(1), Register(2), Register(4)]),
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
                static_: false,
                location: Some(Register(666)),
                scope_id: 0,
                span: None,
            };
            insert_symbol(&mut walker, sym);

            // push a different, local `marf`, to ensure that we don't find it for this assignment.
            walker.context.scopes.push_new();
            let sym = Symbol {
                name: "marf".to_string(),
                type_: LpcType::Int(false),
                static_: false,
                location: Some(Register(123)),
                scope_id: 1,
                span: None,
            };
            insert_symbol(&mut walker, sym);

            let mut node = AssignmentNode {
                lhs: Box::new(ExpressionNode::Var(VarNode {
                    name: "marf".to_string(),
                    span: None,
                    global: true,
                    function_name: false,
                })),
                rhs: Box::new(ExpressionNode::Int(IntNode::new(-12))),
                span: None,
            };

            let _ = walker.visit_assignment(&mut node);
            assert_eq!(
                walker_init_instructions(&mut walker),
                [
                    IConst(Register(1), -12),
                    GLoad(Register(666), Register(2)),
                    RegCopy(Register(1), Register(2)),
                    GStore(Register(2), Register(666))
                ]
            );
        }

        #[test]
        fn test_populates_the_instructions_for_locals() {
            let mut context = CompilationContext::default();
            context.scopes.push_new();
            context.scopes.push_new();
            let mut walker = CodegenWalker::new(context);

            let sym = Symbol {
                name: "marf".to_string(),
                type_: LpcType::Int(false),
                static_: false,
                location: Some(Register(666)),
                scope_id: 1,
                span: None,
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
                    IConst(Register(1), -12),
                    RegCopy(Register(1), Register(666))
                ]
            );
        }

        #[test]
        fn test_populates_the_instructions_for_array_items() {
            let mut context = CompilationContext::default();
            context.scopes.push_new();
            context.scopes.push_new();
            let mut walker = CodegenWalker::new(context);

            let sym = Symbol {
                name: "marf".to_string(),
                type_: LpcType::Int(true),
                static_: false,
                location: Some(Register(666)),
                scope_id: 1,
                span: None,
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
                    IConst(Register(1), -12),
                    IConst1(Register(2)),
                    Store(Register(1), Register(666), Register(2))
                ]
            );
        }
    }

    mod test_binary_op {
        use crate::asm::instruction::Instruction::{
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
                IConst(Register(1), 666),
                IConst(Register(2), 123),
                IConst(Register(3), 456),
                IAdd(Register(2), Register(3), Register(4)),
                IMul(Register(1), Register(4), Register(5)),
            ];

            assert_eq!(walker_init_instructions(&mut walker), expected);
        }

        #[test]
        fn populates_the_instructions_for_floats() {
            let mut context = CompilationContext::default();
            context.scopes.push_new();
            let mut sym = Symbol::new("foo", LpcType::Float(false));
            sym.location = Some(Register(1));
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
                FConst(Register(1), LpcFloat::from(123.45)),
                GLoad(Register(1), Register(2)),
                IConst(Register(3), 456),
                IMul(Register(2), Register(3), Register(4)),
                IAdd(Register(1), Register(4), Register(5)),
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
                SConst(Register(1), String::from("foo")),
                SConst(Register(2), String::from("bar")),
                SConst(Register(3), String::from("baz")),
                MAdd(Register(2), Register(3), Register(4)),
                MAdd(Register(1), Register(4), Register(5)),
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
                IConst(Register(1), 123),
                AConst(Register(2), vec![Register(1)]),
                IConst(Register(3), 456),
                AConst(Register(4), vec![Register(3)]),
                MAdd(Register(2), Register(4), Register(5)),
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
                IConst(Register(1), 123),
                AConst(Register(2), vec![Register(1)]),
                IConst0(Register(3)),
                Load(Register(2), Register(3), Register(4)),
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
                IConst(Register(1), 123),
                AConst(Register(2), vec![Register(1)]),
                IConst1(Register(3)),
                IConst(Register(4), -1),
                Range(Register(2), Register(3), Register(4), Register(5)),
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
                IConst(Register(1), 123),
                Jz(Register(1), "andand-end_0".into()),
                // and also
                SConst(Register(2), "marf!".into()),
                Jz(Register(2), "andand-end_0".into()),
                RegCopy(Register(2), Register(3)),
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
                IConst(Register(1), 123),
                RegCopy(Register(1), Register(2)),
                Jnz(Register(2), "oror-end_0".into()),
                // else
                SConst(Register(3), "sup?".into()),
                RegCopy(Register(3), Register(2)),
                // end is here
            ];

            assert_eq!(walker_init_instructions(&mut walker), expected);
        }
    }

    mod test_break {
        use super::*;
        use crate::asm::{instruction::Instruction::*, register::Register};

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
                IConst(Register(2), 10),
                Lt(Register(1), Register(2), Register(3)),
                Jz(Register(3), "while-end_1".into()),
                Call {
                    name: "dump".into(),
                    num_args: 1,
                    initial_arg: Register(1),
                },
                IConst(Register(4), 5),
                Gt(Register(1), Register(4), Register(5)),
                Jz(Register(5), "if-else_2".into()),
                SConst(Register(6), "breaking".into()),
                Call {
                    name: "dump".into(),
                    num_args: 1,
                    initial_arg: Register(6),
                },
                Jmp("while-end_1".into()),
                IConst1(Register(7)),
                IAdd(Register(1), Register(7), Register(8)),
                RegCopy(Register(8), Register(1)),
                Jmp("while-start_0".into()),
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
                IConst0(Register(1)),
                IConst(Register(2), 10),
                Lt(Register(1), Register(2), Register(3)),
                Jz(Register(3), "for-end_1".into()),
                Call {
                    name: "dump".into(),
                    num_args: 1,
                    initial_arg: Register(1),
                },
                IConst(Register(4), 5),
                Gt(Register(1), Register(4), Register(5)),
                Jz(Register(5), "if-else_3".into()),
                SConst(Register(6), "breaking".into()),
                Call {
                    name: "dump".into(),
                    num_args: 1,
                    initial_arg: Register(6),
                },
                Jmp("for-end_1".into()),
                IConst1(Register(7)),
                IAdd(Register(1), Register(7), Register(8)),
                RegCopy(Register(8), Register(1)),
                IConst1(Register(9)),
                IAdd(Register(1), Register(9), Register(10)),
                RegCopy(Register(10), Register(1)),
                Jmp("for-start_0".into()),
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
                    num_args: 1,
                    initial_arg: Register(1),
                },
                IConst(Register(2), 5),
                Gt(Register(1), Register(2), Register(3)),
                Jz(Register(3), "if-else_3".into()),
                SConst(Register(4), "breaking".into()),
                Call {
                    name: "dump".into(),
                    num_args: 1,
                    initial_arg: Register(4),
                },
                Jmp("do-while-end_1".into()),
                IConst1(Register(5)),
                IAdd(Register(1), Register(5), Register(6)),
                RegCopy(Register(6), Register(1)),
                IConst(Register(7), 10),
                Lt(Register(1), Register(7), Register(8)),
                Jnz(Register(8), "do-while-start_0".into()),
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
                IConst(Register(1), 666),
                Jmp("switch-test_0".into()),
                SConst(Register(2), "YEAH BABY".into()),
                Call {
                    name: "dump".into(),
                    num_args: 1,
                    initial_arg: Register(2),
                },
                Jmp("switch-end_1".into()),
                SConst(Register(3), "very".into()),
                Call {
                    name: "dump".into(),
                    num_args: 1,
                    initial_arg: Register(3),
                },
                SConst(Register(4), "weak".into()),
                Call {
                    name: "dump".into(),
                    num_args: 1,
                    initial_arg: Register(4),
                },
                Jmp("switch-end_1".into()),
                IConst(Register(5), 666),
                EqEq(Register(1), Register(5), Register(6)),
                Jnz(Register(6), "switch-case_2".into()),
                IConst(Register(7), 10),
                IConst(Register(8), 200),
                Gte(Register(8), Register(7), Register(10)),
                Lte(Register(8), Register(8), Register(11)),
                And(Register(10), Register(11), Register(9)),
                Jnz(Register(9), "switch-case_3".into()),
                Jmp("switch-default_4".into()),
                Ret,
            ];

            assert_eq!(
                walker_function_instructions(&mut walker, "create"),
                expected
            );
        }
    }

    mod test_visit_call {
        use crate::asm::instruction::Instruction::{Call, CallOther, CatchEnd, CatchStart, IDiv};

        use super::*;
        use crate::semantic::{
            function_flags::FunctionFlags, function_prototype::FunctionPrototype,
        };

        #[test]
        fn populates_the_instructions() {
            let mut walker = default_walker();
            let call = "dump(4 - 5)";
            let mut tree = lpc_parser::CallParser::new()
                .parse(&CompilationContext::default(), LexWrapper::new(call))
                .unwrap();

            let _ = walker.visit_call(&mut tree);

            let expected = vec![
                IConst(Register(1), -1),
                Call {
                    name: String::from("dump"),
                    num_args: 1,
                    initial_arg: Register(1),
                },
            ];

            assert_eq!(walker_init_instructions(&mut walker), expected);
        }

        #[test]
        fn populates_the_instructions_for_call_other() {
            let mut walker = default_walker();
            let call = "\"foo\"->print(4 - 5)";
            let mut tree = lpc_parser::ExpressionParser::new()
                .parse(&CompilationContext::default(), LexWrapper::new(call))
                .unwrap();

            let _ = tree.visit(&mut walker);

            let expected = vec![
                IConst(Register(1), -1),
                SConst(Register(2), String::from("foo")),
                SConst(Register(3), String::from("print")),
                CallOther {
                    receiver: Register(2),
                    name: Register(3),
                    num_args: 1,
                    initial_arg: Register(1),
                },
                RegCopy(Register(0), Register(4)),
            ];

            assert_eq!(walker_init_instructions(&mut walker), expected);
        }

        #[test]
        fn populates_the_instructions_for_catch() {
            let mut walker = default_walker();
            let call = "catch(12 / 0)";
            let mut tree = lpc_parser::ExpressionParser::new()
                .parse(&CompilationContext::default(), LexWrapper::new(call))
                .unwrap();

            let _ = tree.visit(&mut walker);

            let expected = vec![
                CatchStart(Register(1), "catch_end_0".into()),
                IConst(Register(2), 12),
                IConst0(Register(3)),
                IDiv(Register(2), Register(3), Register(4)),
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
                num_args: 1,
                num_default_args: 0,
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
            sym.location = Some(Register(1));
            context.scopes.current_mut().unwrap().insert(sym);

            let call = "my_func(666)";
            let mut tree = lpc_parser::ExpressionParser::new()
                .parse(&context, LexWrapper::new(call))
                .unwrap();

            let mut walker = CodegenWalker::new(context);
            let _ = tree.visit(&mut walker);

            let expected = vec![
                IConst(Register(1), 666),
                CallFp { location: Register(1), num_args: 1, initial_arg: Register(1) },
                RegCopy(Register(0), Register(2))
            ];

            assert_eq!(walker_init_instructions(&mut walker), expected);
        }

        #[test]
        fn populates_the_instructions_with_defaults() {
            let mut default_function_params = HashMap::new();

            default_function_params.insert(
                String::from("foo"),
                vec![None, Some(ExpressionNode::from("muffuns"))],
            );

            let context = CompilationContext {
                default_function_params,
                ..CompilationContext::default()
            };

            let mut walker = CodegenWalker::new(context);
            let call = "foo(666)";
            let mut tree = lpc_parser::CallParser::new()
                .parse(&walker.context, LexWrapper::new(call))
                .unwrap();

            let _ = walker.visit_call(&mut tree);

            let expected = vec![
                IConst(Register(1), 666),
                SConst(Register(2), String::from("muffuns")),
                RegCopy(Register(1), Register(3)),
                RegCopy(Register(2), Register(4)),
                Call {
                    name: "foo".to_string(),
                    num_args: 2,
                    initial_arg: Register(3),
                },
            ];

            assert_eq!(walker_init_instructions(&mut walker), expected);
        }

        #[test]
        fn copies_non_void_call_results() {
            let mut context = CompilationContext::default();
            let prototype = FunctionPrototype {
                name: "marfin".into(),
                return_type: LpcType::Int(false),
                num_args: 1,
                num_default_args: 0,
                arg_types: vec![],
                span: None,
                arg_spans: vec![],
                flags: FunctionFlags::default(),
            };

            context
                .function_prototypes
                .insert("marfin".into(), prototype);
            let mut walker = CodegenWalker::new(context);
            let call = "marfin(666)";
            let mut tree = lpc_parser::CallParser::new()
                .parse(&walker.context, LexWrapper::new(call))
                .unwrap();

            let _ = walker.visit_call(&mut tree);

            let expected = vec![
                IConst(Register(1), 666),
                Call {
                    name: String::from("marfin"),
                    num_args: 1,
                    initial_arg: Register(1),
                },
                RegCopy(Register(0), Register(2)),
            ];

            assert_eq!(walker_init_instructions(&mut walker), expected);
        }

        #[test]
        fn does_not_copy_void_call_results() {
            let mut context = CompilationContext::default();
            let prototype = FunctionPrototype {
                name: "void_thing".into(),
                return_type: LpcType::Void,
                num_args: 1,
                num_default_args: 0,
                arg_types: vec![],
                span: None,
                arg_spans: vec![],
                flags: FunctionFlags::default(),
            };

            context
                .function_prototypes
                .insert("void_thing".into(), prototype);
            let mut walker = CodegenWalker::new(context);
            let call = "void_thing(666)";
            let mut tree = lpc_parser::CallParser::new()
                .parse(&walker.context, LexWrapper::new(call))
                .unwrap();

            let _ = walker.visit_call(&mut tree);

            let expected = vec![
                IConst(Register(1), 666),
                Call {
                    name: String::from("void_thing"),
                    num_args: 1,
                    initial_arg: Register(1),
                },
            ];

            assert_eq!(walker_init_instructions(&mut walker), expected);
        }

        #[test]
        fn copies_non_void_efun_results() {
            let mut walker = default_walker();
            let call = r#"clone_object("/foo.c")"#;
            let mut tree = lpc_parser::CallParser::new()
                .parse(&CompilationContext::default(), LexWrapper::new(call))
                .unwrap();

            let _ = walker.visit_call(&mut tree);

            let expected = vec![
                SConst(Register(1), String::from("/foo.c")),
                Call {
                    name: String::from("clone_object"),
                    num_args: 1,
                    initial_arg: Register(1),
                },
                RegCopy(Register(0), Register(2)),
            ];

            assert_eq!(walker_init_instructions(&mut walker), expected);
        }

        #[test]
        fn does_not_copy_void_efun_results() {
            let mut walker = default_walker();
            let call = r#"dump("lkajsdflkajsdf")"#;
            let mut tree = lpc_parser::CallParser::new()
                .parse(&CompilationContext::default(), LexWrapper::new(call))
                .unwrap();

            let _ = walker.visit_call(&mut tree);

            let expected = vec![
                SConst(Register(1), String::from("lkajsdflkajsdf")),
                Call {
                    name: String::from("dump"),
                    num_args: 1,
                    initial_arg: Register(1),
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
                num_args: 1,
                num_default_args: 0,
                arg_types: vec![LpcType::String(false)],
                span: None,
                arg_spans: vec![],
                flags: FunctionFlags::default().with_ellipsis(true),
            };

            context
                .function_prototypes
                .insert("my_func".into(), prototype);
            let mut walker = CodegenWalker::new(context);
            let call = "my_func(\"hello!\", 42, \"cool beans\")";
            let mut tree = lpc_parser::CallParser::new()
                .parse(&walker.context, LexWrapper::new(call))
                .unwrap();

            let _ = walker.visit_call(&mut tree);

            let expected = vec![
                SConst(Register(1), "hello!".into()),
                IConst(Register(2), 42),
                SConst(Register(3), "cool beans".into()),
                RegCopy(Register(1), Register(4)),
                RegCopy(Register(2), Register(5)),
                RegCopy(Register(3), Register(6)),
                Call {
                    name: "my_func".into(),
                    num_args: 3,
                    initial_arg: Register(4),
                },
            ];

            assert_eq!(walker_init_instructions(&mut walker), expected);
        }
    }

    #[test]
    fn test_visit_block_populates_instructions() {
        let block = "{ int a = '🏯'; dump(a); }";
        let mut tree = lpc_parser::BlockParser::new()
            .parse(&CompilationContext::default(), LexWrapper::new(block))
            .unwrap();

        let mut scope_walker = ScopeWalker::default();
        let _ = scope_walker.visit_block(&mut tree);

        let context = scope_walker.into_context();
        let mut walker = CodegenWalker::new(context);
        let _ = walker.visit_block(&mut tree);

        let expected = vec![
            IConst(Register(1), 127983),
            Call {
                name: String::from("dump"),
                num_args: 1,
                initial_arg: Register(1),
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
            IConst(Register(1), 123),
            SConst(Register(2), String::from("foo")),
            IConst(Register(3), 666),
            AConst(Register(4), vec![Register(3)]),
        ];

        assert_eq!(walker_init_instructions(&mut walker), expected);
        assert_eq!(walker.current_result, Register(4));
    }

    mod test_continue {
        use super::*;
        use crate::asm::{instruction::Instruction::*, register::Register};

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
                IConst(Register(2), 10),
                Lt(Register(1), Register(2), Register(3)),
                Jz(Register(3), "while-end_1".into()),
                Call {
                    name: "dump".into(),
                    num_args: 1,
                    initial_arg: Register(1),
                },
                IConst(Register(4), 5),
                Gt(Register(1), Register(4), Register(5)),
                Jz(Register(5), "if-else_2".into()),
                SConst(Register(6), "goin' infinite!".into()),
                Call {
                    name: "dump".into(),
                    num_args: 1,
                    initial_arg: Register(6),
                },
                Jmp("while-start_0".into()),
                IConst1(Register(7)),
                IAdd(Register(1), Register(7), Register(8)),
                RegCopy(Register(8), Register(1)),
                Jmp("while-start_0".into()),
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
                IConst0(Register(1)),
                IConst(Register(2), 10),
                Lt(Register(1), Register(2), Register(3)),
                Jz(Register(3), "for-end_1".into()),
                Call {
                    name: "dump".into(),
                    num_args: 1,
                    initial_arg: Register(1),
                },
                IConst(Register(4), 5),
                Gt(Register(1), Register(4), Register(5)),
                Jz(Register(5), "if-else_3".into()),
                SConst(Register(6), "goin' infinite!".into()),
                Call {
                    name: "dump".into(),
                    num_args: 1,
                    initial_arg: Register(6),
                },
                Jmp("for-continue_2".into()),
                IConst1(Register(7)),
                IAdd(Register(1), Register(7), Register(8)),
                RegCopy(Register(8), Register(1)),
                IConst1(Register(9)),
                IAdd(Register(1), Register(9), Register(10)),
                RegCopy(Register(10), Register(1)),
                Jmp("for-start_0".into()),
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
                    num_args: 1,
                    initial_arg: Register(1),
                },
                IConst(Register(2), 5),
                Gt(Register(1), Register(2), Register(3)),
                Jz(Register(3), "if-else_3".into()),
                SConst(Register(4), "goin' infinite!".into()),
                Call {
                    name: "dump".into(),
                    num_args: 1,
                    initial_arg: Register(4),
                },
                Jmp("do-while-continue_2".into()),
                IConst1(Register(5)),
                IAdd(Register(1), Register(5), Register(6)),
                RegCopy(Register(6), Register(1)),
                IConst(Register(7), 10),
                Lt(Register(1), Register(7), Register(8)),
                Jnz(Register(8), "do-while-start_0".into()),
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
        let call = "int foo = 1, *bar = ({ 56 })";
        let mut tree = lpc_parser::DeclParser::new()
            .parse(&CompilationContext::default(), LexWrapper::new(call))
            .unwrap();

        let mut scope_walker = ScopeWalker::default();
        let _ = scope_walker.visit_decl(&mut tree);

        let context = scope_walker.into_context();
        let mut walker = CodegenWalker::new(context);
        let _ = walker.visit_decl(&mut tree);

        let expected = vec![
            IConst1(Register(1)),
            GStore(Register(1), Register(1)),
            IConst(Register(2), 56),
            AConst(Register(3), vec![Register(2)]),
            GStore(Register(3), Register(2)),
        ];

        assert_eq!(walker_init_instructions(&mut walker), expected);

        let scope = walker.context.scopes.current().unwrap();
        assert_eq!(
            scope.lookup("foo").unwrap(),
            Symbol {
                name: String::from("foo"),
                type_: LpcType::Int(false),
                static_: false,
                location: Some(Register(1)),
                scope_id: 0,
                span: Some(Span {
                    file_id: 0,
                    l: 4,
                    r: 11
                })
            }
        );
        assert_eq!(
            scope.lookup("bar").unwrap(),
            Symbol {
                name: String::from("bar"),
                type_: LpcType::Int(true),
                static_: false,
                location: Some(Register(2)),
                scope_id: 0,
                span: Some(Span {
                    file_id: 0,
                    l: 13,
                    r: 25
                })
            }
        );
    }

    mod test_visit_do_while {
        use super::*;
        use crate::{
            asm::instruction::Instruction::{EqEq, Jnz},
            ast::do_while_node::DoWhileNode,
        };

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
                })),
                scope_id: None,
                span: None,
            };

            let _ = walker.visit_do_while(&mut node);

            let expected = vec![
                SConst(Register(1), String::from("body")),
                Call {
                    name: String::from("dump"),
                    num_args: 1,
                    initial_arg: Register(1),
                },
                IConst(Register(2), 666),
                IConst(Register(3), 777),
                EqEq(Register(2), Register(3), Register(4)),
                Jnz(Register(4), "do-while-start_0".into()),
            ];

            assert_eq!(walker_init_instructions(&mut walker), expected);
        }
    }

    mod test_visit_for {
        use super::*;
        use crate::{
            asm::instruction::Instruction::{ISub, Jmp, Jz},
            ast::for_node::ForNode,
        };

        #[test]
        fn populates_the_instructions() {
            let var = VarNode {
                name: "i".to_string(),
                span: None,
                global: false,
                function_name: false,
            };

            let mut node = ForNode {
                initializer: Box::new(Some(AstNode::VarInit(VarInitNode {
                    type_: LpcType::Int(false),
                    name: "i".to_string(),
                    value: Some(ExpressionNode::from(10)),
                    array: false,
                    global: false,
                    span: None,
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
                IConst(Register(1), 10),
                Jz(Register(1), "for-end_1".into()),
                Call {
                    name: String::from("dump"),
                    num_args: 1,
                    initial_arg: Register(1),
                },
                IConst1(Register(2)),
                ISub(Register(1), Register(2), Register(3)),
                RegCopy(Register(3), Register(1)),
                Jmp("for-start_0".into()),
            ];

            assert_eq!(walker_init_instructions(&mut walker), expected);
        }
    }

    #[test]
    fn test_visit_function_def_populates_the_data() {
        let mut scope_walker = ScopeWalker::default();
        let _walker = CodegenWalker::default();
        let call = "int main(int i) { return i + 4; }";
        let tree = lpc_parser::DefParser::new()
            .parse(&CompilationContext::default(), LexWrapper::new(call))
            .unwrap();

        let mut node = if let AstNode::FunctionDef(node) = tree {
            node
        } else {
            panic!("Didn't receive a function def?");
        };

        let _ = scope_walker.visit_function_def(&mut node);

        let mut context = scope_walker.into_context();
        context.scopes.goto_root();

        let mut walker = CodegenWalker::new(context);
        let _ = walker.visit_function_def(&mut node);

        let expected = vec![
            IConst(Register(2), 4),
            IAdd(Register(1), Register(2), Register(3)),
            RegCopy(Register(3), Register(0)),
            Ret,
        ];

        assert_eq!(walker_function_instructions(&mut walker, "main"), expected);
    }

    #[test]
    fn test_visit_function_def_handles_ellipses() {
        let mut scope_walker = ScopeWalker::default();
        let _walker = CodegenWalker::default();
        let call = "int main(int i, ...) { return argv; }";
        let tree = lpc_parser::DefParser::new()
            .parse(&CompilationContext::default(), LexWrapper::new(call))
            .unwrap();

        let mut node = if let AstNode::FunctionDef(node) = tree {
            node
        } else {
            panic!("Didn't receive a function def?");
        };

        let _ = scope_walker.visit_function_def(&mut node);

        let mut context = scope_walker.into_context();
        context.scopes.goto_root();

        let mut walker = CodegenWalker::new(context);
        let _ = walker.visit_function_def(&mut node);

        let expected = vec![RegCopy(Register(2), Register(0)), Ret];

        assert_eq!(walker_function_instructions(&mut walker, "main"), expected);
    }

    mod test_visit_if {
        use super::*;
        use crate::asm::instruction::Instruction::{EqEq, Jmp, Jz};

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
                })),
                else_clause: Box::new(Some(AstNode::Call(CallNode {
                    receiver: None,
                    arguments: vec![ExpressionNode::from("false")],
                    name: "dump".to_string(),
                    span: None,
                }))),
                scope_id: None,
                span: None,
            };

            let _ = walker.visit_if(&mut node);

            let expected = vec![
                IConst(Register(1), 666),
                IConst(Register(2), 777),
                EqEq(Register(1), Register(2), Register(3)),
                Jz(Register(3), "if-else_0".into()),
                SConst(Register(4), String::from("true")),
                Call {
                    name: String::from("dump"),
                    num_args: 1,
                    initial_arg: Register(4),
                },
                Jmp("if-end_1".into()),
                SConst(Register(5), String::from("false")),
                Call {
                    name: String::from("dump"),
                    num_args: 1,
                    initial_arg: Register(5),
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
            IConst(Register(1), 666),
            IConst0(Register(2)),
            IConst1(Register(3)),
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
                    num_args: 0,
                    initial_arg: Register(1),
                },
                Ret,
            ];

            assert_eq!(
                walker.functions.get(INIT_PROGRAM).unwrap().instructions,
                expected
            );

            let expected = vec![
                IConst(Register(1), -1),
                IConst(Register(2), 9),
                Call {
                    name: String::from("dump"),
                    num_args: 1,
                    initial_arg: Register(2),
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
                IConst(Register(1), 123),
                GStore(Register(1), Register(1)),
                SConst(Register(2), String::from("cool")),
                GStore(Register(2), Register(2)),
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
                IConst(Register(1), 666),
                GStore(Register(1), Register(1)),
                Call {
                    name: String::from("create"),
                    num_args: 0,
                    initial_arg: Register(2),
                },
                Ret, // end of initialization
                     // IConst(Register(1), 3),
                     // RegCopy(Register(1), Register(0)),
                     // Ret, // end of marf()
                     // Call {
                     //     name: String::from("marf"),
                     //     num_args: 0,
                     //     initial_arg: Register(1),
                     // },
                     // RegCopy(Register(0), Register(1)),
                     // SConst(Register(2), String::from(" times a winner!")),
                     // MAdd(Register(1), Register(2), Register(3)),
                     // Call {
                     //     name: String::from("dump"),
                     //     num_args: 1,
                     //     initial_arg: Register(3),
                     // },
                     // Ret, // end of create()
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
                IConst(Register(1), 666),
                GStore(Register(1), Register(1)),
                IConst(Register(2), 777),
                GStore(Register(2), Register(2)),
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
            IConst(Register(1), 666),
            RegCopy(Register(1), Register(0)),
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
            SConst(Register(1), String::from("marf")),
            SConst(Register(2), String::from("tacos")),
            SConst(Register(3), String::from("marf")),
        ];

        assert_eq!(walker_init_instructions(&mut walker), expected);
    }

    mod test_visit_ternary {
        use super::*;
        use crate::{
            asm::instruction::Instruction::{Jmp, Jz, Lte},
            ast::ternary_node::TernaryNode,
        };

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
                IConst(Register(2), 2),
                IConst(Register(3), 3),
                Lte(Register(2), Register(3), Register(4)),
                Jz(Register(4), "ternary-else_0".into()), // jump to else
                IConst(Register(5), 666),
                RegCopy(Register(5), Register(1)),
                Jmp("ternary-end_1".into()), // jump to end
                IConst(Register(6), 777),
                RegCopy(Register(6), Register(1)),
            ];

            assert_eq!(walker_init_instructions(&mut walker), expected);
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
                    static_: false,
                    location: Some(Register(666)),
                    scope_id: 0,
                    span: None,
                },
            );
            // push a local scope with a matching variable in a different location
            walker.context.scopes.push_new();
            insert_symbol(
                &mut walker,
                Symbol {
                    name: "marf".to_string(),
                    type_: LpcType::Int(false),
                    static_: false,
                    location: Some(Register(222)),
                    scope_id: 1,
                    span: None,
                },
            );

            let mut node = VarNode {
                name: "marf".to_string(),
                span: None,
                global: true,
                function_name: false,
            };

            let _ = walker.visit_var(&mut node);
            assert_eq!(walker.current_result, Register(1)); // global loaded into r1

            let expected = vec![GLoad(Register(666), Register(1))];
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
                    static_: false,
                    location: Some(Register(444)),
                    scope_id: 0,
                    span: None,
                },
            );
            walker.context.scopes.push_new(); // push a local scope
            insert_symbol(
                &mut walker,
                Symbol {
                    name: "marf".to_string(),
                    type_: LpcType::Int(false),
                    static_: false,
                    location: Some(Register(666)),
                    scope_id: 1,
                    span: None,
                },
            );

            let mut node = VarNode::new("marf");

            let _ = walker.visit_var(&mut node);
            assert_eq!(walker.current_result, Register(666));

            let expected = vec![];
            assert_eq!(walker_init_instructions(&mut walker), expected);
        }
    }

    mod test_visit_var_init {
        use super::*;
        use crate::asm::instruction::Instruction::{FConst, MapConst};
        use decorum::Total;

        fn setup() -> CodegenWalker {
            let mut context = CompilationContext::default();
            context.scopes.push_new();
            context.scopes.push_new();
            CodegenWalker::new(context)
        }

        fn setup_var(type_: LpcType, walker: &mut CodegenWalker) {
            let sym = Symbol {
                location: Some(Register(1)),
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
            };

            insert_symbol(walker, Symbol::from(&mut node.clone()));

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

            let mut map = HashMap::new();
            map.insert(Register(1), Register(2));
            assert_eq!(
                walker_init_instructions(&mut walker),
                [
                    SConst(Register(1), String::from("foo")),
                    SConst(Register(2), String::from("bar")),
                    MapConst(Register(3), map)
                ]
            );
        }

        #[test]
        fn test_copies_mapping_vars() {
            let mut walker = setup();
            setup_var(LpcType::Mapping(false), &mut walker);

            assert_eq!(
                walker_init_instructions(&mut walker),
                [RegCopy(Register(1), Register(2))]
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
                [IConst(Register(1), 123)]
            );
        }

        #[test]
        fn test_copies_int_vars() {
            let mut walker = setup();
            setup_var(LpcType::Int(false), &mut walker);

            assert_eq!(
                walker_init_instructions(&mut walker),
                [RegCopy(Register(1), Register(2))]
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
                [FConst(Register(1), Total::from(123.0))]
            );
        }

        #[test]
        fn test_copies_float_vars() {
            let mut walker = setup();
            setup_var(LpcType::Float(false), &mut walker);

            assert_eq!(
                walker_init_instructions(&mut walker),
                [RegCopy(Register(1), Register(2))]
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
                [SConst(Register(1), String::from("foo"))]
            );
        }

        #[test]
        fn test_copies_string_vars() {
            let mut walker = setup();
            setup_var(LpcType::String(false), &mut walker);

            assert_eq!(
                walker_init_instructions(&mut walker),
                [RegCopy(Register(1), Register(2))]
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
                    IConst(Register(1), 1234),
                    AConst(Register(2), vec![Register(1)])
                ]
            );
        }

        #[test]
        fn test_copies_array_vars() {
            let mut walker = setup();
            setup_var(LpcType::Int(true), &mut walker);

            assert_eq!(
                walker_init_instructions(&mut walker),
                [RegCopy(Register(1), Register(2))]
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
                })),
                array: false,
                global: false,
                span: None,
            };

            insert_symbol(&mut walker, Symbol::from(&mut node.clone()));

            let _ = walker.visit_var_init(&mut node);

            assert_eq!(
                walker_init_instructions(&mut walker),
                [
                    SConst(Register(1), String::from("/foo/bar.c")),
                    Call {
                        name: String::from("clone_object"),
                        num_args: 1,
                        initial_arg: Register(1)
                    },
                    RegCopy(Register(0), Register(2))
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
            };

            insert_symbol(&mut walker, Symbol::from(&mut node.clone()));

            let mut node2 = VarInitNode {
                type_: LpcType::Mixed(true),
                name: "str".to_string(),
                value: Some(ExpressionNode::from("sup")),
                array: false,
                global: true,
                span: None,
            };

            insert_symbol(&mut walker, Symbol::from(&mut node.clone()));
            insert_symbol(&mut walker, Symbol::from(&mut node2.clone()));

            let _ = walker.visit_var_init(&mut node);
            let _ = walker.visit_var_init(&mut node2);

            let expected = vec![
                IConst(Register(1), 12),
                FConst(Register(2), 4.3.into()),
                SConst(Register(3), "hello".into()),
                IConst1(Register(4)),
                IConst(Register(5), 2),
                IConst(Register(6), 3),
                AConst(Register(7), vec![Register(4), Register(5), Register(6)]),
                AConst(
                    Register(8),
                    vec![Register(1), Register(2), Register(3), Register(7)],
                ),
                GStore(Register(8), Register(1)),
                SConst(Register(9), "sup".into()),
                GStore(Register(9), Register(2)),
            ];

            assert_eq!(walker_init_instructions(&mut walker), expected);
            assert_eq!(walker.global_counter.as_usize(), 2);
            assert_eq!(walker.global_init_registers, 9);
        }
    }

    mod test_visit_while {
        use super::*;
        use crate::asm::instruction::Instruction::{EqEq, Jmp, Jz};

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
                })),
                scope_id: None,
                span: None,
            };

            let _ = walker.visit_while(&mut node);

            let expected = vec![
                IConst(Register(1), 666),
                IConst(Register(2), 777),
                EqEq(Register(1), Register(2), Register(3)),
                Jz(Register(3), "while-end_1".into()),
                SConst(Register(4), String::from("body")),
                Call {
                    name: String::from("dump"),
                    num_args: 1,
                    initial_arg: Register(4),
                },
                Jmp("while-start_0".into()),
            ];

            assert_eq!(walker_init_instructions(&mut walker), expected);
        }
    }

    mod test_to_program {
        use super::*;

        #[test]
        fn sets_num_globals() {
            let code = r##"
            int i = 123, j;
            mixed *arr = ({ "foo", "bar", "baz", ({ "quux", 0 }) });
            string asdf = "asdf";
            string b;
        "##;

            let program = walk_prog(code).to_program().expect("failed to compile");
            assert_eq!(program.num_globals, 6)
        }

        #[test]
        fn sets_num_init_registers() {
            let code = r##"
            int i = 123, j;
            mixed *arr = ({ "foo", "bar", "baz", ({ "quux", 0 }) });
            string asdf = "asdf";
            string b;
        "##;

            let program = walk_prog(code).to_program().expect("failed to compile");
            assert_eq!(program.num_init_registers, 11)
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

            let program = walk_prog(code).to_program().expect("failed to compile");
            assert_eq!(program.num_init_registers, 1)
        }
    }

    fn insert_symbol(walker: &mut CodegenWalker, symbol: Symbol) {
        if let Some(node_id) = walker.context.scopes.current_id {
            walker
                .context
                .scopes
                .get_mut(node_id)
                .unwrap()
                .insert(symbol)
        } else {
            panic!("No current scope to insert the symbol into.")
        }
    }
}
