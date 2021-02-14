use crate::{
    asm::{instruction::Instruction, register::Register, register_counter::RegisterCounter},
    ast::{
        array_node::ArrayNode,
        assignment_node::AssignmentNode,
        ast_node::ASTNodeTrait,
        binary_op_node::{BinaryOpNode, BinaryOperation},
        call_node::CallNode,
        decl_node::DeclNode,
        expression_node::ExpressionNode,
        function_def_node::FunctionDefNode,
        int_node::IntNode,
        program_node::ProgramNode,
        return_node::ReturnNode,
        string_node::StringNode,
        var_init_node::VarInitNode,
        var_node::VarNode,
    },
    codegen::tree_walker,
    errors::compiler_error::CompilerError,
    interpreter::{constant_pool::ConstantPool, lpc_value::LPCValue, program::Program},
    parser::span::Span,
    semantic::{
        function_symbol::FunctionSymbol, lpc_type::LPCType, scope_tree::ScopeTree, symbol::Symbol,
    },
};
use multimap::MultiMap;
use std::collections::HashMap;
use tree_walker::TreeWalker;

/// Really just a `pc` index in the vm.
type Address = usize;

/// A tree walker that generates assembly language instructions based on an AST.
#[derive(Debug, Default)]
pub struct AsmTreeWalker {
    /// The vector of instructions generated by this walker
    pub instructions: Vec<Instruction>,

    /// Code spans, corresponding to the instructions, for use in runtime error messaging
    pub debug_spans: Vec<Option<Span>>,

    /// The map of labels, to their respective addresses
    pub labels: HashMap<String, Address>,

    /// The map of function Symbols, to their respective addresses
    pub functions: HashMap<FunctionSymbol, Address>,

    /// Track where the result of a child branch is
    current_result: Register,

    /// A mapping of function names to initializers, for use in dealing
    /// with default parameters while generating code for calls
    function_params: HashMap<String, Vec<Option<ExpressionNode>>>,

    /// The internal counter to track which registers are used.
    register_counter: RegisterCounter,

    /// The counter for tracking globals
    global_counter: RegisterCounter,

    /// The collection of scopes
    scopes: ScopeTree,

    /// All constants (really only string literals) in the program
    constants: ConstantPool,
}

impl AsmTreeWalker {
    /// Create a new `AsmTreeWalker` that consumes the passed scopes
    ///
    /// # Arguments
    /// `scopes` - The ScopeTree to use to resolve symbols and function calls.
    pub fn new(
        scopes: ScopeTree,
        function_params: HashMap<String, Vec<Option<ExpressionNode>>>,
    ) -> Self {
        Self {
            scopes,
            function_params,
            ..Default::default()
        }
    }

    /// Get a listing of a translated AST, suitable for printing
    ///
    /// # Examples
    /// ```
    /// use std::borrow::BorrowMut;
    /// use lpc_rs::ast::binary_op_node::{BinaryOpNode, BinaryOperation};
    /// use lpc_rs::ast::int_node::IntNode;
    /// use lpc_rs::ast::expression_node::ExpressionNode;
    /// use lpc_rs::codegen::asm_tree_walker::AsmTreeWalker;
    /// use lpc_rs::codegen::tree_walker::TreeWalker;
    ///
    /// let mut node = BinaryOpNode {
    ///     l: Box::new(ExpressionNode::Int(IntNode::new(123))),
    ///     r: Box::new(ExpressionNode::Int(IntNode::new(456))),
    ///     op: BinaryOperation::Sub,
    ///     span: None
    /// };
    /// let mut walker = AsmTreeWalker::default();
    ///
    /// walker.visit_binary_op(node.borrow_mut());
    ///
    /// for instruction in walker.listing() {
    ///     println!("{}", instruction);
    /// }
    /// ```
    pub fn listing(&self) -> Vec<String> {
        let mut v = vec![];

        // invert these maps for by-address lookup
        let functions_by_pc = self
            .functions
            .values()
            .zip(self.functions.keys())
            .collect::<HashMap<_, _>>();

        // use MultiMap as multiple labels can be at the same address
        let labels_by_pc = self
            .labels
            .values()
            .zip(self.labels.keys())
            .collect::<MultiMap<_, _>>();

        for (counter, instruction) in self.instructions.iter().enumerate() {
            if let Some(sym) = functions_by_pc.get(&counter) {
                v.push(format!(
                    "fn {} num_args={} num_locals={}:",
                    sym.name, sym.num_args, sym.num_locals
                ));
            }
            if let Some(vec) = labels_by_pc.get_vec(&counter) {
                for label in vec {
                    v.push(format!("{}:", label));
                }
            }
            v.push(format!("    {}", instruction));
        }

        v
    }

    /// Setter for the scopes
    pub fn set_scopes(&mut self, scopes: ScopeTree) {
        self.scopes = scopes;
    }

    /// Return a map of function names to their corresponding full symbol
    pub fn function_map(&self) -> HashMap<String, FunctionSymbol> {
        let mut map = HashMap::new();

        for sym in self.functions.keys() {
            map.insert(sym.name.clone(), sym.clone());
        }

        map
    }

    /// Convert this walker's data into a Program
    pub fn to_program(&self, filepath: &str) -> Program {
        // These are expected and assumed to be in 1:1 correspondence at runtime
        assert_eq!(self.instructions.len(), self.debug_spans.len());

        Program {
            instructions: self.instructions.to_vec(),
            debug_spans: self.debug_spans.to_vec(),
            filename: filepath.to_string(),
            labels: self.labels.clone(),
            functions: self.function_map(),
            constants: self.constants.clone(),
            num_globals: self.global_counter.get_count(),
        }
    }

    /// Get a reference to a symbol in the current scope
    fn lookup_symbol(&self, name: &str) -> Option<&Symbol> {
        self.scopes.lookup(name)
    }

    /// Get a mutable reference to a symbol in the current scope
    fn lookup_symbol_mut(&mut self, name: &str) -> Option<&mut Symbol> {
        self.scopes.lookup_mut(name)
    }

    /// Check for a symbol in the global scope
    fn lookup_global(&self, name: &str) -> Option<&Symbol> {
        self.scopes.lookup_global(name)
    }

    /// encapsulate vars that can find themselves if they're global
    fn lookup_var_symbol(&self, node: &VarNode) -> Option<&Symbol> {
        if node.global {
            self.lookup_global(&node.name)
        } else {
            self.lookup_symbol(&node.name)
        }
    }

    /// Allows for recursive determination of typed add instructions
    fn choose_add_instruction(
        &self,
        node: &BinaryOpNode,
        reg_left: Register,
        reg_right: Register,
        reg_result: Option<Register>,
    ) -> Instruction {
        match (&*node.l, &*node.r) {
            (ExpressionNode::BinaryOp(bin_op), _) => {
                self.choose_add_instruction(bin_op, reg_left, reg_right, reg_result)
            }
            (ExpressionNode::Var(var_node1), ExpressionNode::Var(var_node2)) => {
                let type1 = self.lookup_var_symbol(&var_node1).unwrap().type_;
                let type2 = self.lookup_var_symbol(&var_node2).unwrap().type_;

                if let (LPCType::Int(false), LPCType::Int(false)) = (type1, type2) {
                    Instruction::IAdd(reg_left, reg_right, reg_result.unwrap())
                } else {
                    Instruction::MAdd(reg_left, reg_right, reg_result.unwrap())
                }
            }
            (ExpressionNode::Var(var_node), ExpressionNode::Int(_)) => {
                let type_ = self.lookup_var_symbol(&var_node).unwrap().type_;

                if let LPCType::Int(false) = type_ {
                    Instruction::IAdd(reg_left, reg_right, reg_result.unwrap())
                } else {
                    Instruction::MAdd(reg_left, reg_right, reg_result.unwrap())
                }
            }
            (ExpressionNode::Int(_), ExpressionNode::Var(var_node)) => {
                let type_ = self.lookup_var_symbol(&var_node).unwrap().type_;

                if let LPCType::Int(false) = type_ {
                    Instruction::IAdd(reg_left, reg_right, reg_result.unwrap())
                } else {
                    Instruction::MAdd(reg_left, reg_right, reg_result.unwrap())
                }
            }
            (ExpressionNode::String(_), _) => {
                Instruction::MAdd(reg_left, reg_right, reg_result.unwrap())
            }
            (ExpressionNode::Int(_), ExpressionNode::Int(_)) => {
                Instruction::IAdd(reg_left, reg_right, reg_result.unwrap())
            }
            _ => Instruction::MAdd(reg_left, reg_right, reg_result.unwrap()),
        }
    }

    /// Allows for recursive determination of typed mul instructions
    fn choose_mul_instruction(
        &self,
        node: &BinaryOpNode,
        reg_left: Register,
        reg_right: Register,
        reg_result: Option<Register>,
    ) -> Instruction {
        match (&*node.l, &*node.r) {
            (ExpressionNode::BinaryOp(bin_op), _) => {
                self.choose_mul_instruction(bin_op, reg_left, reg_right, reg_result)
            }
            (ExpressionNode::Var(var_node1), ExpressionNode::Var(var_node2)) => {
                let type1 = self.lookup_var_symbol(&var_node1).unwrap().type_;
                let type2 = self.lookup_var_symbol(&var_node2).unwrap().type_;

                if let (LPCType::Int(false), LPCType::Int(false)) = (type1, type2) {
                    Instruction::IMul(reg_left, reg_right, reg_result.unwrap())
                } else {
                    Instruction::MMul(reg_left, reg_right, reg_result.unwrap())
                }
            }
            (ExpressionNode::Int(_), ExpressionNode::Var(var_node)) => {
                let type_ = self.lookup_var_symbol(&var_node).unwrap().type_;

                if let LPCType::Int(false) = type_ {
                    Instruction::IMul(reg_left, reg_right, reg_result.unwrap())
                } else {
                    Instruction::MMul(reg_left, reg_right, reg_result.unwrap())
                }
            }
            (ExpressionNode::Var(var_node), ExpressionNode::Int(_)) => {
                let type_ = self.lookup_var_symbol(&var_node).unwrap().type_;

                if let LPCType::Int(false) = type_ {
                    Instruction::IMul(reg_left, reg_right, reg_result.unwrap())
                } else {
                    Instruction::MMul(reg_left, reg_right, reg_result.unwrap())
                }
            }
            (ExpressionNode::String(_), _) => {
                Instruction::MMul(reg_left, reg_right, reg_result.unwrap())
            }
            (_, ExpressionNode::String(_)) => {
                Instruction::MMul(reg_left, reg_right, reg_result.unwrap())
            }
            _ => Instruction::IMul(reg_left, reg_right, reg_result.unwrap()),
        }
    }

    /// A special case for function def parameters, where we don't want to generate code
    /// for default arguments - we just want to have it on hand to refer to
    /// when we generate code for calls.
    fn visit_parameter(&mut self, node: &VarInitNode) -> Result<(), CompilerError> {
        let current_register;

        current_register = self.register_counter.next().unwrap();

        let symbol = self.lookup_symbol_mut(&node.name);
        if let Some(sym) = symbol {
            sym.location = Some(current_register);
        }

        Ok(())
    }
}

impl TreeWalker for AsmTreeWalker {
    fn visit_program(&mut self, program: &mut ProgramNode) -> Result<(), CompilerError> {
        self.scopes.goto_root();
        for expr in &mut program.body {
            let _ = expr.visit(self);
        }
        self.scopes.pop();

        Ok(())
    }

    fn visit_call(&mut self, node: &mut CallNode) -> Result<(), CompilerError> {
        let mut arg_results = vec![];

        let params = self.function_params.get(&node.name);

        if let Some(function_args) = params {
            let mut function_args = function_args.to_vec();

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
        } else {
            // TODO: This is where efuns are handled
            for argument in &mut node.arguments {
                argument.visit(self)?;
                arg_results.push(self.current_result);
            }
        }

        let start_register = self.register_counter.next();
        let mut register = start_register;

        // copy each result to the start of the arg register
        for result in &arg_results {
            self.instructions
                .push(Instruction::RegCopy(*result, register.unwrap()));
            self.debug_spans.push(node.span);
            register = self.register_counter.next();
        }

        // Undo the final call to .next() to avoid skipping a register
        self.register_counter.go_back();

        let instruction = Instruction::Call {
            name: node.name.clone(),
            num_args: arg_results.len(),
            initial_arg: start_register.unwrap(),
        };

        self.instructions.push(instruction);
        self.debug_spans.push(node.span);
        self.current_result = Register(0); // returned results are in r0

        Ok(())
    }

    fn visit_int(&mut self, node: &mut IntNode) -> Result<(), CompilerError> {
        let register = self.register_counter.next();
        self.current_result = register.unwrap();
        let instruction = match node.value {
            0 => Instruction::IConst0(register.unwrap()),
            1 => Instruction::IConst1(register.unwrap()),
            v => Instruction::IConst(register.unwrap(), v),
        };
        self.instructions.push(instruction);
        self.debug_spans.push(node.span);

        Ok(())
    }

    fn visit_string(&mut self, node: &mut StringNode) -> Result<(), CompilerError> {
        let register = self.register_counter.next().unwrap();
        self.current_result = register;

        let index = self.constants.insert(LPCValue::from(&node.value));

        self.instructions.push(Instruction::SConst(register, index));
        self.debug_spans.push(node.span);

        Ok(())
    }

    fn visit_binary_op(&mut self, node: &mut BinaryOpNode) -> Result<(), CompilerError> {
        node.l.visit(self)?;
        let reg_left = self.current_result;

        node.r.visit(self)?;
        let reg_right = self.current_result;

        let reg_result = self.register_counter.next();
        self.current_result = reg_result.unwrap();

        let instruction = match node.op {
            BinaryOperation::Add => {
                self.choose_add_instruction(&node, reg_left, reg_right, reg_result)
            }
            BinaryOperation::Sub => Instruction::ISub(reg_left, reg_right, reg_result.unwrap()),
            BinaryOperation::Mul => {
                self.choose_mul_instruction(&node, reg_left, reg_right, reg_result)
            }
            BinaryOperation::Div => Instruction::IDiv(reg_left, reg_right, reg_result.unwrap()),
        };
        self.instructions.push(instruction);
        self.debug_spans.push(node.span);

        Ok(())
    }

    fn visit_function_def(&mut self, node: &mut FunctionDefNode) -> Result<(), CompilerError> {
        let return_address = self.instructions.len();

        let len = self.instructions.len();
        self.scopes.goto_function(&node.name);
        self.register_counter.reset();

        for parameter in &node.parameters {
            self.visit_parameter(parameter)?;
        }

        for expression in &mut node.body {
            expression.visit(self)?;
        }

        // force a final return if one isn't already there.
        if self.instructions.len() == len || *self.instructions.last().unwrap() != Instruction::Ret
        {
            // TODO: This should emit a warning unless the return type is void
            self.instructions.push(Instruction::Ret);
            self.debug_spans.push(node.span);
        }

        self.scopes.pop();

        let num_args = node.parameters.len();
        self.functions.insert(
            FunctionSymbol {
                name: node.name.clone(),
                num_args,
                num_locals: self.register_counter.get_count() - num_args,
                address: return_address,
            },
            return_address,
        );

        Ok(())
    }

    fn visit_return(&mut self, node: &mut ReturnNode) -> Result<(), CompilerError> {
        if let Some(expression) = &mut node.value {
            expression.visit(self)?;
            let copy = Instruction::RegCopy(self.current_result, Register(0));
            self.instructions.push(copy);
            self.debug_spans.push(expression.span());
        }

        self.instructions.push(Instruction::Ret);
        self.debug_spans.push(node.span);

        Ok(())
    }

    fn visit_decl(&mut self, node: &mut DeclNode) -> Result<(), CompilerError> {
        for init in &mut node.initializations {
            let _ = self.visit_var_init(init);
        }

        Ok(())
    }

    fn visit_var_init(&mut self, node: &mut VarInitNode) -> Result<(), CompilerError> {
        let current_register;
        let symbol = self.lookup_symbol(&node.name);

        let sym = symbol.unwrap_or_else(|| {
            panic!(
                "Missing symbol, that passed semantic checks: {}",
                &node.name
            )
        });

        let global = sym.is_global();

        if let Some(expression) = &mut node.value {
            expression.visit(self)?;

            current_register = self.register_counter.current();
        } else {
            // Default value to 0 when uninitialized.
            current_register = self.register_counter.next().unwrap();
        }

        if global {
            // Store the reference in the globals register.
            // Using next() "wastes" global r0, but makes it possible for us to skip a
            // bunch of conditionals.
            let dest_register = self.global_counter.next().unwrap();
            let instruction = Instruction::GStore(current_register, dest_register);

            self.instructions.push(instruction);
            self.debug_spans.push(node.span);
        }

        let current_global_register = self.global_counter.current();
        let symbol = self.lookup_symbol_mut(&node.name);

        if let Some(sym) = symbol {
            if global {
                sym.location = Some(current_global_register);
                self.global_counter.next();
            } else {
                sym.location = Some(current_register);
            }
        }

        Ok(())
    }

    fn visit_var(&mut self, node: &mut VarNode) -> Result<(), CompilerError> {
        let sym = self.lookup_var_symbol(node).unwrap();

        let sym_loc = sym.location.unwrap();

        if sym.is_global() {
            let result_register = self.register_counter.next().unwrap();
            let instruction = Instruction::GLoad(sym_loc, result_register);
            self.instructions.push(instruction);
            self.debug_spans.push(node.span);

            self.current_result = result_register;
        } else {
            self.current_result = sym_loc;
        }

        Ok(())
    }

    fn visit_assignment(&mut self, node: &mut AssignmentNode) -> Result<(), CompilerError> {
        node.lhs.visit(self)?;
        let dest = self.current_result;
        node.rhs.visit(self)?;

        let assign = Instruction::RegCopy(self.current_result, dest);

        self.instructions.push(assign);
        self.debug_spans.push(node.span);

        // Copy over globals if necessary
        if let ExpressionNode::Var(VarNode {
            name, global: true, ..
        }) = &*node.lhs
        {
            if let Some(Symbol {
                scope_id: 0,
                location: Some(register),
                ..
            }) = self.lookup_global(&name)
            {
                let store = Instruction::GStore(dest, *register);
                self.instructions.push(store);
                self.debug_spans.push(node.span);
            }
        }

        self.current_result = dest;

        Ok(())
    }

    fn visit_array(&mut self, node: &mut ArrayNode) -> Result<(), CompilerError> {
        let mut items = Vec::with_capacity(node.value.len());
        for member in &mut node.value {
            let _ = member.visit(self);
            items.push(self.current_result);
        }

        let register = self.register_counter.next().unwrap();
        self.current_result = register;
        self.instructions.push(Instruction::AConst(register, items));
        self.debug_spans.push(node.span);

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        asm::instruction::Instruction::{AConst, GLoad, GStore, IConst, IConst1, RegCopy, SConst},
        ast::{
            assignment_node::AssignmentOperation, ast_node::ASTNode,
            expression_node::ExpressionNode,
        },
        codegen::scope_walker::ScopeWalker,
        lpc_parser,
        parser::span::Span,
        semantic::lpc_type::LPCType,
    };
    use std::borrow::BorrowMut;

    #[test]
    fn test_walk_tree_populates_the_instructions() {
        let mut scope_walker = ScopeWalker::default();
        let mut walker = AsmTreeWalker::default();
        let program = "
            int main() {
                1 + 3 - 5;
                print(4 + 5);
            }
        ";
        let mut tree = lpc_parser::ProgramParser::new().parse(program).unwrap();

        let _ = scope_walker.visit_program(tree.borrow_mut());

        let scopes = ScopeTree::from(scope_walker);
        walker.scopes = scopes;

        let _ = tree.visit(&mut walker);

        let expected = vec![
            Instruction::IConst(Register(1), -1),
            Instruction::IConst(Register(2), 9),
            Instruction::RegCopy(Register(2), Register(3)),
            Instruction::Call {
                name: String::from("print"),
                num_args: 1,
                initial_arg: Register(3),
            },
            Instruction::Ret, // Automatically added
        ];

        for (idx, instruction) in walker.instructions.iter().enumerate() {
            assert_eq!(instruction, &expected[idx]);
        }
    }

    mod test_visit_call {
        use super::*;

        #[test]
        fn test_visit_call_populates_the_instructions() {
            let mut walker = AsmTreeWalker::default();
            let call = "print(4 - 5)";
            let mut tree = lpc_parser::CallParser::new().parse(call).unwrap();

            let _ = walker.visit_call(tree.borrow_mut());

            let expected = vec![
                Instruction::IConst(Register(1), -1),
                Instruction::RegCopy(Register(1), Register(2)),
                Instruction::Call {
                    name: String::from("print"),
                    num_args: 1,
                    initial_arg: Register(2),
                },
            ];

            for (idx, instruction) in walker.instructions.iter().enumerate() {
                assert_eq!(instruction, &expected[idx]);
            }
        }

        #[test]
        fn test_visit_call_populates_the_instructions_with_defaults() {
            let scope_tree = ScopeTree::default();
            let mut functions = HashMap::new();

            functions.insert(
                String::from("foo"),
                vec![None, Some(ExpressionNode::from("muffuns"))],
            );

            let mut walker = AsmTreeWalker::new(scope_tree, functions);
            let call = "foo(666)";
            let mut tree = lpc_parser::CallParser::new().parse(call).unwrap();

            let _ = walker.visit_call(tree.borrow_mut());

            let expected = vec![
                Instruction::IConst(Register(1), 666),
                Instruction::SConst(Register(2), 0),
                Instruction::RegCopy(Register(1), Register(3)),
                Instruction::RegCopy(Register(2), Register(4)),
                Instruction::Call {
                    name: "foo".to_string(),
                    num_args: 2,
                    initial_arg: Register(3),
                },
            ];

            for (idx, instruction) in walker.instructions.iter().enumerate() {
                assert_eq!(instruction, &expected[idx]);
            }
        }
    }

    #[test]
    fn test_visit_int_populates_the_instructions() {
        let mut walker = AsmTreeWalker::default();

        let mut tree = IntNode::new(666);
        let mut tree0 = IntNode::new(0);
        let mut tree1 = IntNode::new(1);

        let _ = walker.visit_int(tree.borrow_mut());
        let _ = walker.visit_int(tree0.borrow_mut());
        let _ = walker.visit_int(tree1.borrow_mut());

        let expected = vec![
            Instruction::IConst(Register(1), 666),
            Instruction::IConst0(Register(2)),
            Instruction::IConst1(Register(3)),
        ];

        for (idx, instruction) in walker.instructions.iter().enumerate() {
            assert_eq!(instruction, &expected[idx]);
        }
    }

    mod test_binary_op {
        use super::*;

        #[test]
        fn test_visit_binary_op_populates_the_instructions_for_ints() {
            let mut walker = AsmTreeWalker::default();

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

            let _ = walker.visit_binary_op(node.borrow_mut());

            let expected = vec![
                Instruction::IConst(Register(1), 666),
                Instruction::IConst(Register(2), 123),
                Instruction::IConst(Register(3), 456),
                Instruction::IAdd(Register(2), Register(3), Register(4)),
                Instruction::IMul(Register(1), Register(4), Register(5)),
            ];

            for (idx, instruction) in walker.instructions.iter().enumerate() {
                assert_eq!(instruction, &expected[idx]);
            }
        }

        #[test]
        fn test_visit_binary_op_populates_the_instructions_for_strings() {
            let mut walker = AsmTreeWalker::default();

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

            let _ = walker.visit_binary_op(node.borrow_mut());

            let expected = vec![
                Instruction::SConst(Register(1), 0),
                Instruction::SConst(Register(2), 1),
                Instruction::SConst(Register(3), 2),
                Instruction::MAdd(Register(2), Register(3), Register(4)),
                Instruction::MAdd(Register(1), Register(4), Register(5)),
            ];

            for (idx, instruction) in walker.instructions.iter().enumerate() {
                assert_eq!(instruction, &expected[idx]);
            }
        }

        #[test]
        fn test_visit_binary_op_populates_the_instructions_for_arrays() {
            let mut walker = AsmTreeWalker::default();

            let mut node = BinaryOpNode {
                l: Box::new(ExpressionNode::from(vec![ExpressionNode::from(123)])),
                r: Box::new(ExpressionNode::from(vec![ExpressionNode::from(456)])),
                op: BinaryOperation::Add,
                span: None,
            };

            let _ = walker.visit_binary_op(node.borrow_mut());

            let expected = vec![
                Instruction::IConst(Register(1), 123),
                Instruction::AConst(Register(2), vec![Register(1)]),
                Instruction::IConst(Register(3), 456),
                Instruction::AConst(Register(4), vec![Register(3)]),
                Instruction::MAdd(Register(2), Register(4), Register(5)),
            ];

            for (idx, instruction) in walker.instructions.iter().enumerate() {
                assert_eq!(instruction, &expected[idx]);
            }
        }
    }

    #[test]
    fn test_visit_string_populates_the_instructions() {
        let mut walker = AsmTreeWalker::default();
        let mut node = StringNode::new("marf");
        let mut node2 = StringNode::new("tacos");
        let mut node3 = StringNode::new("marf");

        let _ = walker.visit_string(node.borrow_mut());
        let _ = walker.visit_string(node2.borrow_mut());
        let _ = walker.visit_string(node3.borrow_mut());

        let expected = vec![
            SConst(Register(1), 0),
            SConst(Register(2), 1),
            SConst(Register(3), 0), // reuses constant
        ];

        for (a, b) in walker.instructions.iter().zip(expected) {
            assert_eq!(*a, b);
        }
    }

    #[test]
    fn test_visit_function_def_populates_the_data() {
        let mut scope_walker = ScopeWalker::default();
        let mut walker = AsmTreeWalker::default();
        let call = "int main(int i) { return i + 4; }";
        let tree = lpc_parser::DefParser::new().parse(call).unwrap();

        let mut node = if let ASTNode::FunctionDef(node) = tree {
            node
        } else {
            panic!("Didn't receive a function def?");
        };

        let _ = scope_walker.visit_function_def(node.borrow_mut());

        let mut scopes = ScopeTree::from(scope_walker);
        scopes.goto_root();
        walker.scopes = scopes;
        let _ = walker.visit_function_def(node.borrow_mut());

        let expected = vec![
            Instruction::IConst(Register(2), 4),
            Instruction::IAdd(Register(1), Register(2), Register(3)),
            Instruction::RegCopy(Register(3), Register(0)),
            Instruction::Ret,
        ];

        for (idx, instruction) in walker.instructions.iter().enumerate() {
            assert_eq!(instruction, &expected[idx]);
        }

        let address: Address = 0;

        let sym = FunctionSymbol {
            name: "main".to_string(),
            num_args: 1,
            num_locals: 2,
            address,
        };

        assert_eq!(walker.functions.get(&sym).unwrap(), &address);
    }

    #[test]
    fn visit_return_populates_the_instructions() {
        let mut walker = AsmTreeWalker::default();

        let mut node = ReturnNode::new(Some(ExpressionNode::from(IntNode::new(666))));
        let _ = walker.visit_return(node.borrow_mut());

        let expected = vec![
            Instruction::IConst(Register(1), 666),
            Instruction::RegCopy(Register(1), Register(0)),
            Instruction::Ret,
        ];

        for (idx, instruction) in walker.instructions.iter().enumerate() {
            assert_eq!(instruction, &expected[idx]);
        }

        /* === */

        let mut walker = AsmTreeWalker::default();
        let mut node = ReturnNode::new(None);
        let _ = walker.visit_return(node.borrow_mut());

        let expected = vec![Instruction::Ret];

        for (idx, instruction) in walker.instructions.iter().enumerate() {
            assert_eq!(instruction, &expected[idx]);
        }
    }

    #[test]
    fn test_decl_sets_scope_and_instructions() {
        let mut scope_walker = ScopeWalker::default();
        let call = "int foo = 1, *bar = ({ 56 })";
        let mut tree = lpc_parser::DeclParser::new().parse(call).unwrap();

        let _ = scope_walker.visit_decl(tree.borrow_mut());

        let mut walker = AsmTreeWalker::new(ScopeTree::from(scope_walker), HashMap::new());
        let _ = walker.visit_decl(tree.borrow_mut());

        let expected = vec![
            IConst1(Register(1)),
            GStore(Register(1), Register(1)),
            IConst(Register(2), 56),
            AConst(Register(3), vec![Register(2)]),
            GStore(Register(3), Register(3)),
        ];

        assert_eq!(walker.instructions, expected);

        let scope = walker.scopes.get_current().unwrap();
        assert_eq!(
            scope.lookup("foo").unwrap(),
            Symbol {
                name: String::from("foo"),
                type_: LPCType::Int(false),
                static_: false,
                location: Some(Register(1)),
                scope_id: 0,
                span: Some(Span { l: 4, r: 11 })
            }
        );
        assert_eq!(
            scope.lookup("bar").unwrap(),
            Symbol {
                name: String::from("bar"),
                type_: LPCType::Int(true),
                static_: false,
                location: Some(Register(3)),
                scope_id: 0,
                span: Some(Span { l: 13, r: 28 })
            }
        );
    }

    #[test]
    fn test_visit_var_loads_the_var_and_sets_the_result_for_globals() {
        let mut walker = AsmTreeWalker::default();
        walker.scopes.push_new();
        insert_symbol(
            walker.borrow_mut(),
            Symbol {
                name: "marf".to_string(),
                type_: LPCType::Int(false),
                static_: false,
                location: Some(Register(666)),
                scope_id: 0,
                span: None,
            },
        );
        // push a local scope with a matching variable in a different location
        walker.scopes.push_new();
        insert_symbol(
            walker.borrow_mut(),
            Symbol {
                name: "marf".to_string(),
                type_: LPCType::Int(false),
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
        };

        let _ = walker.visit_var(node.borrow_mut());
        assert_eq!(walker.current_result, Register(1)); // global loaded into r1

        let expected = vec![GLoad(Register(666), Register(1))];
        assert_eq!(walker.instructions, expected);
    }

    #[test]
    fn test_visit_var_sets_the_result_for_locals() {
        let mut walker = AsmTreeWalker::default();
        walker.scopes.push_new();
        insert_symbol(
            walker.borrow_mut(),
            // push a global marf to ensure we don't find it.
            Symbol {
                name: "marf".to_string(),
                type_: LPCType::Int(false),
                static_: false,
                location: Some(Register(444)),
                scope_id: 0,
                span: None,
            },
        );
        walker.scopes.push_new(); // push a local scope
        insert_symbol(
            walker.borrow_mut(),
            Symbol {
                name: "marf".to_string(),
                type_: LPCType::Int(false),
                static_: false,
                location: Some(Register(666)),
                scope_id: 1,
                span: None,
            },
        );

        let mut node = VarNode::new("marf");

        let _ = walker.visit_var(node.borrow_mut());
        assert_eq!(walker.current_result, Register(666));

        let expected = vec![];
        assert_eq!(walker.instructions, expected);
    }

    #[test]
    fn test_visit_assignment_populates_the_instructions_for_globals() {
        let mut walker = AsmTreeWalker::default();
        walker.scopes.push_new();
        let sym = Symbol {
            name: "marf".to_string(),
            type_: LPCType::Int(false),
            static_: false,
            location: Some(Register(666)),
            scope_id: 0,
            span: None,
        };
        insert_symbol(walker.borrow_mut(), sym);

        // push a different, local `marf`, to ensure that we don't find it for this assignment.
        walker.scopes.push_new();
        let sym = Symbol {
            name: "marf".to_string(),
            type_: LPCType::Int(false),
            static_: false,
            location: Some(Register(123)),
            scope_id: 1,
            span: None,
        };
        insert_symbol(walker.borrow_mut(), sym);

        let mut node = AssignmentNode {
            lhs: Box::new(ExpressionNode::Var(VarNode {
                name: "marf".to_string(),
                span: None,
                global: true,
            })),
            rhs: Box::new(ExpressionNode::Int(IntNode::new(-12))),
            op: AssignmentOperation::Simple,
            span: None,
        };

        let _ = walker.visit_assignment(node.borrow_mut());
        assert_eq!(
            walker.instructions,
            [
                GLoad(Register(666), Register(1)),
                IConst(Register(2), -12),
                RegCopy(Register(2), Register(1)),
                GStore(Register(1), Register(666))
            ]
        );
    }

    #[test]
    fn test_visit_assignment_populates_the_instructions_for_locals() {
        let mut walker = AsmTreeWalker::default();
        walker.scopes.push_new();
        walker.scopes.push_new();
        let sym = Symbol {
            name: "marf".to_string(),
            type_: LPCType::Int(false),
            static_: false,
            location: Some(Register(666)),
            scope_id: 1,
            span: None,
        };

        insert_symbol(walker.borrow_mut(), sym);

        let mut node = AssignmentNode {
            lhs: Box::new(ExpressionNode::Var(VarNode::new("marf"))),
            rhs: Box::new(ExpressionNode::Int(IntNode::new(-12))),
            op: AssignmentOperation::Simple,
            span: None,
        };

        let _ = walker.visit_assignment(node.borrow_mut());
        assert_eq!(
            walker.instructions,
            [
                IConst(Register(1), -12),
                RegCopy(Register(1), Register(666))
            ]
        );
    }

    #[test]
    fn test_visit_array_populates_the_instructions() {
        let mut walker = AsmTreeWalker::default();

        let mut arr = ArrayNode::new(vec![
            ExpressionNode::from(123),
            ExpressionNode::from("foo"),
            ExpressionNode::from(vec![ExpressionNode::from(666)]),
        ]);

        let _ = walker.visit_array(arr.borrow_mut());

        let expected = vec![
            Instruction::IConst(Register(1), 123),
            Instruction::SConst(Register(2), 0),
            Instruction::IConst(Register(3), 666),
            Instruction::AConst(Register(4), vec![Register(3)]),
            Instruction::AConst(Register(5), vec![Register(1), Register(2), Register(4)]),
        ];

        for (idx, instruction) in walker.instructions.iter().enumerate() {
            assert_eq!(instruction, &expected[idx]);
        }
    }

    fn insert_symbol(walker: &mut AsmTreeWalker, symbol: Symbol) {
        if let Some(node_id) = walker.scopes.current_id {
            walker.scopes.get_mut(node_id).unwrap().insert(symbol)
        }
    }
}
