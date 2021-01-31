use std::collections::HashMap;
use multimap::MultiMap;
use tree_walker::TreeWalker;
use crate::ast::program_node::ProgramNode;
use crate::ast::int_node::IntNode;
use crate::codegen::tree_walker;
use crate::ast::ast_node::ASTNodeTrait;
use crate::ast::binary_op_node::{BinaryOpNode, BinaryOperation};
use crate::asm::instruction::Instruction;
use crate::asm::register::Register;
use crate::ast::call_node::CallNode;
use crate::asm::register_counter::RegisterCounter;
use crate::ast::function_def_node::FunctionDefNode;
use crate::interpreter::function_symbol::FunctionSymbol;
use crate::ast::return_node::ReturnNode;
use crate::semantic::scope_tree::ScopeTree;
use crate::ast::decl_node::DeclNode;
use crate::semantic::symbol::Symbol;
use crate::ast::var_init_node::VarInitNode;
use crate::ast::var_node::VarNode;
use crate::ast::assignment_node::AssignmentNode;
use crate::ast::string_node::StringNode;
use crate::ast::expression_node::ExpressionNode;
use crate::semantic::lpc_type::LPCVarType;
use crate::interpreter::program::Program;
use crate::interpreter::constant_pool::ConstantPool;
use crate::errors::CompilerError;

/// Really just a `pc` index in the vm.
type Address = usize;

/// A tree walker that generates assembly language instructions based on an AST.
#[derive(Debug, Default)]
pub struct AsmTreeWalker {
    /// The vector of instructions generated by this walker
    pub instructions: Vec<Instruction>,

    /// The map of labels, to their respective addresses
    pub labels: HashMap<String, Address>,

    /// The map of function Symbols, to their respective addresses
    pub functions: HashMap<FunctionSymbol, Address>,

    /// Track where the result of a child branch is
    current_result: Register,

    /// The internal counter to track which registers are used.
    register_counter: RegisterCounter,

    /// The collection of scopes
    scopes: ScopeTree
}

impl AsmTreeWalker {
    /// Create a new `AsmTreeWalker` that consumes the passed scopes
    ///
    /// # Arguments
    /// `scopes` - The ScopeTree to use to resolve symbols and function calls.
    pub fn new(scopes: ScopeTree) -> Self {
        Self {
            scopes,
            ..Default::default()
        }
    }

    /// Get a listing of a translated AST, suitable for printing
    ///
    /// # Examples
    /// ```
    /// use lpc_rs::ast::binary_op_node::{BinaryOpNode, BinaryOperation};
    /// use lpc_rs::ast::int_node::IntNode;
    /// use lpc_rs::ast::expression_node::ExpressionNode;
    /// use lpc_rs::codegen::asm_tree_walker::AsmTreeWalker;
    /// use lpc_rs::codegen::tree_walker::TreeWalker;
    /// let node = BinaryOpNode {
    ///     l: Box::new(ExpressionNode::Int(IntNode::new(123))),
    ///     r: Box::new(ExpressionNode::Int(IntNode::new(456))),
    ///     op: BinaryOperation::Sub,
    ///     span: None
    /// };
    /// let mut walker = AsmTreeWalker::default();
    /// walker.visit_binary_op(&node);
    /// for instruction in walker.listing() {
    ///     println!("{}", instruction);
    /// }
    /// ```
    pub fn listing(&self) -> Vec<String> {
        let mut v = vec![];

        // invert these maps for by-address lookup
        let functions_by_pc =
            self.functions.values().zip(self.functions.keys()).collect::<HashMap<_, _>>();

        // use MultiMap as multiple labels can be at the same address
        let labels_by_pc =
            self.labels.values().zip(self.labels.keys()).collect::<MultiMap<_, _>>();

        for (counter, instruction) in self.instructions.iter().enumerate() {
            if let Some(sym) = functions_by_pc.get(&counter) {
                v.push(
                    format!(
                        "fn {} num_args={} num_locals={}:",
                        sym.name,
                        sym.num_args,
                        sym.num_locals
                    )
                );
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

    /// Convert this walkers data into a Program
    pub fn to_program(&self) -> Program {
        Program {
            instructions: self.instructions.to_vec(),
            labels: self.labels.clone(),
            functions: self.function_map(),
            constants: ConstantPool::default()
        }
    }

    /// Get a reference to a symbol in the current scope
    fn lookup_symbol(&self, name: &str) -> Option<&Symbol> {
        if let Some(node_id) = self.scopes.current_id {
            self.scopes.get(node_id).unwrap().lookup(name)
        } else {
            None
        }
    }

    /// Get a mutable reference to a symbol in the current scope
    fn lookup_symbol_mut(&mut self, name: &str) -> Option<&mut Symbol> {
        if let Some(node_id) = self.scopes.current_id {
            self.scopes.get_mut(node_id).unwrap().lookup_mut(name)
        } else {
            None
        }
    }
}

impl TreeWalker for AsmTreeWalker {
    fn visit_program(&mut self, program: &ProgramNode) -> Result<(), CompilerError> {
        self.scopes.goto_root();
        for expr in &program.functions {
            expr.visit(self)?
        }
        self.scopes.pop();

        Ok(())
    }

    fn visit_call(&mut self, node: &CallNode) -> Result<(), CompilerError> {
        let mut arg_results: Vec<Register> = vec![];

        // eval args, then save each result register
        for argument in &node.arguments {
            argument.visit(self)?;
            arg_results.push(self.current_result);
        }

        let start_register = self.register_counter.next();
        let mut register = start_register;

        // copy each result to the start of the arg register
        for result in arg_results {
            self.instructions.push(
                Instruction::RegCopy(result, register.unwrap())
            );
            register = self.register_counter.next();
        }

        // Undo the final call to .next() to avoid skipping a register
        self.register_counter.go_back();

        let instruction = Instruction::Call {
            name: node.name.clone(),
            num_args: node.arguments.len(),
            initial_arg: start_register.unwrap()
        };

        self.instructions.push(instruction);
        self.current_result = Register(0); // returned results are in r0

        Ok(())
    }

    fn visit_int(&mut self, node: &IntNode) -> Result<(), CompilerError> {
        let register = self.register_counter.next();
        self.current_result = register.unwrap();
        let instruction = match node.value {
            0 => Instruction::IConst0(register.unwrap()),
            1 => Instruction::IConst1(register.unwrap()),
            v => Instruction::IConst(register.unwrap(), v)
        };
        self.instructions.push(instruction);

        Ok(())
    }

    fn visit_string(&mut self, node: &StringNode) -> Result<(), CompilerError> {
        let register = self.register_counter.next().unwrap();
        self.current_result = register;
        self.instructions.push(Instruction::SConst(register, node.value.clone()));

        Ok(())
    }

    fn visit_binary_op(&mut self, node: &BinaryOpNode) -> Result<(), CompilerError> {
        node.l.visit(self)?;
        let reg_left = self.current_result;

        node.r.visit(self)?;
        let reg_right = self.current_result;

        let reg_result = self.register_counter.next();
        self.current_result = reg_result.unwrap();

        /// Allows for recursive determination of typed add instructions
        fn add_instruction_picker(
            node: &ExpressionNode,
            walker: &AsmTreeWalker,
            reg_left: Register,
            reg_right: Register,
            reg_result: Option<Register>
        ) -> Instruction {
            match node {
                ExpressionNode::BinaryOp(bin_op) =>
                    add_instruction_picker(&bin_op.l, walker, reg_left, reg_right, reg_result),
                ExpressionNode::Var(var_node) => {
                    let type_ = walker.lookup_symbol(&var_node.name).unwrap().type_;
                    match type_ {
                        LPCVarType::String =>
                            Instruction::SAdd(reg_left, reg_right, reg_result.unwrap()),
                        _ => Instruction::IAdd(reg_left, reg_right, reg_result.unwrap())
                    }
                }
                ExpressionNode::String(_) =>
                    Instruction::SAdd(reg_left, reg_right, reg_result.unwrap()),
                _ => Instruction::IAdd(reg_left, reg_right, reg_result.unwrap())
            }
        };

        /// Allows for recursive determination of typed mul instructions
        fn mul_instruction_picker(
            node: &ExpressionNode,
            walker: &AsmTreeWalker,
            reg_left: Register,
            reg_right: Register,
            reg_result: Option<Register>
        ) -> Instruction {
            match node {
                ExpressionNode::BinaryOp(bin_op) =>
                    mul_instruction_picker(&bin_op.l, walker, reg_left, reg_right, reg_result),
                ExpressionNode::Var(var_node) => {
                    let type_ = walker.lookup_symbol(&var_node.name).unwrap().type_;
                    match type_ {
                        LPCVarType::String =>
                            Instruction::SMul(reg_left, reg_right, reg_result.unwrap()),
                        _ => Instruction::IMul(reg_left, reg_right, reg_result.unwrap())
                    }
                }
                ExpressionNode::String(_) =>
                    Instruction::SMul(reg_left, reg_right, reg_result.unwrap()),
                _ => Instruction::IMul(reg_left, reg_right, reg_result.unwrap())
            }
        };

        let instruction = match node.op {
            BinaryOperation::Add =>
                add_instruction_picker(&*node.l, self, reg_left, reg_right, reg_result),
            BinaryOperation::Sub => Instruction::ISub(reg_left, reg_right, reg_result.unwrap()),
            BinaryOperation::Mul =>
                mul_instruction_picker(&*node.l, self, reg_left, reg_right, reg_result),
            BinaryOperation::Div => Instruction::IDiv(reg_left, reg_right, reg_result.unwrap())
        };
        self.instructions.push(instruction);

        Ok(())
    }

    fn visit_function_def(&mut self, node: &FunctionDefNode) -> Result<(), CompilerError> {
        let return_address = self.instructions.len();

        let len = self.instructions.len();
        self.scopes.next();
        self.register_counter.reset();

        for parameter in &node.parameters {
            parameter.visit(self)?;
        }

        for expression in &node.body {
            expression.visit(self)?;
        }

        // force a final return if one isn't already there.
        if self.instructions.len() == len || *self.instructions.last().unwrap() != Instruction::Ret {
            // TODO: This should emit a warning
            self.instructions.push(Instruction::Ret);
        }

        self.scopes.pop();

        let num_args = node.parameters.len();
        self.functions.insert(FunctionSymbol {
            name: node.name.clone(),
            num_args,
            num_locals: self.register_counter.get_count() - num_args,
            address: return_address
        }, return_address);

        Ok(())
    }

    fn visit_return(&mut self, node: &ReturnNode) -> Result<(), CompilerError> {
        if let Some(expression) = &node.value {
            expression.visit(self)?;
            let copy = Instruction::RegCopy(self.current_result, Register(0));
            self.instructions.push(copy);
        }

        self.instructions.push(Instruction::Ret);

        Ok(())
    }

    fn visit_decl(&mut self, node: &DeclNode) -> Result<(), CompilerError> {
        for init in &node.initializations {
            self.visit_var_init(&init).unwrap();
        }

        Ok(())
    }

    fn visit_var_init(&mut self, node: &VarInitNode) -> Result<(), CompilerError> {
        let current_register;

        if let Some(expression) = &node.value {
            expression.visit(self)?;
            current_register = self.register_counter.current();
        } else {
            // Default value to 0 when uninitialized.
            current_register = self.register_counter.next().unwrap();
        }

        let symbol = self.lookup_symbol_mut(&node.name);
        if let Some(sym) = symbol {
            sym.location = Some(current_register);
        }

        Ok(())
    }

    fn visit_var(&mut self, node: &VarNode) -> Result<(), CompilerError> {
        let sym = self.lookup_symbol(&node.name);
        self.current_result = sym.unwrap().location.unwrap();

        Ok(())
    }

    fn visit_assignment(&mut self, node: &AssignmentNode) -> Result<(), CompilerError> {
        node.lhs.visit(self)?;
        let dest = self.current_result;
        node.rhs.visit(self)?;

        let assign = Instruction::RegCopy(self.current_result, dest);
        self.instructions.push(assign);
        self.current_result = dest;

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lpc_parser;
    use crate::ast::expression_node::ExpressionNode;
    use crate::asm::instruction::Instruction::{IConst1, IConst, RegCopy};
    use crate::semantic::lpc_type::LPCVarType;
    use crate::ast::assignment_node::AssignmentOperation;
    use crate::parser::span::Span;
    use crate::codegen::scope_walker::ScopeWalker;
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
        let tree = lpc_parser::ProgramParser::new()
            .parse(program)
            .unwrap();

        scope_walker.visit_program(&tree).unwrap();

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
                initial_arg: Register(3)
            },
            Instruction::Ret // Automatically added
        ];

        for (idx, instruction) in walker.instructions.iter().enumerate() {
            assert_eq!(instruction, &expected[idx]);
        }
    }

    #[test]
    fn test_visit_call_populates_the_instructions() {
        let mut walker = AsmTreeWalker::default();
        let call = "print(4 - 5)";
        let tree = lpc_parser::CallParser::new()
            .parse(call)
            .unwrap();

        walker.visit_call(&tree).unwrap();

        let expected = vec![
            Instruction::IConst(Register(1), -1),
            Instruction::RegCopy(Register(1), Register(2)),
            Instruction::Call {
                name: String::from("print"),
                num_args: 1,
                initial_arg: Register(2)
            }
        ];

        for (idx, instruction) in walker.instructions.iter().enumerate() {
            assert_eq!(instruction, &expected[idx]);
        }
    }

    #[test]
    fn test_visit_int_populates_the_instructions() {
        let mut walker = AsmTreeWalker::default();

        let tree = IntNode::new(666);
        let tree0 = IntNode::new(0);
        let tree1 = IntNode::new(1);

        walker.visit_int(&tree).unwrap();
        walker.visit_int(&tree0).unwrap();
        walker.visit_int(&tree1).unwrap();

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

            let node = BinaryOpNode {
                l: Box::new(ExpressionNode::Int(IntNode::new(666))),
                r: Box::new(ExpressionNode::BinaryOp(BinaryOpNode {
                    l: Box::new(ExpressionNode::Int(IntNode::new(123))),
                    r: Box::new(ExpressionNode::Int(IntNode::new(456))),
                    op: BinaryOperation::Add,
                    span: None
                })),
                op: BinaryOperation::Mul,
                span: None
            };

            walker.visit_binary_op(&node).unwrap();

            let expected = vec![
                Instruction::IConst(Register(1), 666),
                Instruction::IConst(Register(2), 123),
                Instruction::IConst(Register(3), 456),
                Instruction::IAdd(Register(2), Register(3), Register(4)),
                Instruction::IMul(Register(1), Register(4), Register(5))
            ];

            for (idx, instruction) in walker.instructions.iter().enumerate() {
                assert_eq!(instruction, &expected[idx]);
            }
        }

        #[test]
        fn test_visit_binary_op_populates_the_instructions_for_strings() {
            let mut walker = AsmTreeWalker::default();

            let node = BinaryOpNode {
                l: Box::new(ExpressionNode::String(StringNode::new("foo"))),
                r: Box::new(ExpressionNode::BinaryOp(BinaryOpNode {
                    l: Box::new(ExpressionNode::String(StringNode::new("bar"))),
                    r: Box::new(ExpressionNode::String(StringNode::new("baz"))),
                    op: BinaryOperation::Add,
                    span: None
                })),
                op: BinaryOperation::Add,
                span: None
            };

            walker.visit_binary_op(&node).unwrap();

            let expected = vec![
                Instruction::SConst(Register(1), "foo".to_string()),
                Instruction::SConst(Register(2), "bar".to_string()),
                Instruction::SConst(Register(3), "baz".to_string()),
                Instruction::SAdd(Register(2), Register(3), Register(4)),
                Instruction::SAdd(Register(1), Register(4), Register(5))
            ];

            for (idx, instruction) in walker.instructions.iter().enumerate() {
                assert_eq!(instruction, &expected[idx]);
            }
        }
    }

    #[test]
    fn test_visit_function_def_populates_the_data() {
        let mut scope_walker = ScopeWalker::default();
        let mut walker = AsmTreeWalker::default();
        let call = "int main() { 4 + 2 - 5 * 2; }";
        let tree = lpc_parser::FunctionDefParser::new()
            .parse(call)
            .unwrap();

        scope_walker.visit_function_def(&tree).unwrap();

        let mut scopes = ScopeTree::from(scope_walker);
        scopes.goto_root();
        walker.scopes = scopes;
        walker.visit_function_def(&tree).unwrap();

        let expected = vec![
            Instruction::IConst(Register(1), -4),
            Instruction::Ret
        ];

        for (idx, instruction) in walker.instructions.iter().enumerate() {
            assert_eq!(instruction, &expected[idx]);
        }

        let address: Address = 0;

        let sym = FunctionSymbol {
            name: "main".to_string(),
            num_args: 0,
            num_locals: 1,
            address
        };

        assert_eq!(walker.functions.get(&sym).unwrap(), &address);
    }

    #[test]
    fn visit_return_populates_the_instructions() {
        let mut walker = AsmTreeWalker::default();

        let node = ReturnNode::new(Some(ExpressionNode::from(IntNode::new(666))));
        walker.visit_return(&node).unwrap();

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
        let node = ReturnNode::new(None);
        walker.visit_return(&node).unwrap();

        let expected = vec![
            Instruction::Ret,
        ];

        for (idx, instruction) in walker.instructions.iter().enumerate() {
            assert_eq!(instruction, &expected[idx]);
        }
    }

    #[test]
    fn test_decl_sets_scope_and_instructions() {
        let mut scope_walker = ScopeWalker::default();
        let call = "int foo = 1, *bar = 56";
        let tree = lpc_parser::DeclParser::new()
            .parse(call)
            .unwrap();

        scope_walker.visit_decl(&tree).unwrap();

        let mut walker = AsmTreeWalker::new(ScopeTree::from(scope_walker));
        walker.visit_decl(&tree).unwrap();

        let expected = vec![
            IConst1(Register(1)),
            IConst(Register(2), 56)
        ];

        for (idx, instruction) in walker.instructions.iter().enumerate() {
            assert_eq!(instruction, &expected[idx]);
        }

        let scope = walker.scopes.get_current().unwrap();
        assert_eq!(scope.lookup("foo").unwrap(), Symbol {
            name: String::from("foo"),
            type_: LPCVarType::Int,
            array: false,
            static_: false,
            location: Some(Register(1)),
            scope_id: 0,
            span: Some(Span { l: 4, r: 11 })
        });
        assert_eq!(scope.lookup("bar").unwrap(), Symbol {
            name: String::from("bar"),
            type_: LPCVarType::Int,
            array: true,
            static_: false,
            location: Some(Register(2)),
            scope_id: 0,
            span: Some(Span { l: 13, r: 22 })
        });
    }

    #[test]
    fn test_visit_var_sets_the_result() {
        let mut walker = AsmTreeWalker::default();
        walker.scopes.push_new();
        insert_symbol(walker.borrow_mut(), Symbol {
            name: "marf".to_string(),
            type_: LPCVarType::Int,
            array: false,
            static_: false,
            location: Some(Register(666)),
            scope_id: 0,
            span: None
        });

        let node = VarNode {
            name: "marf".to_string()
        };

        walker.visit_var(&node).unwrap();
        assert_eq!(walker.current_result, Register(666));
    }

    #[test]
    fn test_visit_assignment_populates_the_instructions() {
        let mut walker = AsmTreeWalker::default();
        walker.scopes.push_new();
        insert_symbol(walker.borrow_mut(), Symbol {
            name: "marf".to_string(),
            type_: LPCVarType::Int,
            array: false,
            static_: false,
            location: Some(Register(666)),
            scope_id: 0,
            span: None
        });

        let node = AssignmentNode {
            lhs: Box::new(ExpressionNode::Var(VarNode {
                name: "marf".to_string()
            })),
            rhs: Box::new(ExpressionNode::Int(IntNode {
                value: -12
            })),
            op: AssignmentOperation::Simple,
            span: None
        };

        walker.visit_assignment(&node).unwrap();
        assert_eq!(walker.instructions, [
            IConst(Register(1), -12),
            RegCopy(Register(1), Register(666))
        ]);
    }

    fn insert_symbol(walker: &mut AsmTreeWalker, symbol: Symbol) {
        if let Some(node_id) = walker.scopes.current_id {
            walker.scopes.get_mut(node_id).unwrap().insert(symbol)
        }
    }
}