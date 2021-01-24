use std::collections::HashMap;
use multimap::MultiMap;
use crate::ast::program_node::ProgramNode;
use crate::ast::int_node::IntNode;
use crate::codegen::tree_walker;
use tree_walker::TreeWalker;
use crate::ast::ast_node::ASTNodeTrait;
use crate::ast::binary_op_node::{BinaryOpNode, BinaryOperation};
use crate::asm::instruction::Instruction;
use crate::asm::register::Register;
use crate::ast::call_node::CallNode;
use crate::asm::register_counter::RegisterCounter;
use crate::ast::function_def_node::FunctionDefNode;
use crate::interpreter::function_symbol::FunctionSymbol;
use crate::ast::return_node::ReturnNode;
use crate::semantic::scope_collection::ScopeCollection;
use crate::ast::decl_node::DeclNode;
use crate::semantic::symbol::Symbol;
use crate::ast::var_init_node::VarInitNode;
use crate::ast::var_node::VarNode;

#[derive(Debug, Default)]
pub struct AsmTreeWalker {
    pub instructions: Vec<Instruction>,
    pub labels: HashMap<String, usize>,
    pub functions: HashMap<FunctionSymbol, usize>,
    current_result: Register, // Track where the result of a child branch is
    register_counter: RegisterCounter,
    scopes: ScopeCollection
}

impl AsmTreeWalker {
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

    /// Return a map of all labels to addresses, both "normal" and function defs
    pub fn combined_labels(&self) -> HashMap<String, usize> {
        let mut map = self.labels.clone();

        for (sym, address) in &self.functions {
            map.insert(sym.name.clone(), *address);
        }

        map
    }

    pub fn function_map(&self) -> HashMap<String, FunctionSymbol> {
        let mut map = HashMap::new();

        for sym in self.functions.keys() {
            map.insert(sym.name.clone(), sym.clone());
        }

        map
    }

    fn insert_symbol(&mut self, symbol: Symbol) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(symbol)
        }
    }

    fn lookup_symbol(&self, name: &str) -> Option<&Symbol> {
        if let Some(scope) = self.scopes.last() {
            scope.lookup(name)
        } else {
            None
        }
    }

    fn lookup_symbol_mut(&mut self, name: &str) -> Option<&mut Symbol> {
        if let Some(scope) = self.scopes.last_mut() {
            scope.lookup_mut(name)
        } else {
            None
        }
    }
}

impl TreeWalker for AsmTreeWalker {
    fn visit_program(&mut self, program: &ProgramNode) {
        let _global_scope = self.scopes.push_new();
        for expr in &program.functions {
            expr.visit(self);
        }
        self.scopes.pop();
    }

    fn visit_call(&mut self, node: &CallNode) {
        let mut arg_results: Vec<Register> = vec![];

        // eval args, then save each result register
        for argument in &node.arguments {
            argument.visit(self);
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
            name: node.id.clone(),
            num_args: node.arguments.len(),
            initial_arg: start_register.unwrap()
        };

        self.instructions.push(instruction);
    }

    fn visit_int(&mut self, int: &IntNode) {
        let register = self.register_counter.next();
        self.current_result = register.unwrap();
        let instruction = match int.value {
            0 => Instruction::IConst0(register.unwrap()),
            1 => Instruction::IConst1(register.unwrap()),
            v => Instruction::IConst(register.unwrap(), v)
        };
        self.instructions.push(instruction);
    }

    fn visit_binary_op(&mut self, node: &BinaryOpNode) {
        node.l.visit(self);
        let reg_left = self.current_result;
        node.r.visit(self);
        let reg_right = self.current_result;
        let reg_result = self.register_counter.next();
        self.current_result = reg_result.unwrap();
        let instruction = match node.op {
            BinaryOperation::Add => Instruction::IAdd(reg_left, reg_right, reg_result.unwrap()),
            BinaryOperation::Sub => Instruction::ISub(reg_left, reg_right, reg_result.unwrap()),
            BinaryOperation::Mul => Instruction::IMul(reg_left, reg_right, reg_result.unwrap()),
            BinaryOperation::Div => Instruction::IDiv(reg_left, reg_right, reg_result.unwrap())
        };
        self.instructions.push(instruction);
    }

    fn visit_function_def(&mut self, node: &FunctionDefNode) {
        let return_address = self.instructions.len();

        let _scope = self.scopes.push_new();
        self.register_counter.reset();

        for expression in &node.body {
            expression.visit(self);
        }

        self.scopes.pop();

        self.functions.insert(FunctionSymbol {
            name: node.name.clone(),
            num_args: 0, // node.num_args
            num_locals: self.register_counter.get_count(),
            address: return_address
        }, return_address);
    }

    fn visit_return(&mut self, node: &ReturnNode) {
        if let Some(expression) = &node.value {
            expression.visit(self);
            let copy = Instruction::RegCopy(self.current_result, Register(0));
            self.instructions.push(copy);
        }

        self.instructions.push(Instruction::Ret);
    }

    fn visit_decl(&mut self, node: &DeclNode) {
        for init in &node.initializations {
            self.visit_var_init(&init);
        }
    }

    fn visit_var_init(&mut self, node: &VarInitNode) {
        self.insert_symbol(Symbol::from(node));

        if let Some(expression) = &node.value {
            expression.visit(self);
            let current_register = self.register_counter.value();
            let symbol = self.lookup_symbol_mut(&node.name);
            if let Some(sym) = symbol {
                sym.location = Some(current_register);
            }
        }
    }

    fn visit_var(&mut self, node: &VarNode) {
        let sym = self.lookup_symbol(&node.value);
        self.current_result = sym.unwrap().location.unwrap();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::mathstack_parser;
    use crate::ast::expression_node::ExpressionNode;
    use crate::asm::instruction::Instruction::{IConst1, IConst};
    use crate::semantic::lpc_type::LPCVarType;

    #[test]
    fn test_walk_tree_populates_the_instructions() {
        let mut walker = AsmTreeWalker::default();
        let program = "
            int main() {
                1 + 3 - 5;
                print(4 + 5);
            }
        ";
        let tree = mathstack_parser::ProgramParser::new()
            .parse(program)
            .unwrap();

        tree.visit(&mut walker);

        let expected = vec![
            Instruction::IConst1(Register(1)),
            Instruction::IConst(Register(2), 3),
            Instruction::IAdd(Register(1), Register(2), Register(3)),
            Instruction::IConst(Register(4), 5),
            Instruction::ISub(Register(3), Register(4),Register(5)),
            Instruction::IConst(Register(6), 4),
            Instruction::IConst(Register(7), 5),
            Instruction::IAdd(Register(6), Register(7), Register(8)),
            Instruction::RegCopy(Register(8), Register(9)),
            Instruction::Call {
                name: String::from("print"),
                num_args: 1,
                initial_arg: Register(9)
            }
        ];

        for (idx, instruction) in walker.instructions.iter().enumerate() {
            assert_eq!(instruction, &expected[idx]);
        }
    }

    #[test]
    fn test_visit_call_populates_the_instructions() {
        let mut walker = AsmTreeWalker::default();
        let call = "print(4 + 5)";
        let tree = mathstack_parser::CallParser::new()
            .parse(call)
            .unwrap();

        walker.visit_call(&tree);

        let expected = vec![
            Instruction::IConst(Register(1), 4),
            Instruction::IConst(Register(2), 5),
            Instruction::IAdd(Register(1), Register(2), Register(3)),
            Instruction::RegCopy(Register(3), Register(4)),
            Instruction::Call {
                name: String::from("print"),
                num_args: 1,
                initial_arg: Register(4)
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

        walker.visit_int(&tree);
        walker.visit_int(&tree0);
        walker.visit_int(&tree1);

        let expected = vec![
            Instruction::IConst(Register(1), 666),
            Instruction::IConst0(Register(2)),
            Instruction::IConst1(Register(3)),
        ];

        for (idx, instruction) in walker.instructions.iter().enumerate() {
            assert_eq!(instruction, &expected[idx]);
        }
    }

    #[test]
    fn test_visit_binary_op_populates_the_instructions() {
        let mut walker = AsmTreeWalker::default();

        let node = BinaryOpNode {
            l: Box::new(ExpressionNode::Int(IntNode::new(666))),
            r: Box::new(ExpressionNode::BinaryOp(BinaryOpNode {
                l: Box::new(ExpressionNode::Int(IntNode::new(123))),
                r: Box::new(ExpressionNode::Int(IntNode::new(456))),
                op: BinaryOperation::Add
            })),
            op: BinaryOperation::Mul
        };

        walker.visit_binary_op(&node);

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
    fn test_visit_function_def_populates_the_data() {
        let mut walker = AsmTreeWalker::default();
        let call = "int main() { 4 + 2 - 5 * 2; }";
        let tree = mathstack_parser::FunctionDefParser::new()
            .parse(call)
            .unwrap();

        walker.visit_function_def(&tree);

        let expected = vec![
            Instruction::IConst(Register(1), 4),
            Instruction::IConst(Register(2), 2),
            Instruction::IAdd(Register(1), Register(2), Register(3)),
            Instruction::IConst(Register(4), 5),
            Instruction::IConst(Register(5), 2),
            Instruction::IMul(Register(4), Register(5), Register(6)),
            Instruction::ISub(Register(3), Register(6), Register(7))
        ];

        for (idx, instruction) in walker.instructions.iter().enumerate() {
            assert_eq!(instruction, &expected[idx]);
        }

        let address: usize = 0;

        let sym = FunctionSymbol {
            name: "main".to_string(),
            num_args: 0,
            num_locals: 7,
            address
        };

        assert_eq!(walker.functions.get(&sym).unwrap(), &address);
    }

    #[test]
    fn visit_return_populates_the_instructions() {
        let mut walker = AsmTreeWalker::default();

        let node = ReturnNode::new(Some(ExpressionNode::from(IntNode::new(666))));
        walker.visit_return(&node);

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
        walker.visit_return(&node);

        let expected = vec![
            Instruction::Ret,
        ];

        for (idx, instruction) in walker.instructions.iter().enumerate() {
            assert_eq!(instruction, &expected[idx]);
        }
    }

    #[test]
    fn test_decl_sets_scope_and_instructions() {
        let mut walker = AsmTreeWalker::default();
        walker.scopes.push_new(); // we need a scope to test vars are set correctly
        let call = "int foo = 1, *bar = 56";
        let tree = mathstack_parser::DeclParser::new()
            .parse(call)
            .unwrap();

        walker.visit_decl(&tree);

        let expected = vec![
            IConst1(Register(1)),
            IConst(Register(2), 56)
        ];

        for (idx, instruction) in walker.instructions.iter().enumerate() {
            assert_eq!(instruction, &expected[idx]);
        }

        let scope = walker.scopes.last().unwrap();
        assert_eq!(scope.lookup("foo").unwrap(), Symbol {
            name: String::from("foo"),
            type_: LPCVarType::Int,
            array: false,
            static_: false,
            location: Some(Register(1)),
            scope_id: 0
        });
        assert_eq!(scope.lookup("bar").unwrap(), Symbol {
            name: String::from("bar"),
            type_: LPCVarType::Int,
            array: true,
            static_: false,
            location: Some(Register(2)),
            scope_id: 0
        });
    }

    #[test]
    fn test_visit_var_sets_the_result() {
        let mut walker = AsmTreeWalker::default();
        walker.scopes.push_new();
        walker.insert_symbol(Symbol {
            name: "marf".to_string(),
            type_: LPCVarType::Int,
            array: false,
            static_: false,
            location: Some(Register(666)),
            scope_id: 0
        });

        let node = VarNode {
            value: "marf".to_string()
        };

        walker.visit_var(&node);
        assert_eq!(walker.current_result, Register(666));
    }
}