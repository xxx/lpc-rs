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
use crate::codegen::local_counter_walker::LocalCounterWalker;

#[derive(Debug, Default)]
pub struct AsmTreeWalker {
    pub instructions: Vec<Instruction>,
    pub labels: HashMap<String, usize>,
    pub functions: HashMap<FunctionSymbol, usize>,
    current_result: Register, // Track where the result of a child branch is
    register_counter: RegisterCounter,
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

        let mut counter: usize = 0;
        for instruction in &self.instructions {
            if functions_by_pc.contains_key(&counter) {
                let sym = functions_by_pc.get(&counter).unwrap();
                v.push(
                    format!(
                        "fn {} num_args={} num_locals={}:",
                        sym.name,
                        sym.num_args,
                        sym.num_locals
                    )
                );
            }
            if labels_by_pc.contains_key(&counter) {
                for label in labels_by_pc.get_vec(&counter).unwrap() {
                    v.push(format!("{}:", label));
                }
            }
            v.push(format!("    {}", instruction));
            counter += 1;
        }

        v
    }
}

impl TreeWalker for AsmTreeWalker {
    fn visit_program(&mut self, program: &ProgramNode) {
        for expr in &program.functions {
            expr.visit(self);
        }
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
        let address = self.instructions.len();

        let mut counter: LocalCounterWalker = Default::default();
        counter.visit_function_def(node);

        self.functions.insert(FunctionSymbol {
            name: node.name.clone(),
            num_args: 0, // node.num_args
            num_locals: counter.count,
            address
        }, address);

        self.register_counter.reset();

        // TODO: copy args

        for expression in &node.body {
            expression.visit(self);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::mathstack_parser;
    use crate::ast::expression_node::ExpressionNode;

    #[test]
    fn test_walk_tree_populates_the_instructions() {
        let mut walker: AsmTreeWalker = Default::default();
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
        let mut walker: AsmTreeWalker = Default::default();
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
        let mut walker: AsmTreeWalker = Default::default();

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
        let mut walker: AsmTreeWalker = Default::default();

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
    fn test_visit_function_def_populates_the_instructions() {
        assert!(false);
    }
}