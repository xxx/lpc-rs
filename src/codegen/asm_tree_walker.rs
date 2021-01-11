use crate::ast::program_node::ProgramNode;
use crate::ast::int_node::IntNode;
use crate::codegen::tree_walker::TreeWalker;
use crate::ast::ast_node::ASTNodeTrait;
use crate::ast::binary_op_node::{BinaryOpNode, BinaryOperation};
use crate::asm::instruction::Instruction;
use crate::asm::register::Register;
use crate::asm::inst::iconst::IConst;
use crate::asm::inst::iconst0::IConst0;
use crate::asm::inst::iconst1::IConst1;
use crate::asm::inst::iadd::IAdd;
use crate::asm::inst::isub::ISub;
use crate::asm::inst::imul::IMul;
use crate::asm::inst::idiv::IDiv;
use crate::ast::call_node::CallNode;
use crate::asm::inst::call::Call;
use crate::asm::inst::regcopy::RegCopy;
use crate::asm::register_counter::RegisterCounter;

#[derive(Debug, Default)]
pub struct AsmTreeWalker {
    pub instructions: Vec<Instruction>,
    current_result: Register, // Tracks where the result of a child branch is
    register_counter: RegisterCounter
}

impl AsmTreeWalker {
    pub fn listing(&self) -> Vec<String> {
        let mut v = vec![];

        for instruction in &self.instructions {
            v.push(format!("{}", instruction));
        }

        v
    }
}

impl TreeWalker for AsmTreeWalker {
    fn walk_tree(&mut self, root: &impl ASTNodeTrait) {
        root.visit(self);
    }

    fn visit_program(&mut self, program: &ProgramNode) {
        for expr in &program.functions {
            self.walk_tree(&expr);
        }
    }

    fn visit_call(&mut self, node: &CallNode) {
        let mut arg_results: Vec<Register> = vec![];

        // eval args, then save each result register
        for argument in &node.arguments {
            self.walk_tree(argument);
            arg_results.push(self.current_result);
        }

        let start_register = self.register_counter.next();
        let mut register = start_register;

        // copy each result to the start of the arg register
        for result in arg_results {
            self.instructions.push(
                Instruction::RegCopy(RegCopy(result, register))
            );
            register = self.register_counter.next();
        }

        let instruction = Instruction::Call(Call {
            name: node.id.clone(),
            num_args: node.arguments.len(),
            initial_arg: start_register
        });

        self.instructions.push(instruction);
    }

    fn visit_int(&mut self, int: &IntNode) {
        let register = self.register_counter.next();
        self.current_result = register;
        let instruction = match int.value {
            0 => Instruction::IConst0(IConst0(register)),
            1 => Instruction::IConst1(IConst1(register)),
            v => Instruction::IConst(IConst(register, v))
        };
        self.instructions.push(instruction);
    }

    fn visit_binary_op(&mut self, node: &BinaryOpNode) {
        self.walk_tree(&(*node.l));
        let reg_left = self.current_result;
        self.walk_tree(&(*node.r));
        let reg_right = self.current_result;
        let reg_result = self.register_counter.next();
        self.current_result = reg_result;
        let instruction = match node.op {
            BinaryOperation::Add => Instruction::IAdd(IAdd(reg_left, reg_right, reg_result)),
            BinaryOperation::Sub => Instruction::ISub(ISub(reg_left, reg_right, reg_result)),
            BinaryOperation::Mul => Instruction::IMul(IMul(reg_left, reg_right, reg_result)),
            BinaryOperation::Div => Instruction::IDiv(IDiv(reg_left, reg_right, reg_result))
        };
        self.instructions.push(instruction);
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
            1 + 3 - 5;
            print(4 + 5);
        ";
        let tree = mathstack_parser::ProgramParser::new()
            .parse(program)
            .unwrap();

        walker.walk_tree(&tree);

        let expected = vec![
            Instruction::IConst1(IConst1(Register(1))),
            Instruction::IConst(IConst(Register(2), 3)),
            Instruction::IAdd(IAdd(Register(1), Register(2), Register(3))),
            Instruction::IConst(IConst(Register(4), 5)),
            Instruction::ISub(ISub(Register(3), Register(4),Register(5))),
            Instruction::IConst(IConst(Register(6), 4)),
            Instruction::IConst(IConst(Register(7), 5)),
            Instruction::IAdd(IAdd(Register(6), Register(7), Register(8))),
            Instruction::RegCopy(RegCopy(Register(8), Register(9))),
            Instruction::Call(Call {
                name: String::from("print"),
                num_args: 1,
                initial_arg: Register(9)
            })
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
            Instruction::IConst(IConst(Register(1), 4)),
            Instruction::IConst(IConst(Register(2), 5)),
            Instruction::IAdd(IAdd(Register(1), Register(2), Register(3))),
            Instruction::RegCopy(RegCopy(Register(3), Register(4))),
            Instruction::Call(Call {
                name: String::from("print"),
                num_args: 1,
                initial_arg: Register(4)
            })
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
            Instruction::IConst(IConst(Register(1), 666)),
            Instruction::IConst0(IConst0(Register(2))),
            Instruction::IConst1(IConst1(Register(3))),
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
            Instruction::IConst(IConst(Register(1), 666)),
            Instruction::IConst(IConst(Register(2), 123)),
            Instruction::IConst(IConst(Register(3), 456)),
            Instruction::IAdd(IAdd(Register(2), Register(3), Register(4))),
            Instruction::IMul(IMul(Register(1), Register(4), Register(5)))
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