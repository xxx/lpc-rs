use crate::ast::program_node::ProgramNode;
use crate::ast::int_node::IntNode;
use crate::codegen::tree_walker::TreeWalker;
use crate::ast::ast_node::ASTNodeTrait;
use crate::ast::binary_op_node::{BinaryOpNode, BinaryOperation};
use crate::asm::instruction::Instruction;
use crate::asm::register::Register;
use crate::asm::register_counter;
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

#[derive(Debug)]
pub struct AsmTreeWalker {
    pub instructions: Vec<Instruction>,
    current_result: Register // Tracks where the result of a child branch is
}

impl AsmTreeWalker {
    pub fn new() -> Self {
        Self {
            instructions: vec![],
            current_result: Register(0)
        }
    }

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
        for expr in &program.expressions {
            self.walk_tree(&expr);
        }
    }

    fn visit_int(&mut self, int: &IntNode) {
        let register = register_counter::next();
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
        let reg_result = register_counter::next();
        self.current_result = reg_result;
        let instruction = match node.op {
            BinaryOperation::Add => Instruction::IAdd(IAdd(reg_left, reg_right, reg_result)),
            BinaryOperation::Sub => Instruction::ISub(ISub(reg_left, reg_right, reg_result)),
            BinaryOperation::Mul => Instruction::IMul(IMul(reg_left, reg_right, reg_result)),
            BinaryOperation::Div => Instruction::IDiv(IDiv(reg_left, reg_right, reg_result))
        };
        self.instructions.push(instruction);
    }

    fn visit_call(&mut self, node: &CallNode) {
        let mut arg_results: Vec<Register> = vec![];

        // eval args, then save each result register
        for argument in &node.arguments {
            self.walk_tree(argument);
            arg_results.push(self.current_result);
        }

        let start_register = register_counter::next();
        let mut register = start_register;

        // copy each result to the start of the arg register
        for result in arg_results {
            self.instructions.push(
                Instruction::RegCopy(RegCopy(result, register))
            );
            register = register_counter::next();
        }

        let instruction = Instruction::Call(Call {
            name: node.id.clone(),
            num_args: node.arguments.len(),
            initial_arg: start_register
        });

        self.instructions.push(instruction);
    }
}
