use crate::asm::inst::call::Call;
use crate::asm::inst::iadd::IAdd;
use crate::asm::inst::iconst::IConst;
use crate::asm::inst::iconst0::IConst0;
use crate::asm::inst::iconst1::IConst1;
use crate::asm::inst::idiv::IDiv;
use crate::asm::inst::iload::ILoad;
use crate::asm::inst::imul::IMul;
use crate::asm::inst::istore::IStore;
use crate::asm::inst::isub::ISub;
use crate::asm::inst::print::Print;
use crate::asm::inst::regcopy::RegCopy;
use std::fmt::{Formatter,Display};
use std::fmt;

pub trait InstructionTrait: Display {}

#[derive(Debug, Clone)]
pub enum Instruction {
    Call(Call),
    IAdd(IAdd),
    IConst(IConst),
    IConst0(IConst0),
    IConst1(IConst1),
    IDiv(IDiv),
    ILoad(ILoad),
    IMul(IMul),
    IStore(IStore),
    ISub(ISub),
    Print(Print),
    RegCopy(RegCopy)
}

macro_rules! display_trait {
    ( $( $x:path ),+ ) => {
        impl Display for Instruction {
            fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
                match self {
                $(
                    $x(y) => write!(f, "{}", y),
                )*
                }
            }
        }
    };
}

display_trait!(
    Instruction::Call,
    Instruction::IAdd,
    Instruction::IConst,
    Instruction::IConst0,
    Instruction::IConst1,
    Instruction::IDiv,
    Instruction::ILoad,
    Instruction::IMul,
    Instruction::IStore,
    Instruction::ISub,
    Instruction::Print,
    Instruction::RegCopy
);