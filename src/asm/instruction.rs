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
use enum_dispatch::enum_dispatch;

#[enum_dispatch]
pub trait InstructionTrait {
    fn to_str(&self) -> String;
}

#[enum_dispatch(InstructionTrait)]
#[derive(Debug, Clone)]
pub enum Instruction {
    IAdd,
    IConst,
    IConst0,
    IConst1,
    IDiv,
    ILoad,
    IMul,
    IStore,
    ISub,
    Print
}
