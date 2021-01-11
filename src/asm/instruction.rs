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
use crate::asm::inst::regcopy::RegCopy;
use crate::asm::inst::ret::Ret;
use std::fmt::{Formatter,Display};
use std::fmt;

pub trait InstructionTrait: Display {}

macro_rules! build_instructions {
    ( $( $x:ident ),+ ) => {
        #[derive(Debug, Clone, Eq, PartialEq)]
        pub enum Instruction {
            $(
                $x($x),
            )*
        }
        
        impl Display for Instruction {
            fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
                match self {
                $(
                    Instruction::$x(y) => write!(f, "{}", y),
                )*
                }
            }
        }
    };
}

build_instructions!(
    Call,
    IAdd,
    IConst,
    IConst0,
    IConst1,
    IDiv,
    ILoad,
    IMul,
    IStore,
    ISub,
    RegCopy,
    Ret
);