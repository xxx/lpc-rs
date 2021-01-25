use crate::interpreter::stack_frame::StackFrame;
use crate::interpreter::lpc_var::LPCVar;
use crate::interpreter::asm_interpreter::AsmInterpreter;
use crate::interpreter::lpc_constant::LPCConstant;

pub fn print(frame: &StackFrame, interpreter: &AsmInterpreter) {
    match frame.registers.get(1).unwrap() {
        LPCVar::Int(v) => println!("{}", v),
        LPCVar::String(_) => {
            // arguments always start in register 1
            let s = interpreter.resolve_register(1);
            if let LPCConstant::String(str) = s {
                println!("{}", str);
            } else {
                unimplemented!()
            }
        }
    };
}
