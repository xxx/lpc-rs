use crate::interpreter::stack_frame::StackFrame;
use crate::interpreter::lpc_var::LPCVar;
use crate::interpreter::asm_interpreter::AsmInterpreter;
use crate::interpreter::lpc_value::LPCValue;

pub fn print(frame: &StackFrame, interpreter: &AsmInterpreter) {
    // function arguments start in register 1.
    let val = interpreter.resolve_register(1);
    println!("{}", val);
}
