use crate::interpreter::asm_interpreter::AsmInterpreter;
use crate::asm::inst::call::Call;

pub fn print(interpreter: &AsmInterpreter, call: &Call) {
    let value = interpreter.registers.get(call.initial_arg.value());

    println!("{}", value.unwrap());
}
