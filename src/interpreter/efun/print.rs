use crate::asm::inst::call::Call;
use crate::interpreter::stack_frame::StackFrame;

pub fn print(frame: &StackFrame, call: &Call) {
    let value = frame.registers.get(call.initial_arg.value());

    println!("{}", value.unwrap());
}
