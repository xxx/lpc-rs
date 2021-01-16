use crate::interpreter::stack_frame::StackFrame;
use crate::asm::register::Register;

pub fn print(frame: &StackFrame, initial_arg: &Register) {
    let value = frame.registers.get(initial_arg.value());

    println!("{}", value.unwrap());
}
