use crate::interpreter::stack_frame::StackFrame;

pub fn print(frame: &StackFrame) {
    let value = frame.registers.get(1);

    println!("{}", value.unwrap());
}
