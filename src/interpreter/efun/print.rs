use crate::interpreter::stack_frame::StackFrame;
use crate::interpreter::lpc_var::LPCVar;
use crate::interpreter::asm_interpreter::AsmInterpreter;
use crate::interpreter::lpc_value::LPCValue;

fn print_array(arr: &Vec<LPCVar>, interpreter: &AsmInterpreter, indent: usize) {
    println!("{:width$}({{", "", width = indent);

    for var in arr {
        let val = interpreter.resolve_var(var);
        print_val(val, interpreter, indent + 2);
    }

    println!("{:width$}}})", "", width = indent);
}

fn print_val(val: LPCValue, interpreter: &AsmInterpreter, indent: usize) {
    if let LPCValue::Array(a) = &val {
        print_array(a, interpreter, 0);
    } else {
        println!("{:width$}{}", "", val, width = indent);
    }
}

pub fn print(frame: &StackFrame, interpreter: &AsmInterpreter) {
    // function arguments start in register 1, and we know this function has only 1 arg.
    let val = interpreter.resolve_register(1);

    print_val(val, interpreter, 0);
}
