use crate::interpreter::{
    asm_interpreter::AsmInterpreter, lpc_value::LpcValue, lpc_var::LpcVar, stack_frame::StackFrame,
};

fn print_array(arr: &[LpcVar], interpreter: &AsmInterpreter, indent: usize) {
    println!("{:width$}({{", "", width = indent);

    for var in arr {
        let val = interpreter.resolve_var(var);
        print_val(val, interpreter, indent + 2);
    }

    println!("{:width$}}})", "", width = indent);
}

fn print_val(val: LpcValue, interpreter: &AsmInterpreter, indent: usize) {
    if let LpcValue::Array(a) = &val {
        print_array(a, interpreter, indent);
    } else {
        println!("{:width$}{}", "", val, width = indent);
    }
}

/// The dump() Efun
pub fn dump(_frame: &StackFrame, interpreter: &AsmInterpreter) {
    // function arguments start in register 1, and we know this function has only 1 arg.
    let val = interpreter.resolve_register(1);

    print_val(val, interpreter, 0);
}
