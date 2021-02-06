use crate::interpreter::stack_frame::StackFrame;
use crate::interpreter::lpc_var::LPCVar;
use crate::interpreter::asm_interpreter::AsmInterpreter;
use crate::interpreter::lpc_value::LPCValue;

pub fn print(frame: &StackFrame, interpreter: &AsmInterpreter) {
    // function arguments start in register 1.
    match frame.registers.get(1).unwrap() {
        LPCVar::Int(v) => println!("{}", v),
        LPCVar::String(_) => {
            // arguments always start in register 1
            let s = interpreter.resolve_register(1);
            if let LPCValue::String(str) = s {
                println!("{}", str);
            } else {
                unimplemented!()
            }
        }
        LPCVar::Array(_) => {
            let array = interpreter.resolve_register(1);
            if let LPCValue::Array(vec) = array {
                let values =
                    vec
                        .iter()
                        .map(|i| interpreter.resolve_var(i))
                        .collect::<Vec<_>>();
                println!("{:?}", values);
            } else {
                unimplemented!()
            }
        }
    };
}
