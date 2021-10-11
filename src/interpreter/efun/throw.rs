use crate::{errors::LpcError, interpreter::asm_interpreter::AsmInterpreter, Result};

/// `throw`, intentionally throw an error. Can be caught by `catch`.
pub fn throw(interpreter: &mut AsmInterpreter) -> Result<()> {
    let arg = interpreter.register_to_lpc_ref(1);

    return Err(LpcError::new(format!("{}", arg))
        .with_span(interpreter.current_frame()?.current_debug_span()));
}
