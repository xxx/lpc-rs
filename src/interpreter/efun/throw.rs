use crate::{
    interpreter::{asm_interpreter::AsmInterpreter, lpc_ref::LpcRef, lpc_value::LpcValue},
    value_to_ref, Result,
};
use crate::errors::LpcError;

/// `throw`, intentionally throw an error. Can be caught by `catch`.
pub fn throw(interpreter: &mut AsmInterpreter) -> Result<()> {
    let arg = interpreter.register_to_lpc_ref(1);

    return Err(LpcError::new(format!("{}", arg)).with_span(interpreter.process.current_debug_span()));
}
