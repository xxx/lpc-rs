use crate::{
    interpreter::{asm_interpreter::AsmInterpreter, lpc_ref::LpcRef, lpc_value::LpcValue},
    Result,
};
use refpool::PoolRef;
use std::cell::RefCell;

/// `this_object`, an efun for returning a reference to the object it was called within.
pub fn this_object(interpreter: &mut AsmInterpreter) -> Result<()> {
    let proc = interpreter.process.clone();
    let v = LpcValue::Object(proc);
    let result = LpcRef::Object(PoolRef::new(&interpreter.memory, RefCell::new(v)));

    interpreter.return_efun_result(result);

    Ok(())
}
