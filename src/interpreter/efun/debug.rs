use crate::{
    errors::LpcError,
    interpreter::{asm_interpreter::AsmInterpreter, lpc_ref::LpcRef, lpc_value::LpcValue},
    try_extract_value,
    Result,
};
use phf::phf_map;
use itertools::Itertools;

type Operation = fn(&mut AsmInterpreter) -> Result<()>;

/// The enumeration of all operations that can be done through this efun
static OPERATIONS: phf::Map<&'static str, Operation> = phf_map! {
    "in_memory_snapshot" => in_memory_snapshot,
};

/// `debug`, the kitchen sink efun to do things around getting
/// information from, or debugging the vm itself.
pub fn debug(interpreter: &mut AsmInterpreter) -> Result<()> {
    let lpc_ref = interpreter.register_to_lpc_ref(1);
    if let LpcRef::String(x) = lpc_ref {
        let b = x.borrow();
        let string = try_extract_value!(*b, LpcValue::String);

        match OPERATIONS.get(string.as_str()) {
            Some(func) => {
                func(interpreter)?;

                Ok(())
            }
            None => {
                let expected = OPERATIONS.keys().sorted_unstable().join(", ");
                Err(interpreter.runtime_error(format!("Unknown operation `{}` passed to `debug()`. Expected one of {}", string, expected)))
            }
        }
    } else {
        Err(interpreter.runtime_error(format!("Unexpected argument `{}` passed to `debug()`.", lpc_ref)))
    }
}

fn in_memory_snapshot(interpreter: &mut AsmInterpreter) -> Result<()> {
    let klone = interpreter.clone();
    interpreter.snapshot = Some(klone.into());

    Ok(())
}
