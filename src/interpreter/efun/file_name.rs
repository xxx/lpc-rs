use crate::{
    interpreter::{asm_interpreter::AsmInterpreter, lpc_ref::LpcRef, lpc_value::LpcValue},
    value_to_ref, Result,
    try_extract_value,
    errors::LpcError
};
use refpool::PoolRef;
use std::cell::RefCell;
use crate::util::path_maker::LpcPath;

/// `file_name`, an efun for returning the full path and clone number of an object
pub fn file_name(interpreter: &mut AsmInterpreter) -> Result<()> {
    let lpc_ref = interpreter.register_to_lpc_ref(1);
    let value = match lpc_ref {
        LpcRef::Float(_)
        | LpcRef::Int(_)
        | LpcRef::String(_)
        | LpcRef::Array(_)
        | LpcRef::Mapping(_) => {
            LpcValue::from(0)
        }
        LpcRef::Object(x) => {
            let b = x.borrow();
            let proc = try_extract_value!(*b, LpcValue::Object);
            let path = LpcPath::new_server(&*proc.filename());

            LpcValue::from(String::from(path.as_in_game(interpreter.config.lib_dir()).to_string_lossy()))
        }
    };

    let result = value_to_ref!(value, &interpreter.memory);

    interpreter.return_efun_result(result);

    Ok(())
}
