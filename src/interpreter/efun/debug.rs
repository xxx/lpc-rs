use crate::{
    errors::LpcError,
    interpreter::{lpc_ref::LpcRef, lpc_value::LpcValue},
    try_extract_value, Result,
};
use itertools::Itertools;
use phf::phf_map;
use crate::interpreter::efun::efun_context::EfunContext;

type Operation = fn(&mut EfunContext) -> Result<()>;

/// The enumeration of all operations that can be done through this efun
static OPERATIONS: phf::Map<&'static str, Operation> = phf_map! {
    "in_memory_snapshot" => in_memory_snapshot,
};

/// `debug`, the kitchen sink efun to do things around getting
/// information from, or debugging the vm itself.
pub fn debug(context: &mut EfunContext) -> Result<()> {
    let lpc_ref = context.resolve_lpc_ref(1_usize);
    if let LpcRef::String(x) = lpc_ref {
        let b = x.borrow();
        let string = try_extract_value!(*b, LpcValue::String);

        match OPERATIONS.get(string.as_str()) {
            Some(func) => {
                func(context)?;

                Ok(())
            }
            None => {
                let expected = OPERATIONS.keys().sorted_unstable().join(", ");
                Err(context.runtime_error(format!(
                    "Unknown operation `{}` passed to `debug()`. Expected one of {}",
                    string, expected
                )))
            }
        }
    } else {
        Err(context.runtime_error(format!(
            "Unexpected argument `{}` passed to `debug()`.",
            lpc_ref
        )))
    }
}

fn in_memory_snapshot(_context: &mut EfunContext) -> Result<()> {
    // let klone = context.clone();
    // context.snapshot = Some(klone.into());

    Ok(())
}
