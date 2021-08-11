use crate::{
    interpreter::{asm_interpreter::AsmInterpreter, lpc_ref::LpcRef, lpc_value::LpcValue},
    Result,
};
use std::collections::HashMap;
use crate::errors::LpcError;
use crate::try_extract_value;

const MAX_RECURSION: usize = 20;

fn recursion_too_deep(size: usize, interpreter: &AsmInterpreter) -> Result<()> {
    if size > MAX_RECURSION {
        return Err(interpreter.runtime_error("Too deep recursion"));
    }

    Ok(())
}

fn format_ref(lpc_ref: &LpcRef, interpreter: &mut AsmInterpreter, indent: usize, recurse_level: usize) -> Result<String> {
    recursion_too_deep(recurse_level, interpreter)?;
    interpreter.increment_instruction_count(1)?;

    match lpc_ref {
        LpcRef::Float(x) => Ok(format!("{:width$}{}", "", x, width = indent)),
        LpcRef::Int(x) => Ok(format!("{:width$}{}", "", x, width = indent)),
        LpcRef::String(x) => Ok(format!("{:width$}{}", "", try_extract_value!(*x.borrow(), LpcValue::String), width = indent)),
        LpcRef::Object(x) => Ok(format!("{:width$}{}", "", try_extract_value!(*x.borrow(), LpcValue::Object), width = indent)),
        LpcRef::Array(x) => {
            let xb = x.borrow();
            let arr = try_extract_value!(*xb, LpcValue::Array);
            format_array(arr, interpreter, indent, recurse_level + 1)
        }
        LpcRef::Mapping(x) => {
            let xb = x.borrow();
            let map = try_extract_value!(*xb, LpcValue::Mapping);
            format_mapping(map, interpreter, indent, recurse_level + 1)
        }
    }
}

fn format_array(arr: &[LpcRef], interpreter: &mut AsmInterpreter, indent: usize, recurse_level: usize) -> Result<String> {
    recursion_too_deep(recurse_level, interpreter)?;
    interpreter.increment_instruction_count(arr.len())?;

    let mut result = format!("{:width$}({{\n", "", width = indent);

    let inner = arr
        .iter()
        .map(|var| format_ref(var, interpreter, indent + 2, recurse_level + 1))
        .collect::<Result<Vec<_>>>();

    let inner = match inner {
        Ok(x) => x,
        Err(e) => return Err(e),
    };

    let inner = inner.join(",\n");

    result.push_str(&inner);
    result.push_str(&format!("\n{:width$}}})", "", width = indent));

    Ok(result)
}

fn format_mapping(
    map: &HashMap<LpcRef, LpcRef>,
    interpreter: &mut AsmInterpreter,
    indent: usize,
    recurse_level: usize,
) -> Result<String> {
    recursion_too_deep(recurse_level, interpreter)?;
    interpreter.increment_instruction_count(map.len())?;

    let mut result = format!("{:width$}([\n", "", width = indent);

    let inner = map
        .iter()
        .map(|(key, val)| {
            let k_format = format_ref(key, interpreter, 0, recurse_level + 1)?;
            let v_format = format_ref(val, interpreter, 2, recurse_level + 1)?;

            Ok(format!(
                "{:width$}{k}: {v}",
                "",
                width = indent + 2,
                k = k_format,
                v = &v_format[2..]
            ))
        })
        .collect::<Result<Vec<_>>>();

        let inner = match inner {
            Ok(x) => x,
            Err(e) => return Err(e),
        };

        let inner = inner.join(",\n");

    result.push_str(&inner);
    result.push_str(&format!("\n{:width$}])", "", width = indent));

    Ok(result)
}

/// The dump() Efun
pub fn dump(interpreter: &mut AsmInterpreter) -> Result<()> {
    // function arguments start in register 1, and we know this function has only 1 arg.
    let lpc_ref = interpreter.register_to_lpc_ref(1);

    println!("{}", format_ref(&lpc_ref, interpreter, 0, 0)?);

    Ok(())
}
