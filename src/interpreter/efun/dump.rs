use crate::interpreter::{
    asm_interpreter::AsmInterpreter, lpc_ref::LpcRef, lpc_value::LpcValue,
};
use crate::Result;
use std::collections::HashMap;

fn format_ref(lpc_ref: &LpcRef, interpreter: &AsmInterpreter, indent: usize) -> String {
    match lpc_ref {
        LpcRef::Float(x) => format_val(&LpcValue::Float(*x), interpreter, indent),
        LpcRef::Int(x) => format_val(&LpcValue::Int(*x), interpreter, indent),
        LpcRef::String(x) | LpcRef::Array(x) | LpcRef::Mapping(x) | LpcRef::Object(x) => {
            format_val(&*x.borrow(), interpreter, indent)
        }
    }
}

fn format_array(arr: &[LpcRef], interpreter: &AsmInterpreter, indent: usize) -> String {
    let mut result = format!("{:width$}({{\n", "", width = indent);

    let inner = arr
        .iter()
        .map(|var| format_ref(var, interpreter, indent + 2))
        .collect::<Vec<_>>()
        .join(",\n");

    result.push_str(&inner);
    result.push_str(&format!("\n{:width$}}})", "", width = indent));

    result
}

fn format_mapping(
    map: &HashMap<LpcRef, LpcRef>,
    interpreter: &AsmInterpreter,
    indent: usize,
) -> String {
    let mut result = format!("{:width$}([\n", "", width = indent);

    let inner = map
        .iter()
        .map(|(key, val)| {
            let k_format = format_ref(key, interpreter, 0);
            let v_format = format_ref(val, interpreter, 2);

            format!(
                "{:width$}{k}: {v}",
                "",
                width = indent + 2,
                k = k_format,
                v = &v_format[2..]
            )
        })
        .collect::<Vec<_>>()
        .join(",\n");

    result.push_str(&inner);
    result.push_str(&format!("\n{:width$}])", "", width = indent));

    result
}

fn format_val(val: &LpcValue, interpreter: &AsmInterpreter, indent: usize) -> String {
    match val {
        LpcValue::Array(a) => format_array(&a, interpreter, indent),
        LpcValue::Mapping(m) => format_mapping(&m, interpreter, indent),
        x => format!("{:width$}{}", "", x, width = indent),
    }
}

/// The dump() Efun
pub fn dump(interpreter: &mut AsmInterpreter) -> Result<()> {
    // function arguments start in register 1, and we know this function has only 1 arg.
    let lpc_ref = interpreter.register_to_lpc_ref(1);

    println!("{}", format_ref(&lpc_ref, interpreter, 0));

    Ok(())
}
