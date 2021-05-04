use crate::interpreter::{
    asm_interpreter::AsmInterpreter, lpc_value::LpcValue, lpc_var::LpcVar, stack_frame::StackFrame,
};
use std::collections::HashMap;

fn format_array(arr: &[LpcVar], interpreter: &AsmInterpreter, indent: usize) -> String {
    let mut result = format!("{:width$}({{\n", "", width = indent);

    let inner = arr
        .iter()
        .map(|var| {
            let val = interpreter.resolve_var(var);
            format_val(val, interpreter, indent + 2)
        })
        .collect::<Vec<_>>()
        .join(",\n");

    result.push_str(&inner);
    result.push_str(&format!("\n{:width$}}})", "", width = indent));

    result
}

fn format_mapping(
    map: &HashMap<LpcVar, LpcVar>,
    interpreter: &AsmInterpreter,
    indent: usize,
) -> String {
    let mut result = format!("{:width$}([\n", "", width = indent);

    let inner = map
        .iter()
        .map(|(key, val)| {
            let k_val = interpreter.resolve_var(key);
            let k_format = format_val(k_val, interpreter, 0);
            let v_val = interpreter.resolve_var(val);
            let v_format = format_val(v_val, interpreter, 2);

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

fn format_val(val: LpcValue, interpreter: &AsmInterpreter, indent: usize) -> String {
    match val {
        LpcValue::Array(a) => format_array(&a, interpreter, indent),
        LpcValue::Mapping(m) => format_mapping(&m, interpreter, indent),
        x => format!("{:width$}{}", "", x, width = indent),
    }
}

/// The dump() Efun
pub fn dump(_frame: &StackFrame, interpreter: &AsmInterpreter) {
    // function arguments start in register 1, and we know this function has only 1 arg.
    let val = interpreter.register_to_lpc_value(1);

    let formatted = format_val(val, interpreter, 0);
    println!("{}", formatted);
}
