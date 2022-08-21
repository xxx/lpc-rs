use std::fmt::Write;

use indexmap::IndexMap;
use lpc_rs_errors::{LpcError, Result};

use crate::{
    interpreter::{efun::efun_context::EfunContext, lpc_ref::LpcRef, lpc_value::LpcValue},
    try_extract_value,
};

const MAX_RECURSION: usize = 20;

fn recursion_too_deep<const N: usize>(size: usize, context: &EfunContext<N>) -> Result<()> {
    if size > MAX_RECURSION {
        return Err(context.runtime_error("Too deep recursion."));
    }

    Ok(())
}

fn format_ref<const N: usize>(
    lpc_ref: &LpcRef,
    context: &mut EfunContext<N>,
    indent: usize,
    recurse_level: usize,
) -> Result<String> {
    recursion_too_deep(recurse_level, context)?;
    context.increment_instruction_count(1)?;

    match lpc_ref {
        LpcRef::Float(x) => Ok(format!("{:width$}{}", "", x, width = indent)),
        LpcRef::Int(x) => Ok(format!("{:width$}{}", "", x, width = indent)),
        LpcRef::String(x) => Ok(format!(
            "{:width$}{}",
            "",
            try_extract_value!(*x.borrow(), LpcValue::String),
            width = indent
        )),
        LpcRef::Object(x) => Ok(format!(
            "{:width$}{}",
            "",
            try_extract_value!(*x.borrow(), LpcValue::Object).borrow(),
            width = indent
        )),
        LpcRef::Function(x) => Ok(format!(
            "{:width$}{}",
            "",
            try_extract_value!(*x.borrow(), LpcValue::Function),
            width = indent
        )),
        LpcRef::Array(x) => {
            let xb = x.borrow();
            let arr = try_extract_value!(*xb, LpcValue::Array);
            format_array(arr, context, indent, recurse_level + 1)
        }
        LpcRef::Mapping(x) => {
            let xb = x.borrow();
            let map = try_extract_value!(*xb, LpcValue::Mapping);
            format_mapping(map, context, indent, recurse_level + 1)
        }
    }
}

fn format_array<const N: usize>(
    arr: &[LpcRef],
    context: &mut EfunContext<N>,
    indent: usize,
    recurse_level: usize,
) -> Result<String> {
    recursion_too_deep(recurse_level, context)?;
    context.increment_instruction_count(arr.len())?;

    let mut result = format!("{:width$}({{\n", "", width = indent);

    let inner = arr
        .iter()
        .map(|var| format_ref(var, context, indent + 2, recurse_level + 1))
        .collect::<Result<Vec<_>>>();

    let inner = match inner {
        Ok(x) => x,
        Err(e) => return Err(e),
    };

    let inner = inner.join(",\n");

    result.push_str(&inner);
    let _ = write!(result, "\n{:width$}}})", "", width = indent);

    Ok(result)
}

fn format_mapping<const N: usize>(
    map: &IndexMap<LpcRef, LpcRef>,
    context: &mut EfunContext<N>,
    indent: usize,
    recurse_level: usize,
) -> Result<String> {
    recursion_too_deep(recurse_level, context)?;
    context.increment_instruction_count(map.len())?;

    let mut result = format!("{:width$}([\n", "", width = indent);

    let inner = map
        .iter()
        .map(|(key, val)| {
            let k_format = format_ref(key, context, 0, recurse_level + 1)?;
            let v_format = format_ref(val, context, 2, recurse_level + 1)?;

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
    let _ = write!(result, "\n{:width$}])", "", width = indent);

    Ok(result)
}

/// The dump() Efun
pub fn dump<const N: usize>(context: &mut EfunContext<N>) -> Result<()> {
    let arg_count = context.frame().called_with_num_args;

    let s = (1..=arg_count)
        .into_iter()
        .map(|i| {
            let lpc_ref = context.resolve_lpc_ref(i);

            format_ref(&lpc_ref, context, 0, 0)
        })
        .collect::<Result<Vec<_>>>()?
        .join(" ");

    println!("{}", s);

    Ok(())
}

#[cfg(test)]
mod tests {
    use lpc_rs_utils::config::Config;

    use crate::{
        compiler::Compiler,
        interpreter::{memory::Memory, object_space::ObjectSpace, program::Program, task::Task},
    };

    fn compile_prog(code: &str) -> Program {
        let compiler = Compiler::default();
        compiler
            .compile_string("~/my_file.c", code)
            .expect("Failed to compile.")
    }

    #[test]
    fn does_not_crash_on_recursive_structures() {
        // arrays
        let code = r##"
            void create() {
                mixed a = ({ 1, 2, 3 });
                a[2] = a;
                dump(a);
            }
        "##;

        let program = compile_prog(code);
        let mut task: Task<5> = Task::new(Memory::new(10));
        let result = task.initialize_program(program, Config::default(), ObjectSpace::default());

        assert_eq!(
            result.unwrap_err().to_string(),
            "runtime error: Too deep recursion."
        );

        // mappings
        let code = r##"
            void create() {
                mixed a = ([]);
                a["marfin"] = a;
                dump(a);
            }
        "##;

        let program = compile_prog(code);
        let mut task: Task<5> = Task::new(Memory::new(10));
        let result = task.initialize_program(program, Config::default(), ObjectSpace::default());

        assert_eq!(
            result.unwrap_err().to_string(),
            "runtime error: Too deep recursion."
        );
    }
}
