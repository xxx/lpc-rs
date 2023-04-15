use std::fmt::Write;

use lpc_rs_errors::{LpcError, Result};

use crate::{
    interpreter::{
        efun::efun_context::EfunContext, lpc_mapping::LpcMapping, lpc_ref::LpcRef,
        lpc_value::LpcValue,
    },
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
        LpcRef::String(x) => {
            let xb = x.read();
            let s = try_extract_value!(*xb, LpcValue::String).to_str();

            Ok(format!("{:width$}{}", "", s, width = indent))
        }
        LpcRef::Object(x) => {
            let val = try_extract_value!(*x.read(), LpcValue::Object).upgrade();
            if let Some(proc) = val {
                Ok(format!("{:width$}{}", "", proc.read(), width = indent))
            } else {
                Ok(format!("{:width$}{}", "", "NULL", width = indent))
            }
        }
        LpcRef::Function(x) => Ok(format!(
            "{:width$}{}",
            "",
            try_extract_value!(*x.read(), LpcValue::Function),
            width = indent
        )),
        LpcRef::Array(x) => {
            let xb = x.read();
            let arr = try_extract_value!(*xb, LpcValue::Array);
            format_array(arr, context, indent, recurse_level + 1)
        }
        LpcRef::Mapping(x) => {
            let xb = x.read();
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
    map: &LpcMapping,
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
            let k_format = format_ref(&key.value, context, 0, recurse_level + 1)?;
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
pub async fn dump<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let arg_count = context.frame().called_with_num_args;

    let s = (1..=arg_count)
        .map(|i| {
            let lpc_ref = context.resolve_local_register(i);

            format_ref(&lpc_ref, context, 0, 0)
        })
        .collect::<Result<Vec<_>>>()?
        .join(" ");

    println!("{s}");

    Ok(())
}

#[cfg(test)]
mod tests {

    use std::sync::Arc;

    use lpc_rs_utils::config::Config;
    use parking_lot::RwLock;

    use crate::{
        compiler::Compiler,
        interpreter::{
            call_outs::CallOuts, gc::gc_bank::GcBank, memory::Memory, object_space::ObjectSpace,
            program::Program, task::Task,
        },
    };

    fn compile_prog(code: &str) -> Program {
        let compiler = Compiler::default();
        compiler
            .compile_string("~/my_file.c", code)
            .expect("Failed to compile.")
    }

    #[tokio::test]
    async fn does_not_crash_on_recursive_structures() {
        // arrays
        let code = r##"
            void create() {
                mixed a = ({ 1, 2, 3 });
                a[2] = a;
                dump(a);
            }
        "##;

        let (tx, _rx) = tokio::sync::mpsc::channel(128);
        let program = compile_prog(code);
        let result = Task::<10>::initialize_program(
            program,
            Config::default(),
            RwLock::new(ObjectSpace::default()),
            Memory::default(),
            RwLock::new(GcBank::default()),
            Arc::new(RwLock::new(CallOuts::new(tx.clone()))),
            tx.clone(),
        )
        .await;

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
        let result = Task::<5>::initialize_program(
            program,
            Config::default(),
            RwLock::new(ObjectSpace::default()),
            Memory::default(),
            RwLock::new(GcBank::default()),
            Arc::new(RwLock::new(CallOuts::new(tx.clone()))),
            tx,
        )
        .await;

        assert_eq!(
            result.unwrap_err().to_string(),
            "runtime error: Too deep recursion."
        );
    }
}
