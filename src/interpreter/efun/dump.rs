use std::fmt::Write;

use lpc_rs_errors::Result;

use crate::interpreter::{
    efun::efun_context::EfunContext, lpc_mapping::LpcMapping, lpc_ref::LpcRef,
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
            let s = x.read();

            Ok(format!("{:width$}{}", "", s, width = indent))
        }
        LpcRef::Object(x) => {
            let val = x.upgrade();
            if let Some(proc) = val {
                Ok(format!("{:width$}{}", "", proc.read(), width = indent))
            } else {
                Ok(format!("{:width$}{}", "", "0", width = indent))
            }
        }
        LpcRef::Function(x) => Ok(format!("{:width$}{}", "", x.read(), width = indent)),
        LpcRef::Array(x) => {
            let arr = x.read();
            format_array(&arr, context, indent, recurse_level + 1)
        }
        LpcRef::Mapping(x) => {
            let map = x.read();
            format_mapping(&map, context, indent, recurse_level + 1)
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

    use crate::{
        compiler::Compiler,
        interpreter::{program::Program, task::initialize_task::InitializeProgramBuilder},
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
        let result = InitializeProgramBuilder::<10>::default()
            .program(program)
            .tx(tx.clone())
            .build()
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
        let result = InitializeProgramBuilder::<5>::default()
            .program(program)
            .tx(tx)
            .build()
            .await;

        assert_eq!(
            result.unwrap_err().to_string(),
            "runtime error: Too deep recursion."
        );
    }
}
