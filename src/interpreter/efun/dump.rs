use std::fmt::Write;

use lpc_rs_errors::Result;

use crate::interpreter::{
    efun::{efun_context::EfunContext, write::apply_catch_tell},
    lpc_mapping::LpcMapping,
    lpc_ref::LpcRef,
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
                Ok(format!("{:width$}{}", "", proc, width = indent))
            } else {
                Ok(format!("{:width$}{}", "", "0", width = indent))
            }
        }
        LpcRef::Function(x) => Ok(format!("{:width$}{}", "", x, width = indent)),
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
pub async fn dump<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let arg_count = context.frame().called_with_num_args;

    let s = (1..=arg_count)
        .map(|i| {
            let lpc_ref = context.resolve_local_register(i).clone();

            format_ref(&lpc_ref, context, 0, 0)
        })
        .collect::<Result<Vec<_>>>()?
        .join(" ");

    apply_catch_tell(s, context).await?;

    Ok(())
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use crate::{
        interpreter::{
            task::initialize_program::InitializeProgramBuilder, vm::global_state::GlobalState,
        },
        test_support::compile_prog,
    };

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
        let (program, config, _) = compile_prog(code).await;
        let global_state = Arc::new(GlobalState::new(config, tx));
        let result = InitializeProgramBuilder::<10>::default()
            .global_state(global_state.clone())
            .program(program)
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

        let (program, _, _) = compile_prog(code).await;
        let result = InitializeProgramBuilder::<5>::default()
            .global_state(global_state)
            .program(program)
            .build()
            .await;

        assert_eq!(
            result.unwrap_err().to_string(),
            "runtime error: Too deep recursion."
        );
    }
}
