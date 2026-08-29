use std::fmt::Write;

use lpc_rs_errors::Result;

use crate::interpreter::{
    efun::{efun_context::EfunContext, write::tell_this_player},
    lpc_mapping::LpcMapping,
    lpc_ref::LpcRef,
    stm::TxnHandle,
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
    context: &EfunContext<N>,
    txn: &TxnHandle,
    indent: usize,
    recurse_level: usize,
) -> Result<String> {
    recursion_too_deep(recurse_level, context)?;

    match lpc_ref {
        LpcRef::Float(x) => Ok(format!("{:width$}{}", "", x, width = indent)),
        LpcRef::Int(x) => Ok(format!("{:width$}{}", "", x, width = indent)),
        LpcRef::String(_) => lpc_ref.with_string(|s| format!("{:width$}{}", "", s, width = indent)),
        LpcRef::Object(x) => {
            let val = x.upgrade();
            if let Some(proc) = val {
                Ok(format!("{:width$}{}", "", proc, width = indent))
            } else {
                Ok(format!("{:width$}{}", "", "0", width = indent))
            }
        }
        LpcRef::Function(x) => Ok(format!("{:width$}{}", "", x, width = indent)),
        LpcRef::Array(_) => lpc_ref
            .with_array(txn, |arr| {
                format_array(arr, context, txn, indent, recurse_level + 1)
            })
            .flatten(),
        LpcRef::Mapping(_) => lpc_ref
            .with_mapping(txn, |map| {
                format_mapping(map, context, txn, indent, recurse_level + 1)
            })
            .flatten(),
    }
}

fn format_array<const N: usize>(
    arr: &[LpcRef],
    context: &EfunContext<N>,
    txn: &TxnHandle,
    indent: usize,
    recurse_level: usize,
) -> Result<String> {
    recursion_too_deep(recurse_level, context)?;

    let mut result = format!("{:width$}({{\n", "", width = indent);

    let inner = arr
        .iter()
        .map(|var| format_ref(var, context, txn, indent + 2, recurse_level + 1))
        .collect::<Result<Vec<_>>>();

    let inner = inner?;

    let inner = inner.join(",\n");

    result.push_str(&inner);
    let _ = write!(result, "\n{:width$}}})", "", width = indent);

    Ok(result)
}

fn format_mapping<const N: usize>(
    map: &LpcMapping,
    context: &EfunContext<N>,
    txn: &TxnHandle,
    indent: usize,
    recurse_level: usize,
) -> Result<String> {
    recursion_too_deep(recurse_level, context)?;

    let mut result = format!("{:width$}([\n", "", width = indent);

    let inner = map
        .iter()
        .map(|(key, val)| {
            let k_format = format_ref(key, context, txn, 0, recurse_level + 1)?;
            let v_format = format_ref(val, context, txn, 2, recurse_level + 1)?;

            Ok(format!(
                "{:width$}{k}: {v}",
                "",
                width = indent + 2,
                k = k_format,
                v = &v_format[2..]
            ))
        })
        .collect::<Result<Vec<_>>>();

    let inner = inner?;

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

            format_ref(&lpc_ref, context, context.txn(), 0, 0)
        })
        .collect::<Result<Vec<_>>>()?
        .join(" ");

    let received = tell_this_player(context, &s).await?;
    context.return_efun_result(LpcRef::from(received));
    Ok(())
}

#[cfg(test)]
mod tests {
    use crate::test_support::try_run_prog;

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

        let result = try_run_prog(code).await;

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

        let result = try_run_prog(code).await;

        assert_eq!(
            result.unwrap_err().to_string(),
            "runtime error: Too deep recursion."
        );
    }
}
