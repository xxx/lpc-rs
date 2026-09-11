use std::fmt::Write;

use lpc_rs_errors::Result;
use lpc_rs_utils::lpc_bytes;

use crate::interpreter::{
    efun::{efun_context::EfunContext, write::tell_this_player},
    lpc_mapping::LpcMapping,
    lpc_ref::LpcRef,
    stm::TxnHandle,
};

const MAX_RECURSION: usize = 20;

#[derive(Clone, Copy, Default)]
pub(super) struct FormatOptions {
    pub quote_strings: bool,
    pub compact: bool,
}

fn quoted_string(s: &str) -> String {
    let mut result = String::from("\"");
    for c in s.chars() {
        match c {
            '"' => result.push_str("\\\""),
            '\\' => result.push_str("\\\\"),
            '\n' => result.push_str("\\n"),
            '\r' => result.push_str("\\r"),
            '\t' => result.push_str("\\t"),
            '\x07' => result.push_str("\\a"),
            '\x08' => result.push_str("\\b"),
            '\x0b' => result.push_str("\\v"),
            '\x0c' => result.push_str("\\f"),
            c if c.is_control() && c as u32 <= 0xff => {
                let _ = write!(result, "\\x{:02x}", c as u32);
            }
            c if c.is_control() || matches!(c, '\u{2028}' | '\u{2029}') => {
                let _ = write!(result, "\\u{:04x}", c as u32);
            }
            c => result.push(c),
        }
    }
    result.push('"');
    result
}

fn recursion_too_deep<const N: usize>(size: usize, context: &EfunContext<N>) -> Result<()> {
    if size > MAX_RECURSION {
        return Err(context.runtime_error("Too deep recursion."));
    }

    Ok(())
}

/// Render a debug value, optionally escaping strings and compacting collections.
pub(super) fn format_ref<const N: usize>(
    lpc_ref: &LpcRef,
    context: &EfunContext<N>,
    txn: &TxnHandle,
    indent: usize,
    recurse_level: usize,
    options: FormatOptions,
) -> Result<String> {
    recursion_too_deep(recurse_level, context)?;

    match lpc_ref {
        LpcRef::Float(x) => Ok(format!("{:width$}{}", "", x, width = indent)),
        LpcRef::Int(x) => Ok(format!("{:width$}{}", "", x, width = indent)),
        LpcRef::String(_) => lpc_ref.with_string(|s| {
            if options.quote_strings {
                format!("{:width$}{}", "", quoted_string(s.to_str()), width = indent)
            } else {
                format!("{:width$}{}", "", s, width = indent)
            }
        }),
        LpcRef::Bytes(b) => Ok(format!(
            "{:width$}{}",
            "",
            lpc_bytes::literal(b),
            width = indent
        )),
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
                format_array(arr, context, txn, indent, recurse_level + 1, options)
            })
            .flatten(),
        LpcRef::Mapping(_) => lpc_ref
            .with_mapping(txn, |map| {
                format_mapping(map, context, txn, indent, recurse_level + 1, options)
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
    options: FormatOptions,
) -> Result<String> {
    recursion_too_deep(recurse_level, context)?;

    let inner_indent = if options.compact { 0 } else { indent + 2 };
    let inner = arr
        .iter()
        .map(|var| format_ref(var, context, txn, inner_indent, recurse_level + 1, options))
        .collect::<Result<Vec<_>>>();

    let inner = inner?;
    if options.compact {
        return Ok(format!("({{{}}})", inner.join(",")));
    }

    let mut result = format!("{:width$}({{\n", "", width = indent);
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
    options: FormatOptions,
) -> Result<String> {
    recursion_too_deep(recurse_level, context)?;

    let value_indent = if options.compact { 0 } else { 2 };
    let inner = map
        .iter()
        .map(|(key, val)| {
            let k_format = format_ref(key, context, txn, 0, recurse_level + 1, options)?;
            let v_format = format_ref(val, context, txn, value_indent, recurse_level + 1, options)?;

            if options.compact {
                return Ok(format!("{k_format}:{v_format}"));
            }

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
    if options.compact {
        return Ok(format!("([{}])", inner.join(",")));
    }

    let mut result = format!("{:width$}([\n", "", width = indent);
    let inner = inner.join(",\n");

    result.push_str(&inner);
    let _ = write!(result, "\n{:width$}])", "", width = indent);

    Ok(result)
}

/// The dump() Efun
pub async fn dump<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let arg_count = context.arg_count();

    let s = (0..arg_count)
        .map(|i| {
            let lpc_ref = context.arg(i).clone();

            format_ref(
                &lpc_ref,
                context,
                context.txn(),
                0,
                0,
                FormatOptions::default(),
            )
        })
        .collect::<Result<Vec<_>>>()?
        .join(" ");

    tell_this_player(context, &s).await?;
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
