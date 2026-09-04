//! The shared front of the efuns that call a function per element
//! (`filter`, `map`, `sort_array`).

use std::sync::Arc;

use lpc_rs_errors::Result;

use crate::interpreter::{
    apply::call_pointer, efun::efun_context::EfunContext, function_type::function_ptr::FunctionPtr,
    lpc_ref::LpcRef,
};

/// The array's items, or the mapping's entries in order.
pub(crate) enum Items {
    Array(Vec<LpcRef>),
    Mapping(Vec<(LpcRef, LpcRef)>),
}

/// Argument 0 of `name` as a copy of its items; anything but an array or
/// a mapping is an error.
pub(crate) fn items_arg<const N: usize>(context: &EfunContext<'_, N>, name: &str) -> Result<Items> {
    let arg = context.arg(0);
    match arg {
        LpcRef::Array(_) => arg
            .with_array(context.txn(), |a| Items::Array(a.iter().cloned().collect()))
            .map_err(|e| e.with_span(context.call_site_span())),
        LpcRef::Mapping(_) => arg
            .with_mapping(context.txn(), |m| {
                Items::Mapping(m.iter().map(|(k, v)| (k.clone(), v.clone())).collect())
            })
            .map_err(|e| e.with_span(context.call_site_span())),
        other => Err(context.runtime_error(format!(
            "{name}: {} is not an array or mapping",
            other.type_name()
        ))),
    }
}

/// Argument `i` of `name` as the function it calls; anything else is an
/// error.
pub(crate) fn function_arg<const N: usize>(
    context: &EfunContext<'_, N>,
    name: &str,
    i: usize,
) -> Result<Arc<FunctionPtr>> {
    match context.arg(i) {
        LpcRef::Function(f) => Ok(f.clone()),
        other => {
            Err(context.runtime_error(format!("{name}: {} is not a function", other.type_name())))
        }
    }
}

/// The arguments from `from` on: what every call gets after the element.
pub(crate) fn extra_args<const N: usize>(context: &EfunContext<'_, N>, from: usize) -> Vec<LpcRef> {
    (from..context.arg_count())
        .map(|i| context.arg(i).clone())
        .collect()
}

/// `element` then `extra`, as one call's arguments.
pub(crate) fn call_args(element: &[LpcRef], extra: &[LpcRef]) -> Vec<LpcRef> {
    let mut args = Vec::with_capacity(element.len() + extra.len());
    args.extend_from_slice(element);
    args.extend_from_slice(extra);
    args
}

/// Call `pointer` from the object running `name` with `args`; a pointer
/// that no longer resolves is an error.
pub(crate) async fn call_back<const N: usize>(
    context: &EfunContext<'_, N>,
    name: &str,
    pointer: &FunctionPtr,
    args: &[LpcRef],
) -> Result<LpcRef> {
    call_pointer(context.task_context(), context.process(), pointer, args)
        .await?
        .ok_or_else(|| context.runtime_error(format!("{name}: the function no longer resolves")))
}
