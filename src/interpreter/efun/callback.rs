//! What the efuns that call back into LPC share: the collection walked, the
//! pointer, the extra arguments, the results minted.

use std::{sync::Arc, vec};

use indexmap::IndexMap;
use lpc_rs_errors::Result;
use smallvec::SmallVec;

use crate::interpreter::{
    apply::call_pointer, efun::efun_context::EfunContext, function_type::function_ptr::FunctionPtr,
    lpc_array::LpcArray, lpc_mapping::LpcMapping, lpc_ref::LpcRef, stm::TxnHandle,
};

/// The collection an efun walks, its elements still to visit.
#[derive(Debug, Clone)]
pub(crate) enum Items {
    Array(vec::IntoIter<LpcRef>),
    Mapping(vec::IntoIter<(LpcRef, LpcRef)>),
}

impl Items {
    pub(crate) fn len(&self) -> usize {
        match self {
            Items::Array(items) => items.len(),
            Items::Mapping(entries) => entries.len(),
        }
    }
}

/// Argument 0 as the collection to walk.
pub(crate) fn items_arg<const N: usize>(context: &EfunContext<'_, N>, name: &str) -> Result<Items> {
    let arg = context.arg(0);
    match arg {
        LpcRef::Array(_) => arg
            .with_array(context.txn(), |a| {
                Items::Array(a.iter().cloned().collect::<Vec<_>>().into_iter())
            })
            .map_err(|e| e.with_span(context.call_site_span())),
        LpcRef::Mapping(_) => arg
            .with_mapping(context.txn(), |m| {
                Items::Mapping(
                    m.iter()
                        .map(|(k, v)| (k.clone(), v.clone()))
                        .collect::<Vec<_>>()
                        .into_iter(),
                )
            })
            .map_err(|e| e.with_span(context.call_site_span())),
        other => Err(context.runtime_error(format!(
            "{name}: {} is not an array or mapping",
            other.type_name()
        ))),
    }
}

/// Argument `i` as the pointer to call back.
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

/// The arguments from `from` on, passed to every callback after the element.
pub(crate) fn extra_args<const N: usize>(context: &EfunContext<'_, N>, from: usize) -> Vec<LpcRef> {
    (from..context.arg_count())
        .map(|i| context.arg(i).clone())
        .collect()
}

/// One callback's arguments: the element's values, then the extras.
pub(crate) fn callback_args(element: &[LpcRef], extra: &[LpcRef]) -> SmallVec<[LpcRef; 4]> {
    let mut args = SmallVec::with_capacity(element.len() + extra.len());
    args.extend(element.iter().cloned());
    args.extend(extra.iter().cloned());
    args
}

/// `values` as a fresh array.
pub(crate) fn mint_array(txn: &TxnHandle, values: Vec<LpcRef>) -> LpcRef {
    LpcRef::Array(txn.with(|t| t.mint_array(LpcArray::new(values))))
}

/// `keys` zipped with `values` as a fresh mapping.
pub(crate) fn mint_mapping(txn: &TxnHandle, keys: Vec<LpcRef>, values: Vec<LpcRef>) -> LpcRef {
    debug_assert_eq!(keys.len(), values.len());
    let mapping = LpcMapping::new(keys.into_iter().zip(values).collect::<IndexMap<_, _>>());
    LpcRef::Mapping(txn.with(|t| t.mint_mapping(mapping)))
}

/// The empty result of `items`'s kind, needing no callback.
pub(crate) fn return_empty<const N: usize>(context: &mut EfunContext<'_, N>, items: &Items) {
    match items {
        Items::Array(_) => context.return_array(Vec::new()),
        Items::Mapping(_) => context.return_mapping(LpcMapping::new(IndexMap::new())),
    }
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
