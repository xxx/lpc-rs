//! `unique_array(arr, f [, skip])`: the elements of `arr` grouped by what
//! `f(element)` returns.

use std::{sync::Arc, vec};

use indexmap::IndexMap;
use lpc_rs_errors::Result;

use crate::interpreter::{
    continuation::{Callee, Continuation, Next},
    efun::{
        callback::{array_arg, callback_args, function_arg, mint_array},
        efun_context::EfunContext,
    },
    function_type::function_ptr::FunctionPtr,
    lpc_ref::LpcRef,
    stm::TxnHandle,
};

/// `unique_array(arr, f [, skip])`: an array of the groups of elements for
/// which `f(element)` returns the same value, groups and their elements in
/// first-seen order; the group whose key equals `skip` (0 when absent) is
/// left out.
pub fn unique_array<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let items = array_arg(context, "unique_array")?;
    let ptr = function_arg(context, "unique_array", 1)?;
    let skip = if context.arg_count() < 3 {
        LpcRef::from(0)
    } else {
        context.arg(2).clone()
    };
    if items.len() == 0 {
        context.return_array(Vec::new());
        return Ok(());
    }
    context.continue_with(Box::new(UniqueArray {
        ptr,
        items,
        skip,
        asked: None,
        groups: IndexMap::new(),
    }));
    Ok(())
}

/// The walk: the element whose key is in flight, and the groups so far.
#[derive(Debug, Clone)]
struct UniqueArray {
    ptr: Arc<FunctionPtr>,
    items: vec::IntoIter<LpcRef>,
    skip: LpcRef,
    asked: Option<LpcRef>,
    groups: IndexMap<LpcRef, Vec<LpcRef>>,
}

impl Continuation for UniqueArray {
    fn advance(&mut self, result: Option<LpcRef>, txn: &TxnHandle) -> Result<Next> {
        if let (Some(key), Some(element)) = (result, self.asked.take()) {
            self.groups.entry(key).or_default().push(element);
        }
        match self.items.next() {
            Some(item) => {
                self.asked = Some(item.clone());
                Ok(Next::Call(Callee::Pointer {
                    ptr: self.ptr.clone(),
                    args: callback_args(&[item], &[]),
                }))
            }
            None => {
                let groups = std::mem::take(&mut self.groups)
                    .into_iter()
                    .filter(|(key, _)| *key != self.skip)
                    .map(|(_, group)| mint_array(txn, group))
                    .collect();
                Ok(Next::Done(mint_array(txn, groups)))
            }
        }
    }

    fn clone_box(&self) -> Box<dyn Continuation> {
        Box::new(self.clone())
    }
}

#[cfg(test)]
mod tests {
    use crate::test_support::{strings_of, try_run_prog};

    /// Each group joined with commas, so the groups compare as strings.
    fn joined(expr: &str) -> String {
        format!(
            r#"
            string join(mixed *g) {{ return implode(map(g, (: to_string($1) :)), ","); }}
            string *create() {{ return map({expr}, (: join($1) :)); }}
            "#
        )
    }

    #[tokio::test]
    async fn unique_array_groups_by_the_functions_result_in_first_seen_order() {
        let code = joined("unique_array(({ 1, 2, 3, 4, 5, 6 }), (: $1 % 3 :), -1)");
        assert_eq!(strings_of(&code).await, ["1,4", "2,5", "3,6"]);
    }

    #[tokio::test]
    async fn the_group_whose_key_is_the_skip_value_is_left_out() {
        let code = joined("unique_array(({ 10, 20, 11, 21 }), (: $1 / 10 :), 2)");
        assert_eq!(strings_of(&code).await, ["10,11"]);
    }

    #[tokio::test]
    async fn skip_defaults_to_zero() {
        let code = joined("unique_array(({ 1, 2, 3, 4, 5 }), (: $1 % 2 :))");
        assert_eq!(strings_of(&code).await, ["1,3,5"]);
    }

    #[tokio::test]
    async fn keys_compare_by_value() {
        let code = joined(r#"unique_array(({ "a", "b", "a" }), (: $1 :), "")"#);
        assert_eq!(strings_of(&code).await, ["a,a", "b"]);
    }

    #[tokio::test]
    async fn an_empty_array_is_an_empty_array() {
        let code = "mixed *create() { return unique_array(({ }), (: throw(\"never\") :)); }";
        assert!(strings_of(code).await.is_empty());
    }

    #[tokio::test]
    async fn the_functions_error_is_the_callers() {
        let code = r#"mixed create() { return unique_array(({ 1 }), (: throw("boom") :)); }"#;
        let err = try_run_prog(code).await.unwrap_err().to_string();
        assert!(err.contains("boom"), "{err}");
    }

    #[tokio::test]
    async fn the_arguments_are_typed() {
        let code = r#"mixed create() { mixed m = ([ ]); return unique_array(m, (: 1 :)); }"#;
        let err = try_run_prog(code).await.unwrap_err().to_string();
        assert!(
            err.contains("unique_array: mapping is not an array"),
            "{err}"
        );
        let code = r#"mixed create() { mixed s = "f"; return unique_array(({ 1 }), s); }"#;
        let err = try_run_prog(code).await.unwrap_err().to_string();
        assert!(
            err.contains("unique_array: string is not a function"),
            "{err}"
        );
    }
}
