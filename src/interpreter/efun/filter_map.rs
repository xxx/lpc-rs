//! `filter_map(collection, f, extra...)`: `f(element, extra...)` per
//! element, the results that are true kept in a fresh array, or a fresh
//! mapping over their keys.

use std::sync::Arc;

use lpc_rs_errors::Result;

use crate::interpreter::{
    continuation::{Callee, Continuation, Next},
    efun::{
        callback::{
            Items, callback_args, extra_args, function_arg, items_arg, mint_array, mint_mapping,
            return_empty,
        },
        efun_context::EfunContext,
    },
    function_type::function_ptr::FunctionPtr,
    lpc_ref::LpcRef,
    stm::TxnHandle,
};

/// `filter_map(collection, f, extra...)`: `f(element, extra...)` per
/// element, the results that are true kept in a fresh array or mapping.
pub fn filter_map<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let items = items_arg(context, "filter_map")?;
    let ptr = function_arg(context, "filter_map", 1)?;
    let extra = extra_args(context, 2);
    if items.is_empty() {
        return_empty(context, &items);
        return Ok(());
    }
    context.continue_with(Box::new(FilterMap {
        ptr,
        items,
        extra,
        asked: None,
        kept_keys: Vec::new(),
        kept: Vec::new(),
    }));
    Ok(())
}

/// The walk: the key whose result is in flight, and the results kept.
#[derive(Debug, Clone)]
struct FilterMap {
    ptr: Arc<FunctionPtr>,
    items: Items,
    extra: Vec<LpcRef>,
    /// The mapping key asked about; `None` for an array element.
    asked: Option<LpcRef>,
    kept_keys: Vec<LpcRef>,
    kept: Vec<LpcRef>,
}

impl Continuation for FilterMap {
    fn advance(&mut self, result: Option<LpcRef>, txn: &TxnHandle) -> Result<Next> {
        let key = self.asked.take();
        if let Some(value) = result
            && value.is_truthy(txn)
        {
            if let Some(key) = key {
                self.kept_keys.push(key);
            }
            self.kept.push(value);
        }
        let args = match &mut self.items {
            Items::Array(items) => match items.next() {
                Some(item) => callback_args(&[item], &self.extra),
                None => return Ok(Next::Done(mint_array(txn, std::mem::take(&mut self.kept)))),
            },
            Items::Mapping(entries) => match entries.next() {
                Some((key, value)) => {
                    self.asked = Some(key.clone());
                    callback_args(&[key, value], &self.extra)
                }
                None => {
                    return Ok(Next::Done(mint_mapping(
                        txn,
                        std::mem::take(&mut self.kept_keys),
                        std::mem::take(&mut self.kept),
                    )));
                }
            },
        };
        Ok(Next::Call(Callee::Pointer {
            ptr: self.ptr.clone(),
            args,
        }))
    }

    fn clone_box(&self) -> Box<dyn Continuation> {
        Box::new(self.clone())
    }
}

#[cfg(test)]
mod tests {
    use indoc::indoc;
    use lpc_rs_asm::instruction::Instruction;

    use crate::{
        interpreter::{lpc_ref::LpcRef, task::eval_loop::Slice, vm::Vm},
        test_support::{run_prog, task_at, test_config, try_run_prog},
    };

    async fn strings_of(code: &str) -> Vec<String> {
        let task = run_prog(code).await;
        task.result()
            .unwrap()
            .with_array(task.context.txn(), |arr| {
                arr.iter().map(|x| x.to_string()).collect()
            })
            .unwrap()
    }

    #[tokio::test]
    async fn filter_map_keeps_the_results_that_are_true() {
        let code = "int *create() { return filter_map(({ 1, 2, 3, 4 }), (: $1 % 2 == 0 ? $1 * 10 : 0 :)); }";
        assert_eq!(strings_of(code).await, ["20", "40"]);
    }

    #[tokio::test]
    async fn filter_map_drops_a_zero_float_result() {
        let code = "mixed *create() { return filter_map(({ 1, 2 }), (: $1 == 1 ? 0.0 : 1.5 :)); }";
        assert_eq!(strings_of(code).await, ["1.5"]);
    }

    #[tokio::test]
    async fn filter_map_passes_the_extra_arguments_after_the_item() {
        let code =
            "int *create() { return filter_map(({ 1, 2, 3 }), (: $1 > $2 ? $1 + $2 : 0 :), 1); }";
        assert_eq!(strings_of(code).await, ["3", "4"]);
    }

    #[tokio::test]
    async fn filter_map_of_a_mapping_keeps_the_keys_whose_results_are_true() {
        let code = r#"
            mixed *create() {
                return keys(filter_map(([ "a": 1, "b": 2, "c": 3 ]), (: $2 >= 2 ? $2 * 10 : 0 :)));
            }
        "#;
        assert_eq!(strings_of(code).await, ["b", "c"]);
    }

    #[tokio::test]
    async fn filter_map_of_a_mapping_values_each_kept_key_by_its_result() {
        let code = r#"
            mixed *create() {
                return values(filter_map(([ "a": 1, "b": 2, "c": 3 ]), (: $2 >= 2 ? $2 * 10 : 0 :)));
            }
        "#;
        assert_eq!(strings_of(code).await, ["20", "30"]);
    }

    #[tokio::test]
    async fn filter_map_returns_a_new_array() {
        let code = "int create() { mixed *a = ({ 1 }); return filter_map(a, (: $1 :)) == a; }";
        assert_eq!(run_prog(code).await.result(), Some(LpcRef::from(0)));
    }

    #[tokio::test]
    async fn filter_map_of_an_empty_array_needs_no_callback() {
        let code = "int create() { return sizeof(filter_map(({ }), (: 1 / 0 :))); }";
        assert_eq!(run_prog(code).await.result(), Some(LpcRef::from(0)));
    }

    #[tokio::test]
    async fn filter_map_of_an_empty_mapping_is_an_empty_mapping() {
        let code = "int create() { return mappingp(filter_map(([ ]), (: 1 / 0 :))); }";
        assert_eq!(run_prog(code).await.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn the_functions_error_is_the_callers() {
        let code = r#"mixed create() { return filter_map(({ 1 }), (: throw("boom") :)); }"#;
        let err = try_run_prog(code).await.unwrap_err().to_string();
        assert!(err.contains("boom"), "{err}");
    }

    #[tokio::test]
    async fn filter_map_of_a_non_collection_is_an_error() {
        let code = "mixed create() { mixed x = 1; return filter_map(x, (: 1 :)); }";
        let err = try_run_prog(code).await.unwrap_err().to_string();
        assert!(
            err.contains("filter_map: int is not an array or mapping"),
            "{err}"
        );
    }

    #[tokio::test]
    async fn filter_map_with_a_non_function_is_an_error() {
        let code = "mixed create() { mixed f = 1; return filter_map(({ 1 }), f); }";
        let err = try_run_prog(code).await.unwrap_err().to_string();
        assert!(err.contains("filter_map: int is not a function"), "{err}");
    }

    #[tokio::test]
    async fn filter_map_runs_its_first_callback_with_no_await() {
        let vm = Vm::new(test_config());
        let code = indoc! { r#"
            int one(int x) { return x; }
            int *got;
            void create() { got = filter_map(({ 1, 2 }), &one()); }
        "# };
        let (mut task, _live) =
            task_at(&vm, code, |at| matches!(at, Instruction::CallEfun(..))).await;

        let slice = task.run_slice(&mut 1).unwrap();

        assert!(matches!(slice, Slice::Budget));
        assert_eq!(task.stack.current_frame().unwrap().function.name(), "one");
        assert!(task.stack.get(0).unwrap().pending.is_some());
    }
}
