//! `filter(collection, f, extra...)`: the elements for which
//! `f(element, extra...)` is true, in order, in a fresh array or mapping.

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

/// `filter(collection, f, extra...)`: the elements for which
/// `f(element, extra...)` is true, in order, in a fresh array or mapping.
pub fn filter<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let items = items_arg(context, "filter")?;
    let ptr = function_arg(context, "filter", 1)?;
    let extra = extra_args(context, 2);
    if items.len() == 0 {
        return_empty(context, &items);
        return Ok(());
    }
    context.continue_with(Box::new(Filter {
        ptr,
        items,
        extra,
        asked: None,
        kept_keys: Vec::new(),
        kept: Vec::new(),
    }));
    Ok(())
}

/// The walk: the element whose verdict is in flight, and the ones kept.
#[derive(Debug, Clone)]
struct Filter {
    ptr: Arc<FunctionPtr>,
    items: Items,
    extra: Vec<LpcRef>,
    /// The element asked about, with its key for a mapping.
    asked: Option<(Option<LpcRef>, LpcRef)>,
    kept_keys: Vec<LpcRef>,
    kept: Vec<LpcRef>,
}

impl Continuation for Filter {
    fn advance(&mut self, result: Option<LpcRef>, txn: &TxnHandle) -> Result<Next> {
        if let (Some(verdict), Some((key, value))) = (result, self.asked.take())
            && verdict.is_truthy(txn)
        {
            if let Some(key) = key {
                self.kept_keys.push(key);
            }
            self.kept.push(value);
        }
        let args = match &mut self.items {
            Items::Array(items) => match items.next() {
                Some(item) => {
                    self.asked = Some((None, item.clone()));
                    callback_args(&[item], &self.extra)
                }
                None => return Ok(Next::Done(mint_array(txn, std::mem::take(&mut self.kept)))),
            },
            Items::Mapping(entries) => match entries.next() {
                Some((key, value)) => {
                    self.asked = Some((Some(key.clone()), value.clone()));
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
    async fn filter_keeps_the_items_the_function_accepts() {
        let code = "int *create() { return filter(({ 1, 2, 3, 4 }), (: $1 % 2 == 0 :)); }";
        assert_eq!(strings_of(code).await, ["2", "4"]);
    }

    #[tokio::test]
    async fn filter_passes_the_extra_arguments_after_the_item() {
        let code = "int *create() { return filter(({ 1, 2, 3 }), (: $1 > $2 :), 1); }";
        assert_eq!(strings_of(code).await, ["2", "3"]);
    }

    #[tokio::test]
    async fn filter_of_a_mapping_gets_the_key_and_the_value() {
        let code = r#"
            mixed *create() {
                return keys(filter(([ "a": 1, "b": 2, "c": 3 ]), (: $2 >= 2 :)));
            }
        "#;
        assert_eq!(strings_of(code).await, ["b", "c"]);
    }

    #[tokio::test]
    async fn filter_returns_a_new_array() {
        let code = "int create() { mixed *a = ({ 1 }); return filter(a, (: 1 :)) == a; }";
        assert_eq!(run_prog(code).await.result(), Some(LpcRef::from(0)));
    }

    #[tokio::test]
    async fn the_functions_error_is_the_callers() {
        let code = r#"mixed create() { return filter(({ 1 }), (: throw("boom") :)); }"#;
        let err = try_run_prog(code).await.unwrap_err().to_string();
        assert!(err.contains("boom"), "{err}");
    }

    #[tokio::test]
    async fn filter_of_a_non_collection_is_an_error() {
        let code = "mixed create() { mixed x = 1; return filter(x, (: 1 :)); }";
        let err = try_run_prog(code).await.unwrap_err().to_string();
        assert!(
            err.contains("filter: int is not an array or mapping"),
            "{err}"
        );
    }

    #[tokio::test]
    async fn filter_with_a_non_function_is_an_error() {
        let code = "mixed create() { mixed f = 1; return filter(({ 1 }), f); }";
        let err = try_run_prog(code).await.unwrap_err().to_string();
        assert!(err.contains("filter: int is not a function"), "{err}");
    }

    #[tokio::test]
    async fn filter_runs_its_first_callback_with_no_await() {
        let vm = Vm::new(test_config());
        let code = indoc! { r#"
            int one(int x) { return x; }
            int *got;
            void create() { got = filter(({ 1, 2 }), &one()); }
        "# };
        let (mut task, _live) =
            task_at(&vm, code, |at| matches!(at, Instruction::CallEfun(..))).await;

        let slice = task.run_slice(&mut 1).unwrap();

        assert!(matches!(slice, Slice::Budget));
        assert_eq!(task.stack.current_frame().unwrap().function.name(), "one");
        assert!(task.stack.get(0).unwrap().pending.is_some());
    }

    #[tokio::test]
    async fn filter_of_a_mapping_keeps_the_kept_keys_values() {
        let code = r#"
            mixed *create() {
                return values(filter(([ "a": 1, "b": 2, "c": 3 ]), (: $2 != 2 :)));
            }
        "#;
        assert_eq!(strings_of(code).await, ["1", "3"]);
    }

    #[tokio::test]
    async fn filter_of_an_empty_array_needs_no_callback() {
        let code = "int create() { return sizeof(filter(({ }), (: 1 / 0 :))); }";
        assert_eq!(run_prog(code).await.result(), Some(LpcRef::from(0)));
    }
}
