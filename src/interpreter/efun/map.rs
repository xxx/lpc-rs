//! `map(collection, f, extra...)`: `f(element, extra...)` per element, the
//! results in a fresh array, or a fresh mapping over the same keys.

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

/// `map(collection, f, extra...)`: `f(element, extra...)` per element, the
/// results in a fresh array or mapping.
pub fn map<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let items = items_arg(context, "map")?;
    let ptr = function_arg(context, "map", 1)?;
    let extra = extra_args(context, 2);
    if items.len() == 0 {
        return_empty(context, &items);
        return Ok(());
    }
    context.continue_with(Box::new(Map {
        ptr,
        items,
        extra,
        keys: Vec::new(),
        results: Vec::new(),
    }));
    Ok(())
}

/// The walk: one callback per element, its answer kept in order.
#[derive(Debug, Clone)]
struct Map {
    ptr: Arc<FunctionPtr>,
    items: Items,
    extra: Vec<LpcRef>,
    /// A mapping's keys, in `results` order.
    keys: Vec<LpcRef>,
    results: Vec<LpcRef>,
}

impl Continuation for Map {
    fn advance(&mut self, result: Option<LpcRef>, txn: &TxnHandle) -> Result<Next> {
        if let Some(value) = result {
            self.results.push(value);
        }
        let args = match &mut self.items {
            Items::Array(items) => match items.next() {
                Some(item) => callback_args(&[item], &self.extra),
                None => {
                    return Ok(Next::Done(mint_array(
                        txn,
                        std::mem::take(&mut self.results),
                    )));
                }
            },
            Items::Mapping(entries) => match entries.next() {
                Some((key, value)) => {
                    self.keys.push(key.clone());
                    callback_args(&[key, value], &self.extra)
                }
                None => {
                    return Ok(Next::Done(mint_mapping(
                        txn,
                        std::mem::take(&mut self.keys),
                        std::mem::take(&mut self.results),
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
        interpreter::{
            lpc_ref::LpcRef,
            task::eval_loop::{AsyncCall, Slice},
            vm::Vm,
        },
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
    async fn map_replaces_each_item_with_the_functions_result() {
        let code = "int *create() { return map(({ 1, 2 }), (: $1 * 10 :)); }";
        assert_eq!(strings_of(code).await, ["10", "20"]);
    }

    #[tokio::test]
    async fn map_passes_the_extra_arguments_after_the_item() {
        let code = "int *create() { return map(({ 1, 2 }), (: $1 + $2 :), 5); }";
        assert_eq!(strings_of(code).await, ["6", "7"]);
    }

    #[tokio::test]
    async fn map_of_a_mapping_keeps_the_keys() {
        let code = r#"mixed *create() { return keys(map(([ "a": 1, "b": 2 ]), (: $2 + 1 :))); }"#;
        assert_eq!(strings_of(code).await, ["a", "b"]);
    }

    #[tokio::test]
    async fn map_of_a_mapping_maps_each_value_from_key_and_value() {
        let code = r#"mixed *create() { return values(map(([ "a": 1, "b": 2 ]), (: $2 + 1 :))); }"#;
        assert_eq!(strings_of(code).await, ["2", "3"]);
    }

    #[tokio::test]
    async fn map_returns_a_new_mapping() {
        let code = "int create() { mapping m = ([ ]); return map(m, (: $2 :)) == m; }";
        assert_eq!(run_prog(code).await.result(), Some(LpcRef::from(0)));
    }

    #[tokio::test]
    async fn map_of_a_non_collection_is_an_error() {
        let code = r#"mixed create() { mixed x = "s"; return map(x, (: 1 :)); }"#;
        let err = try_run_prog(code).await.unwrap_err().to_string();
        assert!(
            err.contains("map: string is not an array or mapping"),
            "{err}"
        );
    }

    #[tokio::test]
    async fn map_runs_its_first_callback_with_no_await() {
        let vm = Vm::new(test_config());
        let code = indoc! { r#"
            int one(int x) { return x; }
            int *got;
            void create() { got = map(({ 1, 2 }), &one()); }
        "# };
        let (mut task, _live) =
            task_at(&vm, code, |at| matches!(at, Instruction::CallEfun(..))).await;

        let slice = task.run_slice(&mut 1).unwrap();

        assert!(matches!(slice, Slice::Budget));
        assert_eq!(task.stack.len(), 2);
        let callee = task.stack.current_frame().unwrap();
        assert_eq!(callee.function.name(), "one");
        assert!(callee.external);
        assert_eq!(callee.registers[1], LpcRef::from(1));
        assert!(task.stack.get(0).unwrap().pending.is_some());
    }

    #[tokio::test]
    async fn a_returning_callback_starts_the_next_with_no_await() {
        let vm = Vm::new(test_config());
        let code = indoc! { r#"
            int one(int x) { return x; }
            int *got;
            void create() { got = map(({ 1, 2 }), &one()); }
        "# };
        let (mut task, _live) =
            task_at(&vm, code, |at| matches!(at, Instruction::CallEfun(..))).await;
        task.run_slice(&mut 1).unwrap();

        for _ in 0..16 {
            assert!(matches!(task.run_slice(&mut 1).unwrap(), Slice::Budget));
            if task.stack.get(0).unwrap().pending.is_none() {
                break;
            }
            assert!(task.stack.len() >= 2, "the caller never surfaces mid-walk");
        }

        assert!(task.stack.get(0).unwrap().pending.is_none());
    }

    #[tokio::test]
    async fn map_with_a_plain_efun_pointer_runs_it_in_an_entry_frame() {
        let vm = Vm::new(test_config());
        let code = indoc! { r#"
            int *got;
            void create() { got = map(({ 1, "a" }), &intp()); }
        "# };
        let (mut task, _live) =
            task_at(&vm, code, |at| matches!(at, Instruction::CallEfun(..))).await;

        let slice = task.run_slice(&mut 1).unwrap();

        assert!(matches!(slice, Slice::Budget));
        assert_eq!(task.stack.len(), 2);
        assert!(task.stack.current_frame().unwrap().is_entry());
    }

    #[tokio::test]
    async fn map_with_a_suspending_efun_pointer_awaits_the_pending_call() {
        let vm = Vm::new(test_config());
        let code = indoc! { r#"
            mixed *got;
            void create() { got = map(({ "/nowhere" }), &find_object()); }
        "# };
        let (mut task, _live) =
            task_at(&vm, code, |at| matches!(at, Instruction::CallEfun(..))).await;

        let slice = task.run_slice(&mut 1).unwrap();

        assert!(matches!(slice, Slice::Await(AsyncCall::Pending)));
        assert_eq!(task.stack.len(), 1);
    }

    #[tokio::test]
    async fn map_with_a_suspending_efun_pointer_answers() {
        let code =
            r#"mixed *create() { return map(({ "/nowhere", "/nowhere" }), &find_object()); }"#;
        assert_eq!(strings_of(code).await, ["0", "0"]);
    }

    #[tokio::test]
    async fn a_callback_efun_whose_own_callback_suspends_awaits_the_pending_call() {
        let vm = Vm::new(test_config());
        let code = indoc! { r#"
            int *got;
            void create() { got = map(({ ({ "/nowhere" }) }), &map(, &find_object())); }
        "# };
        let (mut task, _live) =
            task_at(&vm, code, |at| matches!(at, Instruction::CallEfun(..))).await;

        let slice = task.run_slice(&mut 1).unwrap();

        assert!(matches!(slice, Slice::Await(AsyncCall::Pending)));
        assert_eq!(task.stack.len(), 2);
        let top = task.stack.current_frame().unwrap();
        assert!(top.is_entry());
        assert!(top.pending.is_some());
        assert!(task.stack.get(0).unwrap().pending.is_some());
    }

    #[tokio::test]
    async fn a_callback_efun_whose_own_callback_suspends_answers() {
        let code = indoc! { r#"
            int create() {
                mixed *r = map(({ ({ "/nowhere", "/nowhere" }) }), &map(, &find_object()));
                return sizeof(r[0]);
            }
        "# };
        assert_eq!(run_prog(code).await.result(), Some(LpcRef::from(2)));
    }

    #[tokio::test]
    async fn map_of_an_empty_array_needs_no_callback() {
        let code = "int create() { return sizeof(map(({ }), (: 1 / 0 :))); }";
        assert_eq!(run_prog(code).await.result(), Some(LpcRef::from(0)));
    }

    #[tokio::test]
    async fn previous_object_inside_a_callback_is_the_caller() {
        let code = "int create() { int *r = map(({ 1 }), (: previous_object() == this_object() :)); return r[0]; }";
        assert_eq!(run_prog(code).await.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn a_callback_calling_map_twenty_deep_succeeds() {
        let code = indoc! { r#"
            int deep(int n) { int *r; if (n == 0) { return 0; } r = map(({ n - 1 }), &deep()); return 1 + r[0]; }
            int create() { return deep(20); }
        "# };
        assert_eq!(run_prog(code).await.result(), Some(LpcRef::from(20)));
    }

    #[tokio::test]
    async fn a_caught_callback_error_leaves_the_frame_walking() {
        let code = indoc! { r#"
            string err;
            int create() { err = catch(map(({ 1 }), (: throw("x") :))); return sizeof(map(({ 1, 2 }), (: $1 :))); }
        "# };
        assert_eq!(run_prog(code).await.result(), Some(LpcRef::from(2)));
    }

    #[tokio::test]
    async fn map_with_a_dynamic_pointer_calls_each_element() {
        let code = indoc! { r#"
            int one(int x) { return x; }
            int create() { int *r = map(({ this_object() }), &->one(5)); return r[0]; }
        "# };
        assert_eq!(run_prog(code).await.result(), Some(LpcRef::from(5)));
    }
}
