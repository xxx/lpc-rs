//! Left-to-right array reduction through `f(accumulator, element)`.

use std::{sync::Arc, vec};

use lpc_rs_errors::Result;
use smallvec::smallvec;

use crate::interpreter::{
    continuation::{Callee, Continuation, Next},
    efun::{
        callback::{array_arg, function_arg},
        efun_context::EfunContext,
    },
    function_type::function_ptr::FunctionPtr,
    lpc_ref::{LpcRef, NULL},
    stm::TxnHandle,
};

/// `reduce(items, f, initial?)`: fold an array with `f(accumulator, element)`.
pub fn reduce<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let mut items = array_arg(context, "reduce")?;
    let ptr = function_arg(context, "reduce", 1)?;
    let initial = if context.arg_count() > 2 {
        context.arg(2).clone()
    } else {
        items.next().unwrap_or(NULL)
    };

    if items.len() == 0 {
        context.return_efun_result(initial);
        return Ok(());
    }

    context.continue_with(Box::new(Reduce {
        ptr,
        items,
        initial,
    }));
    Ok(())
}

#[derive(Debug, Clone)]
struct Reduce {
    ptr: Arc<FunctionPtr>,
    items: vec::IntoIter<LpcRef>,
    initial: LpcRef,
}

impl Continuation for Reduce {
    fn advance(&mut self, result: Option<LpcRef>, _txn: &TxnHandle) -> Result<Next> {
        let accumulator = result.unwrap_or_else(|| std::mem::take(&mut self.initial));
        Ok(match self.items.next() {
            Some(item) => Next::Call(Callee::Pointer {
                ptr: self.ptr.clone(),
                args: smallvec![accumulator, item],
            }),
            None => Next::Done(accumulator),
        })
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
        test_support::{run_prog, strings_of, task_at, test_config, try_run_prog},
    };

    #[tokio::test]
    async fn folds_left_with_the_accumulator_before_the_element() {
        let code = "int create() { return reduce(({ 20, 3, 2 }), &operator(-)()); }";
        assert_eq!(run_prog(code).await.result(), Some(LpcRef::from(15)));
    }

    #[tokio::test]
    async fn omitted_initial_uses_the_first_element_but_explicit_zero_does_not() {
        let code = indoc! { r#"
            mixed *create() {
                return ({
                    reduce(({ 2, 3, 4 }), &operator(*)()),
                    reduce(({ 2, 3, 4 }), &operator(*)(), 0),
                    reduce(({ 2, 3, 4 }), &operator(*)(), 10)
                });
            }
        "# };
        assert_eq!(strings_of(code).await, ["24", "0", "240"]);
    }

    #[tokio::test]
    async fn empty_array_returns_the_initial_or_zero_without_calling_back() {
        let code = indoc! { r#"
            mixed *create() {
                mapping initial = ([ "a": 1 ]);
                return ({
                    reduce(({}), (: 1 / 0 :)),
                    reduce(({}), (: 1 / 0 :), 0),
                    reduce(({}), (: 1 / 0 :), initial) == initial,
                    reduce(({}), (: 1 / 0 :), "unchanged")
                });
            }
        "# };
        assert_eq!(strings_of(code).await, ["0", "0", "1", "unchanged"]);
    }

    #[tokio::test]
    async fn unseeded_singleton_returns_its_element_without_calling_back() {
        let code = indoc! { r#"
            int create() {
                mixed *item = ({ "a" });
                return reduce(({ item }), (: 1 / 0 :)) == item;
            }
        "# };
        assert_eq!(run_prog(code).await.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn seeded_singleton_calls_back_once_with_exactly_two_arguments() {
        let code = indoc! { r#"
            int calls;
            int fold(int acc, int item, ...) {
                if (sizeof(argv)) { throw("extra arguments"); }
                calls++;
                return acc - item;
            }
            mixed *create() {
                int result = reduce(({ 3 }), &fold(), 10);
                return ({ result, calls });
            }
        "# };
        assert_eq!(strings_of(code).await, ["7", "1"]);
    }

    #[tokio::test]
    async fn accumulator_can_change_type_between_callbacks() {
        let code = indoc! { r#"
            mixed fold(mixed acc, mixed item) {
                if (intp(acc)) { return "" + acc + item; }
                return ({ acc, item });
            }
            mixed *create() { return reduce(({ 1, "a", 7 }), &fold()); }
        "# };
        assert_eq!(strings_of(code).await, ["1a", "7"]);
    }

    #[tokio::test]
    async fn array_accumulator_can_collect_elements() {
        let code = indoc! { r#"
            mixed *create() {
                return reduce(({ 1, 2, 3 }), (: $1 + ({ $2 }) :), ({}));
            }
        "# };
        assert_eq!(strings_of(code).await, ["1", "2", "3"]);
    }

    #[tokio::test]
    async fn bound_reduce_pointers_work_as_map_callbacks() {
        let code = indoc! { r#"
            mixed *create() {
                mixed *arrays = ({ ({ 2, 3 }), ({}), ({ 4 }) });
                return map(arrays, &reduce(, &operator(*)(,)))
                    + map(arrays, &reduce(, &operator(*)(,), 0));
            }
        "# };
        assert_eq!(strings_of(code).await, ["6", "0", "4", "0", "0", "0"]);
    }

    #[tokio::test]
    async fn spread_arguments_preserve_whether_initial_was_supplied() {
        let code = indoc! { r#"
            mixed *create() {
                mixed *args = ({ ({ 2, 3, 4 }), &operator(*)() });
                function f = &reduce();
                return ({ reduce(args...), reduce(args..., 0), f(args...), f(args..., 0) });
            }
        "# };
        assert_eq!(strings_of(code).await, ["24", "0", "24", "0"]);
    }

    #[tokio::test]
    async fn callback_context_can_be_partially_applied() {
        let code = indoc! { r#"
            int fold(int scale, int acc, int item) { return acc + scale * item; }
            int create() { return reduce(({ 1, 2 }), &fold(10, , ), 0); }
        "# };
        assert_eq!(run_prog(code).await.result(), Some(LpcRef::from(30)));
    }

    #[tokio::test]
    async fn non_arrays_are_runtime_errors() {
        for (value, kind) in [("0", "int"), (r#""abc""#, "string"), ("([])", "mapping")] {
            let code = format!(
                "mixed create() {{ mixed x = {value}; return reduce(x, &operator(+)()); }}"
            );
            let err = try_run_prog(&code).await.unwrap_err().to_string();
            assert!(
                err.contains(&format!("reduce: {kind} is not an array")),
                "{err}"
            );
        }
    }

    #[tokio::test]
    async fn non_functions_are_errors_even_when_no_callback_is_needed() {
        for items in ["({})", "({ 1 })", "({ 1, 2 })"] {
            let code = format!("mixed create() {{ mixed f = 0; return reduce({items}, f); }}");
            let err = try_run_prog(&code).await.unwrap_err().to_string();
            assert!(err.contains("reduce: int is not a function"), "{err}");
        }
    }

    #[tokio::test]
    async fn callbacks_run_on_the_calling_tasks_stack() {
        let vm = Vm::new(test_config());
        let code = indoc! { r#"
            int fold(int acc, int item) { return acc + item; }
            int create() { return reduce(({ 1, 2, 3 }), &fold()); }
        "# };
        let (mut task, _live) =
            task_at(&vm, code, |at| matches!(at, Instruction::CallEfun(..))).await;

        assert!(matches!(task.run_slice(&mut 1).unwrap(), Slice::Budget));
        assert_eq!(task.stack.len(), 2);
        let callee = task.stack.current_frame().unwrap();
        assert_eq!(callee.function.name(), "fold");
        assert!(callee.external);
        assert_eq!(callee.registers[1], LpcRef::from(1));
        assert_eq!(callee.registers[2], LpcRef::from(2));
        assert!(task.stack.get(0).unwrap().pending.is_some());
    }

    #[tokio::test]
    async fn callback_can_suspend_and_observe_its_caller() {
        let code = indoc! { r#"
            int fold(int acc, int item) {
                find_object("/nowhere");
                if (previous_object() != this_object()) { throw("wrong caller"); }
                return acc + item;
            }
            int create() { return reduce(({ 1, 2, 3 }), &fold(), 0); }
        "# };
        assert_eq!(run_prog(code).await.result(), Some(LpcRef::from(6)));
    }

    #[tokio::test]
    async fn callback_mutations_do_not_replace_the_snapshotted_elements() {
        let code = indoc! { r#"
            int *items = ({ 1, 2, 3 });
            int fold(int acc, int item) {
                items[2] = 100;
                return acc + item;
            }
            mixed *create() {
                int result = reduce(items, &fold(), 0);
                return ({ result, items[2] });
            }
        "# };
        assert_eq!(strings_of(code).await, ["6", "100"]);
    }

    #[tokio::test]
    async fn caught_callback_error_stops_the_fold_and_allows_another() {
        let code = indoc! { r#"
            int calls;
            int fold(int acc, int item) {
                calls++;
                if (item == 2) { throw("stop"); }
                return acc + item;
            }
            mixed *create() {
                mixed err = catch(reduce(({ 1, 2, 3 }), &fold(), 0));
                return ({ err, calls, reduce(({ 4, 5 }), &operator(+)()) });
            }
        "# };
        assert_eq!(strings_of(code).await, ["stop", "2", "9"]);
    }
}
