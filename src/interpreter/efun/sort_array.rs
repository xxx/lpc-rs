//! `sort_array(array, f | direction)`: a fresh array ordered by the
//! comparator `f(a, b)` (positive when `a` goes after `b`), or naturally,
//! `1` ascending and `-1` descending. Binary insertion: stable.

use std::{cmp::Ordering, sync::Arc, vec};

use lpc_rs_errors::{LpcError, Result};
use smallvec::SmallVec;

use crate::interpreter::{
    continuation::{Callee, Continuation, Next},
    efun::{
        callback::{function_arg, mint_array},
        efun_context::EfunContext,
    },
    function_type::function_ptr::FunctionPtr,
    lpc_ref::LpcRef,
    stm::TxnHandle,
};

/// `sort_array(array, f | direction)`: a fresh array ordered by the
/// comparator `f(a, b)` (positive when `a` goes after `b`), or naturally,
/// `1` ascending and `-1` descending. Binary insertion: stable.
pub fn sort_array<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let items: Vec<LpcRef> = match context.arg(0) {
        array @ LpcRef::Array(_) => array.with_array(context.txn(), |a| a.to_vec())?,
        other => {
            return Err(
                context.runtime_error(format!("sort_array: {} is not an array", other.type_name()))
            );
        }
    };
    match context.arg(1) {
        LpcRef::Function(_) => {
            let ptr = function_arg(context, "sort_array", 1)?;
            if items.len() < 2 {
                context.return_array(items);
                return Ok(());
            }
            context.continue_with(Box::new(Sort {
                ptr,
                remaining: items.into_iter(),
                sorted: Vec::new(),
                placing: None,
            }));
            Ok(())
        }
        LpcRef::Int(direction) => {
            let sorted = natural_sort(context, items, direction.0 == -1)?;
            context.return_array(sorted);
            Ok(())
        }
        other => Err(context.runtime_error(format!(
            "sort_array: {} is not a function or an int",
            other.type_name()
        ))),
    }
}

/// `items` by `LpcRef::natural_cmp`, reversed when `descending`.
fn natural_sort<const N: usize>(
    context: &EfunContext<'_, N>,
    items: Vec<LpcRef>,
    descending: bool,
) -> Result<Vec<LpcRef>> {
    let mut sorted: Vec<LpcRef> = Vec::with_capacity(items.len());
    for item in items {
        let (mut low, mut high) = (0, sorted.len());
        while low < high {
            let mid = low + (high - low) / 2;
            let Some(ordering) = sorted[mid].natural_cmp(&item) else {
                return Err(context.runtime_error(format!(
                    "sort_array: cannot order {} and {}",
                    sorted[mid].type_name(),
                    item.type_name()
                )));
            };
            let ordering = if descending {
                ordering.reverse()
            } else {
                ordering
            };
            if ordering == Ordering::Greater {
                high = mid;
            } else {
                low = mid + 1;
            }
        }
        sorted.insert(low, item);
    }
    Ok(sorted)
}

/// The walk: the item being placed and the open range of `sorted` it
/// belongs in; each compare narrows it.
#[derive(Debug, Clone)]
struct Sort {
    ptr: Arc<FunctionPtr>,
    remaining: vec::IntoIter<LpcRef>,
    sorted: Vec<LpcRef>,
    placing: Option<(LpcRef, usize, usize)>,
}

impl Continuation for Sort {
    fn advance(&mut self, result: Option<LpcRef>, txn: &TxnHandle) -> Result<Next> {
        if let Some(verdict) = result {
            let Some((_, low, high)) = &mut self.placing else {
                return Err(LpcError::runtime_bug(
                    "a compare answered with nothing placed",
                ));
            };
            let LpcRef::Int(answer) = verdict else {
                return Err(LpcError::runtime(format!(
                    "sort_array: the comparator returned {}, not an int",
                    verdict.type_name()
                )));
            };
            let mid = *low + (*high - *low) / 2;
            if answer.0 > 0 {
                *high = mid;
            } else {
                *low = mid + 1;
            }
        }
        loop {
            match self.placing.take() {
                Some((item, low, high)) if low < high => {
                    let mid = low + (high - low) / 2;
                    let args = SmallVec::from_vec(vec![self.sorted[mid].clone(), item.clone()]);
                    self.placing = Some((item, low, high));
                    return Ok(Next::Call(Callee::Pointer {
                        ptr: self.ptr.clone(),
                        args,
                    }));
                }
                Some((item, low, _)) => self.sorted.insert(low, item),
                None => match self.remaining.next() {
                    Some(item) => self.placing = Some((item, 0, self.sorted.len())),
                    None => {
                        return Ok(Next::Done(mint_array(
                            txn,
                            std::mem::take(&mut self.sorted),
                        )));
                    }
                },
            }
        }
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
    async fn sort_array_orders_by_the_comparator() {
        let code = "int *create() { return sort_array(({ 3, 1, 2 }), (: $1 - $2 :)); }";
        assert_eq!(strings_of(code).await, ["1", "2", "3"]);
    }

    #[tokio::test]
    async fn a_positive_result_puts_the_first_argument_after_the_second() {
        let code = "int *create() { return sort_array(({ 1, 2, 3 }), (: $2 - $1 :)); }";
        assert_eq!(strings_of(code).await, ["3", "2", "1"]);
    }

    #[tokio::test]
    async fn the_sort_is_stable() {
        let code = r#"
            string *create() {
                mixed *rows = ({ ({ 1, "a" }), ({ 0, "b" }), ({ 1, "c" }) });
                return map(sort_array(rows, (: $1[0] - $2[0] :)), (: $1[1] :));
            }
        "#;
        assert_eq!(strings_of(code).await, ["b", "a", "c"]);
    }

    #[tokio::test]
    async fn an_int_direction_sorts_in_natural_order() {
        let code = r#"string *create() { return sort_array(({ "b", "a", "c" }), 1); }"#;
        assert_eq!(strings_of(code).await, ["a", "b", "c"]);
    }

    #[tokio::test]
    async fn a_negative_direction_sorts_descending() {
        let code = "int *create() { return sort_array(({ 1, 3, 2 }), -1); }";
        assert_eq!(strings_of(code).await, ["3", "2", "1"]);
    }

    #[tokio::test]
    async fn natural_order_spans_ints_and_floats() {
        let code = "mixed *create() { return sort_array(({ 2, 1.5, 1 }), 1); }";
        assert_eq!(strings_of(code).await, ["1", "1.5", "2"]);
    }

    #[tokio::test]
    async fn sort_array_returns_a_new_array() {
        let code = "int create() { mixed *a = ({ 1 }); return sort_array(a, 1) == a; }";
        assert_eq!(run_prog(code).await.result(), Some(LpcRef::from(0)));
    }

    #[tokio::test]
    async fn an_empty_array_sorts_to_an_empty_array() {
        let code = "mixed *create() { return sort_array(({ }), (: $1 - $2 :)); }";
        assert!(strings_of(code).await.is_empty());
    }

    #[tokio::test]
    async fn a_one_element_array_sorts_with_no_callback() {
        let code = "int *create() { return sort_array(({ 7 }), (: 1 / 0 :)); }";
        assert_eq!(strings_of(code).await, ["7"]);
    }

    #[tokio::test]
    async fn a_non_int_comparator_result_is_an_error() {
        let code = r#"mixed create() { return sort_array(({ 1, 2 }), (: "x" :)); }"#;
        let err = try_run_prog(code).await.unwrap_err().to_string();
        assert!(
            err.contains("sort_array: the comparator returned string, not an int"),
            "{err}"
        );
    }

    #[tokio::test]
    async fn items_natural_order_cannot_rank_are_an_error() {
        let code = r#"mixed create() { return sort_array(({ 1, "a" }), 1); }"#;
        let err = try_run_prog(code).await.unwrap_err().to_string();
        assert!(
            err.contains("sort_array: cannot order int and string"),
            "{err}"
        );
    }

    #[tokio::test]
    async fn a_non_array_is_an_error() {
        let code = "mixed create() { mixed x = 1; return sort_array(x, 1); }";
        let err = try_run_prog(code).await.unwrap_err().to_string();
        assert!(err.contains("sort_array: int is not an array"), "{err}");
    }

    #[tokio::test]
    async fn sort_array_runs_its_first_compare_with_no_await() {
        let vm = Vm::new(test_config());
        let code = indoc! { r#"
            int cmp(int a, int b) { return a - b; }
            int *got;
            void create() { got = sort_array(({ 2, 1 }), &cmp()); }
        "# };
        let (mut task, _live) =
            task_at(&vm, code, |at| matches!(at, Instruction::CallEfun(..))).await;

        let slice = task.run_slice(&mut 1).unwrap();

        assert!(matches!(slice, Slice::Budget));
        let frame = task.stack.current_frame().unwrap();
        assert_eq!(frame.function.name(), "cmp");
        assert_eq!(frame.registers[1], LpcRef::from(2));
        assert_eq!(frame.registers[2], LpcRef::from(1));
    }

    #[tokio::test]
    async fn a_comparator_answering_a_non_int_fails_at_the_call_site() {
        let code = r#"mixed create() { return sort_array(({ 2, 1 }), (: "x" :)); }"#;
        let err = try_run_prog(code).await.unwrap_err();
        assert_eq!(
            err.to_string(),
            "runtime error: sort_array: the comparator returned string, not an int"
        );
        assert!(err.span().is_some());
    }

    #[tokio::test]
    async fn a_thousand_items_sort_through_the_comparator() {
        let code = indoc! { r#"
            int cmp(int a, int b) { return a - b; }
            int create() {
                int *a = allocate(1000);
                int i;
                for (i = 0; i < 1000; i++) a[i] = (i * 7919) % 1000;
                a = sort_array(a, &cmp());
                return a[0] == 0 && a[999] == 999 && a[500] == 500;
            }
        "# };
        assert_eq!(run_prog(code).await.result(), Some(LpcRef::from(1)));
    }
}
