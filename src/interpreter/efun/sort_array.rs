use std::cmp::Ordering;

use lpc_rs_errors::Result;

use crate::interpreter::{
    efun::{
        callback::{call_back, function_arg},
        efun_context::EfunContext,
    },
    function_type::function_ptr::FunctionPtr,
    lpc_ref::LpcRef,
};

/// How two items are ranked.
enum Order<'a> {
    /// The comparator `f(a, b)`: negative puts `a` first, positive `b`.
    By(&'a FunctionPtr),
    /// `LpcRef::natural_cmp`, reversed when descending.
    Natural { descending: bool },
}

/// `sort_array(arr, f | direction)`: a new array of `arr`'s items ordered by
/// the comparator `f(a, b)` (negative puts `a` first, positive `b`), or in
/// natural order, descending for a direction of -1. Stable.
pub async fn sort_array<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let items: Vec<LpcRef> = match context.arg(0) {
        array @ LpcRef::Array(_) => {
            array.with_array(context.txn(), |a| a.iter().cloned().collect())?
        }
        other => {
            return Err(
                context.runtime_error(format!("sort_array: {} is not an array", other.type_name()))
            );
        }
    };
    let sorted = match context.arg(1) {
        LpcRef::Function(_) => {
            let f = function_arg(context, "sort_array", 1)?;
            insertion_sort(context, &Order::By(&f), items).await?
        }
        LpcRef::Int(direction) => {
            let order = Order::Natural {
                descending: direction.0 == -1,
            };
            insertion_sort(context, &order, items).await?
        }
        other => {
            return Err(context.runtime_error(format!(
                "sort_array: {} is not a function or an int",
                other.type_name()
            )));
        }
    };
    context.return_array(sorted);
    Ok(())
}

/// `a` against `b` under `order`.
async fn compare<const N: usize>(
    context: &EfunContext<'_, N>,
    order: &Order<'_>,
    a: &LpcRef,
    b: &LpcRef,
) -> Result<Ordering> {
    match order {
        Order::By(f) => {
            let verdict = call_back(context, "sort_array", f, &[a.clone(), b.clone()]).await?;
            match verdict {
                LpcRef::Int(i) => Ok(i.0.cmp(&0)),
                other => Err(context.runtime_error(format!(
                    "sort_array: the comparator returned {}, not an int",
                    other.type_name()
                ))),
            }
        }
        Order::Natural { descending } => {
            let Some(ordering) = a.natural_cmp(b) else {
                return Err(context.runtime_error(format!(
                    "sort_array: cannot order {} and {}",
                    a.type_name(),
                    b.type_name()
                )));
            };
            Ok(if *descending {
                ordering.reverse()
            } else {
                ordering
            })
        }
    }
}

/// Stable binary insertion: each item lands after every item `order` does
/// not put it before, so equal items keep their order. Comparisons are
/// N log N; each may be an LPC call.
async fn insertion_sort<const N: usize>(
    context: &EfunContext<'_, N>,
    order: &Order<'_>,
    items: Vec<LpcRef>,
) -> Result<Vec<LpcRef>> {
    let mut sorted: Vec<LpcRef> = Vec::with_capacity(items.len());
    for item in items {
        let (mut low, mut high) = (0, sorted.len());
        while low < high {
            let mid = low + (high - low) / 2;
            if compare(context, order, &sorted[mid], &item).await? == Ordering::Greater {
                high = mid;
            } else {
                low = mid + 1;
            }
        }
        sorted.insert(low, item);
    }
    Ok(sorted)
}

#[cfg(test)]
mod tests {
    use crate::{
        interpreter::lpc_ref::LpcRef,
        test_support::{run_prog, try_run_prog},
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
}
