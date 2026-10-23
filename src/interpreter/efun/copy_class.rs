use lpc_rs_errors::Result;

use crate::interpreter::{efun::efun_context::EfunContext, lpc_class};

/// `copy_class(value)`: a fresh instance with all retained views and shallow field values.
pub fn copy_class<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let copy = lpc_class::copy_class(context.arg(0), context.txn())?;
    context.return_efun_result(copy);
    Ok(())
}

#[cfg(test)]
mod tests {
    use crate::{interpreter::lpc_ref::LpcRef, test_support::run_prog};

    #[tokio::test]
    async fn copies_keep_nominal_type_and_have_independent_fields_and_identity() {
        let result = run_prog(
            r#"
            class record { int n; }
            class other { int n; }
            int accept(class record r) { return r.n; }
            int create() {
                class record original = new(class record, n: 7);
                class record copy = copy_class(original);
                mapping keys = ([ original: 1, copy: 2 ]);
                int initial = accept(copy);
                copy.n = 12;
                mixed wrong = catch((class other)copy_class(original));
                return initial == 7 && original.n == 7 && copy.n == 12 &&
                    original != copy && keys[original] == 1 && keys[copy] == 2 &&
                    stringp(wrong) && classp(copy);
            }
        "#,
        )
        .await;
        assert_eq!(result.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn copies_share_nested_references_and_captured_function_state() {
        let result = run_prog(
            r#"
            class record {
                int *items;
                mapping labels;
                class record child;
                object owner;
                function callback;
                int n;
            }
            int create() {
                int counter;
                class record original = new(class record,
                    items: ({ 1 }), labels: ([ "x": 2 ]),
                    child: new(class record, n: 3), owner: this_object(),
                    callback: (: ++counter :));
                class record copy = copy_class(original);
                copy.items[0] = 10;
                copy.labels["x"] = 20;
                copy.child.n = 30;
                int first = copy.callback();
                int second = original.callback();
                return original.items[0] == 10 && original.labels["x"] == 20 &&
                    original.child.n == 30 && copy.owner == original.owner &&
                    copy.callback == original.callback && first == 1 && second == 2;
            }
        "#,
        )
        .await;
        assert_eq!(result.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn copying_a_cycle_keeps_references_to_the_original() {
        let result = run_prog(
            r#"
            class record { class record next; }
            int create() {
                class record original = new(class record);
                original.next = original;
                class record copy = copy_class(original);
                return copy != original && copy.next == original && original.next == original;
            }
        "#,
        )
        .await;
        assert_eq!(result.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn non_class_arguments_are_catchable_errors() {
        let result = run_prog(
            r#"
            int create() {
                mixed *values = ({ 0, 1, 1.5, "x", ({}), ([]),
                    this_object(), (: 1 :), to_bytes("x", "utf8") });
                foreach (mixed value : values) {
                    mixed error = catch(copy_class(value));
                    if (!stringp(error)) return 0;
                }
                return 1;
            }
        "#,
        )
        .await;
        assert_eq!(result.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn empty_classes_and_efun_function_pointers_are_supported() {
        let result = run_prog(
            r#"
            class empty {}
            int create() {
                function copier = &copy_class();
                function predicate = &classp();
                class empty original = new(class empty);
                class empty copy = copier(original);
                return predicate(copy) && copy != original && typeof(copy) == 9;
            }
        "#,
        )
        .await;
        assert_eq!(result.result(), Some(LpcRef::from(1)));
    }
}
