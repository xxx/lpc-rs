//! `keys` and `values`: a mapping's keys and values in insertion order, so
//! `keys(m)[i]` pairs with `values(m)[i]`.

use lpc_rs_errors::Result;

use crate::interpreter::{efun::efun_context::EfunContext, lpc_ref::LpcRef};

/// Return `pick` of the mapping argument, its destructed-object keys
/// dropped first; a non-mapping is an error.
fn pick_from_mapping<const N: usize>(
    context: &mut EfunContext<'_, N>,
    name: &str,
    pick: fn(&crate::interpreter::lpc_mapping::LpcMapping) -> Vec<LpcRef>,
) -> Result<()> {
    let mapping = context.arg(0);
    if !matches!(mapping, LpcRef::Mapping(_)) {
        return Err(
            context.runtime_error(format!("{name}: {} is not a mapping", mapping.type_name()))
        );
    }
    mapping.drop_dead_keys(context.txn())?;
    let items = mapping.with_mapping(context.txn(), pick)?;
    context.return_array(items);
    Ok(())
}

/// `keys(m)`: the keys, in insertion order.
pub fn keys<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    pick_from_mapping(context, "keys", |m| m.keys().cloned().collect())
}

/// `values(m)`: the values, in insertion order.
pub fn values<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    pick_from_mapping(context, "values", |m| m.values().cloned().collect())
}

#[cfg(test)]
mod tests {
    use crate::test_support::{run_prog, try_run_prog};

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
    async fn keys_are_in_insertion_order() {
        let items = strings_of(r#"mixed *create() { return keys(([ "b": 1, "a": 2 ])); }"#).await;
        assert_eq!(items, ["b", "a"]);
    }

    #[tokio::test]
    async fn values_pair_with_keys_by_index() {
        let items = strings_of(r#"mixed *create() { return values(([ "b": 1, "a": 2 ])); }"#).await;
        assert_eq!(items, ["1", "2"]);
    }

    #[tokio::test]
    async fn keys_of_an_empty_mapping_is_empty() {
        let items = strings_of("mixed *create() { return keys(([ ])); }").await;
        assert!(items.is_empty());
    }

    #[tokio::test]
    async fn a_destructed_object_key_is_dropped_from_keys() {
        let code = r#"
            mixed *create() {
                object o = clone_object("/clone_target");
                mapping m = ([ o: 1, "k": 2 ]);
                destruct(o);
                return keys(m);
            }
        "#;
        assert_eq!(strings_of(code).await, ["k"]);
    }

    #[tokio::test]
    async fn a_destructed_object_key_is_dropped_from_values() {
        let code = r#"
            mixed *create() {
                object o = clone_object("/clone_target");
                mapping m = ([ o: 1, "k": 2 ]);
                destruct(o);
                return values(m);
            }
        "#;
        assert_eq!(strings_of(code).await, ["2"]);
    }

    #[tokio::test]
    async fn keys_of_a_non_mapping_is_an_error() {
        let err = try_run_prog("mixed *create() { mixed x = 1; return keys(x); }")
            .await
            .unwrap_err()
            .to_string();
        assert!(err.contains("keys: int is not a mapping"), "{err}");
    }

    #[tokio::test]
    async fn values_of_a_non_mapping_is_an_error() {
        let err = try_run_prog("mixed *create() { mixed x = 1; return values(x); }")
            .await
            .unwrap_err()
            .to_string();
        assert!(err.contains("values: int is not a mapping"), "{err}");
    }
}
