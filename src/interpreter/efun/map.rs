use indexmap::IndexMap;
use lpc_rs_errors::Result;

use crate::interpreter::{
    efun::{
        callback::{Items, call_args, call_back, extra_args, function_arg, items_arg},
        efun_context::EfunContext,
    },
    lpc_mapping::LpcMapping,
};

/// `map(coll, f, extra...)`: a new array of `f(item, extra...)` per item,
/// or a new mapping with the same keys and `f(key, value, extra...)` as
/// each value.
pub async fn map<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let items = items_arg(context, "map")?;
    let f = function_arg(context, "map", 1)?;
    let extra = extra_args(context, 2);
    match items {
        Items::Array(items) => {
            let mut mapped = Vec::with_capacity(items.len());
            for item in items {
                let args = call_args(std::slice::from_ref(&item), &extra);
                mapped.push(call_back(context, "map", &f, &args).await?);
            }
            context.return_array(mapped);
        }
        Items::Mapping(entries) => {
            let mut mapped = IndexMap::with_capacity(entries.len());
            for (key, value) in entries {
                let args = call_args(&[key.clone(), value], &extra);
                let result = call_back(context, "map", &f, &args).await?;
                mapped.insert(key, result);
            }
            context.return_mapping(LpcMapping::new(mapped));
        }
    }
    Ok(())
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
}
