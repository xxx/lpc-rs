use indexmap::IndexMap;
use lpc_rs_errors::Result;

use crate::interpreter::{
    efun::{
        callback::{Items, call_args, call_back, extra_args, function_arg, items_arg},
        efun_context::EfunContext,
    },
    lpc_mapping::LpcMapping,
};

/// `filter(coll, f, extra...)`: a new array of the items `f(item, extra...)`
/// accepts, or a new mapping of the entries `f(key, value, extra...)`
/// accepts, in their order.
pub async fn filter<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let items = items_arg(context, "filter")?;
    let f = function_arg(context, "filter", 1)?;
    let extra = extra_args(context, 2);
    match items {
        Items::Array(items) => {
            let mut kept = Vec::new();
            for item in items {
                let args = call_args(std::slice::from_ref(&item), &extra);
                let verdict = call_back(context, "filter", &f, &args).await?;
                if verdict.is_truthy(context.txn()) {
                    kept.push(item);
                }
            }
            context.return_array(kept);
        }
        Items::Mapping(entries) => {
            let mut kept = IndexMap::new();
            for (key, value) in entries {
                let args = call_args(&[key.clone(), value.clone()], &extra);
                let verdict = call_back(context, "filter", &f, &args).await?;
                if verdict.is_truthy(context.txn()) {
                    kept.insert(key, value);
                }
            }
            context.return_mapping(LpcMapping::new(kept));
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
}
