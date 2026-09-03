use lpc_rs_core::RegisterSize;
use lpc_rs_errors::Result;

use crate::interpreter::{efun::efun_context::EfunContext, json, lpc_ref::LpcRef};

/// `json_encode`, an efun rendering a value as JSON text.
pub fn json_encode<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let value = context.resolve_local_register(1 as RegisterSize);
    let text =
        json::encode(value, context.txn()).map_err(|e| e.with_span(context.call_site_span()))?;
    context.return_efun_result(LpcRef::from(text));
    Ok(())
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use crate::{
        interpreter::vm::Vm,
        test_support::{committed_string, test_config},
    };

    #[tokio::test]
    async fn a_mapping_is_rendered_as_json_text() {
        let vm = Vm::new(test_config());
        let main = indoc! { r#"
            string s;
            void create() { s = json_encode(([ "hp": 1, "tags": ({ "a" }) ])); }
        "# };
        let main = vm
            .initialize_process_from_code("/main.c", main)
            .await
            .unwrap()
            .context
            .process;
        assert_eq!(committed_string(&vm, &main, 0), r#"{"hp":1,"tags":["a"]}"#);
    }

    #[tokio::test]
    async fn a_function_pointer_is_a_runtime_error() {
        let vm = Vm::new(test_config());
        let err = vm
            .initialize_process_from_code("/main.c", "void create() { json_encode(&create()); }")
            .await
            .unwrap_err()
            .to_string();
        assert!(err.contains("json_encode: cannot encode function"), "{err}");
    }

    #[tokio::test]
    async fn a_self_containing_array_is_a_runtime_error() {
        let vm = Vm::new(test_config());
        let main = indoc! { r#"
            void create() { mixed *a = ({ 0 }); a[0] = a; json_encode(a); }
        "# };
        let err = vm
            .initialize_process_from_code("/main.c", main)
            .await
            .unwrap_err()
            .to_string();
        assert!(
            err.contains("json_encode: nesting deeper than 128 levels"),
            "{err}"
        );
    }
}
