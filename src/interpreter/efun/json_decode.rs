use lpc_rs_errors::Result;

use crate::interpreter::{efun::efun_context::EfunContext, json};

/// `json_decode`, an efun parsing JSON text into a value.
pub fn json_decode<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let Some(text) = context.arg(0).as_str() else {
        return Err(context.runtime_error("json_decode: text must be a string"));
    };
    let value =
        json::decode(text, context.txn()).map_err(|e| e.with_span(context.call_site_span()))?;
    context.return_efun_result(value);
    Ok(())
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use crate::{
        interpreter::{CommittedReader, lpc_ref::LpcRef, vm::Vm},
        test_support::test_config,
    };

    #[tokio::test]
    async fn text_becomes_a_value_the_program_can_index() {
        let vm = Vm::new(test_config());
        let main = indoc! { r#"
            int hp;
            void create() { mapping m = json_decode("{\"hp\": 12}"); hp = m["hp"]; }
        "# };
        let main = vm
            .initialize_process_from_code("/main.c", main)
            .await
            .unwrap()
            .context
            .process;
        assert_eq!(
            vm.global_state.committed_global(&main, 0u16),
            LpcRef::from(12)
        );
    }

    #[tokio::test]
    async fn malformed_text_is_a_runtime_error() {
        let vm = Vm::new(test_config());
        let err = vm
            .initialize_process_from_code("/main.c", r#"void create() { json_decode("{"); }"#)
            .await
            .unwrap_err()
            .to_string();
        assert!(err.contains("json_decode: EOF while parsing"), "{err}");
    }

    #[tokio::test]
    async fn a_non_string_argument_is_a_runtime_error() {
        let vm = Vm::new(test_config());
        let err = vm
            .initialize_process_from_code(
                "/main.c",
                "void create() { mixed p = 5; json_decode(p); }",
            )
            .await
            .unwrap_err()
            .to_string();
        assert!(err.contains("json_decode: text must be a string"), "{err}");
    }
}
