use lpc_rs_errors::{LpcError, Result};

use crate::interpreter::efun::efun_context::EfunContext;

/// `throw`, intentionally throw an error. Can be caught by `catch`.
pub async fn throw<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let arg = context.resolve_local_register(1_usize);

    return Err(LpcError::new(arg.to_string()).with_span(context.frame().current_debug_span()));
}

#[cfg(test)]
mod tests {

    use crate::{
        interpreter::task::initialize_task::InitializeProgramBuilder, test_support::compile_prog,
    };

    #[tokio::test]
    async fn test_throw() {
        let code = r##"
            void create() {
                throw("foo bar baz error!");
            }
        "##;

        let (tx, _rx) = tokio::sync::mpsc::channel(128);
        let (program, _, _) = compile_prog(code);
        let result = InitializeProgramBuilder::<10>::default()
            .program(program)
            .tx(tx)
            .build()
            .await;

        assert_eq!(result.unwrap_err().to_string(), "foo bar baz error!");
    }
}
