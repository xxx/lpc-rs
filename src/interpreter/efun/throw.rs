use lpc_rs_core::RegisterSize;
use lpc_rs_errors::{lpc_error, Result};

use crate::interpreter::efun::efun_context::EfunContext;

/// `throw`, intentionally throw an error. Can be caught by `catch`.
pub async fn throw<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let arg = context.resolve_local_register(1 as RegisterSize);

    Err(lpc_error!(
        context
            .previous_debug_span()
            .or_else(|| context.current_debug_span()),
        "{}",
        arg.to_string()
    ))
}

#[cfg(test)]
mod tests {

    use crate::{
        interpreter::{
            task::initialize_program::InitializeProgramBuilder, vm::global_state::GlobalState,
        },
        test_support::compile_prog,
    };

    #[tokio::test]
    async fn test_throw() {
        let code = r##"
            void create() {
                throw("foo bar baz error!");
            }
        "##;

        let (tx, _rx) = tokio::sync::mpsc::channel(128);
        let (program, config, _) = compile_prog(code).await;
        let global_state = GlobalState::new(config, tx);
        let result = InitializeProgramBuilder::<10>::default()
            .global_state(global_state)
            .program(program)
            .build()
            .await;

        assert_eq!(result.unwrap_err().to_string(), "foo bar baz error!");
    }
}
