use lpc_rs_errors::{LpcError, Result};

use crate::{interpreter::efun::efun_context::EfunContext};

/// `throw`, intentionally throw an error. Can be caught by `catch`.
pub async fn throw<const N: usize>(
    context: &mut EfunContext<'_, N>,
    ) -> Result<()> {
    let arg = context.resolve_local_register(1_usize);

    return Err(LpcError::new(arg.to_string())
        .with_span(context.frame().current_debug_span()));
}

#[cfg(test)]
mod tests {
    use parking_lot::RwLock;
    use lpc_rs_utils::config::Config;

    use super::*;
    use crate::{
        interpreter::{
            call_outs::CallOuts, gc::gc_bank::GcBank, memory::Memory, object_space::ObjectSpace,
            task::Task,
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

        let (tx, _) = tokio::sync::mpsc::channel(128);
        let (program, _, _) = compile_prog(code);
        let result = Task::<10>::initialize_program(
            program,
            Config::default(),
            RwLock::new(ObjectSpace::default()),
            Memory::default(),
            RwLock::new(GcBank::default()),
            RwLock::new(CallOuts::new(tx.clone())),
            tx,
        ).await;

        assert_eq!(result.unwrap_err().to_string(), "\"foo bar baz error!\"");
    }
}
