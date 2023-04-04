use lpc_rs_errors::{LpcError, Result};
use qcell::QCellOwner;

use crate::{interpreter::efun::efun_context::EfunContext, util::keyable::Keyable};

/// `throw`, intentionally throw an error. Can be caught by `catch`.
pub fn throw<const N: usize>(
    context: &mut EfunContext<N>,
    cell_key: &mut QCellOwner,
) -> Result<()> {
    let arg = context.resolve_local_register(1_usize);

    return Err(LpcError::new(arg.with_key(cell_key).to_string())
        .with_span(context.frame().current_debug_span()));
}

#[cfg(test)]
mod tests {
    use std::rc::Rc;

    use lpc_rs_utils::config::Config;

    use super::*;
    use crate::{
        interpreter::{
            call_outs::CallOuts, gc::gc_bank::GcBank, memory::Memory, object_space::ObjectSpace,
            task::Task,
        },
        test_support::compile_prog,
    };

    #[test]
    fn test_throw() {
        let mut cell_key = QCellOwner::new();

        let code = r##"
            void create() {
                throw("foo bar baz error!");
            }
        "##;

        let (tx, _) = std::sync::mpsc::channel();
        let (program, _, _) = compile_prog(code, &mut cell_key);
        let result = Task::<10>::initialize_program(
            program,
            Config::default(),
            cell_key.cell(ObjectSpace::default()),
            Memory::default(),
            cell_key.cell(GcBank::default()),
            cell_key.cell(CallOuts::new(tx.clone())),
            tx,
            &mut cell_key,
        );

        assert_eq!(result.unwrap_err().to_string(), "\"foo bar baz error!\"");
    }
}
