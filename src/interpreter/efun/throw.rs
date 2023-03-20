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
    use lpc_rs_utils::config::Config;
    use crate::interpreter::gc::gc_bank::GcBank;
    use crate::interpreter::memory::Memory;
    use crate::interpreter::object_space::ObjectSpace;
    use crate::interpreter::task::Task;
    use crate::test_support::compile_prog;
    use super::*;

    #[test]
    fn test_throw() {
        let mut cell_key = QCellOwner::new();

        let code = r##"
            void create() {
                throw("foo bar baz error!");
            }
        "##;

        let (program, _, _) = compile_prog(code, &mut cell_key);
        let mut task: Task<5> = Task::new(Memory::new(10), cell_key.cell(GcBank::default()));
        let result = task.initialize_program(
            program,
            Config::default(),
            cell_key.cell(ObjectSpace::default()),
            &mut cell_key,
        );

        assert_eq!(
            result.unwrap_err().to_string(),
            "\"foo bar baz error!\""
        );
    }
}