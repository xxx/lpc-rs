use lpc_rs_errors::Result;
use qcell::QCellOwner;

use crate::interpreter::{efun::efun_context::EfunContext, lpc_ref::LpcRef};

/// `remove_call_out`, an efun for removing a call out.
/// This will cancel both upcoming and repeating call outs.
pub fn remove_call_out<const N: usize>(
    context: &mut EfunContext<N>,
    cell_key: &mut QCellOwner,
) -> Result<()> {
    let LpcRef::Int(idx) = context.resolve_local_register(1_usize) else {
        return Err(context.runtime_bug("non-int call out ID sent to `remove_call_out`"));
    };

    if idx < 0 {
        return Err(context.runtime_error(format!(
            "invalid call out ID `{idx}` sent to `remove_call_out`"
        )));
    }

    let call_outs = context.call_outs().rw(cell_key);
    let ret = call_outs
        .remove(idx as usize)
        .map(|call_out| {
            call_out
                .time_remaining()
                .map(|duration| duration.num_milliseconds())
                .unwrap_or(0)
        })
        .unwrap_or(-1);

    let result = LpcRef::Int(ret);
    context.return_efun_result(result);

    Ok(())
}

#[cfg(test)]
mod tests {
    
    use std::sync::Arc;

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
    fn test_removes_task() {
        let mut cell_key = QCellOwner::new();

        let code = r##"
            void create() {
                int id = call_out(call_out_test, 100);

                remove_call_out(id);
            }

            void call_out_test() {
                dump("foobar");
            }
        "##;

        let (tx, _rx) = tokio::sync::mpsc::channel(128);
        let (program, _, _) = compile_prog(code, &mut cell_key);
        let call_outs = Arc::new(cell_key.cell(CallOuts::new(tx.clone())));
        let result = Task::<10>::initialize_program(
            program,
            Config::default(),
            cell_key.cell(ObjectSpace::default()),
            Memory::default(),
            cell_key.cell(GcBank::default()),
            call_outs.clone(),
            tx,
            &mut cell_key,
        );

        assert!(result.is_ok());
        assert!(call_outs.ro(&cell_key).is_empty());
    }
}
