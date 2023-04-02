use lpc_rs_errors::Result;
use qcell::QCellOwner;

use crate::interpreter::{efun::efun_context::EfunContext, lpc_value::LpcValue};
use crate::interpreter::lpc_ref::LpcRef;
use crate::try_extract_value;
use lpc_rs_errors::LpcError;
use crate::interpreter::function_type::function_address::FunctionAddress;

/// `call_out`, an efun for calling a function at some future point in time
pub fn call_out<const N: usize>(
    context: &mut EfunContext<N>,
    cell_key: &mut QCellOwner,
) -> Result<()> {
    let func_ref = context.resolve_local_register(1_usize);

    // Some validations
    {
        let LpcRef::Function(func) = func_ref.clone() else {
            return Err(context.runtime_error("invalid function sent to `call_out`"));
        };
        let func = func.borrow();
        let func = try_extract_value!(&*func, LpcValue::Function);
        if let FunctionAddress::Dynamic(_) = func.address {
            return Err(LpcError::new("cannot `call_out` to a function with a dynamic receiver"));
        }
    }

    let duration_ref = context.resolve_local_register(2_usize);
    let duration = match duration_ref {
        LpcRef::Int(x) => chrono::Duration::seconds(x),
        LpcRef::Float(x) => {
            let m = x * 1000.0;
            let millis = if m > i64::MAX as f64 {
                i64::MAX
            } else if m < i64::MIN as f64 {
                i64::MIN
            } else {
                m.into_inner() as i64
            };
            chrono::Duration::milliseconds(millis)
        },
        _ => return Err(context.runtime_error("invalid duration sent to `call_out`")),
    };

    let call_outs = context.call_outs().rw(cell_key);
    let index = call_outs.schedule_task(func_ref, duration)?;

    // TODO: limit the max number of call outs so we don't overflow this
    let result = LpcRef::Int(index as i64);
    context.return_efun_result(result);

    Ok(())
}

#[cfg(test)]
mod tests {
    use std::rc::Rc;
    use lpc_rs_utils::config::Config;
    use crate::interpreter::call_outs::CallOuts;
    use crate::interpreter::gc::gc_bank::GcBank;
    use crate::interpreter::memory::Memory;
    use crate::interpreter::object_space::ObjectSpace;
    use crate::interpreter::task::Task;
    use crate::interpreter::vm_op::VmOp;
    use crate::test_support::compile_prog;
    use super::*;

    #[test]
    fn test_disallows_dynamic_receiver() {
        let mut cell_key = QCellOwner::new();

        let code = r##"
            void create() {
                call_out(&->call_out_test(), 0.1);
            }

            void call_out_test() {
                dump("foobar");
            }
        "##;

        let (tx, _) = std::sync::mpsc::channel();
        let (program, _, _) = compile_prog(code, &mut cell_key);
        let mut task: Task<5> = Task::new(Memory::new(10), cell_key.cell(GcBank::default()));
        let call_outs = Rc::new(cell_key.cell(CallOuts::new(tx.clone())));
        let result = task.initialize_program(
            program,
            Config::default(),
            cell_key.cell(ObjectSpace::default()),
            call_outs,
            tx,
            &mut cell_key,
        );

        assert_eq!(result.unwrap_err().to_string(), "cannot `call_out` to a function with a dynamic receiver");
    }

    #[test]
    fn test_enqueues_task() {
        let mut cell_key = QCellOwner::new();

        let code = r##"
            void create() {
                call_out(&call_out_test(), 0.001);
            }

            void call_out_test() {
                dump("foobar");
            }
        "##;

        let (tx, rx) = std::sync::mpsc::channel();
        let (program, _, _) = compile_prog(code, &mut cell_key);
        let mut task: Task<5> = Task::new(Memory::new(10), cell_key.cell(GcBank::default()));
        let call_outs = Rc::new(cell_key.cell(CallOuts::new(tx.clone())));
        let result = task.initialize_program(
            program,
            Config::default(),
            cell_key.cell(ObjectSpace::default()),
            call_outs,
            tx,
            &mut cell_key,
        );

        assert!(result.is_ok());

        let msg = rx.recv().unwrap();
        assert_eq!(msg, VmOp::RunCallOut(0));
    }
}