use std::rc::{Rc, Weak};
use lpc_rs_errors::Result;
use qcell::{QCell, QCellOwner};

use crate::interpreter::{efun::efun_context::EfunContext, lpc_value::LpcValue};
use crate::interpreter::call_outs::CallOut;
use crate::interpreter::efun::query_call_out::call_out_array_ref;
use crate::interpreter::lpc_array::LpcArray;
use crate::interpreter::lpc_ref::LpcRef;
use crate::interpreter::process::Process;

/// `query_call_outs`, an efun for returning information about all call outs in a specific object
pub fn query_call_outs<const N: usize>(
    context: &mut EfunContext<N>,
    cell_key: &mut QCellOwner,
) -> Result<()> {
    let owner = match context.resolve_local_register(1_usize) {
        LpcRef::Object(object) => {
            let LpcValue::Object(process) = &*object.borrow() else {
                return Err(context.runtime_bug("object in `query_call_outs` is not an object? This shouldn't be reachable."));
            };
            process.clone()
        },
        LpcRef::Int(0) => {
            context.frame().process.clone()
        },
        _ => return Err(context.runtime_bug("non-object sent to `query_call_outs`")),
    };

    let vec = context.call_outs().ro(cell_key).queue().iter().filter_map(|(idx, call_out)| {
        if Rc::ptr_eq(call_out.process(), &owner) {
            Some(call_out_array_ref(context, &call_out).unwrap())
        } else {
            None
        }
    }).collect::<Vec<_>>();

    let result = context.value_to_ref(LpcValue::from(vec));

    context.return_efun_result(result);

    Ok(())
}

#[cfg(test)]
mod tests {
    use std::rc::Rc;
    use if_chain::if_chain;
    use lpc_rs_utils::config::Config;
    use crate::interpreter::call_outs::CallOuts;
    use crate::interpreter::gc::gc_bank::GcBank;
    use crate::interpreter::memory::Memory;
    use crate::interpreter::object_space::ObjectSpace;
    use crate::interpreter::task::Task;
    use crate::test_support::compile_prog;
    use super::*;

    #[test]
    fn test_query_call_out() {
        let mut cell_key = QCellOwner::new();

        let code = r##"
            mixed create() {
                int id = call_out(call_out_test, 100);
                int id2 = call_out(call_out_test, 200);

                mixed *result = query_call_outs();

                remove_call_out(id);
                remove_call_out(id2);

                return result;
            }

            void call_out_test() {
                dump("foobar");
            }
        "##;

        let (tx, _rx) = std::sync::mpsc::channel();
        let (program, _, _) = compile_prog(code, &mut cell_key);
        let call_outs = Rc::new(cell_key.cell(CallOuts::new(tx.clone())));
        let task = Task::<10>::initialize_program(
            program,
            Config::default(),
            cell_key.cell(ObjectSpace::default()),
            Memory::default(),
            cell_key.cell(GcBank::default()),
            call_outs.clone(),
            tx,
            &mut cell_key,
        ).unwrap();

        if_chain! {
            if let LpcRef::Array(arr) = task.result().unwrap();
            if let LpcValue::Array(LpcArray { array, ..}) = &*arr.borrow();
            then {
                assert_eq!(array.len(), 2);

                for call_out in array {
                    if_chain! {
                        if let LpcRef::Array(call_out) = call_out;
                        if let LpcValue::Array(LpcArray { array: call_out, ..}) = &*call_out.borrow();
                        then {
                            assert_eq!(call_out.len(), 4);
                            assert!(matches!(call_out[0], LpcRef::Object(_)));
                            assert!(matches!(call_out[1], LpcRef::Function(_)));
                            assert!(matches!(call_out[2], LpcRef::Int(_)));
                            assert_eq!(call_out[3], LpcRef::Int(0));
                        }
                        else {
                            panic!("inner is not an array");
                        }
                    }
                }
            }
            else {
                panic!("result is not an array");
            }
        }
    }
}