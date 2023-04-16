use std::sync::Arc;

use lpc_rs_errors::Result;

use crate::interpreter::{
    efun::{efun_context::EfunContext, query_call_out::call_out_array_ref},
    into_lpc_ref::IntoLpcRef,
    lpc_array::LpcArray,
    lpc_int::LpcInt,
    lpc_ref::LpcRef,
};

/// `query_call_outs`, an efun for returning information about all call outs in a specific object
pub async fn query_call_outs<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let owner = match context.resolve_local_register(1_usize) {
        LpcRef::Object(process) => process.upgrade(),
        LpcRef::Int(LpcInt(0)) => Some(context.frame().process.clone()),
        _ => return Err(context.runtime_error("non-object sent to `query_call_outs`")),
    };

    let Some(owner) = owner else {
        return Err(context.runtime_error("object in `query_call_outs` is already destructed"));
    };

    let vec = context
        .call_outs()
        .read()
        .queue()
        .iter()
        .filter_map(|(_idx, call_out)| {
            if let Some(process) = call_out.process().upgrade() {
                if Arc::ptr_eq(&process, &owner) {
                    Some(call_out_array_ref(context, call_out).unwrap())
                } else {
                    None
                }
            } else {
                None
            }
        })
        .collect::<Vec<_>>();

    let result = LpcArray::new(vec).into_lpc_ref(context.memory());

    context.return_efun_result(result);

    Ok(())
}

#[cfg(test)]
mod tests {

    use if_chain::if_chain;
    use lpc_rs_utils::config::Config;
    use parking_lot::RwLock;

    use super::*;
    use crate::{
        interpreter::{
            call_outs::CallOuts, gc::gc_bank::GcBank, memory::Memory, object_space::ObjectSpace,
            task::Task,
        },
        test_support::compile_prog,
    };
    use crate::interpreter::task::initialize_task::InitializeProgramBuilder;

    #[tokio::test]
    async fn test_query_call_out() {
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

        let (tx, _rx) = tokio::sync::mpsc::channel(128);
        let (program, _, _) = compile_prog(code);
        let task = InitializeProgramBuilder::<10>::default()
            .program(program)
            .tx(tx)
            .build()
            .await
            .unwrap();

        if_chain! {
            if let LpcRef::Array(arr) = task.result().unwrap();
            let array = arr.read();
            then {
                assert_eq!(array.len(), 2);

                for call_out in array.iter() {
                    if_chain! {
                        if let LpcRef::Array(call_out) = call_out;
                        let call_out = call_out.read();
                        then {
                            assert_eq!(call_out.len(), 4);
                            assert!(matches!(call_out[0], LpcRef::Object(_)));
                            assert!(matches!(call_out[1], LpcRef::Function(_)));
                            assert!(matches!(call_out[2], LpcRef::Int(_)));
                            assert_eq!(call_out[3], LpcRef::Int(0.into()));
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
