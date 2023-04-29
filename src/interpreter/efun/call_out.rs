use std::sync::Arc;

use chrono::Duration;
use lpc_rs_core::{LpcFloatInner, LpcIntInner, RegisterSize};
use lpc_rs_errors::{lpc_error, Result};

use crate::interpreter::{
    efun::efun_context::EfunContext, function_type::function_address::FunctionAddress,
    lpc_int::LpcInt, lpc_ref::LpcRef,
};

/// `call_out`, an efun for calling a function at some future point in time
pub async fn call_out<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    {
        let call_outs = context.call_outs().read();
        if call_outs.next_push_index() > LpcIntInner::MAX as usize {
            return Err(lpc_error!("too many call outs"));
        }
    }

    let func_ref = context.resolve_local_register(1 as RegisterSize).clone();

    // Some validations
    {
        let LpcRef::Function(func) = func_ref.clone() else {
            return Err(context.runtime_error("invalid function sent to `call_out`"));
        };
        let func = func.read();
        if let FunctionAddress::Dynamic(_) = func.address {
            return Err(lpc_error!(
                "cannot `call_out` to a function with a dynamic receiver",
            ));
        }
    }

    let duration_ref = context.resolve_local_register(2 as RegisterSize);
    let duration = match duration_ref {
        LpcRef::Int(x) => Duration::seconds(x.0),
        LpcRef::Float(x) => to_millis(x.0),
        _ => return Err(context.runtime_error("invalid duration sent to `call_out`")),
    };

    let repeat_ref = context.try_resolve_local_register(3 as RegisterSize);
    let repeat = if let Some(repeat_ref) = repeat_ref {
        match repeat_ref {
            LpcRef::Int(x) => {
                if x.0 <= 0 {
                    None
                } else {
                    Some(Duration::seconds(x.0))
                }
            }
            LpcRef::Float(x) => {
                if x.0 <= 0.0 {
                    None
                } else {
                    Some(to_millis(x.0))
                }
            }
            _ => return Err(context.runtime_error("invalid repeat sent to `call_out`")),
        }
    } else {
        None
    };

    let process = Arc::downgrade(&context.frame().process);
    let index = {
        let mut call_outs = context.call_outs().write();
        call_outs.schedule_task(process, func_ref, duration, repeat)?
    };

    let result = LpcRef::Int(LpcInt(index as LpcIntInner));
    context.return_efun_result(result);

    Ok(())
}

fn to_millis(x: LpcFloatInner) -> Duration {
    let m = x * 1000.0;
    let millis = if m > i64::MAX as f64 {
        i64::MAX
    } else if m < i64::MIN as f64 {
        i64::MIN
    } else {
        m.into_inner() as i64
    };
    chrono::Duration::milliseconds(millis)
}

#[cfg(test)]
mod tests {

    use crate::{
        interpreter::{task::initialize_program::InitializeProgramBuilder, vm::vm_op::VmOp},
        test_support::compile_prog,
    };

    #[tokio::test]
    async fn test_disallows_dynamic_receiver() {
        let code = r##"
            void create() {
                call_out(&->call_out_test(), 0.1);
            }

            void call_out_test() {
                dump("foobar");
            }
        "##;

        let (tx, _rx) = tokio::sync::mpsc::channel(128);
        let (program, _, _) = compile_prog(code).await;
        let result = InitializeProgramBuilder::<10>::default()
            .program(program)
            .tx(tx)
            .build()
            .await;

        assert_eq!(
            result.unwrap_err().to_string(),
            "cannot `call_out` to a function with a dynamic receiver"
        );
    }

    #[tokio::test]
    async fn test_enqueues_task() {
        let code = r##"
            void create() {
                call_out(&call_out_test(), 0.001);
            }

            void call_out_test() {
                dump("foobar");
            }
        "##;

        let (tx, mut rx) = tokio::sync::mpsc::channel(128);
        let (program, _, _) = compile_prog(code).await;
        let result = InitializeProgramBuilder::<5>::default()
            .program(program)
            .tx(tx)
            .build()
            .await;

        assert!(result.is_ok());

        let msg = rx.recv().await.unwrap();
        assert_eq!(msg, VmOp::PrioritizeCallOut(0));
    }
}
