use if_chain::if_chain;
use lpc_rs_core::RegisterSize;
use lpc_rs_errors::Result;

use crate::interpreter::{
    efun::efun_context::EfunContext,
    lpc_ref::{LpcRef, NULL},
    vm::Vm,
};

/// `exec`, an efun for for moving a connection into an object.
pub async fn exec<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    if_chain! {
        let new_ref = context.resolve_local_register(1 as RegisterSize);
        if let LpcRef::Object(new_ob) = new_ref;
        if let Some(new_ob) = new_ob.upgrade();
        let old_ref = context.resolve_local_register(2 as RegisterSize);
        if let LpcRef::Object(old_ob) = old_ref;
        if let Some(old_ob) = old_ob.upgrade();
        then {
            Vm::exec(new_ob, old_ob, context.tx()).await;
            context.return_efun_result(LpcRef::from(1));
            Ok(())
        }
        else {
            context.return_efun_result(NULL);
            Ok(())
        }
    }
}
