use lpc_rs_core::{lpc_path::LpcPath, RegisterSize};
use lpc_rs_errors::Result;

use crate::interpreter::{
    efun::efun_context::EfunContext,
    into_lpc_ref::IntoLpcRef,
    lpc_ref::{LpcRef, NULL},
    lpc_string::LpcString,
};

/// `file_name`, an efun for returning the full path and clone number of an
/// object
pub async fn file_name<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let arg_ref = context.resolve_local_register(1 as RegisterSize);
    let result = match arg_ref {
        LpcRef::Float(_)
        | LpcRef::Int(_)
        | LpcRef::String(_)
        | LpcRef::Array(_)
        | LpcRef::Mapping(_)
        | LpcRef::Function(_) => NULL,
        LpcRef::Object(proc) => {
            if let Some(proc) = proc.upgrade() {
                let path = LpcPath::new_server(&*proc.filename());

                let s = LpcString::from(String::from(
                    path.as_in_game(&*context.config().lib_dir)
                        .to_string_lossy(),
                ));

                s.into_lpc_ref(context.memory())
            } else {
                NULL
            }
        }
    };

    context.return_efun_result(result);

    Ok(())
}
