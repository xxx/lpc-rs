use lpc_rs_core::lpc_path::LpcPath;
use lpc_rs_errors::{LpcError, Result};

use crate::{
    interpreter::{efun::efun_context::EfunContext, lpc_ref::LpcRef, lpc_value::LpcValue},
    try_extract_value,
};

/// `file_name`, an efun for returning the full path and clone number of an
/// object
pub async fn file_name<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let lpc_ref = context.resolve_local_register(1_usize);
    let value = match lpc_ref {
        LpcRef::Float(_)
        | LpcRef::Int(_)
        | LpcRef::String(_)
        | LpcRef::Array(_)
        | LpcRef::Mapping(_)
        | LpcRef::Function(_) => LpcValue::from(0),
        LpcRef::Object(x) => {
            let b = x.read();
            let proc = try_extract_value!(*b, LpcValue::Object);

            if let Some(proc) = proc.upgrade() {
                let path = LpcPath::new_server(&*proc.read().filename());

                LpcValue::from(String::from(
                    path.as_in_game(&*context.config().lib_dir)
                        .to_string_lossy(),
                ))
            } else {
                LpcValue::from(0)
            }
        }
    };

    let result = context.value_to_ref(value);

    context.return_efun_result(result);

    Ok(())
}
