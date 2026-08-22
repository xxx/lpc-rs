use lpc_rs_core::{RegisterSize, lpc_path::LpcPath};
use lpc_rs_errors::Result;

use crate::interpreter::{
    efun::efun_context::EfunContext,
    lpc_ref::{LpcRef, NULL},
    lpc_string::LpcString,
};

pub async fn file_name<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let arg_ref = context.resolve_local_register(1 as RegisterSize);
    let result = arg_ref.as_object().map_or(NULL, |proc| {
        let path = LpcPath::new_server(&*proc.filename());
        let s = LpcString::from(String::from(
            path.as_in_game(&*context.config().lib_dir)
                .to_string_lossy(),
        ));
        LpcRef::from(s)
    });

    context.return_efun_result(result);

    Ok(())
}
