use lpc_rs_errors::Result;

use crate::interpreter::{
    efun::{efun_context::EfunContext, in_game_name},
    lpc_ref::{LpcRef, NULL},
};

/// `file_name(ob)`: the object's file as an in-game path without its
/// extension; 0 for a destructed object.
pub fn file_name<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let arg_ref = context.arg(0);
    let result = arg_ref
        .live_object(context.txn())
        .map_or(NULL, |proc| LpcRef::from(in_game_name(context, &proc)));

    context.return_efun_result(result);

    Ok(())
}
