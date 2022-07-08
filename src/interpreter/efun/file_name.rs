use crate::{
    interpreter::{efun::efun_context::EfunContext, lpc_ref::LpcRef, lpc_value::LpcValue},
    try_extract_value,
    util::path_maker::LpcPath,
    Result,
};
use lpc_rs_errors::LpcError;

/// `file_name`, an efun for returning the full path and clone number of an object
pub fn file_name<const N: usize>(context: &mut EfunContext<N>) -> Result<()> {
    let lpc_ref = context.resolve_lpc_ref(1_usize);
    let value = match lpc_ref {
        LpcRef::Float(_)
        | LpcRef::Int(_)
        | LpcRef::String(_)
        | LpcRef::Array(_)
        | LpcRef::Mapping(_)
        | LpcRef::Function(_) => LpcValue::from(0),
        LpcRef::Object(x) => {
            let b = x.borrow();
            let proc = try_extract_value!(*b, LpcValue::Object);
            let path = LpcPath::new_server(&*proc.borrow().filename());

            LpcValue::from(String::from(
                path.as_in_game(context.config().lib_dir())
                    .to_string_lossy(),
            ))
        }
    };

    let result = context.value_to_ref(value);

    context.return_efun_result(result);

    Ok(())
}
