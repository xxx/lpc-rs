use lpc_rs_core::RegisterSize;
use lpc_rs_errors::Result;

use crate::interpreter::{
    efun::efun_context::EfunContext, into_lpc_ref::IntoLpcRef, lpc_array::LpcArray,
    lpc_int::LpcInt, lpc_ref::LpcRef,
};

/// `explode`, an efun for splitting a string into an array of strings.
pub async fn explode<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let subject_ref = context.resolve_local_register(1 as RegisterSize);
    let subject = match subject_ref {
        LpcRef::Float(_)
        | LpcRef::Int(_)
        | LpcRef::Object(_)
        | LpcRef::Array(_)
        | LpcRef::Mapping(_)
        | LpcRef::Function(_) => return Ok(()),
        LpcRef::String(x) => x.read().to_string(),
    };

    let delimiter_ref = context.resolve_local_register(2 as RegisterSize);
    let delimiter = match delimiter_ref {
        LpcRef::Int(LpcInt(0)) => String::from(" "),
        LpcRef::String(x) => x.read().to_string(),
        LpcRef::Float(_)
        | LpcRef::Int(_)
        | LpcRef::Object(_)
        | LpcRef::Array(_)
        | LpcRef::Mapping(_)
        | LpcRef::Function(_) => return Ok(()),
    };

    let result = subject
        .split(&delimiter)
        .map(|s| s.into_lpc_ref(context.memory()))
        .collect::<LpcArray>()
        .into_lpc_ref(context.memory());

    context.return_efun_result(result);

    Ok(())
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use crate::{
        interpreter::{lpc_ref::LpcRef, vm::Vm},
        test_support::test_config,
        util::process_builder::ProcessInitializer,
    };

    #[tokio::test]
    async fn test_explode() {
        let master = indoc! { r#"
            string *create() {
                return explode("the quick brown  fox", " ");
            }
        "# };

        let vm = Vm::new(test_config());

        let master_proc = vm
            .initialize_process_from_code("/master.c", master)
            .await
            .unwrap();

        let LpcRef::Array(arr) = master_proc.result().unwrap() else {
            panic!("Expected array result");
        };

        let result = arr.read().iter().map(|x| x.to_string()).collect::<Vec<_>>();

        assert_eq!(result, vec!["the", "quick", "brown", "", "fox"],);
    }
}
