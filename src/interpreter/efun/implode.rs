use lpc_rs_core::RegisterSize;
use lpc_rs_errors::Result;

use crate::interpreter::{efun::efun_context::EfunContext, lpc_int::LpcInt, lpc_ref::LpcRef};

/// `implode`, an efun for joining an array of strings into a single string.
pub async fn implode<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let subject_ref = context.resolve_local_register(1 as RegisterSize);

    let delimiter_ref = context.resolve_local_register(2 as RegisterSize);
    let delimiter = match delimiter_ref {
        LpcRef::Int(LpcInt(0)) => String::from(" "),
        LpcRef::String(_) => delimiter_ref.with_string(|x| x.to_string())?,
        LpcRef::Float(_)
        | LpcRef::Int(_)
        | LpcRef::Object(_)
        | LpcRef::Array(_)
        | LpcRef::Mapping(_)
        | LpcRef::Function(_) => return Ok(()),
    };

    let result = subject_ref.with_array(|subject| {
        subject
            .iter()
            .map(|x| x.to_string())
            .collect::<Vec<_>>()
            .join(&delimiter)
            .into()
    })?;

    context.return_efun_result(result);

    Ok(())
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use crate::{
        interpreter::vm::Vm, test_support::test_config, util::process_builder::ProcessInitializer,
    };

    #[tokio::test]
    async fn test_implode() {
        let master = indoc! { r#"
            string create() {
                return implode(({ "the", "quick", "brown", "", "fox" }), " ");
            }
        "# };

        let vm = Vm::new(test_config());

        let master_proc = vm
            .initialize_process_from_code("/master.c", master)
            .await
            .unwrap();

        let result = master_proc
            .result()
            .unwrap()
            .with_string(|s| s.to_string())
            .unwrap();

        assert_eq!(result, "the quick brown  fox",);
    }
}
