use lpc_rs_core::RegisterSize;
use lpc_rs_errors::Result;

use crate::interpreter::{efun::efun_context::EfunContext, lpc_ref::LpcRef};

pub fn explode<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let subject_ref = context.resolve_local_register(1 as RegisterSize);
    let Some(subject) = subject_ref.as_str() else {
        return Ok(());
    };

    let delimiter_ref = context.resolve_local_register(2 as RegisterSize);
    let delimiter = if delimiter_ref.is_null() {
        " "
    } else if let Some(delimiter) = delimiter_ref.as_str() {
        delimiter
    } else {
        return Ok(());
    };

    let parts: Vec<LpcRef> = subject.split(delimiter).map(LpcRef::from).collect();
    context.return_array(parts);

    Ok(())
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use crate::{interpreter::vm::Vm, test_support::test_config};

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

        let _ = master_proc
            .result()
            .unwrap()
            .with_array(master_proc.context.txn(), |arr| {
                let result = arr.iter().map(|x| x.to_string()).collect::<Vec<_>>();

                assert_eq!(result, vec!["the", "quick", "brown", "", "fox"],);
            });
    }
}
