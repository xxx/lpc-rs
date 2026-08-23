use std::sync::Arc;

use lpc_rs_core::RegisterSize;
use lpc_rs_errors::Result;

use crate::interpreter::{
    efun::efun_context::EfunContext, lpc_ref::LpcRef, object_flags::ObjectFlags, process::Process,
};

pub async fn destruct<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let lpc_ref = context.resolve_local_register(1 as RegisterSize);
    let destruct = |proc: Arc<Process>| {
        proc.flags.set(ObjectFlags::Destructed);
        context.remove_process(proc);
    };

    if matches!(lpc_ref, LpcRef::Array(_)) {
        lpc_ref.with_array(context.txn(), |arr| {
            arr.iter().filter_map(LpcRef::as_object).for_each(&destruct)
        })?;
    } else if let Some(proc) = lpc_ref.as_object() {
        destruct(proc);
    }

    Ok(())
}

#[cfg(test)]
mod tests {

    use crate::test_support::run_prog;

    #[tokio::test]
    async fn test_destruct() {
        let code = r##"
            void create() {
                dump(file_name(this_object()));
                object ob = clone_object("/my_file"); // this file
                dump(file_name(ob));
                destruct(ob);
            }
        "##;

        let result = run_prog(code).await;

        let space = result
            .context
            .object_space()
            .iter()
            .map(|x| x.key().to_owned())
            .collect::<Vec<_>>();

        assert!(space.contains(&"/my_file".to_owned())); // clone is removed
        // The prototype and the simul-efun object `run_prog` inserts.
        assert_eq!(result.context.object_space().len(), 2);
    }
}
