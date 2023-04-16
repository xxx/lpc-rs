use std::sync::Arc;

use lpc_rs_errors::Result;

use crate::interpreter::{
    efun::efun_context::EfunContext,
    into_lpc_ref::IntoLpcRef,
    lpc_ref::{LpcRef, NULL},
};

/// `find_object`, an efun for finding and returning an object from the [`ObjectSpace`]
/// from its path and clone number.
pub async fn find_object<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let lpc_ref = context.resolve_local_register(1_usize);
    let result = match lpc_ref {
        LpcRef::Float(_)
        | LpcRef::Int(_)
        | LpcRef::Object(_)
        | LpcRef::Array(_)
        | LpcRef::Mapping(_)
        | LpcRef::Function(_) => NULL,
        LpcRef::String(x) => {
            let path = x.read();

            match context.lookup_process(&*path) {
                Some(proc) => {
                    drop(path);
                    Arc::downgrade(&proc).into_lpc_ref(context.memory())
                    // context.value_to_ref(proc)
                }
                None => NULL,
            }
        }
    };

    context.return_efun_result(result);

    Ok(())
}

#[cfg(test)]
mod tests {
    use std::{path::Path, sync::Arc};

    use indoc::indoc;
    use lpc_rs_core::lpc_path::LpcPath;
    use lpc_rs_utils::config::Config;
    use parking_lot::RwLock;

    use super::*;
    use crate::{
        interpreter::{
            call_outs::CallOuts,
            gc::gc_bank::GcBank,
            memory::Memory,
            object_space::ObjectSpace,
            process::Process,
            program::{Program, ProgramBuilder},
            task::Task,
            task_context::TaskContext,
            vm::vm_op::VmOp,
        },
        test_support::compile_prog,
    };

    fn task_context_fixture(
        program: Program,
        config: Arc<Config>,
        tx: tokio::sync::mpsc::Sender<VmOp>,
    ) -> TaskContext {
        let process = Process::new(program);

        TaskContext::new(
            config,
            RwLock::new(process),
            ObjectSpace::default(),
            Memory::new(10),
            RwLock::new(GcBank::default()),
            Arc::new(RwLock::new(CallOuts::new(tx.clone()))),
            tx,
        )
    }

    #[tokio::test]
    async fn test_find_object_success() {
        let code = indoc! { r#"
            object foo = find_object("/example");
        "# };

        let (tx, _rx) = tokio::sync::mpsc::channel(128);

        let (program, config, _) = compile_prog(code);
        let func = program.initializer.clone().expect("no init found?");
        let context = task_context_fixture(program, config, tx);
        let to_find = ProgramBuilder::default()
            .filename(LpcPath::InGame(Path::new("/example").to_path_buf()))
            .build()
            .unwrap();
        let proc = Process::new(to_find);
        ObjectSpace::insert_process(&context.object_space, RwLock::new(proc));

        let mut task = Task::<10>::new(context.clone());
        task.eval(func.clone(), &[]).await.expect("task failed");

        let LpcRef::Object(obj) = task.result().unwrap() else {
            panic!("expected object");
        };

        assert_eq!(
            obj.upgrade()
                .unwrap()
                .read()
                .program
                .filename
                .to_str()
                .unwrap(),
            "/example"
        );
    }

    #[tokio::test]
    async fn test_find_object_failure() {
        let code = indoc! { r#"
            object foo = find_object("/example");
        "# };

        let (tx, _rx) = tokio::sync::mpsc::channel(128);

        let (program, config, _) = compile_prog(code);
        let func = program.initializer.clone().expect("no init found?");
        let context = task_context_fixture(program, config, tx);

        let mut task = Task::<10>::new(context.clone());
        task.eval(func.clone(), &[]).await.expect("task failed");

        assert_eq!(task.result().unwrap(), &NULL);
    }
}
