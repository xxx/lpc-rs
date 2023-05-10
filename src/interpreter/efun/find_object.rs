use std::sync::Arc;

use lpc_rs_core::{lpc_path::LpcPath, RegisterSize};
use lpc_rs_errors::Result;

use crate::interpreter::{
    efun::efun_context::EfunContext,
    into_lpc_ref::IntoLpcRef,
    lpc_ref::{LpcRef, NULL},
};

/// `find_object`, an efun for finding and returning an object from the [`ObjectSpace`]
/// from its path and clone number.
pub async fn find_object<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let lpc_ref = context.resolve_local_register(1 as RegisterSize);
    let result = match lpc_ref {
        LpcRef::Float(_)
        | LpcRef::Int(_)
        | LpcRef::Object(_)
        | LpcRef::Array(_)
        | LpcRef::Mapping(_)
        | LpcRef::Function(_) => NULL,
        LpcRef::String(x) => {
            let path = {
                let string = x.read();
                LpcPath::new_in_game(&*string, context.in_game_cwd(), &*context.config().lib_dir)
            };

            if let Ok(proc) = context.load_object(&path).await {
                Arc::downgrade(&proc).into_lpc_ref(context.memory())
            } else {
                NULL
            }
        }
    };

    context.return_efun_result(result);

    Ok(())
}

#[cfg(test)]
mod tests {
    use std::{
        path::{Path, PathBuf},
        sync::Arc,
    };

    use indoc::indoc;
    use lpc_rs_core::lpc_path::LpcPath;
    use lpc_rs_errors::lazy_files::FILE_CACHE;
    use lpc_rs_utils::config::Config;

    use super::*;
    use crate::{
        interpreter::{
            heap::Heap,
            object_space::ObjectSpace,
            process::Process,
            program::{Program, ProgramBuilder},
            task::Task,
            task_context::{TaskContext, TaskContextBuilder},
            vm::{global_state::GlobalStateBuilder, vm_op::VmOp, Vm},
        },
        test_support::{compile_prog, test_config},
        util::process_builder::ProcessInitializer,
    };

    fn task_context_fixture(
        program: Program,
        config: Arc<Config>,
        tx: tokio::sync::mpsc::Sender<VmOp>,
    ) -> TaskContext {
        let process = Process::new(program);

        let global_state = GlobalStateBuilder::default()
            .config(config)
            .tx(tx)
            .memory(Heap::new(10))
            .build()
            .unwrap();

        TaskContextBuilder::default()
            .global_state(global_state)
            .process(process)
            .build()
            .unwrap()
    }

    #[tokio::test]
    async fn test_find_object_success() {
        let code = indoc! { r#"
            object foo = find_object("/example");
        "# };

        let (tx, _rx) = tokio::sync::mpsc::channel(128);

        let (program, config, _) = compile_prog(code).await;
        let func = program.initializer.clone().expect("no init found?");
        let context = task_context_fixture(program, config, tx);
        let to_find = ProgramBuilder::default()
            .filename(LpcPath::InGame(Path::new("/example").to_path_buf()))
            .build()
            .unwrap();
        let proc = Process::new(to_find);
        ObjectSpace::insert_process(context.object_space(), proc);

        let mut task = Task::<10>::new(context.clone());
        task.timed_eval(func.clone(), &[], 500)
            .await
            .expect("task failed");

        let LpcRef::Object(obj) = task.result().unwrap() else {
            panic!("expected object");
        };

        assert_eq!(
            obj.upgrade().unwrap().program.filename.to_str().unwrap(),
            "/example"
        );
    }

    #[tokio::test]
    async fn test_find_object_failure() {
        let code = indoc! { r#"
            object foo = find_object("/non-existent");
        "# };

        let (tx, _rx) = tokio::sync::mpsc::channel(128);

        let (program, config, _) = compile_prog(code).await;
        let func = program.initializer.clone().expect("no init found?");
        let context = task_context_fixture(program, config, tx);

        let mut task = Task::<10>::new(context.clone());
        task.timed_eval(func.clone(), &[], 500)
            .await
            .expect("task failed");

        assert_eq!(task.result().unwrap(), &NULL);
    }

    #[tokio::test]
    async fn test_creates_object() {
        let master = indoc! { r#"
            object create() {
                return find_object("/foo");
            }
        "# };

        let vm = Vm::new(test_config());

        {
            let mut current_path = PathBuf::from(test_config().lib_dir.to_owned());
            current_path.push("foo.c");

            let mut cache = FILE_CACHE.write();
            cache.add_eager(current_path.to_string_lossy(), "");
        }

        let master_proc = vm
            .process_initialize_from_code("/master.c", master)
            .await
            .unwrap();

        assert!(master_proc.context.object_space().lookup("/foo").is_some());
    }
}
