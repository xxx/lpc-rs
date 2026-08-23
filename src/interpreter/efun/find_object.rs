use std::sync::Arc;

use lpc_rs_core::RegisterSize;
use lpc_rs_errors::Result;

use crate::interpreter::{
    efun::efun_context::EfunContext,
    lpc_ref::{LpcRef, NULL},
};

/// `find_object`, an efun for finding and returning an object from the [`ObjectSpace`]
/// from its path and clone number.
pub async fn find_object<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let lpc_ref = context.resolve_local_register(1 as RegisterSize);
    let result = match lpc_ref.as_str() {
        Some(path) => {
            let path = context.in_game_path(path);
            match context.load_object(&path).await {
                Ok(proc) => LpcRef::from(Arc::downgrade(&proc)),
                Err(_) => NULL,
            }
        }
        None => NULL,
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
            object_space::ObjectSpace,
            process::Process,
            program::{Program, ProgramBuilder},
            task::Task,
            task_context::TaskContext,
            vm::{Vm, global_state::GlobalState, vm_op::VmOp},
        },
        test_support::{compile_prog, test_config},
    };

    fn task_context_fixture(
        program: Program,
        config: Arc<Config>,
        tx: tokio::sync::mpsc::Sender<VmOp>,
    ) -> TaskContext {
        let process = Process::new(program);

        let global_state = GlobalState::new(config, tx);

        TaskContext::new(Arc::new(global_state), process, None)
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
        ObjectSpace::insert_process_physical(context.object_space(), proc);

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

        assert_eq!(task.result().unwrap(), NULL);
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
            .initialize_process_from_code("/master.c", master)
            .await
            .unwrap();

        assert!(master_proc.context.object_space().lookup("/foo").is_some());
    }

    // A clone created this transaction is not yet in the physical object map
    // (its insert is deferred to commit). So the same-transaction
    // `find_object` can only see it through the transactional cell. This is
    // the "clone usable immediately" behavior.
    #[tokio::test]
    async fn test_finds_clone_created_this_transaction() {
        let code = indoc! { r#"
            object clone;

            object create() {
                clone = clone_object("/example");
                return find_object("/example#0");
            }
        "# };

        let (tx, _rx) = tokio::sync::mpsc::channel(128);

        let (program, config, _) = compile_prog(code).await;
        let func = program.initializer.clone().expect("no init found?");
        let context = task_context_fixture(program, config, tx);

        let mut task = Task::<10>::new(context.clone());
        task.timed_eval(func.clone(), &[], 500)
            .await
            .expect("task failed");

        let LpcRef::Object(found) = task.result().unwrap() else {
            panic!("find_object should have found the just-created clone");
        };

        // `process.filename()` is the object path (the cell/physical-map key),
        // e.g. `/example#0` for a clone; `program.filename` is the source `/example.c`.
        assert_eq!(
            found.upgrade().unwrap().filename().as_ref(),
            "/example#0",
            "same-transaction find_object must see the clone created this txn"
        );
    }
}
