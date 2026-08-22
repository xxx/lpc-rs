use std::{future::Future, sync::Arc};

use async_trait::async_trait;
use lpc_rs_core::lpc_path::LpcPath;
use lpc_rs_errors::{Result, lpc_bug};

use crate::{
    compile_time_config::MAX_CALL_STACK_SIZE,
    compiler::Compiler,
    interpreter::{
        object_flags::ObjectFlags,
        object_space::ObjectSpace,
        process::Process,
        program::Program,
        task::{Task, into_task_context::IntoTaskContext},
    },
    util::with_compiler::WithCompiler,
};

/// The one compile core: `compile` runs inside the single shared
/// `with_async_compiler` block and yields a [`Program`], which is wrapped in a
/// fresh, un-inserted [`Process`]. Compile only: no placement happens here.
async fn compile_to_process<W, F, Fut>(w: &W, compile: F) -> Result<Arc<Process>>
where
    W: WithCompiler + ?Sized,
    F: FnOnce(Compiler) -> Fut + Send,
    Fut: Future<Output = Result<Program>> + Send,
{
    let program = w.with_async_compiler(compile).await?;
    Ok(Arc::new(Process::new(program)))
}

/// Compile the in-game file at `path` into an un-inserted [`Process`] (no
/// placement), in `w`'s compiler.
pub(crate) async fn compile_process_from_path<W: WithCompiler + ?Sized>(
    w: &W,
    path: &LpcPath,
) -> Result<Arc<Process>> {
    compile_to_process(w, |compiler| async move {
        compiler.compile_in_game_file(path, None).await
    })
    .await
}

/// Compile `code` (masquerading as `filename`) into an un-inserted
/// [`Process`] (no placement), in `w`'s compiler.
pub(crate) async fn compile_process_from_code<W: WithCompiler + ?Sized, P, S>(
    w: &W,
    filename: P,
    code: S,
) -> Result<Arc<Process>>
where
    P: Into<LpcPath> + Send + Sync,
    S: AsRef<str> + Send + Sync,
{
    compile_to_process(w, |compiler| async move {
        compiler.compile_string(filename, code).await
    })
    .await
}

/// A convenience trait for creating [`Process`]es **physically** (blind, no
/// cell, no initialization), for bootstrap and fixture use. In-game creation
/// must use `insert_process_transactional` instead.
#[async_trait]
pub trait ProcessCreator: WithCompiler {
    /// The [`ObjectSpace`] to physically insert into.
    fn process_creator_data(&self) -> &ObjectSpace;

    /// Compile the passed file and physically insert it into the
    /// [`ObjectSpace`] (blind, no cell, no initialization). Bootstrap only.
    async fn create_process_from_path(&self, filename: &LpcPath) -> Result<Arc<Process>> {
        let process = compile_process_from_path(self, filename).await?;
        ObjectSpace::insert_process_physical(self.process_creator_data(), process.clone());
        Ok(process)
    }

    /// Compile the passed code (masquerading as `filename`) and physically
    /// insert it into the [`ObjectSpace`] (blind, no cell, no
    /// initialization). Bootstrap only.
    async fn create_process_from_code<P, S>(&self, filename: P, code: S) -> Result<Arc<Process>>
    where
        P: Into<LpcPath> + Send + Sync,
        S: AsRef<str> + Send + Sync,
    {
        let process = compile_process_from_code(self, filename, code).await?;
        ObjectSpace::insert_process_physical(self.process_creator_data(), process.clone());
        Ok(process)
    }
}

/// Physically insert `process` into the space it was compiled for (blind, no
/// cell, **before** its initializer runs, to prevent infinite loops), then
/// run the initializer in a fresh task. Bootstrap only.
pub(crate) async fn process_insert_and_initialize_program<T>(
    process: Arc<Process>,
    template: T,
) -> Result<Task<MAX_CALL_STACK_SIZE>>
where
    T: IntoTaskContext + Send,
{
    let Some(prog_function) = process.program.initializer.clone() else {
        return Err(lpc_bug!(
            "Init function not found? This shouldn't happen. Are you trying to initialize an empty program?"
        ));
    };

    let ctx = template.into_task_context(process.clone());

    ObjectSpace::insert_process_physical(ctx.object_space(), process.clone());

    // The Initialized flag is set before the initializer runs so an
    // initializer that calls `this_object()` cannot re-enter initialization.
    ctx.process.flags.set(ObjectFlags::Initialized);

    let max_execution_time = ctx.config().max_execution_time;
    let mut task = Task::<MAX_CALL_STACK_SIZE>::new(ctx);
    task.timed_eval(prog_function, &[], max_execution_time)
        .await?;

    Ok(task)
}
