use std::{future::Future, sync::Arc};

use lpc_rs_core::lpc_path::LpcPath;
use lpc_rs_errors::Result;

use crate::{
    compiler::{Compiler, CompilerBuilder},
    interpreter::{
        object_space::ObjectSpace,
        process::Process,
        program::Program,
        task::{Task, task_template::TaskTemplate},
    },
    util::get_simul_efuns,
};

/// The one compile core: `compile` runs in a [`Compiler`] configured from
/// `object_space` and yields a [`Program`], wrapped in a fresh, un-inserted
/// [`Process`].
async fn compile_to_process<F, Fut>(object_space: &ObjectSpace, compile: F) -> Result<Arc<Process>>
where
    F: FnOnce(Compiler) -> Fut + Send,
    Fut: Future<Output = Result<Program>> + Send,
{
    let config = object_space.config();
    let compiler = CompilerBuilder::default()
        .config(config.clone())
        .simul_efuns(get_simul_efuns(config, object_space))
        .build()?;
    let program = compile(compiler).await?;
    Ok(Arc::new(Process::new(program)))
}

/// Compile the in-game file at `path` into an un-inserted [`Process`] (no
/// placement), in `object_space`'s compiler.
pub(crate) async fn compile_process_from_path(
    object_space: &ObjectSpace,
    path: &LpcPath,
) -> Result<Arc<Process>> {
    compile_to_process(object_space, |compiler| async move {
        compiler.compile_in_game_file(path, None).await
    })
    .await
}

/// Compile `code` (masquerading as `filename`) into an un-inserted
/// [`Process`] (no placement), in `object_space`'s compiler.
pub(crate) async fn compile_process_from_code<P, S>(
    object_space: &ObjectSpace,
    filename: P,
    code: S,
) -> Result<Arc<Process>>
where
    P: Into<LpcPath> + Send + Sync,
    S: AsRef<str> + Send + Sync,
{
    compile_to_process(object_space, |compiler| async move {
        compiler.compile_string(filename, code).await
    })
    .await
}

/// Physically insert `process` into the space it was compiled for (blind, no
/// cell, **before** its initializer runs, to prevent infinite loops), then
/// run the initializer in a fresh task. Bootstrap only.
pub async fn process_insert_and_initialize_program<const N: usize>(
    process: Arc<Process>,
    template: TaskTemplate,
) -> Result<Task<N>> {
    let ctx = template.into_task_context(process.clone());

    ObjectSpace::insert_process_physical(ctx.object_space(), process);

    Task::initialize_process(ctx).await
}
