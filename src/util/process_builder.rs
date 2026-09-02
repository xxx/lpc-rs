use std::{future::Future, sync::Arc};

use lpc_rs_core::lpc_path::LpcPath;
use lpc_rs_errors::{LpcError, Result};
use lpc_rs_utils::config::Config;

use crate::{
    compiler::{Compiled, Compiler, CompilerBuilder, compile_gate::CompileGate},
    interpreter::{
        object_space::ObjectSpace,
        process::Process,
        task::{Task, task_template::TaskTemplate},
    },
    util::get_simul_efuns,
};

/// The one compile core: `compile` runs in a [`Compiler`] configured from
/// `object_space`, with `gate` installed, and yields a
/// [`Program`](crate::interpreter::program::Program) wrapped in a fresh,
/// un-inserted [`Process`], with the compile's warnings.
async fn compile_to_process<F, Fut>(
    object_space: &ObjectSpace,
    gate: Option<Arc<dyn CompileGate>>,
    compile: F,
) -> Result<(Arc<Process>, Vec<LpcError>)>
where
    F: FnOnce(Compiler) -> Fut + Send,
    Fut: Future<Output = Result<Compiled>> + Send,
{
    let config = object_space.config();
    let compiler = CompilerBuilder::default()
        .config(config.clone())
        .simul_efuns(get_simul_efuns(config, object_space))
        .gate(gate)
        .build()?;
    let Compiled { program, warnings } = compile(compiler).await?;
    let warnings = warnings.into_iter().flat_map(|w| w.warnings).collect();
    Ok((Arc::new(Process::new(program)), warnings))
}

/// Where warnings go with no task running to hand them to the master: the
/// debug log, one rendered diagnostic each.
pub(crate) async fn log_warnings(config: &Config, warnings: Vec<LpcError>) {
    for warning in warnings {
        config.debug_log(warning.diagnostic_string()).await;
    }
}

/// Compile the in-game file at `path` into an un-inserted [`Process`] (no
/// placement), in `object_space`'s compiler with `gate` installed, with the
/// compile's warnings.
pub(crate) async fn compile_process_from_path(
    object_space: &ObjectSpace,
    path: &LpcPath,
    gate: Option<Arc<dyn CompileGate>>,
) -> Result<(Arc<Process>, Vec<LpcError>)> {
    compile_to_process(object_space, gate, |compiler| async move {
        compiler.compile_in_game_file(path, None).await
    })
    .await
}

/// Compile `code` (masquerading as `filename`) into an un-inserted
/// [`Process`] (no placement), in `object_space`'s compiler with `gate`
/// installed, with the compile's warnings.
pub(crate) async fn compile_process_from_code<P, S>(
    object_space: &ObjectSpace,
    filename: P,
    code: S,
    gate: Option<Arc<dyn CompileGate>>,
) -> Result<(Arc<Process>, Vec<LpcError>)>
where
    P: Into<LpcPath> + Send + Sync,
    S: AsRef<str> + Send + Sync,
{
    compile_to_process(object_space, gate, |compiler| async move {
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
