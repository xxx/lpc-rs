use std::{future::Future, sync::Arc};

use lpc_rs_core::lpc_path::LpcPath;
use lpc_rs_errors::{LpcError, Result};
use lpc_rs_utils::config::Config;

use crate::{
    compiler::{
        Compiled, Compiler, CompilerBuilder, ast::program_node::ProgramNode,
        compile_gate::CompileGate, source_reader::SourceReader,
    },
    interpreter::{
        object_space::ObjectSpace,
        process::Process,
        program::Program,
        task::{Task, task_template::TaskTemplate},
    },
    util::get_simul_efuns,
};

fn configured_compiler(
    object_space: &ObjectSpace,
    gate: Option<Arc<dyn CompileGate>>,
    source_reader: Option<Arc<dyn SourceReader>>,
    simul_efuns: Option<Arc<Program>>,
) -> Result<Compiler> {
    CompilerBuilder::default()
        .config(object_space.config().clone())
        .code_pool(object_space.code_pool.clone())
        .definition_cache(object_space.definition_cache.clone())
        .parser_cache(object_space.parser_cache.clone())
        .simul_efuns(simul_efuns)
        .gate(gate)
        .source_reader(source_reader)
        .build()
}

/// The one compile core: `compile` runs in a [`Compiler`] configured from
/// `object_space`, with `gate` installed, and yields a
/// [`Program`](crate::interpreter::program::Program) wrapped in a fresh,
/// un-inserted [`Process`], with the compile's warnings.
async fn compile_to_process<F, Fut>(
    object_space: &ObjectSpace,
    gate: Option<Arc<dyn CompileGate>>,
    source_reader: Option<Arc<dyn SourceReader>>,
    simul_efuns: Option<Arc<Program>>,
    compile: F,
) -> Result<(Arc<Process>, Vec<LpcError>)>
where
    F: FnOnce(Compiler) -> Fut + Send,
    Fut: Future<Output = Result<Compiled>> + Send,
{
    let compiler = configured_compiler(object_space, gate, source_reader, simul_efuns)?;
    let Compiled { program, warnings } = compile(compiler)
        .await
        .map_err(LpcError::with_catch_diagnostics)?;
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
    source_reader: Option<Arc<dyn SourceReader>>,
) -> Result<(Arc<Process>, Vec<LpcError>)> {
    compile_to_process(
        object_space,
        gate,
        source_reader,
        get_simul_efuns(object_space.config(), object_space)
            .map(|process| process.initial_program().clone()),
        |compiler| async move { compiler.compile_in_game_file(path, None).await },
    )
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
    source_reader: Option<Arc<dyn SourceReader>>,
) -> Result<(Arc<Process>, Vec<LpcError>)>
where
    P: Into<LpcPath> + Send + Sync,
    S: AsRef<str> + Send + Sync,
{
    compile_to_process(
        object_space,
        gate,
        source_reader,
        get_simul_efuns(object_space.config(), object_space)
            .map(|process| process.initial_program().clone()),
        |compiler| async move { compiler.compile_string(filename, code).await },
    )
    .await
}

/// Compile against the calling attempt's simul-efun binding.
pub(crate) async fn compile_process_in_context(
    ctx: &crate::interpreter::task_context::TaskContext,
    path: &LpcPath,
    code: Option<&str>,
    gate: Arc<dyn CompileGate>,
    source_reader: Arc<dyn SourceReader>,
) -> Result<(Arc<Process>, Vec<LpcError>)> {
    let simul = ctx.simul_efuns().map(|process| process.program(ctx.txn()));
    compile_to_process(
        ctx.object_space(),
        Some(gate),
        Some(source_reader),
        simul,
        |compiler| async move {
            match code {
                Some(code) => compiler.compile_string(path.source_file(), code).await,
                None => compiler.compile_in_game_file(path, None).await,
            }
        },
    )
    .await
}

/// Check supplied source against the attempt's dependencies without creating a process.
pub(crate) async fn analyze_in_context(
    ctx: &crate::interpreter::task_context::TaskContext,
    path: &LpcPath,
    code: &str,
    gate: Arc<dyn CompileGate>,
    source_reader: Arc<dyn SourceReader>,
) -> Result<(ProgramNode, Vec<LpcError>)> {
    let compiler = configured_compiler(
        ctx.object_space(),
        Some(gate),
        Some(source_reader),
        ctx.simul_efuns().map(|process| process.program(ctx.txn())),
    )?;
    let (node, mut context) = compiler
        .analyze_string(path.source_file(), code)
        .await
        .map_err(LpcError::with_catch_diagnostics)?;
    let own = context
        .diagnostics
        .finish()
        .map_err(LpcError::with_catch_diagnostics)?;
    let warnings = context
        .inherited_warnings
        .into_iter()
        .flat_map(|group| group.warnings)
        .chain(own)
        .collect();
    Ok((node, warnings))
}

/// Bootstrap an object, publishing system objects transactionally with initialization.
pub async fn process_insert_and_initialize_program<const N: usize>(
    process: Arc<Process>,
    template: TaskTemplate,
) -> Result<Task<N>> {
    let space = &template.global_state.object_space;
    if space.is_system_key(&space.process_key(&process)) {
        Task::bootstrap_system_process(template.into_task_context(process)).await
    } else {
        ObjectSpace::insert_process_physical(space, process.clone());
        Task::initialize_process(template.into_task_context(process)).await
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_support::log_capture::LogCapture;
    use lpc_rs_errors::lpc_warning;
    use lpc_rs_utils::{config::ConfigBuilder, debug_log::DebugLog};

    #[tokio::test]
    async fn host_compilation_warnings_keep_their_order_and_diagnostic_notes() {
        let log = LogCapture::default();
        let config = ConfigBuilder::default()
            .debug_log(DebugLog::new(log.clone()))
            .build()
            .unwrap();
        log_warnings(
            &config,
            vec![
                lpc_warning!("bootstrap-first").with_note("first diagnostic note"),
                lpc_warning!("bootstrap-second").with_note("second diagnostic note"),
            ],
        )
        .await;
        config.debug_log.as_ref().unwrap().flush().await;
        let rendered = log.contents();
        let first = rendered
            .find("bootstrap-first")
            .expect("first warning reached the log");
        let second = rendered
            .find("bootstrap-second")
            .expect("second warning reached the log");
        assert!(first < second);
        for message in [
            "bootstrap-first",
            "bootstrap-second",
            "first diagnostic note",
            "second diagnostic note",
        ] {
            assert_eq!(rendered.matches(message).count(), 1, "{rendered}");
        }
    }
}
