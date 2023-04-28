use std::sync::Arc;

use async_trait::async_trait;
use lpc_rs_core::lpc_path::LpcPath;
use lpc_rs_errors::{lpc_bug, Result};

use crate::{
    compile_time_config::MAX_CALL_STACK_SIZE,
    interpreter::{
        object_space::ObjectSpace,
        process::Process,
        program::Program,
        task::{into_task_context::IntoTaskContext, Task},
    },
    util::with_compiler::WithCompiler,
};
use crate::interpreter::object_flags::ObjectFlags;

/// A convenience trait for creating and initializing [`Process`]es.
#[async_trait]
pub trait ProcessBuilder: WithCompiler {
    /// Compile the passed file, and insert it into the [`ObjectSpace`]
    /// _without_ initialization.
    async fn process_create_from_path(&self, filename: &LpcPath) -> Result<Arc<Process>>;

    /// Compile the passed code, masquerading as the passed filename, and insert it into the [`ObjectSpace`]
    async fn process_create_from_code<P, S>(&self, filename: P, code: S) -> Result<Arc<Process>>
    where
        P: Into<LpcPath> + Send + Sync,
        S: AsRef<str> + Send + Sync;

    /// Helper to directly insert a [`Program`] into the [`ObjectSpace`], _without_ initialization
    #[inline]
    async fn process_insert_program(
        program: Program,
        object_space: &Arc<ObjectSpace>,
    ) -> Arc<Process> {
        let process = Arc::new(Process::new(program));
        ObjectSpace::insert_process(object_space, process.clone());
        process
    }

    /// Compile the passed file, and insert it into the [`ObjectSpace`]
    /// _with_ initialization.
    async fn process_initialize_from_path(
        &self,
        filename: &LpcPath,
    ) -> Result<Task<MAX_CALL_STACK_SIZE>>;

    /// Compile the passed code, masquerading as the passed filename, and insert it into the [`ObjectSpace`]
    /// _with_ initialization.
    async fn process_initialize_from_code<P, S>(
        &self,
        filename: P,
        code: S,
    ) -> Result<Task<MAX_CALL_STACK_SIZE>>
    where
        P: Into<LpcPath> + Send + Sync,
        S: AsRef<str> + Send + Sync;

    /// Helper to directly insert a [`Program`] into the [`ObjectSpace`], _with_ initialization.
    async fn process_insert_and_initialize_program<T>(
        program: Program,
        template: T,
    ) -> Result<Task<MAX_CALL_STACK_SIZE>>
    where
        T: IntoTaskContext + Send,
    {
        let process = Arc::new(Process::new(program));
        let ctx = template.into_task_context(process.clone());

        // We insert *before* initialization.
        ObjectSpace::insert_process(&ctx.object_space, process.clone());

        let Some(prog_function) = process.program.initializer.clone() else {
            return Err(lpc_bug!("Init function not found? This shouldn't happen."));
        };

        // We mark ourselves as initialized before actually initializing, to avoid
        // infinite loops where this_object() is used in global initialization.
        ctx.process.flags.set(ObjectFlags::INITIALIZED);

        let max_execution_time = ctx.config.max_execution_time;
        let mut task = Task::<MAX_CALL_STACK_SIZE>::new(ctx);
        task.timed_eval(prog_function, &[], max_execution_time)
            .await?;


        Ok(task)
    }
}
