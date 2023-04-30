use std::sync::Arc;

use async_trait::async_trait;
use lpc_rs_core::lpc_path::LpcPath;
use lpc_rs_errors::{lpc_bug, Result};

use crate::{
    compile_time_config::MAX_CALL_STACK_SIZE,
    interpreter::{
        object_flags::ObjectFlags,
        object_space::ObjectSpace,
        process::Process,
        program::Program,
        task::{into_task_context::IntoTaskContext, Task},
    },
    util::with_compiler::WithCompiler,
};
use crate::interpreter::task::task_template::TaskTemplate;

/// A convenience trait for creating [`Process`]es without initialization.
#[async_trait]
pub trait ProcessCreator: WithCompiler {
    /// Return the data that's required to create a [`Process`], and insert it
    /// into the [`ObjectSpace`] without any initialization.
    fn process_creator_data(&self) -> &ObjectSpace;

    /// Compile the passed file, and insert it into the [`ObjectSpace`]
    /// _without_ initialization.
    async fn process_create_from_path(&self, filename: &LpcPath) -> Result<Arc<Process>> {
        let object_space = self.process_creator_data();

        let proc = self
            .with_async_compiler(|compiler| async move {
                compiler.compile_in_game_file(filename, None).await
            })
            .await
            .map(
                |prog| process_insert_program(prog, object_space)
            )?;

        Ok(proc)
    }

    /// Compile the passed code, masquerading as the passed filename, and insert it into the [`ObjectSpace`],
    /// _without_ initialization.
    async fn process_create_from_code<P, S>(&self, filename: P, code: S) -> Result<Arc<Process>> where P: Into<LpcPath> + Send + Sync, S: AsRef<str> + Send + Sync {
        let object_space = self.process_creator_data();

        let proc = self
            .with_async_compiler(|compiler| async move {
                compiler.compile_string(filename, code).await
            })
            .await
            .map(
                |prog| process_insert_program(prog, object_space)
            )?;

        Ok(proc)
    }
}

/// Helper to directly insert a [`Program`] into the [`ObjectSpace`], _without_ initialization
#[inline]
fn process_insert_program(
    program: Program,
    object_space: &ObjectSpace,
) -> Arc<Process> {
    let process = Arc::new(Process::new(program));
    ObjectSpace::insert_process(object_space, process.clone());
    process
}

/// A convenience trait for creating and initializing [`Process`]es.
#[async_trait]
pub trait ProcessInitializer: WithCompiler {
    /// Return the data that's required to create a [`Process`], and insert it
    /// into the [`ObjectSpace`], and initialize it (i.e. initialize global variables,
    /// and apply create()).
    fn process_initializer_data(&self) -> TaskTemplate;

    /// Compile the passed file, and insert it into the [`ObjectSpace`]
    /// _with_ initialization.
    async fn process_initialize_from_path(
        &self,
        filename: &LpcPath,
    ) -> Result<Task<MAX_CALL_STACK_SIZE>> {
        let program = self
            .with_async_compiler(|compiler| async move {
                compiler.compile_in_game_file(filename, None).await
            })
            .await?;

        let template = self.process_initializer_data();
        process_insert_and_initialize_program(program, template).await
    }

    /// Compile the passed code, masquerading as the passed filename, and insert it into the [`ObjectSpace`]
    /// _with_ initialization.
    async fn process_initialize_from_code<P, S>(
        &self,
        filename: P,
        code: S,
    ) -> Result<Task<MAX_CALL_STACK_SIZE>>
        where
            P: Into<LpcPath> + Send + Sync,
            S: AsRef<str> + Send + Sync,
    {
        let template = self.process_initializer_data();

        let program = self
            .with_async_compiler(
                |compiler| async move { compiler.compile_string(filename, code).await },
            )
            .await?;

        process_insert_and_initialize_program(program, template).await
    }
}

/// Helper to directly insert a [`Program`] into the [`ObjectSpace`], _with_ initialization.
async fn process_insert_and_initialize_program<T>(
    program: Program,
    template: T,
) -> Result<Task<MAX_CALL_STACK_SIZE>>
    where
        T: IntoTaskContext + Send,
{
    let process = Arc::new(Process::new(program));

    let Some(prog_function) = process.program.initializer.clone() else {
        return Err(lpc_bug!("Init function not found? This shouldn't happen. Are you trying to initialize an empty program?"));
    };

    let ctx = template.into_task_context(process.clone());

    // We insert *before* initialization, to prevent infinite loops.
    ObjectSpace::insert_process(&ctx.object_space, process.clone());

    // We mark ourselves as initialized before actually initializing, to avoid
    // infinite loops where this_object() is used in global initialization.
    ctx.process.flags.set(ObjectFlags::INITIALIZED);

    let max_execution_time = ctx.config.max_execution_time;
    let mut task = Task::<MAX_CALL_STACK_SIZE>::new(ctx);
    task.timed_eval(prog_function, &[], max_execution_time)
        .await?;

    Ok(task)
}
