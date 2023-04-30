use std::{future::Future, path::Path, sync::Arc};

use arc_swap::ArcSwapAny;
use async_trait::async_trait;
use lpc_rs_core::lpc_path::LpcPath;
use lpc_rs_errors::Result;

use crate::{
    compile_time_config::MAX_CALL_STACK_SIZE,
    compiler::Compiler,
    interpreter::{
        process::Process,
        task::{task_template::TaskTemplate, Task},
        task_context::TaskContext,
        vm::Vm,
    },
    util::{with_compiler::WithCompiler},
};
use crate::interpreter::object_space::ObjectSpace;
use crate::util::process_builder::{ProcessCreator, ProcessInitializer};

impl Vm {
    /// Initialize the simulated efuns file, if it is configured.
    ///
    /// # Returns
    ///
    /// * `Some(Ok(()))` - The simul_efun file was loaded successfully
    /// * `Some(Err(LpcError))` - If there was an error loading the simul_efun file
    /// * `None` - If there is no simul_efun file configured
    pub async fn initialize_simul_efuns(&mut self) -> Option<Result<()>> {
        let Some(path) = &self.config.simul_efun_file else {
            return None
        };

        let simul_efun_path = LpcPath::new_in_game(path.as_str(), "/", &*self.config.lib_dir);
        Some(
            self.process_create_from_path(&simul_efun_path)
                .await
                .map(|_| ()),
        )
    }

    /// Compile and initialize arbitrary code from the passed string.
    /// The filename is assigned as if the code were read from a real file.
    ///
    /// # Arguments
    ///
    /// * `code` - The code to compile and initialize
    /// * `filename` - The filename to assign to the code. It's assumed to be an in-game path,
    ///                with [`lib_dir`](lpc_rs_utils::config::Config) as the root.
    ///
    /// # Returns
    ///
    /// * `Ok(TaskContext)` - The [`TaskContext`] for the code
    /// * `Err(LpcError)` - If there was an error compiling or initializing the code
    ///
    /// # Examples
    ///
    /// ```
    /// # tokio_test::block_on(async {
    /// use lpc_rs::interpreter::{lpc_int::LpcInt, lpc_ref::LpcRef, vm::Vm};
    /// use lpc_rs_utils::config::Config;
    ///
    /// let mut vm = Vm::new(Config::default());
    /// let ctx = vm.initialize_string("int x = 5;", "test.c").await.unwrap();
    ///
    /// assert_eq!(
    ///     ctx.process().globals.read().registers[0],
    ///     LpcRef::Int(LpcInt(5))
    /// );
    /// assert!(vm.object_space.lookup("/test").is_some());
    /// # })
    /// ```
    pub async fn initialize_string<P, S>(&mut self, code: S, filename: P) -> Result<TaskContext>
    where
        P: AsRef<Path>,
        S: AsRef<str> + Send + Sync,
    {
        let lpc_path = LpcPath::new_in_game(filename.as_ref(), "/", &*self.config.lib_dir);
        self.config.validate_in_game_path(&lpc_path, None)?;

        self.process_initialize_from_code(&lpc_path, code).await.map(|t| t.context)
        // let prog = self
        //     .with_async_compiler(
        //         |compiler| async move { compiler.compile_string(lpc_path, code).await },
        //     )
        //     .await?;
        //
        // Self::process_insert_and_initialize_program(prog, self.new_task_template())
        //     .await
        //     .map(|t| t.context)
    }

    /// A convenience helper to create a populated [`TaskTemplate`]
    pub fn new_task_template(&self) -> TaskTemplate {
        TaskTemplate {
            config: self.config.clone(),
            object_space: self.object_space.clone(),
            memory: self.memory.clone(),
            vm_upvalues: self.upvalues.clone(),
            call_outs: self.call_outs.clone(),
            this_player: ArcSwapAny::from(None),
            upvalue_ptrs: None,
            tx: self.tx.clone(),
        }
    }
}

#[async_trait]
impl WithCompiler for Vm {
    async fn with_async_compiler<F, U, T>(&self, f: F) -> Result<T>
    where
        F: FnOnce(Compiler) -> U + Send,
        U: Future<Output = Result<T>> + Send,
    {
        Self::with_async_compiler_associated(f, &self.config, &self.object_space).await
    }
}

#[async_trait]
impl ProcessCreator for Vm {
    #[inline]
    fn process_creator_data(&self) -> &ObjectSpace {
        &self.object_space
    }
}

#[async_trait]
impl ProcessInitializer for Vm {
    #[inline]
    fn process_initializer_data(&self) -> TaskTemplate {
        self.new_task_template()
    }
}
