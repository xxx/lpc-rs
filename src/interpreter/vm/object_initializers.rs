use std::{path::Path, sync::Arc};

use lpc_rs_core::lpc_path::LpcPath;
use lpc_rs_errors::Result;

use crate::{
    compile_time_config::MAX_CALL_STACK_SIZE,
    interpreter::{
        process::Process,
        task::{self, task_template::TaskTemplate},
        task_context::TaskContext,
        vm::{Vm, global_state::GlobalState},
    },
    util::process_builder::{
        compile_process_from_code, compile_process_from_path, log_warnings,
        process_insert_and_initialize_program,
    },
};

impl GlobalState {
    /// Initialize the simulated efuns file, if it is configured.
    ///
    /// # Returns
    ///
    /// * `Some(Ok(()))` - The simul_efun file was loaded successfully
    /// * `Some(Err(LpcError))` - If there was an error loading the simul_efun file
    /// * `None` - If there is no simul_efun file configured
    pub async fn initialize_simul_efuns(&self) -> Option<Result<()>> {
        let path = self.config.simul_efun_file.as_ref()?;

        let simul_efun_path = LpcPath::new_in_game(path.as_str(), "/", &*self.config.lib_dir);
        Some(
            self.object_space
                .create_process_from_path(&simul_efun_path)
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
    ///   with [`lib_dir`](lpc_rs_utils::config::Config) as the root.
    ///
    /// # Returns
    ///
    /// * `Ok(TaskContext)` - The [`TaskContext`] for the code
    /// * `Err(LpcError)` - If there was an error compiling or initializing the code
    pub async fn initialize_string<P, S>(
        self: &Arc<Self>,
        code: S,
        filename: P,
    ) -> Result<TaskContext>
    where
        P: AsRef<Path>,
        S: AsRef<str> + Send + Sync,
    {
        let lpc_path = LpcPath::new_in_game(filename.as_ref(), "/", &*self.config.lib_dir);
        self.config.validate_in_game_path(&lpc_path, None)?;

        self.initialize_process_from_code(lpc_path, code)
            .await
            .map(|t| t.context)
    }

    /// Compile the in-game file at `path` and initialize it (insert it into the
    /// [`ObjectSpace`](crate::interpreter::object_space::ObjectSpace), then run its initializer in a fresh task).
    pub async fn initialize_process_from_path(
        self: &Arc<Self>,
        path: &LpcPath,
    ) -> Result<task::Task<MAX_CALL_STACK_SIZE>> {
        let (process, warnings) = compile_process_from_path(&self.object_space, path).await?;
        log_warnings(&self.config, warnings).await;
        process_insert_and_initialize_program(process, TaskTemplate::from(self.clone())).await
    }

    /// Compile `code` (masquerading as `filename`) and initialize it (insert it
    /// into the [`ObjectSpace`](crate::interpreter::object_space::ObjectSpace), then run its initializer in a fresh task).
    pub async fn initialize_process_from_code<P, S>(
        self: &Arc<Self>,
        filename: P,
        code: S,
    ) -> Result<task::Task<MAX_CALL_STACK_SIZE>>
    where
        P: Into<LpcPath> + Send + Sync,
        S: AsRef<str> + Send + Sync,
    {
        let (process, warnings) =
            compile_process_from_code(&self.object_space, filename, code).await?;
        log_warnings(&self.config, warnings).await;
        process_insert_and_initialize_program(process, TaskTemplate::from(self.clone())).await
    }
}

impl Vm {
    /// See [`GlobalState::initialize_simul_efuns`].
    pub async fn initialize_simul_efuns(&self) -> Option<Result<()>> {
        self.global_state.initialize_simul_efuns().await
    }

    /// See [`GlobalState::initialize_string`].
    ///
    /// # Examples
    ///
    /// ```
    /// # tokio_test::block_on(async {
    /// use lpc_rs::interpreter::{CommittedReader, lpc_int::LpcInt, lpc_ref::LpcRef, vm::Vm};
    /// use lpc_rs_utils::config::Config;
    ///
    /// let vm = Vm::new(Config::default());
    /// let ctx = vm.initialize_string("int x = 5;", "test.c").await.unwrap();
    ///
    /// let value = vm.global_state.committed_global(&ctx.process, 0u16);
    /// assert_eq!(value, LpcRef::Int(LpcInt(5)));
    ///
    /// assert!(vm.global_state.object_space.lookup("/test").is_some());
    /// # })
    /// ```
    pub async fn initialize_string<P, S>(&self, code: S, filename: P) -> Result<TaskContext>
    where
        P: AsRef<Path>,
        S: AsRef<str> + Send + Sync,
    {
        self.global_state.initialize_string(code, filename).await
    }

    /// See [`GlobalState::initialize_process_from_path`].
    pub async fn initialize_process_from_path(
        &self,
        path: &LpcPath,
    ) -> Result<task::Task<MAX_CALL_STACK_SIZE>> {
        self.global_state.initialize_process_from_path(path).await
    }

    /// See [`GlobalState::initialize_process_from_code`].
    pub async fn initialize_process_from_code<P, S>(
        &self,
        filename: P,
        code: S,
    ) -> Result<task::Task<MAX_CALL_STACK_SIZE>>
    where
        P: Into<LpcPath> + Send + Sync,
        S: AsRef<str> + Send + Sync,
    {
        self.global_state
            .initialize_process_from_code(filename, code)
            .await
    }

    /// Compile the in-game file at `path` and physically insert it. Bootstrap
    /// only; see [`ObjectSpace::create_process_from_path`](crate::interpreter::object_space::ObjectSpace::create_process_from_path).
    pub async fn create_process_from_path(&self, path: &LpcPath) -> Result<Arc<Process>> {
        self.global_state
            .object_space
            .create_process_from_path(path)
            .await
    }

    /// Compile `code` (masquerading as `filename`) and physically insert it.
    /// Bootstrap and test fixtures only; see
    /// [`ObjectSpace::create_process_from_code`](crate::interpreter::object_space::ObjectSpace::create_process_from_code).
    pub async fn create_process_from_code<P, S>(&self, filename: P, code: S) -> Result<Arc<Process>>
    where
        P: Into<LpcPath> + Send + Sync,
        S: AsRef<str> + Send + Sync,
    {
        self.global_state
            .object_space
            .create_process_from_code(filename, code)
            .await
    }
}
