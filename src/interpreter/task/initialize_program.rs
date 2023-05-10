use std::sync::Arc;

use derive_builder::Builder;
use lpc_rs_core::register::Register;
use lpc_rs_errors::{lpc_error, Result};

use crate::interpreter::{
    process::Process, program::Program, task::Task, vm::global_state::GlobalState,
};

/// This struct exists solely to allow a Builder to be derived,
/// making calls to Task::initialize_program more ergonomic.
#[derive(Debug, Builder)]
#[builder(build_fn(name = "real_build"))]
pub struct InitializeProgram<const N: usize> {
    #[builder(setter(into), default = "Arc::new(Program::default())")]
    program: Arc<Program>,

    #[builder(setter(into))]
    global_state: Arc<GlobalState>,

    #[builder(setter(into), default = "None")]
    this_player: Option<Arc<Process>>,

    #[builder(setter(into), default = "None")]
    upvalue_ptrs: Option<Vec<Register>>,
}

impl<const N: usize> InitializeProgramBuilder<N> {
    pub async fn build(&self) -> Result<Task<N>> {
        let init = match self.real_build() {
            Ok(i) => i,
            Err(e) => return Err(lpc_error!(e.to_string())),
        };

        Task::initialize_program(
            init.program,
            init.global_state,
            init.this_player,
            init.upvalue_ptrs.as_deref(),
        )
        .await
    }
}
