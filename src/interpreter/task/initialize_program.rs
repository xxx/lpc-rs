use std::sync::Arc;

use derive_builder::Builder;
use lpc_rs_errors::{LpcError, Result};
use lpc_rs_utils::config::Config;
use parking_lot::RwLock;
use tokio::sync::mpsc::Sender;

use crate::interpreter::{
    call_outs::CallOuts, gc::gc_bank::GcRefBank, heap::Heap, object_space::ObjectSpace,
    process::Process, program::Program, task::Task, vm::vm_op::VmOp,
};

/// This struct exists solely to allow a Builder to be derived,
/// making calls to Task::initialize_program more ergonomic.
#[derive(Debug, Builder)]
#[builder(build_fn(name = "real_build"))]
pub struct InitializeProgram<const N: usize> {
    #[builder(setter(into), default = "Arc::new(Program::default())")]
    program: Arc<Program>,
    #[builder(setter(into), default = "Arc::new(Config::default())")]
    config: Arc<Config>,
    #[builder(setter(into), default = "Arc::new(ObjectSpace::default())")]
    object_space: Arc<ObjectSpace>,
    #[builder(setter(into), default = "Arc::new(Heap::default())")]
    memory: Arc<Heap>,
    #[builder(setter(into), default = "Arc::new(RwLock::new(GcRefBank::default()))")]
    vm_upvalues: Arc<RwLock<GcRefBank>>,
    #[builder(
        setter(into),
        default = "Arc::new(RwLock::new(CallOuts::new(self.tx.as_ref().unwrap().clone())))"
    )]
    call_outs: Arc<RwLock<CallOuts>>,
    #[builder(setter(into), default = "None")]
    this_player: Option<Arc<Process>>,

    tx: Sender<VmOp>,
}

impl<const N: usize> InitializeProgramBuilder<N> {
    pub async fn build(&self) -> Result<Task<N>> {
        let init = match self.real_build() {
            Ok(i) => i,
            Err(e) => return Err(LpcError::new(e.to_string())),
        };

        Task::initialize_program(
            init.program,
            init.config,
            init.object_space,
            init.memory,
            init.vm_upvalues,
            init.call_outs,
            init.this_player,
            init.tx,
        )
        .await
    }
}
