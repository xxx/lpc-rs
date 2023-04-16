use std::sync::Arc;
use derive_builder::Builder;
use parking_lot::RwLock;
use tokio::sync::mpsc::Sender;
use lpc_rs_utils::config::Config;
use crate::interpreter::call_outs::CallOuts;
use crate::interpreter::gc::gc_bank::GcRefBank;
use crate::interpreter::memory::Memory;
use crate::interpreter::object_space::ObjectSpace;
use crate::interpreter::program::Program;
use crate::interpreter::task::Task;
use crate::interpreter::vm::vm_op::VmOp;
use lpc_rs_errors::{LpcError, Result};

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
    #[builder(setter(into), default = "Arc::new(Memory::default())")]
    memory: Arc<Memory>,
    #[builder(setter(into), default = "Arc::new(RwLock::new(GcRefBank::default()))")]
    vm_upvalues: Arc<RwLock<GcRefBank>>,
    #[builder(setter(into), default = "Arc::new(RwLock::new(CallOuts::new(self.tx.as_ref().unwrap().clone())))")]
    call_outs: Arc<RwLock<CallOuts>>,

    tx: Sender<VmOp>,
}

impl<const N: usize> InitializeProgramBuilder<N> {
    pub async fn build(&self) -> Result<Task<N>> {
        let init = match self.real_build() {
            Ok(i) => i,
            Err(e) => return Err(LpcError::new(e.to_string()))
        };

        Task::initialize_program(
            init.program,
            init.config,
            init.object_space,
            init.memory,
            init.vm_upvalues,
            init.call_outs,
            init.tx,
        ).await
    }
}