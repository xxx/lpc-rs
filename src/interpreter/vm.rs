use std::rc::Rc;

use bit_set::BitSet;
use educe::Educe;
use lpc_rs_core::lpc_path::LpcPath;
use lpc_rs_errors::Result;
use lpc_rs_utils::config::Config;
use qcell::{QCell, QCellOwner};
use tracing::instrument;

use crate::{
    compile_time_config::MAX_CALL_STACK_SIZE,
    compiler::CompilerBuilder,
    interpreter::{
        gc::{
            gc_bank::{GcBank, GcRefBank},
        },
        memory::Memory,
        object_space::ObjectSpace,
        task::Task,
        task_context::TaskContext,
    },
    util::{get_simul_efuns, qcell_debug},
};
use crate::interpreter::gc::sweep::{Sweep, KeylessSweep};

#[derive(Educe)]
#[educe(Debug)]
pub struct Vm {
    /// Our object space, which stores all of the system objects (masters and
    /// clones)
    #[educe(Debug(method = "qcell_debug"))]
    object_space: Rc<QCell<ObjectSpace>>,

    /// Shared VM memory
    memory: Memory,

    /// All upvalues are stored in the [`Vm`], and are shared between all [`Task`]s
    #[educe(Debug(method = "qcell_debug"))]
    upvalues: Rc<QCell<GcRefBank>>,

    /// The [`Config`] that's in use for this [`Vm`]
    config: Rc<Config>,
}

impl Vm {
    pub fn new<C>(config: C, cell_key: &QCellOwner) -> Self
    where
        C: Into<Rc<Config>>,
    {
        let object_space = ObjectSpace::default();
        Self {
            object_space: Rc::new(cell_key.cell(object_space)),
            memory: Memory::default(),
            config: config.into(),
            upvalues: Rc::new(cell_key.cell(GcBank::default())),
        }
    }

    /// The main initialization method for the VM.
    pub fn initialize(&mut self, cell_key: &mut QCellOwner) -> Result<()> {
        if let Some(path) = &self.config.simul_efun_file {
            let simul_efun_path = LpcPath::new_in_game(path, "/", &self.config.lib_dir);
            if let Err(e) = self.initialize_file(&simul_efun_path, cell_key) {
                e.emit_diagnostics();
                return Err(e);
            }
        }

        let master_path =
            LpcPath::new_in_game(&self.config.master_object, "/", &self.config.lib_dir);
        self.initialize_file(&master_path, cell_key)
            .map(|_| ())
            .map_err(|e| {
                e.emit_diagnostics();
                e
            })
    }

    fn initialize_file(
        &mut self,
        filename: &LpcPath,
        cell_key: &mut QCellOwner,
    ) -> Result<TaskContext> {
        let compiler = {
            let borrowed = self.object_space.ro(cell_key);
            CompilerBuilder::default()
                .config(self.config.clone())
                .simul_efuns(get_simul_efuns(&self.config, borrowed))
                .build()?
        };

        compiler
            .compile_in_game_file(filename, None, cell_key)
            .and_then(|program| {
                let mut task: Task<MAX_CALL_STACK_SIZE> =
                    Task::new(&self.memory, self.upvalues.clone());
                task.initialize_program(
                    program,
                    self.config.clone(),
                    self.object_space.clone(),
                    cell_key,
                )
            })
    }
}

impl Sweep for Vm {
    #[instrument(skip(self, cell_key))]
    fn sweep(&mut self, marked: &BitSet, cell_key: &mut QCellOwner) -> Result<()> {
        self.upvalues.rw(cell_key).keyless_sweep(marked)
    }
}
