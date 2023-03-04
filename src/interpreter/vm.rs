use std::rc::Rc;
use educe::Educe;
use qcell::{QCell, QCellOwner};

use lpc_rs_core::lpc_path::LpcPath;
use lpc_rs_errors::Result;
use lpc_rs_utils::config::Config;

use crate::{
    compile_time_config::MAX_CALL_STACK_SIZE,
    compiler::CompilerBuilder,
    interpreter::{
        memory::Memory, object_space::ObjectSpace, task::Task, task_context::TaskContext,
    },
    util::get_simul_efuns,
};

#[derive(Educe)]
#[educe(Debug)]
pub struct Vm {
    /// Our object space, which stores all of the system objects (masters and
    /// clones)
    #[educe(Debug(ignore))]
    object_space: Rc<QCell<ObjectSpace>>,

    /// Shared VM memory
    memory: Memory,

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
        self.initialize_file(&master_path, cell_key).map(|_| ()).map_err(|e| {
            e.emit_diagnostics();
            e
        })
    }

    fn initialize_file(&mut self, filename: &LpcPath, cell_key: &mut QCellOwner) -> Result<TaskContext> {
        let compiler = {
            let borrowed = self.object_space.ro(cell_key);
            CompilerBuilder::default()
                .config(self.config.clone())
                .simul_efuns(get_simul_efuns(&self.config, &borrowed))
                .build()?
        };

        compiler
            .compile_in_game_file(filename, None)
            .and_then(|program| {
                let mut task: Task<MAX_CALL_STACK_SIZE> = Task::new(&self.memory);
                task.initialize_program(program, self.config.clone(), self.object_space.clone(), cell_key)
            })
    }
}
