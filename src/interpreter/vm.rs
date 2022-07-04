use std::cell::RefCell;
use std::rc::Rc;
use crate::compiler::Compiler;
use crate::interpreter::{memory::Memory, object_space::ObjectSpace};
use crate::interpreter::task::Task;
use crate::util::config::Config;
use crate::Result;
use crate::util::path_maker::LpcPath;
use crate::compile_time_config::MAX_CALL_STACK_SIZE;
use crate::interpreter::task_context::TaskContext;

#[derive(Debug)]
pub struct Vm {
    /// Our object space, which stores all of the system objects (masters and clones)
    pub object_space: Rc<RefCell<ObjectSpace>>,

    /// Shared VM memory
    pub memory: Memory,

    pub config: Rc<Config>,
}

impl Vm {
    pub fn new<C>(config: C) -> Self
    where
        C: Into<Rc<Config>>,
    {

        let object_space = ObjectSpace::default();
        Self {
            object_space: Rc::new(RefCell::new(object_space)),
            memory: Memory::default(),
            config: config.into(),
        }
    }

    /// The main initialization method for the VM.
    pub fn initialize(&mut self) -> Result<()> {
        if let Some(path) = self.config.simul_efun_file() {
            let simul_efun_path = LpcPath::new_in_game(
                path, "/", self.config.lib_dir()
            );
            if let Err(e) = self.initialize_file(&simul_efun_path) {
                e.emit_diagnostics();
                return Err(e);
            }
        }

        let master_path = LpcPath::new_in_game(self.config.master_object(), "/", self.config.lib_dir());
        self.initialize_file(&master_path).map(|_| ()).or_else(|e| {
            e.emit_diagnostics();
            Err(e)
        })
    }

    fn initialize_file(&mut self, filename: &LpcPath) -> Result<TaskContext> {
        let compiler = Compiler::new(self.config.clone());

        compiler.compile_in_game_file(filename, None).and_then(|program| {
            let mut task: Task<MAX_CALL_STACK_SIZE> = Task::new(&self.memory);
            task.initialize_program(program, self.config.clone(), self.object_space.clone())
        })
    }
}
