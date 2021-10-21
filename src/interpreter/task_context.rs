use std::rc::Rc;
use crate::util::config::Config;
use crate::interpreter::object_space::ObjectSpace;
use crate::interpreter::process::Process;
use std::path::PathBuf;
use crate::errors::LpcError;
use crate::Result;
use crate::interpreter::instruction_counter::InstructionCounter;
use delegate::delegate;
use std::cell::RefCell;

/// A struct to carry context during a single function's evaluation.
#[derive(Debug, Clone)]
pub struct TaskContext {
    /// The [`Config`] that's in use for the [`Task`]
    config: Rc<Config>,
    /// The [`Process`] that owns the function being called in this [`Task`]
    process: Rc<RefCell<Process>>,
    /// The global [`ObjectSpace`]
    object_space: Rc<RefCell<ObjectSpace>>,
    /// A counter, to ensure that too-long-evaluations do not occur
    instruction_counter: InstructionCounter
}

impl TaskContext {
    delegate! {
        to self.instruction_counter {
            /// Increment the current task instruction count, checking for too-long-evaluations
            #[call(increment)]
            pub fn increment_instruction_count(&self, amount: usize) -> Result<usize>;
        }
    }

    /// Create a new [`TaskContext`]
    pub fn new<C, P, O>(config: C, process: P, object_space: O) -> Self
    where
        C: Into<Rc<Config>>,
        P: Into<Rc<RefCell<Process>>>,
        O: Into<Rc<RefCell<ObjectSpace>>>
    {
        let config = config.into();
        let instruction_counter =  InstructionCounter::new_from_config(&*config);
        Self {
            config,
            process: process.into(),
            object_space: object_space.into(),
            instruction_counter,
        }
    }

    /// Set the process for an existing TaskContext
    pub fn with_process<P>(mut self, process: P) -> Self
    where
        P: Into<Rc<RefCell<Process>>>,
    {
        self.process = process.into();

        self
    }

    /// Lookup the process with the passed path.
    #[inline]
    pub fn lookup_process<T>(&self, path: T) -> Option<Rc<RefCell<Process>>>
    where
        T: AsRef<str>,
    {
        match self.object_space.borrow().lookup(path) {
            Some(x) => Some(x.clone()),
            None => None
        }
    }

    /// Directly insert the passed [`Process`] into the space, with in-game local filename.
    #[inline]
    pub fn insert_process<P>(&self, process: P)
    where
        P: Into<Rc<RefCell<Process>>>,
    {
        self.object_space.borrow_mut().insert_process(process);
    }

    /// Get the in-game directory of the current process
    pub fn in_game_cwd(&self) -> Result<PathBuf> {
        println!("in=game cwd {:?} || {:?}", self.process.borrow().cwd(), self.config.lib_dir());
        match self.process.borrow().cwd().strip_prefix(self.config.lib_dir()) {
            Ok(x) => Ok(x.to_path_buf()),
            Err(e) => Err(LpcError::new(format!("{} in TaskContext", e.to_string()))),
        }
    }

    /// Return the [`Config`] used for the task
    pub fn config(&self) -> Rc<Config> {
        self.config.clone()
    }

    /// Return the [`Process`] that the task roots from.
    /// This *does not* change over the life of the task.
    pub fn process(&self) -> Rc<RefCell<Process>> {
        self.process.clone()
    }

    /// Return the [`ObjectSpace`]
    pub fn object_space(&self) -> &Rc<RefCell<ObjectSpace>> {
        &self.object_space
    }
}