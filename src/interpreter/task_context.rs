use std::{cell::RefCell, path::PathBuf, rc::Rc};

use delegate::delegate;
use lpc_rs_errors::Result;
use lpc_rs_utils::config::Config;

use crate::{
    interpreter::{
        instruction_counter::InstructionCounter, lpc_ref::LpcRef, object_space::ObjectSpace,
        process::Process, program::Program,
    },
    util::get_simul_efuns,
};

/// A struct to carry context during a single function's evaluation.
#[derive(Debug, Clone)]
pub struct TaskContext {
    /// The [`Config`] that's in use for the
    /// [`Task`](crate::interpreter::task::Task).
    config: Rc<Config>,
    /// The [`Process`] that owns the function being
    /// called in this [`Task`](crate::interpreter::task::Task).
    process: Rc<RefCell<Process>>,
    /// The global [`ObjectSpace`]
    object_space: Rc<RefCell<ObjectSpace>>,
    /// A counter, to ensure that too-long-evaluations do not occur
    instruction_counter: InstructionCounter,
    /// The final result of the original function that was called
    result: RefCell<LpcRef>,
    /// Direct pointer to the simul efuns
    simul_efuns: Option<Rc<RefCell<Process>>>,
}

impl TaskContext {
    delegate! {
        to self.instruction_counter {
            /// Increment the current task instruction count, checking for too-long-evaluations
            #[call(increment)]
            pub fn increment_instruction_count(&self, amount: usize) -> Result<usize>;

            /// Get the current instruction count
            #[call(count)]
            pub fn instruction_count(&self) -> usize;
        }
    }

    /// Create a new [`TaskContext`]
    pub fn new<C, P, O>(config: C, process: P, object_space: O) -> Self
    where
        C: Into<Rc<Config>>,
        P: Into<Rc<RefCell<Process>>>,
        O: Into<Rc<RefCell<ObjectSpace>>>,
    {
        let config = config.into();
        let object_space = object_space.into();
        let instruction_counter = InstructionCounter::new_from_config(&config);
        let simul_efuns = {
            let space = object_space.borrow();
            get_simul_efuns(&config, &space)
        };

        Self {
            config,
            process: process.into(),
            object_space,
            instruction_counter,
            result: RefCell::new(LpcRef::default()),
            simul_efuns,
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
        self.object_space.borrow().lookup(path).cloned()
    }

    /// Directly insert the passed [`Process`] into the object space, with
    /// in-game local filename.
    #[inline]
    pub fn insert_process<P>(&self, process: P)
    where
        P: Into<Rc<RefCell<Process>>>,
    {
        self.object_space.borrow_mut().insert_process(process);
    }

    /// Convert the passed [`Program`] into a [`Process`], set its clone ID,
    /// then insert it into the object space.
    pub fn insert_clone(&self, program: Rc<Program>) -> Rc<RefCell<Process>> {
        self.object_space.borrow_mut().insert_clone(program)
    }

    /// Get the in-game directory of the current process.
    /// This assumes an already-dedotted path
    pub fn in_game_cwd(&self) -> PathBuf {
        let process = self.process.borrow();
        let current_cwd = process.cwd();

        // TODO: If process / program cwd is intended to be the absolute in-game path,
        //       then why are we stripping the prefix here?
        match current_cwd.strip_prefix(&self.config.lib_dir) {
            Ok(x) => {
                let buf = if x.as_os_str().is_empty() {
                    PathBuf::from("/")
                } else if x.starts_with("/") {
                    x.to_path_buf()
                } else {
                    PathBuf::from(format!("/{}", x.display()))
                };

                buf
            }
            Err(_e) => current_cwd.into_owned(),
        }
    }

    /// Update the context's `result` with the passed [`LpcRef`]
    #[inline]
    pub fn set_result(&self, new_result: LpcRef) {
        self.result.replace(new_result);
    }

    /// Consume this context, and return its `result` field.
    #[inline]
    pub fn into_result(self) -> LpcRef {
        self.result.into_inner()
    }

    /// Return the [`Config`] used for the task
    #[inline]
    pub fn config(&self) -> Rc<Config> {
        self.config.clone()
    }

    /// Return the current pointer to the simul_efuns, if any
    #[inline]
    pub fn simul_efuns(&self) -> Option<Rc<RefCell<Process>>> {
        self.simul_efuns.clone()
    }

    /// Return the [`Process`] that the task roots from.
    /// This *does not* change over the life of the task.
    #[inline]
    pub fn process(&self) -> Rc<RefCell<Process>> {
        self.process.clone()
    }

    /// Return the [`ObjectSpace`]
    #[inline]
    pub fn object_space(&self) -> &Rc<RefCell<ObjectSpace>> {
        &self.object_space
    }
}

#[cfg(test)]
mod tests {
    use lpc_rs_core::lpc_path::LpcPath;
    use lpc_rs_utils::config::ConfigBuilder;

    use super::*;
    use crate::interpreter::program::ProgramBuilder;

    #[test]
    fn test_in_game_cwd() {
        let config = ConfigBuilder::default()
            .lib_dir("./tests/fixtures/code/")
            .build()
            .unwrap();
        let space = ObjectSpace::default();
        let program = ProgramBuilder::default()
            .filename(LpcPath::new_server("./tests/fixtures/code/foo/bar/baz.c"))
            .build()
            .unwrap();
        let process = Process::new(program);
        let context = TaskContext::new(config, process, space);

        assert_eq!(context.in_game_cwd().to_str().unwrap(), "/foo/bar");
    }
}
