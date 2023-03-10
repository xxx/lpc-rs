use std::{cell::RefCell, path::PathBuf, rc::Rc};

use delegate::delegate;
use educe::Educe;
use lpc_rs_errors::Result;
use lpc_rs_utils::config::Config;
use qcell::{QCell, QCellOwner};

use crate::{
    interpreter::{
        instruction_counter::InstructionCounter, lpc_ref::LpcRef, object_space::ObjectSpace,
        process::Process, program::Program,
    },
    util::{get_simul_efuns, qcell_debug},
};

/// A struct to carry context during a single function's evaluation.
#[derive(Educe, Clone)]
#[educe(Debug)]
pub struct TaskContext {
    /// The [`Config`] that's in use for the
    /// [`Task`](crate::interpreter::task::Task).
    config: Rc<Config>,
    /// The [`Process`] that owns the function being
    /// called in this [`Task`](crate::interpreter::task::Task).
    #[educe(Debug(method = "qcell_debug"))]
    process: Rc<QCell<Process>>,
    /// The global [`ObjectSpace`]
    #[educe(Debug(method = "qcell_debug"))]
    object_space: Rc<QCell<ObjectSpace>>,
    /// A counter, to ensure that too-long-evaluations do not occur
    instruction_counter: InstructionCounter,
    /// The final result of the original function that was called
    #[educe(Debug(method = "qcell_debug"))]
    result: RefCell<LpcRef>,
    /// Direct pointer to the simul efuns
    #[educe(Debug(method = "qcell_debug"))]
    simul_efuns: Option<Rc<QCell<Process>>>,
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
    pub fn new<C, P, O>(config: C, process: P, object_space: O, cell_key: &QCellOwner) -> Self
    where
        C: Into<Rc<Config>>,
        P: Into<Rc<QCell<Process>>>,
        O: Into<Rc<QCell<ObjectSpace>>>,
    {
        let config = config.into();
        let object_space = object_space.into();
        let instruction_counter = InstructionCounter::new_from_config(&config);
        let simul_efuns = {
            let space = object_space.ro(cell_key);
            get_simul_efuns(&config, space)
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
        P: Into<Rc<QCell<Process>>>,
    {
        self.process = process.into();

        self
    }

    /// Lookup the process with the passed path.
    #[inline]
    pub fn lookup_process<T>(&self, path: T, cell_key: &QCellOwner) -> Option<Rc<QCell<Process>>>
    where
        T: AsRef<str>,
    {
        self.object_space.ro(cell_key).lookup(path).cloned()
    }

    /// Directly insert the passed [`Process`] into the object space, with
    /// in-game local filename.
    #[inline]
    pub fn insert_process<P>(&self, process: P, cell_key: &mut QCellOwner)
    where
        P: Into<Rc<QCell<Process>>>,
    {
        ObjectSpace::insert_process(&self.object_space, process, cell_key)
        // let process = process.into();

        // self.object_space.rw(cell_key).insert_process(process, &cell_key);
    }

    /// Convert the passed [`Program`] into a [`Process`], set its clone ID,
    /// then insert it into the object space.
    #[inline]
    pub fn insert_clone(
        &self,
        program: Rc<Program>,
        cell_key: &mut QCellOwner,
    ) -> Rc<QCell<Process>> {
        ObjectSpace::insert_clone(
            &self.object_space,
            program,
            // self.process.ro(cell_key).clone_id(),
            cell_key,
        )
        // self.object_space.rw(cell_key).insert_clone(program, &cell_key)
    }

    /// Get the in-game directory of the current process.
    /// This assumes an already-dedotted path
    pub fn in_game_cwd(&self, cell_key: &QCellOwner) -> PathBuf {
        let process = self.process.ro(cell_key);
        let current_cwd = process.cwd();

        match current_cwd.strip_prefix(&self.config.lib_dir) {
            Ok(x) => {
                

                if x.as_os_str().is_empty() {
                    PathBuf::from("/")
                } else if x.starts_with("/") {
                    x.to_path_buf()
                } else {
                    PathBuf::from(format!("/{}", x.display()))
                }
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
    pub fn simul_efuns(&self) -> Option<Rc<QCell<Process>>> {
        self.simul_efuns.clone()
    }

    /// Return the [`Process`] that the task roots from.
    /// This *does not* change over the life of the task.
    #[inline]
    pub fn process(&self) -> Rc<QCell<Process>> {
        self.process.clone()
    }

    /// Return the [`ObjectSpace`]
    #[inline]
    pub fn object_space(&self) -> &Rc<QCell<ObjectSpace>> {
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
        let cell_key = QCellOwner::new();
        let config = ConfigBuilder::default()
            .lib_dir("./tests/fixtures/code/")
            .build()
            .unwrap();
        let space = cell_key.cell(ObjectSpace::default());
        let program = ProgramBuilder::default()
            .filename(LpcPath::new_server("./tests/fixtures/code/foo/bar/baz.c"))
            .build()
            .unwrap();
        let process = Process::new(program);
        let context = TaskContext::new(config, cell_key.cell(process), space, &cell_key);

        assert_eq!(context.in_game_cwd(&cell_key).to_str().unwrap(), "/foo/bar");
    }
}
