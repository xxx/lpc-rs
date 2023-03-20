use std::{cmp::Ordering, fmt::Formatter, hash::Hasher, path::Path, rc::Rc};
use std::collections::HashSet;

use bit_set::BitSet;
use educe::Educe;
use lpc_rs_core::lpc_path::LpcPath;
use lpc_rs_errors::Result;
use lpc_rs_utils::config::Config;
use qcell::{QCell, QCellOwner};
use tracing::instrument;

use crate::{
    compile_time_config::MAX_CALL_STACK_SIZE,
    compiler::{Compiler, CompilerBuilder},
    interpreter::{
        gc::{
            gc_bank::{GcBank, GcRefBank},
            sweep::{KeylessSweep, Sweep},
        },
        memory::Memory,
        object_space::ObjectSpace,
        program::Program,
        task::Task,
        task_context::TaskContext,
    },
    util::{get_simul_efuns, keyable::Keyable, qcell_debug},
};
use crate::interpreter::gc::mark::Mark;
use crate::interpreter::gc::unique_id::UniqueId;

#[derive(Educe)]
#[educe(Debug)]
#[readonly::make]
pub struct Vm {
    /// Our object space, which stores all of the system objects (masters and
    /// clones)
    #[educe(Debug(method = "qcell_debug"))]
    pub object_space: Rc<QCell<ObjectSpace>>,

    /// Shared VM memory
    memory: Memory,

    /// All upvalues are stored in the [`Vm`], and are shared between all [`Task`]s
    #[educe(Debug(method = "qcell_debug"))]
    pub upvalues: Rc<QCell<GcRefBank>>,

    /// The [`Config`] that's in use for this [`Vm`]
    config: Rc<Config>,
}

impl Vm {
    /// Create a new [`Vm`].
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
    ///
    /// This method will load the master object and simul_efun file, and add
    /// the master object to the object space.
    ///
    /// # Arguments
    ///
    /// * `cell_key` - The [`QCellOwner`] that will be used to create _all_ [`QCell`]s
    ///                in the system. Don't lose this key.
    ///
    /// # Returns
    ///
    /// * `Ok(TaskContext)` - The [`TaskContext`] for the master object
    /// * `Err(LpcError)` - If there was an error loading the master object or simul_efun file.
    pub fn boot(&mut self, cell_key: &mut QCellOwner) -> Result<TaskContext> {
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
    }

    /// Compile and initialize code from the passed file.
    fn initialize_file(
        &mut self,
        filename: &LpcPath,
        cell_key: &mut QCellOwner,
    ) -> Result<TaskContext> {
        debug_assert!(matches!(filename, &LpcPath::InGame(_)));

        self.with_compiler(cell_key, |compiler, cell_key| {
            compiler.compile_in_game_file(filename, None, cell_key)
        })
        .and_then(|program| self.create_and_initialize_task(program, cell_key))
        .map_err(|e| {
            e.emit_diagnostics();
            e
        })
    }

    /// Compile and initialize arbitrary code from the passed string.
    /// The filename is assigned as if the code were read from a real file.
    ///
    /// # Arguments
    ///
    /// * `code` - The code to compile and initialize
    /// * `filename` - The filename to assign to the code. It's assumed to be an in-game path,
    ///                with [`lib_dir`](lpc_rs_utils::config::Config) as the root.
    /// * `cell_key` - The [`QCellOwner`] that will be used to create any necessary [`QCell`]s
    ///
    /// # Returns
    ///
    /// * `Ok(TaskContext)` - The [`TaskContext`] for the code
    /// * `Err(LpcError)` - If there was an error compiling or initializing the code
    ///
    /// # Examples
    ///
    /// ```
    /// use lpc_rs::interpreter::{lpc_ref::LpcRef, vm::Vm};
    /// use lpc_rs_utils::config::Config;
    /// use qcell::QCellOwner;
    ///
    /// let mut cell_key = QCellOwner::new();
    /// let mut vm = Vm::new(Config::default(), &cell_key);
    /// let ctx = vm
    ///     .initialize_string("int x = 5;", "test.c", &mut cell_key)
    ///     .unwrap();
    ///
    /// assert_eq!(
    ///     ctx.process().ro(&cell_key).globals.registers[0],
    ///     LpcRef::Int(5)
    /// );
    /// assert!(vm.object_space.ro(&cell_key).lookup("/test").is_some());
    /// ```
    pub fn initialize_string<P, S>(
        &mut self,
        code: S,
        filename: P,
        cell_key: &mut QCellOwner,
    ) -> Result<TaskContext>
    where
        P: AsRef<Path>,
        S: AsRef<str>,
    {
        let f = LpcPath::new_in_game(filename.as_ref(), "/", &self.config.lib_dir);
        self.config.validate_in_game_path(&f, None)?;

        self.with_compiler(cell_key, |compiler, cell_key| {
            compiler.compile_string(f, code, cell_key)
        })
        .and_then(|program| self.create_and_initialize_task(program, cell_key))
        .map_err(|e| {
            e.emit_diagnostics();
            e
        })
    }

    /// Run a callback with a new, initialized [`Compiler`].
    fn with_compiler<F, T>(&self, cell_key: &mut QCellOwner, f: F) -> Result<T>
    where
        F: FnOnce(Compiler, &mut QCellOwner) -> Result<T>,
    {
        let object_space = self.object_space.ro(cell_key);
        let compiler = CompilerBuilder::default()
            .config(self.config.clone())
            .simul_efuns(get_simul_efuns(&self.config, object_space))
            .build()?;
        f(compiler, cell_key)
    }

    /// Create a new [`Task`] and initialize it with the given [`Program`].
    fn create_and_initialize_task(
        &mut self,
        program: Program,
        cell_key: &mut QCellOwner,
    ) -> Result<TaskContext> {
        let mut task: Task<MAX_CALL_STACK_SIZE> = Task::new(&self.memory, self.upvalues.clone());

        task.initialize_program(
            program,
            self.config.clone(),
            self.object_space.clone(),
            cell_key,
        )
        .map(|ctx| {
            let process = ctx.process();
            ObjectSpace::insert_process(&self.object_space, process, cell_key);

            ctx
        })
    }
}

impl Mark for Vm {
    #[instrument(skip(self, cell_key))]
    fn mark(&self, marked: &mut BitSet, processed: &mut HashSet<UniqueId>, cell_key: &QCellOwner) -> Result<()> {
        // TODO: mark all tasks
        self.object_space.ro(cell_key).mark(marked, processed, cell_key)
    }
}

impl Sweep for Vm {
    #[instrument(skip(self, cell_key))]
    #[inline]
    fn sweep(&mut self, marked: &BitSet, cell_key: &mut QCellOwner) -> Result<()> {
        self.upvalues.rw(cell_key).keyless_sweep(marked)
    }
}

impl<'a> Keyable<'a> for Vm {
    fn keyable_debug(&self, f: &mut Formatter<'_>, cell_key: &QCellOwner) -> std::fmt::Result {
        write!(
            f,
            "Vm {{ object_space: {:?}, memory {:?}, upvalues: {:?}, config: {:?} }}",
            self.object_space.ro(cell_key).with_key(cell_key),
            self.memory,
            self.upvalues.ro(cell_key),
            self.config
        )
    }

    fn keyable_hash<H: Hasher>(&self, _state: &mut H, _cell_key: &QCellOwner) {
        unimplemented!()
    }

    fn keyable_eq(&self, _other: &Self, _cell_key: &QCellOwner) -> bool {
        unimplemented!()
    }

    fn keyable_partial_cmp(&self, _other: &Self, _cell_key: &QCellOwner) -> Option<Ordering> {
        unimplemented!()
    }
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use super::*;
    use crate::test_support::test_config;

    #[test]
    fn test_gc() {
        let mut cell_key = QCellOwner::new();
        let mut vm = Vm::new(test_config(), &cell_key);
        let storage = indoc! { r#"
            function *storage = ({});

            void store(function f) {
                dump("storing", f);
                storage += ({ f });
            }

            void runem() {
                dump("running", storage);
                foreach (f: storage) {
                    f();
                }
            }
        "# };

        let runner = indoc! { r#"
            void create() {
                int i = -1;

                object storage = clone_object("/storage");

                dump("storage", storage);

                while(++i < 5) {
                    storage->store((:
                        dump("yo", i);
                    :));
                }

                storage->runem();
            }
        "# };

        vm.initialize_string(storage, "storage", &mut cell_key)
            .map_err(|e| {
                e.emit_diagnostics();
                e
            })
            .unwrap();
        vm.initialize_string(runner, "runner", &mut cell_key)
            .map_err(|e| {
                e.emit_diagnostics();
                e
            })
            .unwrap();
    }
}
