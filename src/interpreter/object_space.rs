use std::sync::{
    atomic::{AtomicUsize, Ordering},
    Arc,
};

use bit_set::BitSet;
use dashmap::{mapref::one::Ref, DashMap};
use delegate::delegate;
use lpc_rs_core::lpc_path::LpcPath;
use lpc_rs_utils::config::Config;

use crate::interpreter::{gc::mark::Mark, process::Process, program::Program};

/// A wrapper around a [`HashMap`] of [`Process`]es, to hold all of the master
/// and cloned objects. In other words, this is the map that `find_object()`
/// uses.

/// The initial size (in objects) of the object space
const OBJECT_SPACE_SIZE: usize = 100_000;

#[derive(Debug)]
pub struct ObjectSpace {
    /// The actual mapping of "paths" to processes
    processes: DashMap<String, Arc<Process>>,

    /// How many clones have been created so far?
    clone_count: AtomicUsize,

    /// Our configuration
    config: Arc<Config>,
}

impl ObjectSpace {
    delegate! {
        to self.processes {
            /// Get the number of objects in the space
            pub fn len(&self) -> usize;

            /// Get whether or not the space is empty
            pub fn is_empty(&self) -> bool;

            /// Clear the entire space
            pub fn clear(&self);
        }
    }

    /// Create a new [`ObjectSpace`] with the passed [`Config`]
    pub fn new<T>(config: T) -> Self
    where
        T: Into<Arc<Config>>,
    {
        Self {
            config: config.into(),
            ..Default::default()
        }
    }

    /// Get a reference to the master object.
    pub fn master_object(&self) -> Option<Ref<'_, String, Arc<Process>>> {
        // TODO: this should not need to allocate the filename every time.
        let filename = LpcPath::new_in_game(self.config.master_object.as_str(), "/", self.config.lib_dir.as_str());
        self.processes.get(&Self::prepare_filename(filename.as_ref()))
    }

    // /// Create a [`Process`] from a [`Program`], and add add it to the process
    // /// table. If a new program with the same filename as an existing one is
    // /// added, the new will overwrite the old in the table.
    // /// Storage keys are the in-game filename
    // pub fn insert_master(&mut self, program: Program) ->
    // Arc<Process> {     let new = Process::new(program);
    //     let process: Arc<Process> = RwLock::new(new).into();
    //     let name = self.prepare_filename(&process);
    //     self.insert_process_directly(name, process.clone());
    //     process
    // }

    /// Insert a clone of the passed [`Program`] into the space.
    pub fn insert_clone(space_cell: &Arc<Self>, program: Arc<Program>) -> Arc<Process> {
        let count = space_cell.clone_count.fetch_add(1, Ordering::Relaxed);

        let clone = Process::new_clone(program, count);

        let name = space_cell.prepare_process_filename(&clone);

        let process: Arc<Process> = clone.into();

        space_cell.insert_process_directly(name, process.clone());
        process
    }

    /// Directly insert the passed [`Process`] into the space, with in-game
    /// local filename.
    pub fn insert_process<P>(object_space: &Arc<Self>, process: P)
    where
        P: Into<Arc<Process>>,
    {
        let process = process.into();
        let name = { object_space.prepare_process_filename(&process) };

        object_space.insert_process_directly(name, process);
    }

    /// Remove the passed [`Process`] from the space.
    pub fn remove_process<P>(object_space: &Arc<Self>, process: P)
    where
        P: Into<Arc<Process>>,
    {
        let process = process.into();
        let name = { object_space.prepare_process_filename(&process) };

        object_space.processes.remove(&name);
    }

    fn prepare_process_filename(&self, process: &Process) -> String {
        let name = process.localized_filename(&self.config.lib_dir);
        Self::prepare_filename(&name)
    }

    fn prepare_filename(filename: &str) -> String {
        filename
            .strip_suffix(".c")
            .map(ToString::to_string)
            .unwrap_or(filename.to_string())
    }

    #[inline]
    fn insert_process_directly<P, S>(&self, name: S, process: P)
    where
        P: Into<Arc<Process>>,
        S: Into<String>,
    {
        self.processes.insert(name.into(), process.into());
    }

    /// Lookup a process from its path.
    pub fn lookup<T>(&self, path: T) -> Option<Ref<String, Arc<Process>>>
    where
        T: AsRef<str>,
    {
        self.processes.get(path.as_ref())
    }
}

impl Clone for ObjectSpace {
    fn clone(&self) -> Self {
        Self {
            processes: self.processes.clone(),
            clone_count: AtomicUsize::new(self.clone_count.load(Ordering::Relaxed)),
            config: self.config.clone(),
        }
    }
}

impl Default for ObjectSpace {
    fn default() -> Self {
        let processes = DashMap::with_capacity(OBJECT_SPACE_SIZE);

        Self {
            processes,
            clone_count: AtomicUsize::new(0),
            config: Config::default().into(),
        }
    }
}

impl Mark for ObjectSpace {
    #[inline]
    fn mark(&self, marked: &mut BitSet, processed: &mut BitSet) -> lpc_rs_errors::Result<()> {
        for process in self.processes.iter() {
            process.mark(marked, processed)?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;
    use ustr::ustr;

    use lpc_rs_core::lpc_path::LpcPath;
    use lpc_rs_utils::config::ConfigBuilder;

    use super::*;
    use crate::interpreter::{heap::Heap, into_lpc_ref::IntoLpcRef, lpc_array::LpcArray};

    // #[test]
    // fn test_insert_master() {
    //     let mut space = ObjectSpace::default();
    //     let prog = Program::default();
    //     space.insert_master(prog.clone());
    //     let filename = prog.filename.to_str().unwrap();
    //
    //     assert_eq!(space.len(), 1);
    //     assert!(space.processes.contains_key(filename));
    // }

    #[test]
    fn test_insert_clone() {
        let space = ObjectSpace::default();
        let prog: Arc<Program> = Program::default().into();
        let filename = prog.filename.to_str().unwrap();

        let mut prog2: Program = Program::default();
        let filename2: Arc<LpcPath> = Arc::new("/foo/bar/baz".into());
        prog2.filename = filename2.clone();

        let object_space = space.into();

        ObjectSpace::insert_clone(&object_space, prog.clone());
        ObjectSpace::insert_clone(&object_space, prog.clone());
        ObjectSpace::insert_clone(&object_space, prog2.into());
        ObjectSpace::insert_clone(&object_space, prog.clone());

        assert_eq!(object_space.len(), 4);
        assert!(object_space
            .processes
            .contains_key(&format!("{}#{}", filename, 0)));
        assert!(object_space
            .processes
            .contains_key(&format!("{}#{}", filename, 1)));
        assert!(object_space
            .processes
            .contains_key(&format!("{}#{}", filename2, 2)));
        assert!(object_space
            .processes
            .contains_key(&format!("{}#{}", filename, 3)));
    }

    #[test]
    fn test_insert_process() {
        let config = ConfigBuilder::default()
            .lib_dir("./tests/fixtures/code/")
            .build()
            .unwrap();
        let space = ObjectSpace::new(config);

        let mut prog: Program = Program::default();
        let filename: Arc<LpcPath> = Arc::new("./tests/fixtures/code/foo/bar/baz.c".into());
        prog.filename = filename;

        let process = Process::new(prog);
        let space_cell = space.into();
        ObjectSpace::insert_process(&space_cell, process);

        assert_eq!(space_cell.len(), 1);
        assert!(space_cell.processes.contains_key("/foo/bar/baz"));
    }

    #[test]
    fn test_mark() {
        let config = Config::default();
        let space = ObjectSpace::new(config);

        let memory = Heap::new(5);
        let array = LpcArray::new(vec![]);
        let array_id = array.unique_id;
        let lpc_ref = array.into_lpc_ref(&memory);

        let process = Process::default();
        process.globals.write().push(lpc_ref);

        space.insert_process_directly("process", process);

        let mut marked = BitSet::new();
        let mut processed = BitSet::new();
        space.mark(&mut marked, &mut processed).unwrap();

        assert!(processed.contains(*array_id.as_ref()));
    }

    #[test]
    fn test_master_object() {
        let config = ConfigBuilder::default()
            .master_object(ustr("/master.c"))
            .lib_dir("/foo/bar")
            .build()
            .unwrap();
        let space = ObjectSpace::new(config);

        assert!(space.master_object().is_none());

        let proc = Arc::new(Process::default());
        space.insert_process_directly("/master", proc.clone());

        let master = space.master_object();
        assert_eq!(*master.unwrap(), proc);
    }
}
