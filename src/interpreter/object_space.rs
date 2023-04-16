use std::{collections::HashMap, fmt::Formatter, sync::Arc};

use bit_set::BitSet;
use delegate::delegate;
use educe::Educe;
use itertools::Itertools;
use lpc_rs_utils::config::Config;
use parking_lot::RwLock;

use crate::interpreter::{gc::mark::Mark, process::Process, program::Program};

/// A wrapper around a [`HashMap`] of [`Process`]es, to hold all of the master
/// and cloned objects. In other words, this is the map that `find_object()`
/// uses.

/// The initial size (in objects) of the object space
const OBJECT_SPACE_SIZE: usize = 100_000;

#[derive(Educe, Clone)]
#[educe(Debug)]
pub struct ObjectSpace {
    /// The actual mapping of "paths" to processes
    #[educe(Debug(method = "processes_debug"))]
    processes: HashMap<String, Arc<RwLock<Process>>>,

    /// How many clones have been created so far?
    clone_count: usize,

    /// Our configuration
    config: Arc<Config>,
}

fn processes_debug(
    processes: &HashMap<String, Arc<RwLock<Process>>>,
    f: &mut Formatter<'_>,
) -> std::fmt::Result {
    write!(f, "{}", processes.keys().join(", "))
}

impl ObjectSpace {
    delegate! {
        to self.processes {
            /// Get the number of objects in the space
            pub fn len(&self) -> usize;

            /// Get whether or not the space is empty
            pub fn is_empty(&self) -> bool;

            /// Clear the entire space
            pub fn clear(&mut self);
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

    // /// Create a [`Process`] from a [`Program`], and add add it to the process
    // /// table. If a new program with the same filename as an existing one is
    // /// added, the new will overwrite the old in the table.
    // /// Storage keys are the in-game filename
    // pub fn insert_master(&mut self, program: Program) ->
    // Arc<RwLock<Process>> {     let new = Process::new(program);
    //     let process: Arc<RwLock<Process>> = RwLock::new(new).into();
    //     let name = self.prepare_filename(&process);
    //     self.insert_process_directly(name, process.clone());
    //     process
    // }

    /// Insert a clone of the passed [`Program`] into the space.
    pub fn insert_clone(
        space_cell: &Arc<RwLock<Self>>,
        program: Arc<Program>,
    ) -> Arc<RwLock<Process>> {
        let clone = {
            let object_space = space_cell.read();
            Process::new_clone(program, object_space.clone_count)
        };

        let name = space_cell.read().prepare_filename(&clone);

        let process: Arc<RwLock<Process>> = RwLock::new(clone).into();

        let mut space = space_cell.write();
        space.clone_count += 1;
        space.insert_process_directly(name, process.clone());
        process
    }

    /// Directly insert the passed [`Process`] into the space, with in-game
    /// local filename.
    pub fn insert_process<P>(space_cell: &Arc<RwLock<Self>>, process: P)
    where
        P: Into<Arc<RwLock<Process>>>,
    {
        let process = process.into();
        let name = {
            let space = space_cell.read();
            space.prepare_filename(&process.read())
        };

        let mut space = space_cell.write();
        space.insert_process_directly(name, process);
    }

    pub fn remove_process<P>(space_cell: &Arc<RwLock<Self>>, process: P)
    where
        P: Into<Arc<RwLock<Process>>>,
    {
        let process = process.into();
        let name = {
            let space = space_cell.read();
            space.prepare_filename(&process.read())
        };

        let mut space = space_cell.write();
        space.processes.remove(&name);
    }

    fn prepare_filename(&self, process: &Process) -> String {
        let name = process.localized_filename(&self.config.lib_dir);
        let name = name
            .strip_suffix(".c")
            .map(ToString::to_string)
            .unwrap_or(name);

        name
    }

    #[inline]
    fn insert_process_directly<P, S>(&mut self, name: S, process: P)
    where
        P: Into<Arc<RwLock<Process>>>,
        S: Into<String>,
    {
        self.processes.insert(name.into(), process.into());
    }

    /// Lookup a process from its path.
    pub fn lookup<T>(&self, path: T) -> Option<&Arc<RwLock<Process>>>
    where
        T: AsRef<str>,
    {
        self.processes.get(path.as_ref())
    }
}

impl Default for ObjectSpace {
    fn default() -> Self {
        let processes = HashMap::with_capacity(OBJECT_SPACE_SIZE);

        Self {
            processes,
            clone_count: 0,
            config: Config::default().into(),
        }
    }
}

impl Mark for ObjectSpace {
    #[inline]
    fn mark(&self, marked: &mut BitSet, processed: &mut BitSet) -> lpc_rs_errors::Result<()> {
        for process in self.processes.values() {
            process.read().mark(marked, processed)?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use lpc_rs_core::lpc_path::LpcPath;
    use lpc_rs_utils::config::ConfigBuilder;

    use super::*;
    use crate::interpreter::{into_lpc_ref::IntoLpcRef, lpc_array::LpcArray, memory::Memory};

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

        let object_space = RwLock::new(space).into();

        ObjectSpace::insert_clone(&object_space, prog.clone());
        ObjectSpace::insert_clone(&object_space, prog.clone());
        ObjectSpace::insert_clone(&object_space, prog2.into());
        ObjectSpace::insert_clone(&object_space, prog.clone());

        let space = object_space.read();
        assert_eq!(space.len(), 4);
        assert!(space.processes.contains_key(&format!("{}#{}", filename, 0)));
        assert!(space.processes.contains_key(&format!("{}#{}", filename, 1)));
        assert!(space
            .processes
            .contains_key(&format!("{}#{}", filename2, 2)));
        assert!(space.processes.contains_key(&format!("{}#{}", filename, 3)));
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
        let space_cell = RwLock::new(space).into();
        ObjectSpace::insert_process(&space_cell, RwLock::new(process));

        let space = space_cell.read();
        assert_eq!(space.len(), 1);
        assert!(space.processes.contains_key("/foo/bar/baz"));
    }

    #[test]
    fn test_mark() {
        let config = Config::default();
        let mut space = ObjectSpace::new(config);

        let memory = Memory::new(5);
        let array = LpcArray::new(vec![]);
        let array_id = array.unique_id;
        let lpc_ref = array.into_lpc_ref(&memory);

        let mut process = Process::default();
        process.globals.push(lpc_ref);

        space.insert_process_directly("process", RwLock::new(process));

        let mut marked = BitSet::new();
        let mut processed = BitSet::new();
        space.mark(&mut marked, &mut processed).unwrap();

        assert!(processed.contains(*array_id.as_ref()));
    }
}
