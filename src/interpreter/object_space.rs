use std::{cell::RefCell, collections::HashMap, rc::Rc};

use delegate::delegate;
use lpc_rs_utils::config::{Config, ConfigBuilder};

use crate::interpreter::{process::Process, program::Program};

/// A wrapper around a [`HashMap`] of [`Process`]es, to hold all of the master
/// and cloned objects. In other words, this is the map that `find_object()`
/// uses.

/// The initial size (in objects) of the object space
const OBJECT_SPACE_SIZE: usize = 100_000;

#[derive(Debug, Clone)]
pub struct ObjectSpace {
    /// The actual mapping of "paths" to processes
    processes: HashMap<String, Rc<RefCell<Process>>>,

    /// How many clones have been created so far?
    clone_count: usize,

    /// Our configuration
    config: Rc<Config>,
}

impl ObjectSpace {
    delegate! {
        to self.processes {
            /// Get the number of objects in the space
            pub fn len(&self) -> usize;

            /// Get whether or not the space is empty
            pub fn is_empty(&self) -> bool;
        }
    }

    /// Create a new [`ObjectSpace`] with the passed [`Config`]
    pub fn new<T>(config: T) -> Self
    where
        T: Into<Rc<Config>>,
    {
        Self {
            config: config.into(),
            ..Default::default()
        }
    }

    /// Create a [`Process`] from a [`Program`], and add add it to the process
    /// table. If a new program with the same filename as an existing one is
    /// added, the new will overwrite the old in the table.
    /// Storage keys are the in-game filename
    pub fn insert_master(&mut self, program: Program) -> Rc<RefCell<Process>> {
        let process: Rc<RefCell<Process>> = Process::new(program).into();
        self.insert_process(process.clone());
        process
    }

    /// Insert a clone of the passed [`Program`] into the space.
    pub fn insert_clone(&mut self, program: Rc<Program>) -> Rc<RefCell<Process>> {
        let process: Rc<RefCell<Process>> = Process::new_clone(program, self.clone_count).into();
        self.clone_count += 1;
        self.insert_process(process.clone());
        process
    }

    /// Directly insert the passed [`Process`] into the space, with in-game
    /// local filename.
    pub fn insert_process<P>(&mut self, process: P)
    where
        P: Into<Rc<RefCell<Process>>>,
    {
        let process = process.into();
        let name = process.borrow().localized_filename(self.config.lib_dir());
        let name = match name.strip_suffix(".c") {
            Some(x) => x.to_string(),
            None => name,
        };

        self.processes.insert(name, process);
    }

    /// Lookup a process from its path.
    pub fn lookup<T>(&self, path: T) -> Option<&Rc<RefCell<Process>>>
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

impl From<ObjectSpace> for Rc<RefCell<ObjectSpace>> {
    fn from(object_space: ObjectSpace) -> Self {
        Rc::new(RefCell::new(object_space))
    }
}

#[cfg(test)]
mod tests {
    use lpc_rs_core::lpc_path::LpcPath;

    use super::*;

    #[test]
    fn test_insert_master() {
        let mut space = ObjectSpace::default();
        let prog = Program::default();
        space.insert_master(prog.clone());
        let filename = prog.filename.to_str().unwrap();

        assert_eq!(space.len(), 1);
        assert!(space.processes.contains_key(filename));
    }

    #[test]
    fn test_insert_clone() {
        let mut space = ObjectSpace::default();
        let prog: Rc<Program> = Program::default().into();
        let filename = prog.filename.to_str().unwrap();

        let mut prog2: Program = Program::default();
        let filename2: LpcPath = "/foo/bar/baz".into();
        prog2.filename = filename2.clone();

        space.insert_clone(prog.clone());
        space.insert_clone(prog.clone());
        space.insert_clone(prog2.into());
        space.insert_clone(prog.clone());

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
        let mut space = ObjectSpace::new(config);

        let mut prog: Program = Program::default();
        let filename: LpcPath = "./tests/fixtures/code/foo/bar/baz.c".into();
        prog.filename = filename;

        let process = Process::new(prog);
        space.insert_process(Rc::new(RefCell::new(process)));

        assert_eq!(space.len(), 1);
        assert!(space.processes.contains_key("/foo/bar/baz"));
    }
}
