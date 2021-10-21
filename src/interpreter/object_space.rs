use std::collections::HashMap;
use crate::interpreter::process::Process;
use std::rc::Rc;
use crate::interpreter::program::Program;
use delegate::delegate;
use crate::util::config::Config;
use std::cell::RefCell;

/// A wrapper around a [`HashMap`] of [`Process`]es, to encapsulate
/// the cloning behavior, etc.

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
        }
    }

    /// Create a new [`ObjectSpace`] with the passed [`Config`]
    pub fn new<T>(config: T) -> Self
    where
        T: Into<Rc<Config>>
    {
        Self {
            config: config.into(),
            ..Default::default()
        }
    }

    /// Create a [`Process`] from a [`Program`], and add add it to the process table.
    /// If a new program with the same filename as an existing one is added,
    /// the new will overwrite the old in the table.
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

    /// Directly insert the passed [`Process`] into the space, with in-game local filename.
    pub fn insert_process<P>(&mut self, process: P)
    where
        P: Into<Rc<RefCell<Process>>>
    {
        let process = process.into();
        let name = process.borrow().localized_filename(self.config.lib_dir());

        self.processes.insert(name, process);
        println!("inserted process? {:?}", self.processes);
    }

    /// Lookup a process from its path. `.c` will be appended and checked,
    /// if the passed name doesn't have an exact match.
    pub fn lookup<T>(&self, path: T) -> Option<&Rc<RefCell<Process>>>
    where
        T: AsRef<str>,
    {
        let s = path.as_ref();

        match self.processes.get(s) {
            Some(proc) => Some(proc),
            None => {
                if !s.ends_with(".c") {
                    let mut owned = s.to_string();
                    owned.push_str(".c");
                    return self.lookup(owned);
                }

                None
            }
        }
    }
}

impl Default for ObjectSpace {
    fn default() -> Self {
        let processes = HashMap::with_capacity(OBJECT_SPACE_SIZE);

        Self {
            processes,
            clone_count: 0,
            config: Config::default().into()
        }
    }
}

impl From<ObjectSpace> for Rc<RefCell<ObjectSpace>> {
    fn from(object_space: ObjectSpace) -> Self {
        Rc::new(RefCell::new(object_space))
    }
}

