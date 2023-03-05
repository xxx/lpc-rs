use std::{collections::HashMap, rc::Rc};

use delegate::delegate;
use educe::Educe;
use lpc_rs_utils::config::Config;
use qcell::{QCell, QCellOwner};

use crate::{
    interpreter::{process::Process, program::Program},
    util::qcell_debug,
};

/// A wrapper around a [`HashMap`] of [`Process`]es, to hold all of the master
/// and cloned objects. In other words, this is the map that `find_object()`
/// uses.

/// The initial size (in objects) of the object space
const OBJECT_SPACE_SIZE: usize = 100_000;

// fn qcell_debug_hashmap(map: &HashMap<String, Rc<QCell<Process>>>, f: &mut
// Formatter) -> std::fmt::Result {     let s = map.iter()
//         .map(|(k, v)| {
//             format!("{}: {:?}", k, v.clone().get_mut())
//         })
//         .collect::<Vec<String>>()
//         .join(", ");
//
//     f.write_str(&s)
// }

#[derive(Educe, Clone)]
#[educe(Debug)]
pub struct ObjectSpace {
    /// The actual mapping of "paths" to processes
    #[educe(Debug(method = "qcell_debug"))]
    processes: HashMap<String, Rc<QCell<Process>>>,

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

    // /// Create a [`Process`] from a [`Program`], and add add it to the process
    // /// table. If a new program with the same filename as an existing one is
    // /// added, the new will overwrite the old in the table.
    // /// Storage keys are the in-game filename
    // pub fn insert_master(&mut self, program: Program, cell_key: &QCellOwner) ->
    // Rc<QCell<Process>> {     let new = Process::new(program);
    //     let process: Rc<QCell<Process>> = cell_key.cell(new).into();
    //     let name = self.prepare_filename(&process, &cell_key);
    //     self.insert_process_directly(name, process.clone());
    //     process
    // }

    /// Insert a clone of the passed [`Program`] into the space.
    pub fn insert_clone(
        space_cell: &Rc<QCell<Self>>,
        program: Rc<Program>,
        cell_key: &mut QCellOwner,
    ) -> Rc<QCell<Process>> {
        let clone = {
            let space = space_cell.rw(cell_key);
            Process::new_clone(program, space.clone_count)
        };

        let name = { space_cell.ro(cell_key).prepare_filename(&clone) };

        let process: Rc<QCell<Process>> = cell_key.cell(clone).into();

        let space = space_cell.rw(cell_key);
        space.clone_count += 1;
        space.insert_process_directly(name, process.clone());
        process
    }

    /// Directly insert the passed [`Process`] into the space, with in-game
    /// local filename.
    pub fn insert_process<P>(space_cell: &Rc<QCell<Self>>, process: P, cell_key: &mut QCellOwner)
    where
        P: Into<Rc<QCell<Process>>>,
    {
        let process = process.into();
        let space = space_cell.ro(cell_key);
        let name = space.prepare_filename(process.ro(cell_key));

        let space = space_cell.rw(cell_key);
        space.insert_process_directly(name, process);
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
    pub fn insert_process_directly<P, S>(&mut self, name: S, process: P)
    where
        P: Into<Rc<QCell<Process>>>,
        S: Into<String>,
    {
        self.processes.insert(name.into(), process.into());
    }

    /// Lookup a process from its path.
    pub fn lookup<T>(&self, path: T) -> Option<&Rc<QCell<Process>>>
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

#[cfg(test)]
mod tests {
    use lpc_rs_core::lpc_path::LpcPath;
    use lpc_rs_utils::config::ConfigBuilder;
    use qcell::QCellOwner;

    use super::*;

    // #[test]
    // fn test_insert_master() {
    //     let cell_key = QCellOwner::new();
    //     let mut space = ObjectSpace::default();
    //     let prog = Program::default();
    //     space.insert_master(prog.clone(), &cell_key);
    //     let filename = prog.filename.to_str().unwrap();
    //
    //     assert_eq!(space.len(), 1);
    //     assert!(space.processes.contains_key(filename));
    // }

    #[test]
    fn test_insert_clone() {
        let mut cell_key = QCellOwner::new();
        let space = ObjectSpace::default();
        let prog: Rc<Program> = Program::default().into();
        let filename = prog.filename.to_str().unwrap();

        let mut prog2: Program = Program::default();
        let filename2: LpcPath = "/foo/bar/baz".into();
        prog2.filename = filename2.clone();

        let object_space = cell_key.cell(space).into();

        ObjectSpace::insert_clone(&object_space, prog.clone(), &mut cell_key);
        ObjectSpace::insert_clone(&object_space, prog.clone(), &mut cell_key);
        ObjectSpace::insert_clone(&object_space, prog2.into(), &mut cell_key);
        ObjectSpace::insert_clone(&object_space, prog.clone(), &mut cell_key);

        let space = object_space.ro(&cell_key);
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
        let mut cell_key = QCellOwner::new();
        let config = ConfigBuilder::default()
            .lib_dir("./tests/fixtures/code/")
            .build()
            .unwrap();
        let space = ObjectSpace::new(config);

        let mut prog: Program = Program::default();
        let filename: LpcPath = "./tests/fixtures/code/foo/bar/baz.c".into();
        prog.filename = filename;

        let process = Process::new(prog);
        let space_cell = cell_key.cell(space).into();
        ObjectSpace::insert_process(&space_cell, cell_key.cell(process), &mut cell_key);

        let space = space_cell.ro(&cell_key);
        assert_eq!(space.len(), 1);
        println!("{:?}", space.processes.keys());
        assert!(space.processes.contains_key("/foo/bar/baz"));
    }
}
