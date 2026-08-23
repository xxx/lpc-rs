use std::sync::{
    Arc,
    atomic::{AtomicUsize, Ordering},
};

use arc_swap::ArcSwapAny;
use bit_set::BitSet;
use dashmap::{DashMap, mapref::multiple::RefMulti};
use delegate::delegate;
use lpc_rs_core::lpc_path::LpcPath;
use lpc_rs_errors::Result;
use lpc_rs_utils::config::Config;
use tracing::{debug, trace};

use crate::{
    interpreter::{gc::mark::Mark, process::Process, program::Program, stm::VarId},
    util::process_builder::{compile_process_from_code, compile_process_from_path},
};

/// The initial size (in objects) of the object space
const OBJECT_SPACE_SIZE: usize = 100_000;

/// A wrapper around a [`DashMap`] of [`Process`]es, to hold all of the master
/// and cloned objects. In other words, this is the map that `find_object()`
/// uses.
#[derive(Debug)]
pub struct ObjectSpace {
    /// The actual mapping of "paths" to processes. This is the *committed*
    /// object space: the physical state, read directly by non-transactional
    /// consumers (GC, the committed reader, function-pointer lookup) and kept
    /// in sync with the committer's object cells at commit time.
    processes: DashMap<String, Arc<Process>>,

    /// The stable transactional cell for each object path. Minted once on
    /// first reference to a path and stable for the object's life. This is
    /// what lets the committer's read-write conflict rule apply to the object
    /// space: a `find` of a path reads its cell, so a concurrent create
    /// commits-and-conflicts the finder and makes it re-run.
    cell_ids: DashMap<String, VarId>,

    /// How many clones have been created so far?
    clone_count: AtomicUsize,

    /// The master object.
    master_object: ArcSwapAny<Option<Arc<Process>>>,

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

            /// Get an iterator over the space
            pub fn iter(&self) -> impl Iterator<Item = RefMulti<'_, String, Arc<Process>>>;
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

    /// The [`Config`] this space (and every compile into it) runs under.
    pub fn config(&self) -> &Arc<Config> {
        &self.config
    }

    /// Compile the in-game file at `path` and physically insert it (blind, no
    /// cell, no initialization). Bootstrap only; in-game creation goes through
    /// `insert_process_transactional`.
    pub async fn create_process_from_path(&self, path: &LpcPath) -> Result<Arc<Process>> {
        let process = compile_process_from_path(self, path).await?;
        Self::insert_process_physical(self, process.clone());
        Ok(process)
    }

    /// Compile `code` (masquerading as `filename`) and physically insert it
    /// (blind, no cell, no initialization). Bootstrap only.
    pub async fn create_process_from_code<P, S>(&self, filename: P, code: S) -> Result<Arc<Process>>
    where
        P: Into<LpcPath> + Send + Sync,
        S: AsRef<str> + Send + Sync,
    {
        let process = compile_process_from_code(self, filename, code).await?;
        Self::insert_process_physical(self, process.clone());
        Ok(process)
    }

    /// Get a reference to the master object.
    pub fn master_object(&self) -> Option<Arc<Process>> {
        self.master_object.load_full()
    }

    /// The cell a *read* of the object at `key` consults, or `None` if the
    /// key has no committed/in-flight cell yet. Deliberately does not mint:
    /// a failed lookup must not claim a `VarId` that the object's create would
    /// then mint as a second, distinct cell (one object, one cell, for life).
    pub(crate) fn get_cell_id(&self, key: &str) -> Option<VarId> {
        self.cell_ids.get(key).map(|cell| *cell)
    }

    /// The stable transactional cell for the object at `key`, minted on first
    /// sight and reused for the object's life. Called by the *writing*
    /// (transactional) insert path; a read must use `get_cell_id` instead so
    /// a failed lookup does not mint a cell the create would then re-mint.
    pub(crate) fn cell_id(&self, key: &str) -> VarId {
        *self
            .cell_ids
            .entry(key.to_owned())
            .or_insert_with(VarId::new)
    }

    /// Every cell id minted for this space, for the quiescent world sweep's
    /// root set. Stale ids (destructed objects) read back as absent in the
    /// world, so they add no edges.
    pub(crate) fn all_cell_ids(&self) -> Vec<VarId> {
        self.cell_ids.iter().map(|cell| *cell).collect()
    }

    /// Every global-slot and structural cell of every live object, for the
    /// world sweep's root set. Complete where [`all_cell_ids`](Self::all_cell_ids)
    /// is not: bootstrap objects live only in the physical map and have no
    /// committed `Process` cell, so their globals would otherwise be wrongly
    /// reclaimed. Destructed objects are safe to skip: not in the physical map.
    pub(crate) fn all_live_object_slots(&self) -> Vec<VarId> {
        self.processes
            .iter()
            .flat_map(|process| process.global_cell_ids())
            .collect()
    }

    /// The one key under which an object lives in the committed world: the
    /// in-game filename, `.c` stripped, clone-id suffix, leading `/` forced.
    /// The same normalization [`process_key`](Self::process_key) applies to a
    /// process, so a cell, the physical map, and a deferred insert/remove all
    /// agree on identity.
    pub(crate) fn path_key(&self, path: &str) -> String {
        let mut key = Self::with_leading_slash(path);
        key = key
            .strip_suffix(".c")
            .map(ToString::to_string)
            .unwrap_or(key);
        key
    }

    /// The physical-map key for a process: its in-game filename (`.c`
    /// stripped, clone-id suffix, leading `/` forced). This is the one key
    /// used by the committed `processes` map, the `cell_ids` registry, and
    /// the deferred insert/remove, so all three agree on identity.
    pub fn process_key(&self, process: &Process) -> String {
        Self::with_leading_slash(process.filename().as_ref())
    }

    /// Whether `key` is the master object's path. `key` is a [`process_key`]
    /// (raw in-game filename, leading `/` forced), so the config's master path
    /// is normalized the same way before comparing (with or without the `.c`
    /// extension, to mirror the old insert-time check).
    fn is_master_key(&self, key: &str) -> bool {
        let master = Self::with_leading_slash(self.config.master_object.as_str());
        let stripped = master.strip_suffix(".c").unwrap_or(&*master);
        key == master.as_str() || key == stripped
    }

    pub fn with_leading_slash(s: &str) -> String {
        if s.starts_with('/') {
            s.to_owned()
        } else {
            format!("/{s}")
        }
    }

    /// Apply a committed deferred insert: place the process in the physical
    /// map under its key (updating the master pointer if relevant). Called
    /// by the retry loop when flushing a committed `InsertObject` effect.
    pub(crate) fn apply_insert(&self, key: &str, process: Arc<Process>) {
        if self.is_master_key(key) {
            debug!("Setting new master object: {}", key);
            self.master_object.swap(Some(process.clone()));
        }
        self.processes.insert(key.to_string(), process);
    }

    /// Apply a committed deferred removal: delete `process` from the physical
    /// map, leaving a newer object under `key` untouched. A removed master
    /// clears the master pointer.
    pub(crate) fn apply_remove(&self, key: &str, process: &Arc<Process>) {
        let removed = self
            .processes
            .remove_if(key, |_, current| Arc::ptr_eq(current, process))
            .is_some();
        if removed && self.is_master_key(key) {
            self.master_object.store(None);
        }
    }

    /// Create a fresh clone process (assigning the next clone id) without
    /// inserting it into the physical map. The caller performs the (deferred)
    /// insert, so the clone isn't physically visible until its transaction
    /// commits.
    pub(crate) fn create_clone_process(&self, program: Arc<Program>) -> Arc<Process> {
        let count = self.clone_count.fetch_add(1, Ordering::Relaxed);

        let clone = Process::new_clone(program, count);
        let process: Arc<Process> = clone.into();

        trace!("Creating clone: {}", process.filename());

        process
    }

    /// Blindly insert the passed [`Process`] into the physical process map,
    /// with in-game local filename. No cell is written and no transaction is
    /// involved: this is the bootstrap/fixture placement. In-game creation
    /// must use the transactional insert instead.
    pub fn insert_process_physical<P>(object_space: &Self, process: P)
    where
        P: Into<Arc<Process>>,
    {
        let process = process.into();
        object_space.apply_insert(&object_space.process_key(&process), process);
    }

    /// Lookup a process from its path.
    /// The path should be absolute, in-game path, without the `.c` extension.
    pub fn lookup<T>(&self, path: T) -> Option<Arc<Process>>
    where
        T: AsRef<str>,
    {
        self.processes.get(path.as_ref()).map(|s| s.clone())
    }
}

impl Default for ObjectSpace {
    fn default() -> Self {
        let processes = DashMap::with_capacity(OBJECT_SPACE_SIZE);

        Self {
            processes,
            cell_ids: DashMap::new(),
            clone_count: AtomicUsize::new(0),
            config: Config::default().into(),
            master_object: ArcSwapAny::from(None),
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

    use lpc_rs_core::lpc_path::LpcPath;
    use lpc_rs_utils::config::ConfigBuilder;
    use ustr::ustr;

    use super::*;
    use crate::interpreter::program::ProgramBuilder;

    // #[test]
    // fn test_insert_prototype() {
    //     let mut space = ObjectSpace::default();
    //     let prog = Program::default();
    //     space.insert_prototype(prog.clone());
    //     let filename = prog.filename.to_str().unwrap();
    //
    //     assert_eq!(space.len(), 1);
    //     assert!(space.processes.contains_key(filename));
    // }

    /// A destruct flushed after a newer create of the same key must not
    /// evict the newer object.
    #[test]
    fn apply_remove_leaves_a_newer_object_in_place() {
        let config = ConfigBuilder::default()
            .lib_dir("./tests/fixtures/code/")
            .build()
            .unwrap();
        let space = ObjectSpace::new(config);
        let program = ProgramBuilder::default()
            .filename(LpcPath::new_in_game(
                "/foo/bar.c",
                "/",
                "./tests/fixtures/code/",
            ))
            .build()
            .unwrap();
        let old = Arc::new(Process::new(program.clone()));
        let new = Arc::new(Process::new(program));
        let key = space.process_key(&old);

        space.apply_insert(&key, old.clone());
        space.apply_insert(&key, new.clone());
        space.apply_remove(&key, &old);
        assert!(Arc::ptr_eq(&space.lookup(&key).unwrap(), &new));

        space.apply_remove(&key, &new);
        assert!(space.lookup(&key).is_none());
    }

    #[test]
    fn test_insert_process() {
        let config = ConfigBuilder::default()
            .lib_dir("./tests/fixtures/code/")
            .build()
            .unwrap();
        let space = ObjectSpace::new(config);

        let mut prog: Program = Program::default();
        let filename: Arc<LpcPath> = Arc::new("/foo/bar/baz.c".into());
        prog.filename = filename;

        let process = Process::new(prog);
        let space_cell = space;
        ObjectSpace::insert_process_physical(&space_cell, process);

        assert_eq!(space_cell.len(), 1);
        assert!(space_cell.processes.contains_key("/foo/bar/baz"));
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

        let prog = ProgramBuilder::default()
            .filename(Arc::new(LpcPath::InGame("/master.c".into())))
            .build()
            .unwrap();
        let proc = Arc::new(Process::new(prog));
        ObjectSpace::insert_process_physical(&space, proc.clone());

        let master = space.master_object();
        assert_eq!(master.unwrap(), proc);
    }
}
