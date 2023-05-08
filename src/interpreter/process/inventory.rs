use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use std::sync::{Arc, Weak};
use dashmap::{DashSet};
use crate::interpreter::process::Process;

#[derive(Debug, Clone)]
pub struct HashableProcess {
    pub(crate) process: Weak<Process>,
    pub(crate) hash: u64,
}

impl Hash for HashableProcess {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.hash.hash(state);
    }
}

impl PartialEq for HashableProcess {
    fn eq(&self, other: &Self) -> bool {
        self.hash == other.hash
    }
}

impl Eq for HashableProcess {}

impl From<Weak<Process>> for HashableProcess {
    fn from(process: Weak<Process>) -> Self {
        let mut hasher = DefaultHasher::new();

        let hash = if let Some(upgrade) = process.upgrade() {
            upgrade.hash(&mut hasher);
            hasher.finish()
        } else {
            0
        };

        Self { process: process.into(), hash }
    }
}

impl From<&Weak<Process>> for HashableProcess {
    fn from(value: &Weak<Process>) -> Self {
        Self::from(value.clone())
    }
}

impl From<&Arc<Process>> for HashableProcess {
    fn from(value: &Arc<Process>) -> Self {
        let mut hasher = DefaultHasher::new();
        value.hash(&mut hasher);
        let hash = hasher.finish();

        Self {
            process: Arc::downgrade(value),
            hash,
        }
    }
}

#[derive(Debug, Default)]
pub struct Inventory {
    objects: DashSet<HashableProcess>,
}

impl Inventory {
    pub fn new() -> Self {
        Self {
            objects: DashSet::new(),
        }
    }

    pub fn contains<T>(&self, process: T) -> bool
    where
        T: Into<HashableProcess>
    {
        self.objects.contains(&process.into())
    }

    pub fn insert(&self, process: Weak<Process>) {
        let hashable = HashableProcess::from(process);
        if hashable.hash != 0 {
            self.objects.insert(hashable);
        }
    }

    pub fn remove(&self, process: &Weak<Process>) -> Option<Weak<Process>> {
        self.objects.remove(&HashableProcess::from(process)).map(|hp| hp.process)
    }

    pub fn iter(&self) -> impl Iterator<Item = Weak<Process>> + '_ {
        self.objects.iter().map(|hp| hp.process.clone())
    }
}