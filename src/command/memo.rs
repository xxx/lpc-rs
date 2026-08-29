//! Bounded memos of compiled forms, keyed by source text: pure caches outside
//! STM — an eviction costs one recompile.

use std::{borrow::Borrow, convert::Infallible, hash::Hash, num::NonZeroUsize};

use lru::LruCache;
use parking_lot::Mutex;

/// An exact LRU of built values; a hit is a clone.
pub(crate) struct Memo<K, V> {
    entries: Mutex<LruCache<K, V>>,
}

impl<K: Hash + Eq, V: Clone> Memo<K, V> {
    /// Holding up to `capacity` entries, allocated as they come; a zero
    /// capacity holds one.
    pub(crate) fn new(capacity: usize) -> Self {
        let capacity = NonZeroUsize::new(capacity).unwrap_or(NonZeroUsize::MIN);
        Memo {
            entries: Mutex::new(LruCache::sparse(capacity)),
        }
    }

    /// The value for `key`, built by `build` on a miss — outside the lock,
    /// and remembered only on success; builders racing on one key both get
    /// the value that landed first.
    pub(crate) fn get_or_try_build<Q, E>(
        &self,
        key: &Q,
        build: impl FnOnce() -> Result<V, E>,
    ) -> Result<V, E>
    where
        K: Borrow<Q>,
        Q: Hash + Eq + ToOwned<Owned = K> + ?Sized,
    {
        if let Some(hit) = self.entries.lock().get(key) {
            return Ok(hit.clone());
        }
        let built = build()?;
        let owned = key.to_owned();
        let mut entries = self.entries.lock();
        if let Some(landed) = entries.get(key) {
            return Ok(landed.clone());
        }
        let evicted = entries.push(owned, built.clone());
        drop(entries);
        drop(evicted);
        Ok(built)
    }

    /// [`Self::get_or_try_build`] for a build that cannot fail.
    pub(crate) fn get_or_build<Q>(&self, key: &Q, build: impl FnOnce() -> V) -> V
    where
        K: Borrow<Q>,
        Q: Hash + Eq + ToOwned<Owned = K> + ?Sized,
    {
        self.get_or_try_build(key, || Ok::<V, Infallible>(build()))
            .unwrap_or_else(|never| match never {})
    }
}

#[cfg(test)]
mod tests {
    use std::cell::Cell;

    use super::*;

    /// `key` through `memo`, counting builds.
    fn fetch(memo: &Memo<String, usize>, builds: &Cell<usize>, key: &str) -> usize {
        memo.get_or_build(key, || {
            builds.set(builds.get() + 1);
            key.len()
        })
    }

    #[test]
    fn a_hit_does_not_rebuild() {
        let memo = Memo::new(2);
        let builds = Cell::new(0);
        assert_eq!(fetch(&memo, &builds, "abc"), 3);
        assert_eq!(fetch(&memo, &builds, "abc"), 3);
        assert_eq!(builds.get(), 1);
    }

    #[test]
    fn the_least_recently_used_entry_is_evicted() {
        let memo = Memo::new(2);
        let builds = Cell::new(0);
        fetch(&memo, &builds, "a");
        fetch(&memo, &builds, "b");
        fetch(&memo, &builds, "a"); // `a` is now the most recent
        fetch(&memo, &builds, "c"); // evicts `b`
        assert_eq!(builds.get(), 3);
        fetch(&memo, &builds, "a");
        assert_eq!(builds.get(), 3, "`a` survived");
        fetch(&memo, &builds, "b");
        assert_eq!(builds.get(), 4, "`b` was rebuilt");
    }

    #[test]
    fn a_failed_build_is_not_remembered() {
        let memo: Memo<String, usize> = Memo::new(2);
        let builds = Cell::new(0);
        let failed: Result<usize, &str> = memo.get_or_try_build("x", || {
            builds.set(builds.get() + 1);
            Err("no")
        });
        assert_eq!(failed, Err("no"));
        assert_eq!(fetch(&memo, &builds, "x"), 1);
        assert_eq!(builds.get(), 2);
    }

    /// A build that reaches the memo again for its own key stands in for a
    /// racing builder.
    #[test]
    fn a_build_runs_unlocked_and_the_first_value_to_land_wins() {
        let memo: Memo<String, usize> = Memo::new(2);
        let outer = memo.get_or_build("k", || {
            assert!(memo.entries.try_lock().is_some(), "the lock is held");
            memo.get_or_build("k", || 2);
            1
        });
        assert_eq!(outer, 2);
        assert_eq!(memo.get_or_build("k", || 3), 2);
    }

    #[test]
    fn a_zero_capacity_holds_one() {
        let memo = Memo::new(0);
        let builds = Cell::new(0);
        fetch(&memo, &builds, "a");
        fetch(&memo, &builds, "a");
        assert_eq!(builds.get(), 1);
    }
}
