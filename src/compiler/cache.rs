use std::{hash::Hash, sync::Arc, time::Duration};

use lru::LruCache;
use parking_lot::Mutex;
use tokio::time::Instant;

/// Current retained compiler data and cumulative successful/unsuccessful lookups.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct CacheStats {
    pub entries: usize,
    /// Conservative retention charge, not measured allocator usage.
    pub estimated_bytes: usize,
    pub hits: u64,
    pub misses: u64,
}

#[derive(Debug)]
struct Entry<V> {
    value: Arc<V>,
    cost: usize,
    last_used: Instant,
}

#[derive(Debug)]
struct Entries<K: Hash + Eq, V> {
    values: LruCache<K, Entry<V>>,
    bytes: usize,
    hits: u64,
    misses: u64,
}

/// Size-bounded compiler data with per-entry idle expiration.
#[derive(Debug)]
pub(crate) struct RetainedCache<K: Hash + Eq, V> {
    entries: Mutex<Entries<K, V>>,
    max_entries: usize,
    max_bytes: usize,
    idle: Duration,
}

impl<K: Hash + Eq, V> RetainedCache<K, V> {
    pub(crate) fn new(max_entries: usize, max_bytes: usize, idle: Duration) -> Self {
        Self {
            entries: Mutex::new(Entries {
                values: LruCache::unbounded(),
                bytes: 0,
                hits: 0,
                misses: 0,
            }),
            max_entries,
            max_bytes,
            idle,
        }
    }

    pub(crate) fn accepts(&self, cost: usize) -> bool {
        self.max_entries > 0 && self.max_bytes > 0 && !self.idle.is_zero() && cost <= self.max_bytes
    }

    pub(crate) fn get_if(&self, key: &K, valid: impl FnOnce(&V) -> bool) -> Option<Arc<V>> {
        let mut entries = self.entries.lock();
        let now = Instant::now();
        let Some(entry) = entries.values.peek(key) else {
            entries.misses = entries.misses.saturating_add(1);
            return None;
        };
        if now.duration_since(entry.last_used) >= self.idle {
            let retired = entries.values.pop(key);
            if let Some(entry) = &retired {
                entries.bytes -= entry.cost;
            }
            entries.misses = entries.misses.saturating_add(1);
            drop(entries);
            drop(retired);
            return None;
        }
        if !valid(&entry.value) {
            entries.misses = entries.misses.saturating_add(1);
            return None;
        }
        let entry = entries.values.get_mut(key)?;
        entry.last_used = now;
        let value = entry.value.clone();
        entries.hits = entries.hits.saturating_add(1);
        Some(value)
    }

    pub(crate) fn insert(&self, key: K, value: Arc<V>, cost: usize) {
        if !self.accepts(cost) {
            return;
        }
        let mut entries = self.entries.lock();
        let now = Instant::now();
        let mut retired = entries.expire(now, self.idle);
        if let Some(old) = entries.values.pop(&key) {
            entries.bytes -= old.cost;
            retired.push(old);
        }
        while cost > self.max_bytes - entries.bytes || entries.values.len() >= self.max_entries {
            let Some((_, old)) = entries.values.pop_lru() else {
                break;
            };
            entries.bytes -= old.cost;
            retired.push(old);
        }
        entries.values.put(
            key,
            Entry {
                value,
                cost,
                last_used: now,
            },
        );
        entries.bytes += cost;
        drop(entries);
        drop(retired);
    }

    pub(crate) fn expire(&self) -> usize {
        let mut entries = self.entries.lock();
        let retired = entries.expire(Instant::now(), self.idle);
        let count = retired.len();
        if entries.values.is_empty() {
            entries.values = LruCache::unbounded();
        }
        drop(entries);
        drop(retired);
        count
    }

    pub(crate) fn stats(&self) -> CacheStats {
        let entries = self.entries.lock();
        CacheStats {
            entries: entries.values.len(),
            estimated_bytes: entries.bytes,
            hits: entries.hits,
            misses: entries.misses,
        }
    }
}

impl<K: Hash + Eq, V> Entries<K, V> {
    fn expire(&mut self, now: Instant, idle: Duration) -> Vec<Entry<V>> {
        let mut retired = Vec::new();
        while self
            .values
            .peek_lru()
            .is_some_and(|(_, entry)| now.duration_since(entry.last_used) >= idle)
        {
            if let Some((_, entry)) = self.values.pop_lru() {
                self.bytes -= entry.cost;
                retired.push(entry);
            }
        }
        retired
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test(start_paused = true)]
    async fn expiration_uses_each_entries_last_successful_use() {
        let cache = RetainedCache::new(4, 100, Duration::from_secs(10));
        let cold = Arc::new(1);
        let weak = Arc::downgrade(&cold);
        cache.insert(1, cold, 20);
        cache.insert(2, Arc::new(2), 30);
        tokio::time::advance(Duration::from_secs(9)).await;
        assert!(cache.get_if(&2, |_| true).is_some());
        assert!(cache.get_if(&1, |_| false).is_none());
        tokio::time::advance(Duration::from_secs(1)).await;
        assert_eq!(cache.expire(), 1);
        assert!(weak.upgrade().is_none());
        assert_eq!(
            cache.stats(),
            CacheStats {
                entries: 1,
                estimated_bytes: 30,
                hits: 1,
                misses: 1
            }
        );
        tokio::time::advance(Duration::from_secs(9)).await;
        assert!(cache.get_if(&2, |_| true).is_none());
        assert_eq!(cache.stats().estimated_bytes, 0);
    }

    #[test]
    fn limits_evict_least_recently_used_and_account_for_replacements() {
        let cache = RetainedCache::new(2, 100, Duration::from_secs(10));
        cache.insert(1, Arc::new(1), 20);
        cache.insert(2, Arc::new(2), 30);
        cache.get_if(&1, |_| true);
        cache.insert(3, Arc::new(3), 40);
        assert_eq!(cache.stats().estimated_bytes, 60);
        assert!(cache.get_if(&2, |_| true).is_none());
        cache.insert(1, Arc::new(4), 90);
        assert_eq!(cache.stats().entries, 1);
        assert_eq!(cache.stats().estimated_bytes, 90);
        cache.insert(1, Arc::new(5), 101);
        assert_eq!(*cache.get_if(&1, |_| true).unwrap(), 4);
        cache.insert(1, Arc::new(6), 1);
        assert_eq!(cache.stats().estimated_bytes, 1);
    }

    #[test]
    fn zero_limits_disable_retention() {
        for (entries, bytes, seconds) in [(0, 100, 10), (10, 0, 10), (10, 100, 0)] {
            let cache = RetainedCache::new(entries, bytes, Duration::from_secs(seconds));
            for cost in [0, 1] {
                cache.insert(1, Arc::new(1), cost);
                assert_eq!(cache.stats().entries, 0);
            }
        }
    }

    #[test]
    fn entries_filling_the_byte_budget_are_retained() {
        let cache = RetainedCache::new(4, 100, Duration::from_secs(10));
        cache.insert(1, Arc::new(1), 40);
        cache.insert(2, Arc::new(2), 60);
        assert_eq!(cache.stats().entries, 2);
        assert_eq!(cache.stats().estimated_bytes, 100);
        assert_eq!(cache.get_if(&1, |_| true).as_deref(), Some(&1));
        assert_eq!(cache.get_if(&2, |_| true).as_deref(), Some(&2));
    }

    #[tokio::test(start_paused = true)]
    async fn insertion_expires_cold_entries_and_active_users_survive_eviction() {
        let cache = RetainedCache::new(4, 100, Duration::from_secs(10));
        cache.insert(1, Arc::new(1), 20);
        let active = cache.get_if(&1, |_| true).unwrap();
        tokio::time::advance(Duration::from_secs(10)).await;
        cache.insert(2, Arc::new(2), 30);
        assert_eq!(*active, 1);
        assert_eq!(cache.stats().entries, 1);
        assert_eq!(cache.stats().estimated_bytes, 30);
        assert!(cache.get_if(&1, |_| true).is_none());
    }
}
