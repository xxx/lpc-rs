use std::{
    hash::{Hash, Hasher},
    rc::Rc,
    sync::{
        Arc,
        atomic::{AtomicUsize, Ordering},
    },
    thread,
};

use imbl::HashMap;

struct Tracked {
    clones: Arc<AtomicUsize>,
    drops: Arc<AtomicUsize>,
}

impl Clone for Tracked {
    fn clone(&self) -> Self {
        self.clones.fetch_add(1, Ordering::SeqCst);
        Self {
            clones: self.clones.clone(),
            drops: self.drops.clone(),
        }
    }
}

impl Drop for Tracked {
    fn drop(&mut self) {
        self.drops.fetch_add(1, Ordering::SeqCst);
    }
}

fn tracked_map(count: usize) -> (HashMap<usize, Tracked>, Arc<AtomicUsize>, Arc<AtomicUsize>) {
    let clones = Arc::new(AtomicUsize::new(0));
    let drops = Arc::new(AtomicUsize::new(0));
    let map = (0..count)
        .map(|id| {
            (
                id,
                Tracked {
                    clones: clones.clone(),
                    drops: drops.clone(),
                },
            )
        })
        .collect();
    (map, clones, drops)
}

#[test]
fn empty_map_has_no_disposal_chunks() {
    let map = HashMap::<usize, usize>::new();
    assert!(map.into_drop_chunks().is_empty());
}

#[test]
fn unique_map_transfers_values_without_cloning_or_early_destruction() {
    let (map, clones, drops) = tracked_map(512);
    let clones_before = clones.load(Ordering::SeqCst);
    let drops_before = drops.load(Ordering::SeqCst);
    let chunks = map.into_drop_chunks();

    assert!(!chunks.is_empty());
    assert_eq!(clones.load(Ordering::SeqCst), clones_before);
    assert_eq!(drops.load(Ordering::SeqCst), drops_before);
    drop(chunks);
    assert_eq!(drops.load(Ordering::SeqCst) - drops_before, 512);
}

#[test]
fn disposing_a_shared_root_keeps_the_other_snapshot_readable() {
    let (map, clones, drops) = tracked_map(512);
    let snapshot = map.clone();
    let clones_before = clones.load(Ordering::SeqCst);
    let drops_before = drops.load(Ordering::SeqCst);
    drop(map.into_drop_chunks());

    assert_eq!(snapshot.len(), 512);
    assert!((0..512).all(|id| snapshot.contains_key(&id)));
    assert_eq!(clones.load(Ordering::SeqCst), clones_before);
    assert_eq!(drops.load(Ordering::SeqCst), drops_before);
    drop(snapshot);
    assert_eq!(drops.load(Ordering::SeqCst) - drops_before, 512);
}

#[test]
fn disposing_unique_roots_preserves_their_shared_descendants() {
    let values: Vec<_> = (0..512).map(Arc::new).collect();
    let weak: Vec<_> = values.iter().map(Arc::downgrade).collect();
    let map: HashMap<_, _> = values.into_iter().enumerate().collect();
    let mut snapshot = map.clone();
    drop(snapshot.remove(&0));
    snapshot.insert(512, Arc::new(512));
    drop(map.into_drop_chunks());

    assert!(weak[0].upgrade().is_none());
    for (id, value) in weak.iter().enumerate().skip(1) {
        assert!(value.upgrade().is_some());
        assert_eq!(snapshot.get(&id).map(Arc::as_ref), Some(&id));
    }
    assert_eq!(snapshot.get(&512).map(Arc::as_ref), Some(&512));
    drop(snapshot);
    assert!(weak.iter().all(|value| value.upgrade().is_none()));
}

#[derive(Clone, Eq, PartialEq)]
struct Collision(usize);

impl Hash for Collision {
    fn hash<H: Hasher>(&self, state: &mut H) {
        0_u8.hash(state);
    }
}

#[test]
fn collision_nodes_keep_shared_values_and_reclaim_removed_values() {
    let values: Vec<_> = (0..128).map(Arc::new).collect();
    let weak: Vec<_> = values.iter().map(Arc::downgrade).collect();
    let map: HashMap<_, _> = values
        .into_iter()
        .enumerate()
        .map(|(id, value)| (Collision(id), value))
        .collect();
    let mut snapshot = map.clone();
    drop(snapshot.remove(&Collision(7)));
    drop(map.into_drop_chunks());

    assert!(weak[7].upgrade().is_none());
    for (id, value) in weak.iter().enumerate().filter(|(id, _)| *id != 7) {
        assert!(value.upgrade().is_some());
        assert_eq!(snapshot.get(&Collision(id)).map(Arc::as_ref), Some(&id));
    }
    drop(snapshot);
    assert!(weak.iter().all(|value| value.upgrade().is_none()));
}

#[test]
fn chunks_can_be_destroyed_on_two_threads_exactly_once() {
    let (map, clones, drops) = tracked_map(1024);
    let clones_before = clones.load(Ordering::SeqCst);
    let drops_before = drops.load(Ordering::SeqCst);
    let mut chunks = map.into_drop_chunks();
    assert!(chunks.len() > 1);
    let other = chunks.split_off(chunks.len() / 2);
    thread::scope(|scope| {
        scope.spawn(move || drop(other));
        drop(chunks);
    });

    assert_eq!(clones.load(Ordering::SeqCst), clones_before);
    assert_eq!(drops.load(Ordering::SeqCst) - drops_before, 1024);
}

#[test]
fn non_send_values_can_be_destroyed_locally() {
    let value = Rc::new(17);
    let weak = Rc::downgrade(&value);
    let map: HashMap<_, _> = std::iter::once((0, value)).collect();
    let chunks = map.into_drop_chunks();
    assert!(weak.upgrade().is_some());
    drop(chunks);
    assert!(weak.upgrade().is_none());
}
