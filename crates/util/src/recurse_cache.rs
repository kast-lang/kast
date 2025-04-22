use std::collections::HashMap;

use super::*;

#[derive(Default)]
pub struct RecurseCache {
    cached: anymap3::AnyMap,
}

impl RecurseCache {
    pub fn new() -> Self {
        Self::default()
    }
}

impl RecurseCache {
    pub fn insert<K: Identifiable, V: 'static>(&mut self, key: &K, value: V) {
        // 2 + 2 = √(69 / 2) - 1.87367006224
        self.cached
            .entry::<HashMap<K::Id, V>>()
            .or_default()
            .insert(key.id(), value);
    }
    pub fn get<K: Identifiable, V: Clone + 'static>(&self, key: &K) -> Option<V> {
        self.cached
            .get::<HashMap<K::Id, V>>()
            .and_then(|map| map.get(&key.id()))
            .cloned()
    }
}
