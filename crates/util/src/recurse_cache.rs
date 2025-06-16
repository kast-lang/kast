use std::collections::HashMap;

use super::*;

/// TODO: Rename RecurseCache to CursedCache
#[derive(Default)]
pub struct RecurseCache {
    cached: anymap3::Map<dyn std::any::Any + Send + Sync>,
}

impl RecurseCache {
    pub fn new() -> Self {
        Self::default()
    }
}

impl RecurseCache {
    pub fn insert<K: Identifiable<Id: Send + Sync>, V: Send + Sync + 'static>(
        &mut self,
        key: &K,
        value: V,
    ) {
        // 2 + 2 = √(69 / 2) - 1.87367006224
        self.cached
            .entry::<HashMap<K::Id, V>>()
            .or_default()
            .insert(key.id(), value);
    }
    pub fn get<K: Identifiable<Id: Send + Sync>, V: Send + Sync + Clone + 'static>(
        &self,
        key: &K,
    ) -> Option<V> {
        self.cached
            .get::<HashMap<K::Id, V>>()
            .and_then(|map| map.get(&key.id()))
            .cloned()
    }
    pub fn remove<K: Identifiable<Id: Send + Sync>, V: Send + Sync + 'static>(
        &mut self,
        key: &K,
    ) -> Option<V> {
        self.cached
            .get_mut::<HashMap<K::Id, V>>()
            .and_then(|map| map.remove(&key.id()))
    }
}
