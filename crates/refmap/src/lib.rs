use std::collections::HashMap;
use std::hash::Hash;
use std::rc::Rc;
use std::sync::Arc;

pub trait Ref: Clone {
    fn is_same_ref(&self, other: &Self) -> bool;
    fn hash_ref(&self, hasher: &mut impl std::hash::Hasher);
}

impl<T> Ref for Rc<T> {
    fn is_same_ref(&self, other: &Self) -> bool {
        Rc::ptr_eq(self, other)
    }
    fn hash_ref(&self, hasher: &mut impl std::hash::Hasher) {
        Rc::as_ptr(self).hash(hasher)
    }
}

impl<T> Ref for Arc<T> {
    fn is_same_ref(&self, other: &Self) -> bool {
        Arc::ptr_eq(self, other)
    }
    fn hash_ref(&self, hasher: &mut impl std::hash::Hasher) {
        Arc::as_ptr(self).hash(hasher)
    }
}

impl<T> Ref for &'_ T {
    fn is_same_ref(&self, other: &Self) -> bool {
        std::ptr::eq(*self, *other)
    }
    fn hash_ref(&self, hasher: &mut impl std::hash::Hasher) {
        (*self as *const T).hash(hasher)
    }
}

#[derive(Clone)]
struct RefKey<K: Ref>(K);

impl<K: Ref> PartialEq for RefKey<K> {
    fn eq(&self, other: &Self) -> bool {
        self.0.is_same_ref(&other.0)
    }
}
impl<K: Ref> Eq for RefKey<K> {}

impl<K: Ref> Hash for RefKey<K> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash_ref(state)
    }
}

pub struct RefMap<K: Ref, V>(HashMap<RefKey<K>, V>);

impl<K: Ref, V: Clone> Clone for RefMap<K, V> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<K: Ref, V> Default for RefMap<K, V> {
    fn default() -> Self {
        Self::new()
    }
}

impl<K: Ref, V> RefMap<K, V> {
    pub fn new() -> Self {
        Self(HashMap::new())
    }
    pub fn insert(&mut self, key: K, value: V) {
        self.0.insert(RefKey(key), value);
    }
    pub fn get(&self, key: &K) -> Option<&V> {
        self.0.get(&RefKey(key.clone()))
    }
    pub fn get_mut(&mut self, key: K) -> Option<&mut V> {
        self.0.get_mut(&RefKey(key))
    }
    pub fn iter(&self) -> impl Iterator<Item = (&K, &V)> {
        self.0.iter().map(|(RefKey(key), value)| (key, value))
    }
    pub fn iter_mut(&mut self) -> impl Iterator<Item = (&K, &mut V)> {
        self.0.iter_mut().map(|(RefKey(key), value)| (key, value))
    }
}

impl<K: Ref + 'static, V: 'static> IntoIterator for RefMap<K, V> {
    type Item = (K, V);
    type IntoIter = Box<dyn Iterator<Item = (K, V)>>; // TODO impl or concrete
    fn into_iter(self) -> Self::IntoIter {
        Box::new(self.0.into_iter().map(|(RefKey(key), value)| (key, value)))
    }
}
