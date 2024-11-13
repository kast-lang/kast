use std::sync::Arc;

#[derive(Default)]
pub struct Parc<T: ?Sized>(Arc<T>);

impl<T: ?Sized + std::fmt::Debug> std::fmt::Debug for Parc<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let value: &T = self;
        value.fmt(f)
    }
}

impl<T: ?Sized> From<Parc<T>> for Arc<T> {
    fn from(val: Parc<T>) -> Self {
        val.0
    }
}

impl<T: ?Sized> From<Arc<T>> for Parc<T> {
    fn from(value: Arc<T>) -> Self {
        Self(value)
    }
}

impl<T> Parc<T> {
    pub fn new(value: T) -> Self {
        Self(Arc::new(value))
    }
}

impl<T: ?Sized> std::ops::Deref for Parc<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T: ?Sized> Clone for Parc<T> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<T: ?Sized> PartialEq for Parc<T> {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}

impl<T: ?Sized> Eq for Parc<T> {}

impl<T: ?Sized> std::hash::Hash for Parc<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        Arc::as_ptr(&self.0).hash(state)
    }
}
