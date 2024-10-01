use std::sync::atomic::AtomicBool;

use super::*;

pub struct Scope {
    pub parent: Option<Arc<Scope>>,
    recursive: bool,
    closed: AtomicBool,
    closed_notify: async_notify::Notify,
    pub locals: Mutex<HashMap<String, Value>>,
}

impl Scope {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self::new_impl(false)
    }
    fn new_impl(recursive: bool) -> Self {
        Self {
            parent: None,
            recursive,
            closed: AtomicBool::new(false),
            closed_notify: async_notify::Notify::new(),
            locals: Mutex::new(HashMap::new()),
        }
    }
    pub fn recursive() -> Self {
        Self::new_impl(true)
    }
    pub fn close(&self) {
        self.closed.store(true, std::sync::atomic::Ordering::SeqCst);
        self.closed_notify.notify();
    }
    pub fn get<'a>(&'a self, name: &'a str) -> BoxFuture<'a, Option<Value>> {
        async move {
            loop {
                let was_closed = self.closed.load(std::sync::atomic::Ordering::Relaxed);
                if let Some(value) = self.locals.lock().unwrap().get(name).cloned() {
                    tracing::trace!("found {name:?} in resursive={:?}", self.recursive);
                    return Some(value);
                }
                if !self.recursive {
                    tracing::trace!("non recursive not found {name:?}");
                    break;
                }
                if was_closed {
                    break;
                }
                // TODO maybe wait for the name, not entire scope?
                self.closed_notify.notified().await
            }
            if let Some(parent) = &self.parent {
                if let Some(value) = parent.get(name).await {
                    return Some(value);
                }
            }
            None
        }
        .boxed()
    }
}
