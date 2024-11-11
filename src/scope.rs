use std::sync::atomic::AtomicBool;

use super::*;

pub type Locals = HashMap<String, Value>;

pub struct Scope {
    id: Id,
    pub parent: Option<Arc<Scope>>,
    recursive: bool,
    closed: AtomicBool,
    closed_event: event_listener::Event,
    pub syntax_definitions: Mutex<Vec<Arc<ast::SyntaxDefinition>>>,
    locals: Mutex<Locals>,
}

impl Drop for Scope {
    fn drop(&mut self) {
        if !*self.closed.get_mut() {
            if self.recursive {
                panic!("recursive scope should be closed manually to advance executor");
            }
            self.close();
        }
    }
}

impl Scope {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self::new_impl(false)
    }
    fn new_impl(recursive: bool) -> Self {
        let id = Id::new();
        tracing::trace!("new scope {id:?} (recursive={recursive:?})");
        Self {
            id,
            parent: None,
            recursive,
            closed: AtomicBool::new(false),
            closed_event: event_listener::Event::new(),
            syntax_definitions: Default::default(),
            locals: Mutex::new(Locals::new()),
        }
    }
    pub fn recursive() -> Self {
        Self::new_impl(true)
    }
    pub fn close(&self) {
        tracing::trace!("close scope {:?}", self.id);
        self.closed.store(true, std::sync::atomic::Ordering::SeqCst);
        self.closed_event.notify(usize::MAX);
    }
    pub fn inspect<R>(&self, f: impl FnOnce(&Locals) -> R) -> R {
        f(&self.locals.lock().unwrap())
    }
    pub fn insert(&self, name: String, value: Value) {
        self.locals.lock().unwrap().insert(name, value);
    }
    pub fn extend(&self, values: impl IntoIterator<Item = (Arc<Binding>, Value)>) {
        for (binding, value) in values {
            self.insert(binding.name.raw().to_owned(), value);
        }
    }
    pub fn get_nowait(&self, name: &str) -> Option<Value> {
        self.get_impl(name, false).now_or_never().unwrap()
    }
    pub fn get_impl<'a>(&'a self, name: &'a str, do_await: bool) -> BoxFuture<'a, Option<Value>> {
        tracing::trace!("looking for {name:?} in {:?}", self.id);
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
                if !do_await {
                    break;
                }
                // TODO maybe wait for the name, not entire scope?
                self.closed_event.listen().await;
                tracing::trace!("continuing searching for {name:?}");
            }
            if let Some(parent) = &self.parent {
                if let Some(value) = parent.get_impl(name, do_await).await {
                    return Some(value);
                }
            }
            None
        }
        .boxed()
    }
}
