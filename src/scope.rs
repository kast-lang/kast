use std::sync::atomic::AtomicBool;

use super::*;

#[derive(Default)]
pub struct Locals {
    // TODO insertion order
    names: std::collections::BTreeMap<String, Name>,
    id_by_name: HashMap<String, Id>,
    by_id: HashMap<Id, Value>,
}

impl Locals {
    fn new() -> Self {
        Self::default()
    }
    fn insert(&mut self, name: Name, value: Value) {
        self.id_by_name.insert(name.raw().to_owned(), name.id());
        self.by_id.insert(name.id(), value);
        self.names.insert(name.raw().to_owned(), name);
    }
    fn get(&self, lookup: Lookup<'_>) -> Option<&Value> {
        let id: Id = match lookup {
            Lookup::Name(name) => *self.id_by_name.get(name)?,
            Lookup::Id(id) => id,
        };
        self.by_id.get(&id)
    }
    pub fn iter(&self) -> impl Iterator<Item = (&Name, &Value)> + '_ {
        self.names.iter().map(|(s, name)| {
            let id = self.id_by_name.get(s).unwrap();
            let value = self.by_id.get(id).unwrap();
            (name, value)
        })
    }
}

pub struct Scope {
    pub id: Id,
    pub parent: Option<Parc<Scope>>,
    recursive: bool,
    closed: AtomicBool,
    closed_event: event_listener::Event,
    pub syntax_definitions: Mutex<Vec<Parc<ast::SyntaxDefinition>>>,
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

#[derive(Copy, Clone)]
pub enum Lookup<'a> {
    Name(&'a str),
    Id(Id),
}

impl std::fmt::Display for Lookup<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Lookup::Name(name) => write!(f, "{name:?}"),
            Lookup::Id(id) => write!(f, "id#{id}"),
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
    pub fn insert(&self, name: Name, value: Value) {
        self.locals.lock().unwrap().insert(name, value);
    }
    pub fn extend(&self, values: impl IntoIterator<Item = (Parc<Binding>, Value)>) {
        for (binding, value) in values {
            self.insert(binding.name.clone(), value);
        }
    }
    pub fn get_nowait(&self, lookup: Lookup<'_>) -> Option<Value> {
        self.get_impl(lookup, false).now_or_never().unwrap()
    }
    pub fn get_impl<'a>(
        &'a self,
        lookup: Lookup<'a>,
        do_await: bool,
    ) -> BoxFuture<'a, Option<Value>> {
        tracing::trace!("looking for {lookup} in {:?}", self.id);
        async move {
            loop {
                let was_closed = self.closed.load(std::sync::atomic::Ordering::Relaxed);
                if let Some(value) = self.locals.lock().unwrap().get(lookup).cloned() {
                    tracing::trace!("found {lookup} in resursive={:?}", self.recursive);
                    return Some(value);
                }
                if !self.recursive {
                    tracing::trace!("non recursive not found {lookup}");
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
                tracing::trace!("continuing searching for {lookup}");
            }
            if let Some(parent) = &self.parent {
                if let Some(value) = parent.get_impl(lookup, do_await).await {
                    return Some(value);
                }
            }
            None
        }
        .boxed()
    }
}
