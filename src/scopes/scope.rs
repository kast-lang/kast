use std::sync::atomic::AtomicBool;

use super::*;

#[derive(Default)]
pub struct Locals {
    // TODO insertion order
    id_by_name: HashMap<String, Id>,
    by_id: HashMap<Id, (Symbol, OwnedPlace)>,
}

impl Locals {
    fn new() -> Self {
        Self::default()
    }
    fn insert_place(&mut self, name: Symbol, place: OwnedPlace) {
        self.id_by_name.insert(name.name().to_owned(), name.id());
        self.by_id.insert(name.id(), (name, place));
    }
    fn insert(&mut self, name: Symbol, value: Value, mutability: Mutability) {
        value.name_if_neeeded(&name);
        self.insert_place(name, OwnedPlace::new(value, mutability));
    }
    fn insert_uninitialized(&mut self, name: Symbol, ty: Type, mutability: Mutability) {
        self.insert_place(name, OwnedPlace::new_uninitialized(ty, mutability));
    }
    #[allow(dead_code)]
    fn get(&self, lookup: Lookup<'_>) -> Option<&(Symbol, OwnedPlace)> {
        let id: Id = match lookup {
            Lookup::Name(name) => *self.id_by_name.get(name)?,
            Lookup::Id(id) => id,
        };
        self.by_id.get(&id)
    }
    pub fn iter(&self) -> impl Iterator<Item = &(Symbol, OwnedPlace)> + '_ {
        self.by_id.values()
    }
}

#[derive(Debug, Copy, Clone)]
pub enum ScopeType {
    NonRecursive,
    Recursive,
}

pub struct Scope {
    pub id: Id,
    pub spawn_id: Id,
    pub parent: Option<Parc<Scope>>,
    pub ty: ScopeType,
    closed: AtomicBool,
    closed_event: event_listener::Event,
    pub syntax_definitions: Mutex<Vec<Parc<ast::SyntaxDefinition>>>,
    locals: Mutex<Locals>,
}

impl Drop for Scope {
    fn drop(&mut self) {
        if !*self.closed.get_mut() {
            match self.ty {
                ScopeType::Recursive => {
                    panic!("recursive scope should be closed manually to advance executor")
                }
                ScopeType::NonRecursive => {}
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
    pub fn new(spawn_id: Id, ty: ScopeType, parent: Option<Parc<Self>>) -> Self {
        let id = Id::new();
        tracing::trace!("new scope {id:?} (ty={ty:?})");
        Self {
            id,
            spawn_id,
            parent,
            ty,
            closed: AtomicBool::new(false),
            closed_event: event_listener::Event::new(),
            syntax_definitions: Default::default(),
            locals: Mutex::new(Locals::new()),
        }
    }
    pub fn close(&self) {
        tracing::trace!("close scope {:?}", self.id);
        self.closed.store(true, std::sync::atomic::Ordering::SeqCst);
        self.closed_event.notify(usize::MAX);
    }
    pub fn inspect<R>(&self, f: impl FnOnce(&Locals) -> R) -> R {
        f(&self.locals.lock().unwrap())
    }
    pub fn insert(&self, name: Symbol, value: Value, mutability: Mutability) {
        tracing::trace!("insert into {:?} {:?} = {:?}", self.id, name, value);
        self.locals.lock().unwrap().insert(name, value, mutability);
    }
    pub fn insert_uninitialized(&self, name: Symbol, ty: Type, mutability: Mutability) {
        tracing::trace!(
            "insert into {:?} {:?} = <uninitialized of {ty}>",
            self.id,
            name,
        );
        self.locals
            .lock()
            .unwrap()
            .insert_uninitialized(name, ty, mutability);
    }
    pub fn lookup<'a>(
        &'a self,
        lookup: Lookup<'a>,
        spawn_id: Option<Id>,
    ) -> BoxFuture<'a, Option<(Symbol, PlaceRef)>> {
        tracing::trace!("looking for {lookup} in {:?} (ty={:?})", self.id, self.ty);
        async move {
            loop {
                let was_closed = self.closed.load(std::sync::atomic::Ordering::Relaxed);
                if let Some((symbol, place)) = self.locals.lock().unwrap().get(lookup) {
                    tracing::trace!("found {lookup}");
                    // tracing::trace!("found in {:?} {:?} = {:?}", self.id, symbol, place);
                    return Some((symbol.clone(), place.get_ref()));
                }
                match self.ty {
                    ScopeType::NonRecursive => {
                        tracing::trace!("non recursive not found {lookup}");
                        break;
                    }
                    ScopeType::Recursive => {}
                }
                if was_closed {
                    tracing::trace!("not found in recursive - was closed");
                    break;
                }
                if spawn_id.is_none_or(|spawn_id| spawn_id == self.spawn_id) {
                    tracing::trace!("not awaiting (same spawn id) - break");
                    break;
                }
                // TODO maybe wait for the name, not entire scope?
                self.closed_event.listen().await;
                tracing::trace!("continuing searching for {lookup}");
            }
            if let Some(parent) = &self.parent {
                if let Some(result) = parent.lookup(lookup, spawn_id).await {
                    return Some(result);
                }
            }
            None
        }
        .boxed()
    }
}
