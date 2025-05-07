use super::*;
use std::{
    collections::HashMap,
    marker::PhantomData,
    sync::{OnceLock, atomic::AtomicUsize},
};

#[derive(Clone)]
struct ConcreteState<T> {
    vars: HashMap<usize, T>,
}

impl<T> Default for ConcreteState<T> {
    fn default() -> Self {
        Self {
            vars: Default::default(),
        }
    }
}

#[derive(Default, Clone)]
struct State {
    concrete: anymap3::Map<dyn anymap3::CloneAny + Send + Sync>,
}

static STATE: OnceLock<Mutex<State>> = OnceLock::new();
fn state() -> &'static Mutex<State> {
    STATE.get_or_init(Default::default)
}

pub struct Snapshot {
    state: State,
}

pub fn snapshot() -> Snapshot {
    Snapshot {
        state: state().lock().unwrap().clone(),
    }
}

pub fn revert(snapshot: Snapshot) {
    *state().lock().unwrap() = snapshot.state;
}

#[derive(Clone)]
pub struct Id<T> {
    pub index: usize,
    phantom_data: PhantomData<T>,
}

impl<T: Send + Sync + Clone + 'static> Id<T> {
    pub fn new(value: T) -> Self {
        static NEXT_INDEX: AtomicUsize = AtomicUsize::new(0);
        let index = NEXT_INDEX.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        let result = Self {
            index,
            phantom_data: PhantomData,
        };
        result.set(value);
        result
    }
    pub fn set(&self, value: T) {
        state()
            .lock()
            .unwrap()
            .concrete
            .entry::<ConcreteState<T>>()
            .or_default()
            .vars
            .insert(self.index, value);
    }
    pub fn get(&self) -> T {
        state()
            .lock()
            .unwrap()
            .concrete
            .entry::<ConcreteState<T>>()
            .or_default()
            .vars
            .get(&self.index)
            .expect("var not exist???")
            .clone()
    }
    pub fn modify<R>(&self, f: impl FnOnce(&mut T) -> R) -> R {
        let mut value = self.get();
        let result = f(&mut value);
        self.set(value);
        result
    }
}

pub fn ptr_eq<T>(a: &Id<T>, b: &Id<T>) -> bool {
    a.index == b.index
}
