use std::{
    marker::PhantomData,
    sync::{Arc, Mutex},
};

use super::*;

enum VarState<T> {
    Root { value: Option<Arc<T>> },
    NotRoot { closer_to_root: Arc<Mutex<Self>> },
}

impl<T> VarState<T> {
    fn as_root(&mut self) -> &mut Option<Arc<T>> {
        match self {
            VarState::Root { value } => value,
            VarState::NotRoot { .. } => unreachable!(),
        }
    }
    fn get_root(this: &Arc<Mutex<Self>>) -> Arc<Mutex<Self>> {
        match *this.lock().unwrap() {
            VarState::Root { .. } => this.clone(),
            VarState::NotRoot {
                ref mut closer_to_root,
            } => {
                let root = Self::get_root(closer_to_root);
                *closer_to_root = root.clone();
                root
            }
        }
    }
}

#[derive(Clone)]
pub struct Var<T> {
    state: Arc<Mutex<VarState<T>>>,
}

pub trait Inferrable: Clone {
    fn make_same(a: Self, b: Self) -> Self;
}

impl<T: Inferrable> Var<T> {
    pub fn new() -> Self {
        Self {
            state: Arc::new(Mutex::new(VarState::Root { value: None })),
        }
    }
    pub fn get(&self) -> Option<Arc<T>> {
        VarState::get_root(&self.state)
            .lock()
            .unwrap()
            .as_root()
            .clone()
    }
    pub fn set(&self, value: T) {
        let root = VarState::get_root(&self.state);
        let mut root = root.lock().unwrap();
        let root = root.as_root();
        let new_value = match root.take() {
            Some(prev_value) => T::make_same((*prev_value).clone(), value),
            None => value,
        };
        *root = Some(Arc::new(new_value));
    }
    pub fn make_same(&self, other: &Self) {}
}
