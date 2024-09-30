use std::sync::{Arc, Mutex};

use rand::{thread_rng, Rng};

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

impl<T> Var<T> {
    pub fn is_same_as(&self, other: &Self) -> bool {
        let self_root = VarState::get_root(&self.state);
        let other_root = VarState::get_root(&other.state);
        Arc::ptr_eq(&self_root, &other_root)
    }
}

impl<T: Inferrable + std::fmt::Debug> std::fmt::Debug for Var<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Var(")?;
        match self.get() {
            Some(inferred) => <T as std::fmt::Debug>::fmt(&inferred, f)?,
            None => write!(f, "<not inferred>")?,
        }
        write!(f, ")")
    }
}

pub trait Inferrable: Clone {
    fn make_same(a: Self, b: Self) -> eyre::Result<Self>;
}

impl<T: Inferrable> Var<T> {
    #[allow(clippy::new_without_default)]
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
    pub fn set(&self, value: T) -> eyre::Result<()> {
        let root = VarState::get_root(&self.state);
        let mut root = root.lock().unwrap();
        let root = root.as_root();
        let new_value = match root.take() {
            Some(prev_value) => T::make_same((*prev_value).clone(), value)?,
            None => value,
        };
        *root = Some(Arc::new(new_value));
        Ok(())
    }
    pub fn make_same(&self, other: &Self) -> eyre::Result<()> {
        let root_state = VarState::get_root(&self.state);
        let other_root_state = VarState::get_root(&other.state);
        if Arc::ptr_eq(&root_state, &other_root_state) {
            return Ok(());
        }
        let mut root = root_state.lock().unwrap();
        let mut other_root = other_root_state.lock().unwrap();
        let value = root.as_root().take();
        let other_value = other_root.as_root().take();
        let common_value = match (value, other_value) {
            (None, None) => None,
            (Some(value), None) | (None, Some(value)) => Some(value),
            (Some(self_value), Some(other_value)) => Some(Arc::new(T::make_same(
                T::clone(&self_value),
                T::clone(&other_value),
            )?)),
        };
        if thread_rng().gen() {
            *other_root.as_root() = common_value;
            *root = VarState::NotRoot {
                closer_to_root: other_root_state.clone(),
            };
        } else {
            *root.as_root() = common_value;
            *other_root = VarState::NotRoot {
                closer_to_root: root_state.clone(),
            };
        }
        Ok(())
    }
}
