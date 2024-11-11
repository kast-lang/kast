use std::sync::{Arc, Mutex};

use rand::{thread_rng, Rng};

type Check<T> = Box<dyn Fn(&T) -> eyre::Result<()> + Send + Sync>;

struct Data<T> {
    value: Option<Arc<T>>,
    checks: Vec<Check<T>>,
}

impl<T> Data<T> {
    fn run_checks(&self) -> eyre::Result<()> {
        if let Some(value) = &self.value {
            for check in &self.checks {
                check(value)?;
            }
        }
        Ok(())
    }
}

enum VarState<T> {
    Root(Data<T>),
    NotRoot { closer_to_root: Arc<Mutex<Self>> },
}

impl<T> VarState<T> {
    fn as_root(&mut self) -> &mut Data<T> {
        match self {
            VarState::Root(data) => data,
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
    pub fn add_check(&self, check: impl Fn(&T) -> eyre::Result<()> + Sync + Send + 'static) {
        VarState::get_root(&self.state)
            .lock()
            .unwrap()
            .as_root()
            .checks
            .push(Box::new(check));
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
            state: Arc::new(Mutex::new(VarState::Root(Data {
                value: None,
                checks: Vec::new(),
            }))),
        }
    }
    pub fn get(&self) -> Option<Arc<T>> {
        VarState::get_root(&self.state)
            .lock()
            .unwrap()
            .as_root()
            .value
            .clone()
    }
    pub fn set(&self, value: T) -> eyre::Result<()> {
        let root = VarState::get_root(&self.state);
        let mut root = root.lock().unwrap();
        let root = root.as_root();
        let new_value = match root.value.take() {
            Some(prev_value) => T::make_same((*prev_value).clone(), value)?,
            None => value,
        };
        root.value = Some(Arc::new(new_value));
        root.run_checks()?;
        Ok(())
    }
    pub fn make_same(&self, other: &Self) -> eyre::Result<()> {
        let root_state = VarState::get_root(&self.state);
        let other_root_state = VarState::get_root(&other.state);
        if Arc::ptr_eq(&root_state, &other_root_state) {
            return Ok(());
        }
        let mut root_locked = root_state.lock().unwrap();
        let root = root_locked.as_root();
        let mut other_root_locked = other_root_state.lock().unwrap();
        let other_root = other_root_locked.as_root();
        let value = root.value.take();
        let other_value = other_root.value.take();
        let common_value = match (value, other_value) {
            (None, None) => None,
            (Some(value), None) | (None, Some(value)) => Some(value),
            (Some(self_value), Some(other_value)) => Some(Arc::new(T::make_same(
                T::clone(&self_value),
                T::clone(&other_value),
            )?)),
        };
        if thread_rng().gen() {
            other_root.value = common_value;
            other_root.checks.append(&mut root.checks);
            other_root.run_checks()?;
            *root_locked = VarState::NotRoot {
                closer_to_root: other_root_state.clone(),
            };
        } else {
            root.value = common_value;
            root.checks.append(&mut other_root.checks);
            root.run_checks()?;
            *other_root_locked = VarState::NotRoot {
                closer_to_root: root_state.clone(),
            };
        }
        Ok(())
    }
}
