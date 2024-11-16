use std::sync::{Arc, Mutex};

use rand::{thread_rng, Rng};

type Check<T> = Arc<dyn Fn(&T) -> eyre::Result<()> + Send + Sync>;

#[derive(Clone)]
struct Data<T> {
    value: Option<Arc<T>>,
    default_value: Option<Arc<T>>,
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

#[derive(Clone)]
enum VarState<T> {
    Root(Data<T>),
    NotRoot {
        closer_to_root: global_state::Id<Self>,
    },
}

impl<T: Sync + Send + Clone + 'static> VarState<T> {
    fn as_root(&mut self) -> &mut Data<T> {
        match self {
            VarState::Root(data) => data,
            VarState::NotRoot { .. } => unreachable!(),
        }
    }
    fn get_root(this: &global_state::Id<Self>) -> global_state::Id<Self> {
        let mut this_value = this.get();
        let result = match this_value {
            VarState::Root { .. } => this.clone(),
            VarState::NotRoot {
                ref mut closer_to_root,
            } => {
                let root = Self::get_root(closer_to_root);
                *closer_to_root = root.clone();
                root
            }
        };
        this.set(this_value);
        result
    }
}

pub mod global_state {
    use super::*;
    use std::{
        collections::HashMap,
        marker::PhantomData,
        sync::{atomic::AtomicUsize, OnceLock},
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
        index: usize,
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
}

#[derive(Clone)]
pub struct Var<T> {
    state: global_state::Id<VarState<T>>,
}

impl<T: Inferrable> Var<T> {
    pub fn is_same_as(&self, other: &Self) -> bool {
        let self_root = VarState::get_root(&self.state);
        let other_root = VarState::get_root(&other.state);
        global_state::ptr_eq(&self_root, &other_root)
    }
    pub fn add_check(&self, check: impl Fn(&T) -> eyre::Result<()> + Sync + Send + 'static) {
        VarState::get_root(&self.state).modify(|state| {
            let root = state.as_root();
            root.checks.push(Arc::new(check));
            root.run_checks().expect("check failed when adding");
        });
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

pub trait Inferrable: Clone + Send + Sync + 'static {
    fn make_same(a: Self, b: Self) -> eyre::Result<Self>;
}

impl<T: Inferrable> Var<T> {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self {
            state: global_state::Id::new(VarState::Root(Data {
                value: None,
                default_value: None,
                checks: Vec::new(),
            })),
        }
    }
    #[allow(clippy::new_without_default)]
    pub fn new_with_default(default: T) -> Self {
        Self {
            state: global_state::Id::new(VarState::Root(Data {
                value: None,
                default_value: Some(Arc::new(default)),
                checks: Vec::new(),
            })),
        }
    }
    pub fn get_or_default(&self) -> eyre::Result<Option<Arc<T>>> {
        VarState::get_root(&self.state).modify(|root| {
            let root = root.as_root();
            if root.value.is_none() {
                root.value = root.default_value.clone();
                root.run_checks()?;
            }
            Ok(root.value.clone())
        })
    }
    pub fn get(&self) -> Option<Arc<T>> {
        VarState::get_root(&self.state)
            .get()
            .as_root()
            .value
            .clone()
    }
    pub fn set(&self, value: T) -> eyre::Result<()> {
        let root_var = VarState::get_root(&self.state);
        let mut root_data = root_var.get();
        let root = root_data.as_root();
        let new_value = match root.value.take() {
            Some(prev_value) => T::make_same((*prev_value).clone(), value)?,
            None => value,
        };
        root.value = Some(Arc::new(new_value));
        root.run_checks()?;
        root_var.set(root_data);
        Ok(())
    }
    pub fn make_same(&self, other: &Self) -> eyre::Result<()> {
        let root_state = VarState::get_root(&self.state);
        let other_root_state = VarState::get_root(&other.state);
        if global_state::ptr_eq(&root_state, &other_root_state) {
            return Ok(());
        }
        let mut root_locked = root_state.get();
        let root = root_locked.as_root();
        let mut other_root_locked = other_root_state.get();
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
        let common_default_value = if common_value.is_some() {
            None
        } else {
            match (root.default_value.take(), other_root.default_value.take()) {
                (None, None) => None,
                (None, Some(value)) => Some(value),
                (Some(value), None) => Some(value),
                (Some(a), Some(b)) => T::make_same(T::clone(&a), T::clone(&b)).ok().map(Arc::new),
            }
        };
        if thread_rng().gen() {
            other_root.value = common_value;
            other_root.default_value = common_default_value;
            other_root.checks.append(&mut root.checks);
            other_root.run_checks()?;
            root_locked = VarState::NotRoot {
                closer_to_root: other_root_state.clone(),
            };
        } else {
            root.value = common_value;
            root.default_value = common_default_value;
            root.checks.append(&mut other_root.checks);
            root.run_checks()?;
            other_root_locked = VarState::NotRoot {
                closer_to_root: root_state.clone(),
            };
        }
        root_state.set(root_locked);
        other_root_state.set(other_root_locked);
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct MaybeNotInferred<T: Inferrable>(Var<T>);

impl<T: Inferrable> From<T> for MaybeNotInferred<T> {
    fn from(value: T) -> Self {
        Self({
            let var = Var::new();
            var.set(value).unwrap();
            var
        })
    }
}

impl<T: Inferrable> From<Var<T>> for MaybeNotInferred<T> {
    fn from(value: Var<T>) -> Self {
        Self(value)
    }
}

impl<T: Inferrable + try_hash::TryHash> try_hash::TryHash for MaybeNotInferred<T>
where
    <T as try_hash::TryHash>::Error: std::fmt::Debug + std::fmt::Display + Send + Sync,
{
    type Error = eyre::Report;
    fn try_hash(&self, hasher: &mut impl std::hash::Hasher) -> Result<(), Self::Error> {
        match self.inferred_or_default()? {
            Ok(ty) => ty.try_hash(hasher).map_err(|e| eyre::eyre!(e))?,
            Err(_) => eyre::bail!("type is not inferred, fail to hash"),
        }
        Ok(())
    }
}

impl<T: Inferrable + PartialEq> PartialEq for MaybeNotInferred<T> {
    fn eq(&self, other: &Self) -> bool {
        match (self.0.get(), other.0.get()) {
            (Some(a), Some(b)) => a == b,
            _ => self.0.is_same_as(&other.0),
        }
    }
}

impl<T: Inferrable + Eq> Eq for MaybeNotInferred<T> {}

impl<T: Inferrable> MaybeNotInferred<T> {
    pub fn expect_inferred(&self, value: T) -> eyre::Result<()> {
        self.0.set(value)
    }
    pub fn new_not_inferred() -> Self {
        Self(Var::new())
    }
    pub fn new_not_inferred_with_default(default: T) -> Self {
        Self(Var::new_with_default(default))
    }
    /// Get actual value (if it is an inference var)
    pub fn inferred(&self) -> Result<T, &Var<T>> {
        self.0.get().map(|value| (*value).clone()).ok_or(&self.0)
    }
    /// Get actual value (if it is an inference var)
    pub fn inferred_or_default(&self) -> eyre::Result<Result<T, &Var<T>>> {
        Ok(self
            .0
            .get_or_default()?
            .map(|value| (*value).clone())
            .ok_or(&self.0))
    }
    pub fn make_same(&mut self, other: Self) -> eyre::Result<()> {
        *self = Inferrable::make_same(self.clone(), other)?;
        Ok(())
    }
}

impl<T: Inferrable> Inferrable for MaybeNotInferred<T> {
    fn make_same(a: Self, b: Self) -> eyre::Result<Self> {
        a.0.make_same(&b.0)?;
        Ok(a)
    }
}

impl<T: Inferrable + std::fmt::Display> std::fmt::Display for MaybeNotInferred<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0.get() {
            Some(inferred) => inferred.fmt(f),
            None => write!(f, "<not inferred>"),
        }
    }
}
