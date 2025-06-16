use async_trait::async_trait;
use kast_util::*;
use rand::{thread_rng, Rng};
use std::sync::{Arc, Mutex};

mod checks;
mod default;
pub mod global_state;

pub use self::checks::{Check, CheckResult};
use self::default::*;

#[derive(Clone)]
struct Data<T> {
    #[allow(dead_code)]
    description: Arc<str>,
    value: Option<Arc<T>>,
    default_value: DefaultValue<T>,
    checks: Vec<Check<T>>,
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

#[derive(Clone)]
pub struct Var<T> {
    state: global_state::Id<VarState<T>>,
}

impl<T: Sync + Send + Clone + 'static> Identifiable for Var<T> {
    type Id = usize;
    fn id(&self) -> Self::Id {
        VarState::get_root(&self.state).index
    }
}

impl<T: Inferrable> Var<T> {
    pub fn is_same_as(&self, other: &Self) -> bool {
        let self_root = VarState::get_root(&self.state);
        let other_root = VarState::get_root(&other.state);
        global_state::ptr_eq(&self_root, &other_root)
    }
    pub fn add_check(
        &self,
        check: impl Fn(&T) -> eyre::Result<CheckResult> + Sync + Send + 'static,
    ) -> eyre::Result<()> {
        VarState::get_root(&self.state).modify(|state| {
            let root = state.as_root();
            root.checks.push(Arc::new(check));
            root.run_checks()
        })
    }
}

impl<T: Inferrable + std::fmt::Debug> std::fmt::Debug for Var<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Var(")?;
        match self.get() {
            Some(inferred) => <T as std::fmt::Debug>::fmt(&inferred, f)?,
            None => write!(f, "_")?,
        }
        write!(f, ")")
    }
}

#[async_trait]
pub trait Inferrable: Clone + Send + Sync + PartialEq + 'static {
    async fn await_fully_inferred(&self, cache: &mut RecurseCache) -> eyre::Result<()>;
    fn make_same(a: Self, b: Self) -> eyre::Result<Self>;
}

impl<T: Inferrable> Var<T> {
    #[allow(clippy::new_without_default)]
    pub fn new(description: &str) -> Self {
        Self {
            state: global_state::Id::new(VarState::Root(Data {
                description: description.into(),
                value: None,
                default_value: DefaultValue::None,
                checks: Vec::new(),
            })),
        }
    }
    #[allow(clippy::new_without_default)]
    pub fn new_with_default(description: &str, default: T) -> Self {
        Self {
            state: global_state::Id::new(VarState::Root(Data {
                description: description.into(),
                value: None,
                default_value: DefaultValue::Some(Arc::new(default)),
                checks: Vec::new(),
            })),
        }
    }
    pub fn default(&self) -> Option<Arc<T>> {
        VarState::get_root(&self.state)
            .get()
            .as_root()
            .default_value
            .get()
    }
    pub fn get_or_default(&self) -> eyre::Result<Option<Arc<T>>> {
        VarState::get_root(&self.state).modify(|root| {
            let root = root.as_root();
            if root.value.is_none() {
                root.value = root.default_value.get();
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
            Some(prev_value) => {
                if *prev_value == value {
                    root.value = Some(prev_value);
                    return Ok(());
                }
                T::make_same((*prev_value).clone(), value)?
            }
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
            DefaultValue::None
        } else {
            DefaultValue::common(
                std::mem::take(&mut root.default_value),
                std::mem::take(&mut other_root.default_value),
            )
        };
        if thread_rng().r#gen() {
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
    async fn await_inferred(&self) -> eyre::Result<T> {
        let (sender, receiver) = async_oneshot::oneshot();
        let sender = Mutex::new(sender);
        self.add_check(move |inferred| {
            match sender.lock().unwrap().send(inferred.clone()) {
                Ok(()) | Err(async_oneshot::Closed()) => {}
            }
            Ok(CheckResult::Completed)
        })?;
        match receiver.await {
            Ok(inferred) => Ok(inferred),
            Err(async_oneshot::Closed()) => {
                eyre::bail!("value was never inferred");
            }
        }
    }
    async fn await_fully_inferred(&self, cache: &mut RecurseCache) -> eyre::Result<()> {
        if let Some(()) = cache.get(self) {
            return Ok(());
        }
        let inferred = self.await_inferred().await?;
        cache.insert(self, ());
        inferred.await_fully_inferred(cache).await?;
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct MaybeNotInferred<T: Inferrable>(Var<T>);

impl<T: Inferrable> MaybeNotInferred<T> {
    pub fn map(
        self,
        mut f: impl FnMut(T, &mut RecurseCache) -> MaybeNotInferred<T>,
        cache: &mut RecurseCache,
    ) -> Self {
        let inferred = match self.inferred() {
            Ok(inferred) => inferred,
            Err(_) => {
                if let Some(default) = self.var().default() {
                    let mapped_default = f((*default).clone(), cache);
                    let mapped_default = mapped_default
                        .inferred()
                        .unwrap_or_else(|_| panic!("mapped default is not inferred"));
                    return Self(Var::new_with_default("mapped", mapped_default));
                }
                return self;
            }
        };
        if let Some(result) = cache.get(self.var()) {
            return result;
        }
        cache.insert(self.var(), self.clone());
        let result: Self = f(inferred, cache);
        cache.insert(self.var(), result.clone());
        result
    }
}

impl<T: Inferrable> From<T> for MaybeNotInferred<T> {
    fn from(value: T) -> Self {
        Self({
            let var = Var::new("already inferred");
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
            Err(_) => eyre::bail!(
                "{:?} is not inferred, fail to hash",
                std::any::type_name::<T>(),
            ),
        }
        Ok(())
    }
}

impl<T: Inferrable + PartialOrd> PartialOrd for MaybeNotInferred<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        T::partial_cmp(&*self.0.get()?, &*other.0.get()?)
    }
}

impl<T: Inferrable + Ord> Ord for MaybeNotInferred<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match self.partial_cmp(other) {
            Some(result) => result,
            None => throw_catchable(eyre::eyre!("cant ord not inferred types")),
        }
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
    pub fn var(&self) -> &Var<T> {
        &self.0
    }
    pub fn infer_as(&self, value: T) -> eyre::Result<()> {
        self.0.set(value)
    }
    pub fn new_set(value: T) -> Self {
        let var = Var::new("set");
        var.set(value).unwrap();
        Self(var)
    }
    pub fn new_not_inferred(description: &str) -> Self {
        Self(Var::new(description))
    }
    pub fn new_not_inferred_with_default(description: &str, default: T) -> Self {
        Self(Var::new_with_default(description, default))
    }
    pub fn expect_inferred(&self) -> eyre::Result<T> {
        self.inferred().map_err(|_| eyre::eyre!("var not inferred"))
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

#[async_trait]
impl<T: Inferrable> Inferrable for MaybeNotInferred<T> {
    fn make_same(a: Self, b: Self) -> eyre::Result<Self> {
        a.0.make_same(&b.0)?;
        Ok(a)
    }
    async fn await_fully_inferred(&self, cache: &mut RecurseCache) -> eyre::Result<()> {
        self.0.await_fully_inferred(cache).await
    }
}

impl<T: Inferrable + crate::ShowShort> crate::ShowShort for MaybeNotInferred<T> {
    fn show_short(&self) -> &'static str {
        match self.0.get() {
            Some(inferred) => inferred.show_short(),
            None => "_",
        }
    }
}

impl<T: Inferrable + std::fmt::Display> std::fmt::Display for MaybeNotInferred<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        thread_local! { static CACHE: std::cell::RefCell<(usize, Option<crate::RecurseCache>)> = Default::default(); }
        let cached = CACHE.with_borrow_mut(|(level, cache)| {
            if *level == 0 {
                assert!(cache.is_none());
                *cache = Some(crate::RecurseCache::new());
            }
            *level += 1;
            let cache = cache.as_mut().unwrap();
            match cache.get::<Var<T>, ()>(self.var()) {
                Some(_cached) => true,
                None => {
                    cache.insert::<Var<T>, ()>(self.var(), ());
                    false
                }
            }
        });
        let result = if cached {
            write!(f, "<recursive>")
        } else {
            match self.0.get() {
                Some(inferred) => inferred.fmt(f),
                None => write!(f, "_"),
            }
        };
        CACHE.with_borrow_mut(|(level, cache)| {
            if !cached {
                let removed = cache.as_mut().unwrap().remove::<Var<T>, ()>(self.var());
                assert!(removed.is_some());
            }
            *level -= 1;
            if *level == 0 {
                assert!(cache.take().is_some());
            }
        });
        result
    }
}

#[async_trait]
impl<T: Inferrable> Inferrable for Option<T> {
    fn make_same(a: Self, b: Self) -> eyre::Result<Self> {
        match (a, b) {
            (None, None) => Ok(None),
            (Some(a), Some(b)) => Ok(Some(T::make_same(a, b)?)),
            (Some(_), None) => eyre::bail!("some != none"),
            (None, Some(_)) => eyre::bail!("none != some"),
        }
    }
    async fn await_fully_inferred(&self, cache: &mut RecurseCache) -> eyre::Result<()> {
        if let Some(value) = self {
            value.await_fully_inferred(cache).await?;
        }
        Ok(())
    }
}

#[async_trait]
impl<T: Inferrable> Inferrable for Box<T> {
    fn make_same(a: Self, b: Self) -> eyre::Result<Self> {
        Ok(Box::new(Inferrable::make_same(*a, *b)?))
    }
    async fn await_fully_inferred(&self, cache: &mut RecurseCache) -> eyre::Result<()> {
        T::await_fully_inferred(self, cache).await
    }
}

#[async_trait]
impl<T: Inferrable> Inferrable for Vec<T> {
    fn make_same(a: Self, b: Self) -> eyre::Result<Self> {
        if a.len() != b.len() {
            eyre::bail!("length mismatch, {} vs {}", a.len(), b.len());
        }
        a.into_iter()
            .zip(b)
            .map(|(a, b)| Inferrable::make_same(a, b))
            .collect()
    }
    async fn await_fully_inferred(&self, cache: &mut RecurseCache) -> eyre::Result<()> {
        for value in self {
            value.await_fully_inferred(cache).await?;
        }
        Ok(())
    }
}

#[async_trait]
impl<T: Inferrable> Inferrable for Tuple<T> {
    async fn await_fully_inferred(&self, cache: &mut RecurseCache) -> eyre::Result<()> {
        for value in self.values() {
            value.await_fully_inferred(cache).await?;
        }
        Ok(())
    }
    fn make_same(a: Self, b: Self) -> eyre::Result<Self> {
        let mut result = Tuple::empty();
        for (member, (a, b)) in a.zip(b)?.into_iter() {
            let value = Inferrable::make_same(a, b)?;
            result.add_member(member, value);
        }
        Ok(result)
    }
}
