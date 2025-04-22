//! https://infoscience.epfl.ch/entities/publication/106da598-3385-4029-892b-27ea85194046
pub use kast_inference_derive::*;

use crate as kast_inference;

use kast_util::*;
use std::{
    collections::HashSet,
    sync::{Arc, Mutex, OnceLock},
};

#[derive(Clone)]
struct SubsetCacheKey<T: Inferrable> {
    subset: WithSame<MaybeVar<T>>,
    superset: WithSame<MaybeVar<T>>,
}

impl<T: Inferrable> PartialEq for SubsetCacheKey<T> {
    fn eq(&self, other: &Self) -> bool {
        self.subset == other.subset && self.superset == other.superset
    }
}

impl<T: Inferrable> Eq for SubsetCacheKey<T> {}

impl<T: Inferrable> std::hash::Hash for SubsetCacheKey<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.subset.hash(state);
        self.superset.hash(state);
    }
}

#[derive(Default)]
pub struct Context {
    subset_cache: anymap3::Map<dyn anymap3::CloneAny + Send + Sync>,
}

impl Context {
    fn subset_cache<T: Inferrable>(&mut self) -> &mut HashSet<SubsetCacheKey<T>> {
        self.subset_cache.entry().or_default()
    }
}

impl Context {
    fn get<'a>() -> std::sync::MutexGuard<'a, Self> {
        static CONTEXT: OnceLock<Mutex<Context>> = OnceLock::new();
        CONTEXT.get_or_init(Default::default).lock().unwrap()
    }
}

pub trait Same {
    fn is_same(&self, other: &Self) -> bool;
    fn hash(&self, state: &mut impl std::hash::Hasher);
}

macro_rules! impl_same_for {
    ($($ty:ty),* $(,)?) => {
        $(
            impl Same for $ty {
                fn is_same(&self, other: &Self) -> bool {
                    self == other
                }
                fn hash(&self, state: &mut impl std::hash::Hasher) {
                    std::hash::Hash::hash(self, state)
                }
            }
        )*
    };
}

impl_same_for!(u8, u16, u32, u64, u128, usize);
impl_same_for!(i8, i16, i32, i64, i128, isize);
impl_same_for!(String);

#[derive(Debug, Clone)]
pub struct WithSame<T>(T);

impl<T: Same> std::hash::Hash for WithSame<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        <T as Same>::hash(&self.0, state)
    }
}
impl<T: Same> PartialEq for WithSame<T> {
    fn eq(&self, other: &Self) -> bool {
        <T as Same>::is_same(&self.0, &other.0)
    }
}
impl<T: Same> Eq for WithSame<T> {}

struct Data<T: Inferrable> {
    #[allow(dead_code)]
    description: Arc<str>,
    lower_bounds: Vec<MaybeVar<T>>,
    upper_bounds: Vec<MaybeVar<T>>,
}

#[derive(Clone, Same)]
pub enum MaybeVar<T: Inferrable> {
    Var(Var<T>),
    Value(T),
}

impl<T: Inferrable> From<Var<T>> for MaybeVar<T> {
    fn from(var: Var<T>) -> Self {
        Self::Var(var)
    }
}

impl<'a, T: Inferrable> From<&'a Var<T>> for MaybeVar<T> {
    fn from(var: &'a Var<T>) -> Self {
        Self::Var(var.clone())
    }
}

impl<T: Inferrable> From<T> for MaybeVar<T> {
    fn from(value: T) -> Self {
        Self::Value(value)
    }
}

impl<'a, T: Inferrable> From<&'a T> for MaybeVar<T> {
    fn from(value: &'a T) -> Self {
        Self::Value(value.clone())
    }
}

impl<T: Inferrable> MaybeVar<T> {
    fn inferred_bound(&self, bound: Bound) -> eyre::Result<T> {
        Ok(match self {
            MaybeVar::Var(var) => {
                let data = var.data.lock().unwrap();
                let result = match bound {
                    Bound::Lower => data.lower_bounds.iter().try_fold(None, |acc, item| {
                        let item = item.inferred_bound(bound)?;
                        Ok::<_, eyre::Report>(Some(match acc {
                            Some(acc) => T::union(acc, item)?,
                            None => item,
                        }))
                    })?,
                    Bound::Upper => data.upper_bounds.iter().try_fold(None, |acc, item| {
                        let item = item.inferred_bound(bound)?;
                        Ok::<_, eyre::Report>(Some(match acc {
                            Some(acc) => T::intersect(acc, item)?,
                            None => item,
                        }))
                    })?,
                };
                match result {
                    Some(result) => result,
                    None => eyre::bail!("not inferred"),
                }
            }
            MaybeVar::Value(value) => value.clone(),
        })
    }
    pub fn infer_as_subset_of(&self, superset: &Self) -> eyre::Result<()> {
        if !Context::get().subset_cache().insert(SubsetCacheKey {
            subset: WithSame(self.clone()),
            superset: WithSame(superset.clone()),
        }) {
            return Ok(());
        };
        match (self, superset) {
            (MaybeVar::Value(self_value), MaybeVar::Value(superset)) => {
                <T as Inferrable>::infer_as_subset_of(self_value, superset)
            }
            (MaybeVar::Var(subset), superset) => {
                let subset_lower_bounds = {
                    let mut subset_data = subset.data.lock().unwrap();
                    subset_data.upper_bounds.push(superset.clone());
                    subset_data.lower_bounds.clone()
                };
                for lower_bound in subset_lower_bounds {
                    lower_bound.infer_as_subset_of(superset)?;
                }
                Ok(())
            }
            (subset, MaybeVar::Var(superset)) => {
                let superset_upper_bounds = {
                    let mut superset_data = superset.data.lock().unwrap();
                    superset_data.lower_bounds.push(subset.clone());
                    superset_data.upper_bounds.clone()
                };
                for upper_bound in superset_upper_bounds {
                    upper_bound.infer_as_superset_of(subset)?;
                }
                Ok(())
            }
        }
    }
    pub fn infer_as_superset_of(&self, subset: &Self) -> eyre::Result<()> {
        subset.infer_as_subset_of(self)
    }
    pub fn infer_as(&self, value: &Self) -> eyre::Result<()> {
        self.infer_as_subset_of(value)?;
        self.infer_as_superset_of(value)?;
        Ok(())
    }
}

pub trait Inferrable: 'static + Clone + Same + Send + Sync {
    fn infer_as_subset_of(&self, superset: &Self) -> eyre::Result<()>;
    fn infer_as(&self, other: &Self) -> eyre::Result<()> {
        self.infer_as_subset_of(other)?;
        other.infer_as_subset_of(self)?;
        Ok(())
    }
    fn infer_as_superset_of(&self, subset: &Self) -> eyre::Result<()> {
        subset.infer_as_subset_of(self)
    }
    fn union(a: Self, b: Self) -> eyre::Result<Self>;
    fn intersect(a: Self, b: Self) -> eyre::Result<Self>;
}

pub struct Var<T: Inferrable> {
    data: Arc<Mutex<Data<T>>>,
}

impl<T: Inferrable> Clone for Var<T> {
    fn clone(&self) -> Self {
        Self {
            data: self.data.clone(),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Bound {
    Lower,
    Upper,
}

impl<T: Inferrable> Var<T> {
    pub fn new_not_inferred(description: impl AsRef<str>) -> Self {
        let data = Data {
            description: description.as_ref().into(),
            lower_bounds: vec![],
            upper_bounds: vec![],
        };
        Self {
            data: Arc::new(Mutex::new(data)),
        }
    }
    pub fn known(value: T) -> Self {
        let result = Self::new_not_inferred("<known>");
        result.infer_as(MaybeVar::Value(value)).unwrap();
        result
    }
    pub fn infer_as_subset_of(&self, superset: impl Into<MaybeVar<T>>) -> eyre::Result<()> {
        superset.into().infer_as_superset_of(&self.into())
    }
    pub fn infer_as_superset_of(&self, subset: impl Into<MaybeVar<T>>) -> eyre::Result<()> {
        subset.into().infer_as_subset_of(&self.into())
    }
    pub fn infer_as(&self, value: impl Into<MaybeVar<T>>) -> eyre::Result<()> {
        value.into().infer_as(&self.into())
    }
    pub fn inferred(&self) -> eyre::Result<T> {
        self.inferred_bound(Bound::Lower)
    }
    pub fn inferred_bound(&self, bound: Bound) -> eyre::Result<T> {
        MaybeVar::Var(self.clone()).inferred_bound(bound)
    }
}

impl<T: Inferrable> std::fmt::Debug for Var<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // TODO
        write!(f, "<var>")
    }
}

impl<T: Inferrable> Inferrable for Var<T> {
    fn infer_as_subset_of(&self, superset: &Self) -> eyre::Result<()> {
        todo!()
    }
    fn intersect(a: Self, b: Self) -> eyre::Result<Self> {
        todo!()
    }
    fn union(a: Self, b: Self) -> eyre::Result<Self> {
        todo!()
    }
}

impl<T: Inferrable> Same for Var<T> {
    fn is_same(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.data, &other.data)
    }
    fn hash(&self, state: &mut impl std::hash::Hasher) {
        std::hash::Hash::hash(&Arc::as_ptr(&self.data), state)
    }
}

impl<T: Inferrable> From<T> for Var<T> {
    fn from(value: T) -> Self {
        Self::known(value)
    }
}
