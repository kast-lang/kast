use super::*;

#[derive(TryHash, PartialEq, Eq, Clone)]
struct Key(#[try_hash] Type);

impl Key {
    fn new(ty: Type) -> eyre::Result<Self> {
        let err = eyre!("context ty: {ty}");
        let key = Self(ty);
        key.try_hash(&mut std::hash::DefaultHasher::new())
            .map_err(|e| eyre!(e))
            .wrap_err(err)?;
        Ok(key)
    }
}

impl std::hash::Hash for Key {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.try_hash(state).unwrap()
    }
}

#[derive(Default, Clone)]
pub struct State {
    contexts: HashMap<Key, Value>,
}

impl State {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn insert(&mut self, context: Value) -> eyre::Result<()> {
        let key = Key::new(context.ty())?;
        self.contexts.insert(key, context);
        Ok(())
    }
    pub fn get(&self, ty: Type) -> eyre::Result<Option<Value>> {
        let key = Key::new(ty)?;
        Ok(self.contexts.get(&key).cloned())
    }
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq)]
pub struct ContextsData {
    // TODO multiset?
    pub types: std::collections::BTreeSet<Type>,
    pub growable: bool,
}

#[derive(Debug, Clone, TryHash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Contexts(#[try_hash] inference::MaybeNotInferred<ContextsData>);

impl TryHash for ContextsData {
    type Error = eyre::Report;
    fn try_hash(&self, hasher: &mut impl std::hash::Hasher) -> Result<(), Self::Error> {
        if self.growable {
            eyre::bail!("cant hash growable contexts");
        }
        for ty in &self.types {
            ty.try_hash(hasher)?;
        }
        Ok(())
    }
}

impl std::fmt::Display for ContextsData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (index, ty) in self.types.iter().enumerate() {
            if index != 0 {
                write!(f, " | ")?;
            }
            write!(f, "{ty}")?;
        }
        if self.growable {
            if !self.types.is_empty() {
                write!(f, " | ")?;
            }
            write!(f, "_")?;
        }
        Ok(())
    }
}

impl std::fmt::Display for Contexts {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl Contexts {
    pub fn empty() -> Self {
        Self::from_list([])
    }
    pub fn from_list(list: impl IntoIterator<Item = Type>) -> Self {
        Self(inference::MaybeNotInferred::new_set(ContextsData {
            types: list.into_iter().collect(),
            growable: false,
        }))
    }
    pub fn is_empty(&self) -> bool {
        let data = self.0.inferred().unwrap();
        !data.growable && data.types.is_empty()
    }
    pub fn new_not_inferred() -> Self {
        Self(inference::MaybeNotInferred::new_set(ContextsData {
            types: Default::default(),
            growable: true,
        }))
    }
}

impl ContextsData {
    pub fn extend_with(&mut self, that: &Self) -> eyre::Result<()> {
        // TODO multiple occurences
        for ty in &that.types {
            if !self.types.contains(ty) {
                if !self.growable {
                    eyre::bail!("context not listed: {ty}");
                }
                self.types.insert(ty.clone());
            }
        }
        Ok(())
    }
}

impl SubstituteBindings for ContextsData {
    fn substitute_bindings(self, kast: &Kast) -> Self {
        Self {
            types: self
                .types
                .clone()
                .into_iter()
                .map(|ty| ty.substitute_bindings(kast))
                .collect(),
            growable: self.growable,
        }
    }
}

impl SubstituteBindings for Contexts {
    fn substitute_bindings(self, kast: &Kast) -> Self {
        Self(inference::MaybeNotInferred::new_set(
            self.0.inferred().unwrap().substitute_bindings(kast),
        ))
    }
}

impl Inferrable for ContextsData {
    fn make_same(mut a: Self, mut b: Self) -> eyre::Result<Self> {
        a.extend_with(&b)?;
        b.extend_with(&a)?;
        Ok(Self {
            types: a.types,
            growable: a.growable && b.growable,
        })
    }
}

impl Inferrable for Contexts {
    fn make_same(a: Self, b: Self) -> eyre::Result<Self> {
        let mut a = a;
        a.0.make_same(b.0)?;
        Ok(a)
    }
}
