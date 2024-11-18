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
