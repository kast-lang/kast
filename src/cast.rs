use super::*;

#[derive(TryHash, PartialEq, Eq, Clone)]
struct Key {
    #[try_hash]
    value: Value,
    #[try_hash]
    target: Value,
}

impl Key {
    fn new(value: Value, target: Value) -> eyre::Result<Self> {
        let err = eyre!("cast key: value = {}, target = {}", value, target);
        let key = Self { value, target };
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

#[derive(Default)]
pub struct CastMap {
    map: HashMap<Key, Value>,
}

impl CastMap {
    #[allow(dead_code)]
    pub fn new() -> Self {
        Self::default()
    }
    pub fn impl_cast(&mut self, value: Value, target: Value, r#impl: Value) -> eyre::Result<()> {
        self.map.insert(Key::new(value, target)?, r#impl);
        Ok(())
    }
    pub fn cast(&self, value: Value, target: &Value) -> eyre::Result<Result<Value, Value>> {
        let key = Key::new(value.clone(), target.clone())?;
        #[allow(clippy::single_match)]
        match target {
            Value::Type(ty) => match ty.inferred() {
                Ok(InferredType::Type) => {
                    return Ok(value.expect_type().map(Value::Type).map_err(|e| e.value))
                }
                _ => {}
            },
            _ => {}
        }
        Ok(self.map.get(&key).cloned().ok_or(value))
    }
}
